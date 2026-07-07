import { spawn } from "node:child_process";
import {
  AnalyzePayloadV2, CapabilitiesPayload, ListOnchainPayload,
  parseAnalyzePayload, parseCapabilities, parseListOnchain
} from "../core/schema";

export type AnalyzeScope = { kind: "workspace" } | { kind: "module"; moduleName: string };

export type AnalyzerErrorKind =
  | "not-found"        // binary missing (ENOENT)
  | "ghc-mismatch"     // .hie files built by a different GHC
  | "build-failed"     // cabal build inside plustan failed
  | "crash"            // GHC panic
  | "no-json";         // anything else that produced no usable JSON

export class AnalyzerError extends Error {
  constructor(readonly kind: AnalyzerErrorKind, message: string) {
    super(message);
    this.name = "AnalyzerError";
  }
}

export interface SpawnConfig {
  binaryPath: string;
  /** Args inserted before the subcommand — empty in production, used by tests to run `node fake-plustan.js …`. */
  binaryPrefixArgs: string[];
  cwd: string;
  hieDir: string;
  extraArgs: string[];
}

export interface AnalyzerClient {
  capabilities(signal?: AbortSignal): Promise<CapabilitiesPayload>;
  listOnchain(signal?: AbortSignal): Promise<ListOnchainPayload>;
  analyze(scope: AnalyzeScope, signal?: AbortSignal): Promise<AnalyzePayloadV2>;
}

export class SpawnAnalyzerClient implements AnalyzerClient {
  constructor(
    private readonly getConfig: () => SpawnConfig,
    private readonly log: (line: string) => void
  ) {}

  async capabilities(signal?: AbortSignal): Promise<CapabilitiesPayload> {
    return parseCapabilities(await this.runJson(["capabilities"], signal));
  }

  async listOnchain(signal?: AbortSignal): Promise<ListOnchainPayload> {
    const config = this.getConfig();
    return parseListOnchain(await this.runJson(["list-onchain", "--json", "--hiedir", config.hieDir], signal));
  }

  async analyze(scope: AnalyzeScope, signal?: AbortSignal): Promise<AnalyzePayloadV2> {
    const config = this.getConfig();
    const args = ["analyze", "--json", "--hiedir", config.hieDir, ...config.extraArgs];
    if (scope.kind === "module") {
      args.push("--module", scope.moduleName);
    }
    return parseAnalyzePayload(await this.runJson(args, signal));
  }

  private async runJson(args: string[], signal?: AbortSignal): Promise<unknown> {
    const config = this.getConfig();
    const fullArgs = [...config.binaryPrefixArgs, ...args];
    this.log(`$ ${config.binaryPath} ${fullArgs.join(" ")}`);

    const { stdout, stderr, exitCode } = await this.spawnOnce(config, fullArgs, signal);

    let parsed: unknown;
    try {
      parsed = parseJsonFromOutput(stdout);
    } catch {
      throw classifyNoJsonFailure(stdout, stderr, exitCode);
    }
    if (exitCode !== 0) {
      this.log(`plustan exited with code ${exitCode}; using emitted JSON payload.`);
    }
    return parsed;
  }

  private spawnOnce(
    config: SpawnConfig,
    args: string[],
    signal?: AbortSignal
  ): Promise<{ stdout: string; stderr: string; exitCode: number }> {
    return new Promise((resolve, reject) => {
      const child = spawn(config.binaryPath, args, { cwd: config.cwd, env: process.env });
      let stdout = "";
      let stderr = "";
      const onAbort = (): void => {
        child.kill("SIGTERM");
      };
      signal?.addEventListener("abort", onAbort, { once: true });

      child.stdout.on("data", (chunk: Buffer) => { stdout += chunk.toString("utf8"); });
      child.stderr.on("data", (chunk: Buffer) => {
        const text = chunk.toString("utf8");
        stderr += text;
        this.log(text.trimEnd());
      });
      child.on("error", (error: NodeJS.ErrnoException) => {
        signal?.removeEventListener("abort", onAbort);
        if (error.code === "ENOENT") {
          reject(new AnalyzerError("not-found",
            `Plu-Stan binary not found: ${config.binaryPath}. Set \`plustan.binaryPath\` or run "Plu-Stan: Check for Updates".`));
        } else {
          reject(new AnalyzerError("no-json", `Failed to start plustan: ${error.message}`));
        }
      });
      child.on("close", (code) => {
        signal?.removeEventListener("abort", onAbort);
        resolve({ stdout, stderr, exitCode: code ?? 1 });
      });
    });
  }
}

/** Scan stdout lines from the end for the JSON payload (build noise may precede it). */
export function parseJsonFromOutput(stdout: string): unknown {
  const lines = stdout.split(/\r?\n/).map((l) => l.trim()).filter(Boolean);
  for (let i = lines.length - 1; i >= 0; i -= 1) {
    try {
      return JSON.parse(lines[i]);
    } catch {
      // keep scanning earlier lines
    }
  }
  return JSON.parse(stdout);
}

/** Turn a no-JSON plustan run into a typed, user-actionable error. */
export function classifyNoJsonFailure(stdout: string, stderr: string, exitCode: number): AnalyzerError {
  const haystack = `${stderr}\n${stdout}`;
  if (/hie file versions|readHieFile|built by a different ghc|different ghc/i.test(haystack)) {
    return new AnalyzerError("ghc-mismatch",
      "Plu-Stan couldn't read your project's .hie files: they were built with a different GHC than the plustan binary. " +
      "Rebuild with the matching GHC, or run \"Plu-Stan: Check for Updates\".");
  }
  if (/panic!|the 'impossible' happened/i.test(haystack)) {
    return new AnalyzerError("crash", `Plu-Stan crashed (exit ${exitCode}). See the Plu-Stan output channel.`);
  }
  if (exitCode !== 0 && /error:|\[error\]/i.test(stderr)) {
    return new AnalyzerError("build-failed",
      "The project build failed, so analysis could not run. Fix the compile errors and save again.");
  }
  return new AnalyzerError("no-json",
    `Plu-Stan produced no JSON output (exit ${exitCode}). The binary may be outdated — try "Plu-Stan: Check for Updates".`);
}
