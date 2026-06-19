import * as https from "node:https";
import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";

const GITHUB_API_LATEST = "https://api.github.com/repos/input-output-hk/plu-stan/releases/latest";
// Map of GHC version -> { path, version } for binaries we've downloaded.
// stan reads .hie files whose format is locked to the exact GHC version, so we
// cache (and download) one binary per GHC rather than a single global one.
const CACHED_BINARIES_KEY = "plustan.cachedBinaries";

type Platform = "linux-x64" | "darwin-arm64" | "windows-x64";

interface CachedBinary {
  path: string;
  version: string;
}
type CacheMap = Record<string, CachedBinary>;

function detectPlatform(): Platform | null {
  const { arch, platform } = process;
  if (platform === "linux" && arch === "x64") return "linux-x64";
  if (platform === "darwin") return "darwin-arm64"; // Rosetta runs it on Intel too
  if (platform === "win32" && arch === "x64") return "windows-x64";
  return null;
}

function platformExt(platform: Platform): string {
  return platform === "windows-x64" ? ".exe" : "";
}

function assetName(version: string, platform: Platform, ghc: string): string {
  return `plustan-${version}-${platform}-ghc${ghc}${platformExt(platform)}`;
}

/**
 * Detect the GHC version a project's .hie files were built with.
 *
 * stan consumes .hie files, and their on-disk format is locked to the exact
 * GHC version that produced them — a binary built with a different GHC panics
 * with `readHieFile: hie file versions`. Every .hie file begins with a
 * plaintext header like `HIE9063\n9.6.3\n`, so we read the version straight
 * from the same artifact the binary will read. That is exactly the GHC the
 * downloaded binary must match.
 */
export function detectProjectGhc(hieDir: string, projectDir: string): string | null {
  const absHieDir = path.isAbsolute(hieDir) ? hieDir : path.join(projectDir, hieDir);
  const hieFile = findFirstHieFile(absHieDir);
  if (!hieFile) return null;
  try {
    const fd = fs.openSync(hieFile, "r");
    try {
      const buf = Buffer.alloc(64);
      fs.readSync(fd, buf, 0, 64, 0);
      const header = buf.toString("latin1");
      const match = header.match(/^HIE\d+\s+([0-9]+(?:\.[0-9]+)+)/);
      return match ? match[1] : null;
    } finally {
      fs.closeSync(fd);
    }
  } catch {
    return null;
  }
}

function findFirstHieFile(dir: string): string | null {
  let entries: fs.Dirent[];
  try {
    entries = fs.readdirSync(dir, { withFileTypes: true });
  } catch {
    return null;
  }
  for (const entry of entries) {
    if (entry.isFile() && entry.name.endsWith(".hie")) {
      return path.join(dir, entry.name);
    }
  }
  for (const entry of entries) {
    if (entry.isDirectory()) {
      const found = findFirstHieFile(path.join(dir, entry.name));
      if (found) {
        return found;
      }
    }
  }
  return null;
}

interface GitHubRelease {
  tag_name: string;
  assets: Array<{ name: string; browser_download_url: string }>;
}

/** GHC versions a release ships a binary for on the given platform. */
function availableGhcsFor(release: GitHubRelease, platform: Platform): string[] {
  const ext = platformExt(platform).replace(".", "\\.");
  const re = new RegExp(`^plustan-.+-${platform}-ghc([0-9]+(?:\\.[0-9]+)+)${ext}$`);
  return release.assets
    .map((a) => a.name.match(re))
    .filter((m): m is RegExpMatchArray => m !== null)
    .map((m) => m[1]);
}

function getCacheMap(globalState: vscode.Memento): CacheMap {
  return globalState.get<CacheMap>(CACHED_BINARIES_KEY, {});
}

function fetchJson(url: string): Promise<unknown> {
  return new Promise((resolve, reject) => {
    https
      .get(url, { headers: { "User-Agent": "vscode-plustan", "Accept": "application/vnd.github.v3+json" } }, (res) => {
        if ((res.statusCode === 301 || res.statusCode === 302) && res.headers.location) {
          fetchJson(res.headers.location).then(resolve, reject);
          return;
        }
        let data = "";
        res.on("data", (chunk: Buffer) => { data += chunk.toString(); });
        res.on("end", () => {
          try { resolve(JSON.parse(data)); }
          catch (e) { reject(e); }
        });
      })
      .on("error", reject);
  });
}

function downloadFile(
  url: string,
  destPath: string,
  token: vscode.CancellationToken
): Promise<void> {
  return new Promise((resolve, reject) => {
    const doGet = (currentUrl: string) => {
      https
        .get(currentUrl, { headers: { "User-Agent": "vscode-plustan" } }, (res) => {
          if ((res.statusCode === 301 || res.statusCode === 302) && res.headers.location) {
            doGet(res.headers.location);
            return;
          }
          if (res.statusCode !== 200) {
            reject(new Error(`Download failed: HTTP ${res.statusCode}`));
            return;
          }
          const file = fs.createWriteStream(destPath);
          const cancelSub = token.onCancellationRequested(() => {
            res.destroy();
            file.close(() => fs.unlink(destPath, () => {}));
            reject(new Error("Download cancelled"));
          });
          res.pipe(file);
          file.on("finish", () => { cancelSub.dispose(); file.close(); resolve(); });
          file.on("error", (err) => {
            cancelSub.dispose();
            fs.unlink(destPath, () => {});
            reject(err);
          });
        })
        .on("error", reject);
    };
    doGet(url);
  });
}

export function getCachedBinaryPath(globalState: vscode.Memento, ghc: string | null): string | undefined {
  if (!ghc) {
    return undefined;
  }
  const entry = getCacheMap(globalState)[ghc];
  if (entry && fs.existsSync(entry.path)) {
    return entry.path;
  }
  return undefined;
}

export async function offerDownload(
  context: vscode.ExtensionContext,
  output: vscode.OutputChannel,
  ghc: string | null
): Promise<string | undefined> {
  const platform = detectPlatform();
  if (!platform) {
    return undefined;
  }

  const choice = await vscode.window.showInformationMessage(
    "Plu-Stan: No binary configured. Download the latest plu-stan binary automatically?",
    "Download",
    "Set Path Manually"
  );

  if (choice === "Set Path Manually") {
    await vscode.commands.executeCommand("plustan.openSettings");
    return undefined;
  }

  if (choice !== "Download") {
    return undefined;
  }

  return downloadLatest(context, output, ghc, platform);
}

export async function downloadLatest(
  context: vscode.ExtensionContext,
  output: vscode.OutputChannel,
  ghc: string | null,
  platform?: Platform
): Promise<string | undefined> {
  const resolved = platform ?? detectPlatform();
  if (!resolved) {
    vscode.window.showErrorMessage("Plu-Stan: auto-download is not supported on this platform/architecture.");
    return undefined;
  }

  if (!ghc) {
    vscode.window.showErrorMessage(
      "Plu-Stan: couldn't detect your project's GHC version (no .hie files found). " +
      "Build your project first, or set `plustan.binaryPath` to a plustan you built yourself."
    );
    return undefined;
  }

  return vscode.window.withProgress(
    { location: vscode.ProgressLocation.Notification, title: "Plu-Stan: Downloading binary", cancellable: true },
    async (_progress, token) => {
      try {
        output.appendLine("Plu-Stan: fetching latest release info from GitHub...");
        const release = await fetchJson(GITHUB_API_LATEST) as GitHubRelease;
        const version = release.tag_name.replace(/^v/, "");
        const name = assetName(version, resolved, ghc);
        const asset = release.assets.find((a) => a.name === name);

        if (!asset) {
          const available = availableGhcsFor(release, resolved);
          throw new Error(
            `No prebuilt plustan for GHC ${ghc} on ${resolved} in release ${release.tag_name}. ` +
            (available.length
              ? `That release ships binaries for GHC: ${available.join(", ")}. `
              : `That release ships no ${resolved} binaries. `) +
            `Rebuild your project with one of those GHC versions, or build plustan with GHC ${ghc} ` +
            `and point \`plustan.binaryPath\` at it.`
          );
        }

        const storageDir = context.globalStorageUri.fsPath;
        if (!fs.existsSync(storageDir)) {
          fs.mkdirSync(storageDir, { recursive: true });
        }

        const ext = platformExt(resolved);
        const binaryPath = path.join(storageDir, `plustan-ghc${ghc}${ext}`);

        output.appendLine(`Plu-Stan: downloading ${name}...`);
        await downloadFile(asset.browser_download_url, binaryPath, token);

        if (process.platform !== "win32") {
          fs.chmodSync(binaryPath, 0o755);
        }

        const cache = getCacheMap(context.globalState);
        cache[ghc] = { path: binaryPath, version };
        await context.globalState.update(CACHED_BINARIES_KEY, cache);

        output.appendLine(`Plu-Stan: ${version} (GHC ${ghc}) installed at ${binaryPath}`);
        vscode.window.showInformationMessage(`Plu-Stan ${version} (GHC ${ghc}) installed. Ready to use.`);
        return binaryPath;
      } catch (error) {
        const msg = error instanceof Error ? error.message : String(error);
        output.appendLine(`Plu-Stan: download error: ${msg}`);
        vscode.window.showErrorMessage(`Plu-Stan: download failed — ${msg}`);
        return undefined;
      }
    }
  );
}

export async function checkForUpdates(
  context: vscode.ExtensionContext,
  output: vscode.OutputChannel,
  ghc: string | null
): Promise<void> {
  const platform = detectPlatform();
  if (!platform) {
    vscode.window.showWarningMessage("Plu-Stan: auto-download is not supported on this platform.");
    return;
  }

  if (!ghc) {
    vscode.window.showWarningMessage(
      "Plu-Stan: couldn't detect your project's GHC version (no .hie files found). " +
      "Build your project first so Plu-Stan can fetch a matching binary."
    );
    return;
  }

  try {
    output.appendLine(`Plu-Stan: checking for updates (GHC ${ghc})...`);
    const release = await fetchJson(GITHUB_API_LATEST) as GitHubRelease;
    const latestVersion = release.tag_name.replace(/^v/, "");
    const cachedVersion = getCacheMap(context.globalState)[ghc]?.version;

    if (cachedVersion === latestVersion) {
      vscode.window.showInformationMessage(`Plu-Stan: already up to date (${latestVersion}, GHC ${ghc}).`);
      return;
    }

    const label = cachedVersion ? `Update to ${latestVersion}` : `Download ${latestVersion}`;
    const detail = cachedVersion
      ? `Plu-Stan ${latestVersion} is available for GHC ${ghc} (installed: ${cachedVersion}).`
      : `Plu-Stan ${latestVersion} is available for GHC ${ghc}.`;

    const choice = await vscode.window.showInformationMessage(detail, label, "Later");
    if (choice === label) {
      await downloadLatest(context, output, ghc, platform);
    }
  } catch (error) {
    const msg = error instanceof Error ? error.message : String(error);
    output.appendLine(`Plu-Stan: update check failed: ${msg}`);
    vscode.window.showErrorMessage(`Plu-Stan: update check failed — ${msg}`);
  }
}
