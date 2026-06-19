import * as https from "node:https";
import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";

const GITHUB_API_LATEST = "https://api.github.com/repos/input-output-hk/plu-stan/releases/latest";
const CACHED_BINARY_KEY = "plustan.cachedBinaryPath";
const CACHED_VERSION_KEY = "plustan.cachedVersion";

type Platform = "linux-x64" | "darwin-x64" | "darwin-arm64" | "windows-x64";

function detectPlatform(): Platform | null {
  const { arch, platform } = process;
  if (platform === "linux" && arch === "x64") return "linux-x64";
  if (platform === "darwin" && arch === "x64") return "darwin-x64";
  if (platform === "darwin" && arch === "arm64") return "darwin-arm64";
  if (platform === "win32" && arch === "x64") return "windows-x64";
  return null;
}

function assetName(version: string, platform: Platform): string {
  return `plustan-${version}-${platform}${platform === "windows-x64" ? ".exe" : ""}`;
}

interface GitHubRelease {
  tag_name: string;
  assets: Array<{ name: string; browser_download_url: string }>;
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

export function getCachedBinaryPath(globalState: vscode.Memento): string | undefined {
  const cached = globalState.get<string>(CACHED_BINARY_KEY);
  if (cached && fs.existsSync(cached)) {
    return cached;
  }
  return undefined;
}

export async function offerDownload(
  context: vscode.ExtensionContext,
  output: vscode.OutputChannel
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

  return downloadLatest(context, output, platform);
}

export async function downloadLatest(
  context: vscode.ExtensionContext,
  output: vscode.OutputChannel,
  platform?: Platform
): Promise<string | undefined> {
  const resolved = platform ?? detectPlatform();
  if (!resolved) {
    vscode.window.showErrorMessage("Plu-Stan: auto-download is not supported on this platform/architecture.");
    return undefined;
  }

  return vscode.window.withProgress(
    { location: vscode.ProgressLocation.Notification, title: "Plu-Stan: Downloading binary", cancellable: true },
    async (_progress, token) => {
      try {
        output.appendLine("Plu-Stan: fetching latest release info from GitHub...");
        const release = await fetchJson(GITHUB_API_LATEST) as GitHubRelease;
        const version = release.tag_name.replace(/^v/, "");
        const name = assetName(version, resolved);
        const asset = release.assets.find((a) => a.name === name);

        if (!asset) {
          throw new Error(
            `No pre-built binary found for ${resolved} in release ${release.tag_name}. ` +
            `Expected asset: ${name}. You can set plustan.binaryPath manually instead.`
          );
        }

        const storageDir = context.globalStorageUri.fsPath;
        if (!fs.existsSync(storageDir)) {
          fs.mkdirSync(storageDir, { recursive: true });
        }

        const ext = resolved === "windows-x64" ? ".exe" : "";
        const binaryPath = path.join(storageDir, `plustan${ext}`);

        output.appendLine(`Plu-Stan: downloading ${name}...`);
        await downloadFile(asset.browser_download_url, binaryPath, token);

        if (resolved !== "windows-x64") {
          fs.chmodSync(binaryPath, 0o755);
        }

        await context.globalState.update(CACHED_BINARY_KEY, binaryPath);
        await context.globalState.update(CACHED_VERSION_KEY, version);

        output.appendLine(`Plu-Stan: ${version} installed at ${binaryPath}`);
        vscode.window.showInformationMessage(`Plu-Stan ${version} installed. Ready to use.`);
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
  output: vscode.OutputChannel
): Promise<void> {
  const platform = detectPlatform();
  if (!platform) {
    vscode.window.showWarningMessage("Plu-Stan: auto-download is not supported on this platform.");
    return;
  }

  try {
    output.appendLine("Plu-Stan: checking for updates...");
    const release = await fetchJson(GITHUB_API_LATEST) as GitHubRelease;
    const latestVersion = release.tag_name.replace(/^v/, "");
    const cachedVersion = context.globalState.get<string>(CACHED_VERSION_KEY);

    if (cachedVersion === latestVersion) {
      vscode.window.showInformationMessage(`Plu-Stan: already up to date (${latestVersion}).`);
      return;
    }

    const label = cachedVersion ? `Update to ${latestVersion}` : `Download ${latestVersion}`;
    const detail = cachedVersion
      ? `Plu-Stan ${latestVersion} is available (installed: ${cachedVersion}).`
      : `Plu-Stan ${latestVersion} is available.`;

    const choice = await vscode.window.showInformationMessage(detail, label, "Later");
    if (choice === label) {
      await downloadLatest(context, output, platform);
    }
  } catch (error) {
    const msg = error instanceof Error ? error.message : String(error);
    output.appendLine(`Plu-Stan: update check failed: ${msg}`);
    vscode.window.showErrorMessage(`Plu-Stan: update check failed — ${msg}`);
  }
}
