import * as path from "node:path";
import * as vscode from "vscode";
import { getCachedBinaryPath, offerDownload, checkForUpdates, detectProjectGhc } from "./downloadManager";
import { SpawnAnalyzerClient, AnalyzerError } from "./analyzer/client";
import { AnalyzePayloadV2, ListOnchainPayload, OnchainModule } from "./core/schema";
import { ReviewController } from "./session/controller";
import { DismissalsStore } from "./session/dismissalsStore";
import { FindingsTreeProvider, FindingTreeItem } from "./ui/findingsTree";
import { FindingDetailProvider } from "./ui/detailPanel";
import { PluStanStatusBar } from "./ui/statusBar";
import { DismissCodeActionProvider, publishSessionDiagnostics, toRange } from "./diagnostics";
import { SessionFinding, initialSessionState, reduceSession } from "./core/sessionState";

// Throttle for the background "newer backend available?" check on activation.
const LAST_AUTO_UPDATE_CHECK_KEY = "plustan.lastAutoUpdateCheck";
const AUTO_UPDATE_INTERVAL_MS = 24 * 60 * 60 * 1000; // once per day

interface PluStanSettings {
  binaryPath: string;
  projectDir: string;
  hieDir: string;
  extraArgs: string[];
  showOutputChannel: boolean;
}

class OnchainModuleItem extends vscode.TreeItem {
  constructor(
    readonly moduleInfo: OnchainModule,
    workspaceRoot: string
  ) {
    super(moduleInfo.moduleName, vscode.TreeItemCollapsibleState.None);
    this.description = toRelativePath(workspaceRoot, moduleInfo.file);
    this.tooltip = `${moduleInfo.moduleName} (${moduleInfo.annotationSource})\n${moduleInfo.file}`;
    this.contextValue = "plustanModule";
    this.iconPath = new vscode.ThemeIcon("symbol-module");
    this.command = {
      command: "plustan.runModule",
      title: "Run Module",
      arguments: [this]
    };
  }
}

class MessageItem extends vscode.TreeItem {
  constructor(
    label: string,
    description: string,
    commandId: string
  ) {
    super(label, vscode.TreeItemCollapsibleState.None);
    this.description = description;
    this.contextValue = "plustanMessage";
    this.iconPath = new vscode.ThemeIcon("warning");
    this.command = {
      command: commandId,
      title: "Open Plu-Stan Settings"
    };
  }
}

type PluStanTreeItem = OnchainModuleItem | MessageItem;

class OnchainModulesProvider implements vscode.TreeDataProvider<PluStanTreeItem> {
  private modules: OnchainModule[] = [];
  private workspaceRoot = "";
  private binaryConfigured = false;
  private readonly emitter = new vscode.EventEmitter<PluStanTreeItem | undefined>();

  readonly onDidChangeTreeData = this.emitter.event;

  setData(modules: OnchainModule[], workspaceRoot: string): void {
    this.modules = [...modules].sort((a, b) => a.moduleName.localeCompare(b.moduleName));
    this.workspaceRoot = workspaceRoot;
    this.emitter.fire(undefined);
  }

  setBinaryConfigured(configured: boolean): void {
    this.binaryConfigured = configured;
    if (!configured) {
      this.modules = [];
    }
    this.emitter.fire(undefined);
  }

  getData(): OnchainModule[] {
    return this.modules;
  }

  getTreeItem(element: PluStanTreeItem): vscode.TreeItem {
    return element;
  }

  getChildren(): Thenable<PluStanTreeItem[]> {
    if (!this.binaryConfigured) {
      return Promise.resolve([
        new MessageItem(
          "Set plustan.binaryPath to enable Plu-Stan",
          "Open settings and configure the absolute executable path",
          "plustan.openSettings"
        )
      ]);
    }

    // Start/Run/Refresh/Clear now live in the Findings view title + command
    // palette, so this view is just the discovered onchain modules.
    const moduleItems = this.modules.map((moduleInfo) => new OnchainModuleItem(moduleInfo, this.workspaceRoot));
    return Promise.resolve(moduleItems);
  }
}

export function activate(context: vscode.ExtensionContext): void {
  const output = vscode.window.createOutputChannel("Plu-Stan");
  const diagnostics = vscode.languages.createDiagnosticCollection("plu-stan");
  const provider = new OnchainModulesProvider();

  const resolveSettings = (folder: vscode.WorkspaceFolder): PluStanSettings => {
    const settings = readSettings(folder);
    if (!settings.binaryPath) {
      const ghc = detectProjectGhc(settings.hieDir, settings.projectDir);
      const cached = getCachedBinaryPath(context.globalState, ghc);
      if (cached) return { ...settings, binaryPath: cached };
    }
    return settings;
  };

  const isEffectivelyConfigured = (folder: vscode.WorkspaceFolder): boolean => {
    const settings = readSettings(folder);
    if (hasConfiguredBinaryPath(settings)) {
      return true;
    }
    const ghc = detectProjectGhc(settings.hieDir, settings.projectDir);
    return getCachedBinaryPath(context.globalState, ghc) !== undefined;
  };

  // Background, throttled "is there a newer backend?" check. Runs at most once
  // per AUTO_UPDATE_INTERVAL_MS and only when the extension manages the binary
  // (no user-set binaryPath) and one is already downloaded — first-time setup
  // is handled by offerDownload. Quiet: it only surfaces UI when an update
  // actually exists, so a new backend release reaches users with no extension
  // republish and no startup nagging.
  const maybeAutoCheckForUpdates = async (): Promise<void> => {
    try {
      const folder = getWorkspaceFolder();
      const settings = readSettings(folder);
      if (hasConfiguredBinaryPath(settings)) {
        return; // user manages their own binary
      }
      const ghc = detectProjectGhc(settings.hieDir, settings.projectDir);
      if (getCachedBinaryPath(context.globalState, ghc) === undefined) {
        return; // nothing downloaded yet; offerDownload covers that
      }
      const last = context.globalState.get<number>(LAST_AUTO_UPDATE_CHECK_KEY, 0);
      const now = Date.now();
      if (now - last < AUTO_UPDATE_INTERVAL_MS) {
        return;
      }
      await context.globalState.update(LAST_AUTO_UPDATE_CHECK_KEY, now);
      await checkForUpdates(context, output, ghc, /* quiet */ true);
    } catch {
      // best-effort; never block activation on an update check
    }
  };

  context.subscriptions.push(output, diagnostics);
  context.subscriptions.push(
    vscode.window.registerTreeDataProvider("plustanOnchainModules", provider)
  );

  // The analyzer client resolves its spawn config lazily (per call), so it is
  // safe to construct even when no workspace folder is open — its getConfig
  // throws only when actually invoked without a folder, and every command
  // guards for that via getWorkspaceFolderOrNotify().
  const client = new SpawnAnalyzerClient(
    () => {
      const f = getWorkspaceFolder();
      const settings = resolveSettings(f);
      return {
        binaryPath: settings.binaryPath,
        binaryPrefixArgs: [],
        cwd: settings.projectDir,
        hieDir: settings.hieDir,
        extraArgs: settings.extraArgs
      };
    },
    (line) => output.appendLine(line)
  );

  // Assigned below once a workspace folder is resolved (inside `if (folder)`).
  // The legacy one-shot commands read it to detect an active review session so
  // they don't clobber the session's diagnostics; stays undefined with no folder.
  let activeController: ReviewController | undefined;

  context.subscriptions.push(
    vscode.commands.registerCommand("plustan.openSettings", async () => {
      await vscode.commands.executeCommand("workbench.action.openSettings", "plustan.binaryPath");
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("plustan.refreshOnchainModules", async () => {
      if (!await saveWorkspaceBeforeRun()) {
        return;
      }
      const folder = getWorkspaceFolderOrNotify();
      if (!folder) {
        return;
      }
      const settings = await ensureBinaryConfigured(folder, provider, resolveSettings);
      if (!settings) {
        return;
      }
      await withUserProgress("Refreshing onchain modules", async () => {
        const payload = await client.listOnchain();
        appendOnchainModulesSummary(payload, folder, output);
        provider.setData(payload.modules, payload.workspaceRoot);
        vscode.window.setStatusBarMessage(
          `Plu-Stan: loaded ${payload.modules.length} onchain module(s)`,
          3000
        );
      });
    })
  );

  // Legacy one-shot analysis: run once, publish diagnostics via a throwaway
  // SessionState, and stop. No review session is started and there is no
  // auto-rerun on save (that is what "Start Review" is for now).
  context.subscriptions.push(
    vscode.commands.registerCommand("plustan.runWorkspace", async () => {
      if (activeController?.sessionState.phase === "active") {
        vscode.window.showInformationMessage(
          "Plu-Stan: a review session is active — use the Findings view (or End Review first). " +
          "Legacy one-shot analysis is disabled during a session."
        );
        return;
      }
      if (!await saveWorkspaceBeforeRun()) {
        return;
      }
      const folder = getWorkspaceFolderOrNotify();
      if (!folder) {
        return;
      }
      const settings = await ensureBinaryConfigured(folder, provider, resolveSettings);
      if (!settings) {
        return;
      }
      await withUserProgress("Running Plu-Stan on workspace", async (token) => {
        await runOneShot(client, { kind: "workspace" }, folder, diagnostics, output, token);
      });
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("plustan.runModule", async (item?: OnchainModuleItem) => {
      if (activeController?.sessionState.phase === "active") {
        vscode.window.showInformationMessage(
          "Plu-Stan: a review session is active — use the Findings view (or End Review first). " +
          "Legacy one-shot analysis is disabled during a session."
        );
        return;
      }
      if (!await saveWorkspaceBeforeRun()) {
        return;
      }
      const folder = getWorkspaceFolderOrNotify();
      if (!folder) {
        return;
      }
      const settings = await ensureBinaryConfigured(folder, provider, resolveSettings);
      if (!settings) {
        return;
      }
      await withUserProgress("Running Plu-Stan on module", async (token) => {
        const moduleName = item?.moduleInfo.moduleName ?? (await pickModuleName(client, provider, folder));
        if (!moduleName) {
          return;
        }
        await runOneShot(client, { kind: "module", moduleName }, folder, diagnostics, output, token, moduleName);
      });
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("plustan.clearDiagnostics", () => {
      diagnostics.clear();
      vscode.window.setStatusBarMessage("Plu-Stan diagnostics cleared", 2000);
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("plustan.openOutput", () => {
      output.show(true);
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("plustan.checkForUpdates", async () => {
      const ghc = detectGhcForActiveFolder();
      await checkForUpdates(context, output, ghc);
      try {
        const folder = getWorkspaceFolder();
        provider.setBinaryConfigured(isEffectivelyConfigured(folder));
      } catch { /* no workspace open */ }
    })
  );

  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration(async (event) => {
      if (!event.affectsConfiguration("plustan.binaryPath")) {
        return;
      }
      try {
        const folder = getWorkspaceFolder();
        provider.setBinaryConfigured(isEffectivelyConfigured(folder));
      } catch {
        provider.setBinaryConfigured(false);
      }
    })
  );

  // Auto-reveal the Plu-Stan view (once per session) when the user views a
  // Haskell file annotated as an onchain contract. Seeing an annotation — not
  // mere activation — is also what gates the first-time download offer: with
  // `onLanguage:haskell` / `workspaceContains:**/*.hs` activation the
  // extension wakes up in every Haskell workspace, and non-Plutus projects
  // must never get a toast or a sidebar takeover.
  let onchainSeenThisSession = false;
  const offerDownloadIfUnconfigured = (): void => {
    try {
      const folder = getWorkspaceFolder();
      if (isEffectivelyConfigured(folder)) {
        return;
      }
      const settings = readSettings(folder);
      const ghc = detectProjectGhc(settings.hieDir, settings.projectDir);
      void offerDownload(context, output, ghc).then((downloadedPath) => {
        if (downloadedPath) {
          provider.setBinaryConfigured(true);
        }
      });
    } catch {
      // no workspace open
    }
  };
  const maybeRevealOnchainView = (editor: vscode.TextEditor | undefined): void => {
    if (onchainSeenThisSession || !editor || !isOnchainContractDocument(editor.document)) {
      return;
    }
    onchainSeenThisSession = true;
    void vscode.commands.executeCommand("plustanOnchainModules.focus", { preserveFocus: true });
    offerDownloadIfUnconfigured();
  };
  context.subscriptions.push(vscode.window.onDidChangeActiveTextEditor(maybeRevealOnchainView));

  // The review cockpit needs a definite workspace folder (DismissalsStore
  // persists into it). Constructing it eagerly with a bogus folder would crash
  // activation in the no-folder / single-loose-file case, so gate it here. The
  // analyzer client and the legacy commands above stay available regardless.
  const folder = ((): vscode.WorkspaceFolder | undefined => {
    try { return getWorkspaceFolder(); } catch { return undefined; }
  })();

  if (folder) {
    // Base for resolving findings' file paths. plu-stan emits them relative to
    // the directory it runs in — the analyzer's cwd, i.e. settings.projectDir
    // (e.g. `onchain/` in a monorepo) — NOT the VS Code workspace folder. So
    // resolve against projectDir; otherwise the `onchain/` segment is dropped
    // and click-to-open / inline diagnostics point at nonexistent files.
    // projectDir defaults to the folder when unset, so single-package projects
    // are unaffected. Captured once: the `folder` narrowing is not carried into
    // the hoisted openFinding declaration below.
    const analysisRoot = readSettings(folder).projectDir;
    const statusBar = new PluStanStatusBar();
    const findingsTree = new FindingsTreeProvider();

    const controller = new ReviewController(
      client,
      new DismissalsStore(folder),
      statusBar,
      context.workspaceState,
      (state, inspections) => {
        findingsTree.setData(state, inspections);
        publishSessionDiagnostics(state, inspections, analysisRoot, diagnostics);
      },
      output
    );
    // Expose to the legacy one-shot commands so they can defer while a session runs.
    activeController = controller;

    const detailPanel = new FindingDetailProvider(
      (finding) => { void controller.dismiss(finding.fingerprint, finding.inspectionId); },
      (finding) => { void openFinding(finding); }
    );

    controller.restore();

    async function openFinding(finding: SessionFinding): Promise<void> {
      // Use the captured analysisRoot (definite inside this block); re-deriving
      // via getWorkspaceFolder() could throw, and this runs void-ed from the
      // detail panel where a throw would become an unhandled rejection.
      const filePath = path.isAbsolute(finding.file) ? finding.file : path.join(analysisRoot, finding.file);
      const doc = await vscode.workspace.openTextDocument(filePath);
      const editor = await vscode.window.showTextDocument(doc, { preserveFocus: false });
      const range = toRange(finding);
      editor.revealRange(range, vscode.TextEditorRevealType.InCenter);
      editor.selection = new vscode.Selection(range.start, range.start);
    }

    context.subscriptions.push(
      statusBar,
      controller,
      vscode.window.registerTreeDataProvider("plustanFindings", findingsTree),
      vscode.window.registerWebviewViewProvider(FindingDetailProvider.viewId, detailPanel),
      vscode.languages.registerCodeActionsProvider(
        { language: "haskell", scheme: "file" },
        new DismissCodeActionProvider(),
        { providedCodeActionKinds: [vscode.CodeActionKind.QuickFix] }
      ),
      vscode.commands.registerCommand("plustan.startReview", async (scopeArg?: "all" | string[]) => {
        if (!await saveWorkspaceBeforeRun()) { return; }
        const f = getWorkspaceFolderOrNotify();
        if (!f) { return; }
        const settings = await ensureBinaryConfigured(f, provider, resolveSettings);
        if (!settings) { return; }
        await controller.startReview(scopeArg);
        // Reuse the listing startReview() already fetched (captured before the
        // module-scope picker, so it's the full set) — don't call listOnchain
        // twice. Undefined only until a first successful handshake; once set it
        // persists, so we populate the Onchain Modules view whenever we have it.
        if (controller.onchainListing) {
          provider.setData(controller.onchainListing.modules, controller.onchainListing.workspaceRoot);
        }
      }),
      vscode.commands.registerCommand("plustan.endReview", () => controller.endReview()),
      vscode.commands.registerCommand("plustan.toggleFindingsGrouping", () => findingsTree.toggleGrouping()),
      vscode.commands.registerCommand("plustan.openFinding", async (item?: FindingTreeItem) => {
        if (!item) { return; }
        detailPanel.showFinding(item.finding, controller.inspectionDocs.get(item.finding.inspectionId));
        await openFinding(item.finding);
      }),
      vscode.commands.registerCommand("plustan.dismissFinding",
        async (arg?: FindingTreeItem | { fingerprint: string; inspectionId: string }) => {
          const target = arg instanceof FindingTreeItem
            ? { fingerprint: arg.finding.fingerprint, inspectionId: arg.finding.inspectionId }
            : arg;
          if (!target) { return; }
          const note = await vscode.window.showInputBox({
            prompt: "Optional note: why is this finding not applicable?",
            placeHolder: "e.g. credential-only comparison is intentional here"
          });
          await controller.dismiss(target.fingerprint, target.inspectionId, note || undefined);
        }),
      vscode.commands.registerCommand("plustan.undismissFinding", async (item?: FindingTreeItem) => {
        if (item) { await controller.undismiss(item.finding.fingerprint); }
      })
    );
  }

  try {
    const activeFolder = getWorkspaceFolder();
    const configured = isEffectivelyConfigured(activeFolder);
    provider.setBinaryConfigured(configured);
    if (configured) {
      // Already have a binary — quietly see if a newer backend shipped.
      void maybeAutoCheckForUpdates();
    }
  } catch {
    provider.setBinaryConfigured(false);
  }

  // Covers the restored-tabs case: the annotated file may already be the
  // active editor by the time we activate, so the listener alone would miss it.
  maybeRevealOnchainView(vscode.window.activeTextEditor);
}

export function deactivate(): void {
  // no-op
}

/**
 * Run a single analysis and publish its observations as diagnostics through a
 * throwaway SessionState. No review session is started; this is the legacy
 * one-shot path used by the Run Workspace / Run Module commands.
 */
async function runOneShot(
  client: SpawnAnalyzerClient,
  scope: { kind: "workspace" } | { kind: "module"; moduleName: string },
  folder: vscode.WorkspaceFolder,
  diagnostics: vscode.DiagnosticCollection,
  output: vscode.OutputChannel,
  token: vscode.CancellationToken,
  label?: string
): Promise<void> {
  const abort = new AbortController();
  const sub = token.onCancellationRequested(() => abort.abort());
  try {
    const payload = await client.analyze(scope, abort.signal);
    appendAnalyzeSummary(payload, folder, output);

    const coveredFiles = [...new Set(payload.observations.map((o) => o.file))];
    const oneShot = reduceSession(
      reduceSession(initialSessionState, { type: "sessionStarted", startedAt: new Date().toISOString() }),
      { type: "runCompleted", coveredFiles, observations: payload.observations, dismissedFingerprints: [] }
    );
    publishSessionDiagnostics(
      oneShot,
      new Map(payload.inspections.map((i) => [i.id, i])),
      // Same base as the session path: plu-stan reports finding paths relative
      // to its cwd (settings.projectDir), not the VS Code workspace folder.
      readSettings(folder).projectDir,
      diagnostics
    );

    const prefix = label ? `${label} -> ` : "";
    vscode.window.setStatusBarMessage(
      `Plu-Stan: ${prefix}${payload.observations.length} observation(s)`,
      3000
    );
  } catch (error) {
    // A user-cancelled run is not a failure; withUserProgress would otherwise
    // surface the AnalyzerError("cancelled") message as an error toast.
    if (error instanceof AnalyzerError && error.kind === "cancelled") {
      return;
    }
    throw error;
  } finally {
    sub.dispose();
  }
}

async function pickModuleName(
  client: SpawnAnalyzerClient,
  provider: OnchainModulesProvider,
  folder: vscode.WorkspaceFolder
): Promise<string | undefined> {
  let modules = provider.getData();
  if (modules.length === 0) {
    const payload = await client.listOnchain();
    provider.setData(payload.modules, payload.workspaceRoot);
    modules = payload.modules;
  }

  if (modules.length === 0) {
    vscode.window.showInformationMessage("Plu-Stan: no onchain modules found.");
    return undefined;
  }

  const pick = await vscode.window.showQuickPick(
    modules.map((moduleInfo) => ({
      label: moduleInfo.moduleName,
      description: toRelativePath(folder.uri.fsPath, moduleInfo.file),
      detail: moduleInfo.annotationSource
    })),
    {
      placeHolder: "Select onchain module"
    }
  );

  return pick?.label;
}

function getWorkspaceFolder(): vscode.WorkspaceFolder {
  const folders = vscode.workspace.workspaceFolders;
  if (!folders || folders.length === 0) {
    throw new Error("Plu-Stan requires an open workspace folder.");
  }

  const active = vscode.window.activeTextEditor
    ? vscode.workspace.getWorkspaceFolder(vscode.window.activeTextEditor.document.uri)
    : undefined;

  return active ?? folders[0];
}

/**
 * Whether a document is a Haskell source file carrying the onchain-contract
 * module annotation. Mirrors the backend's source-side detection
 * (hasOnchainAnnotationInSource in app/PluStan.hs) — one line containing both
 * markers — so the extension and the binary never disagree on what counts.
 */
function isOnchainContractDocument(document: vscode.TextDocument): boolean {
  if (document.uri.scheme !== "file" || !document.fileName.endsWith(".hs")) {
    return false;
  }
  return document
    .getText()
    .split("\n")
    .some((line) => line.includes("{-# ANN module") && line.includes("onchain-contract"));
}

/** Best-effort GHC version of the active workspace folder's .hie files. */
function detectGhcForActiveFolder(): string | null {
  try {
    const settings = readSettings(getWorkspaceFolder());
    return detectProjectGhc(settings.hieDir, settings.projectDir);
  } catch {
    return null;
  }
}

function getWorkspaceFolderOrNotify(): vscode.WorkspaceFolder | undefined {
  try {
    return getWorkspaceFolder();
  } catch (error) {
    vscode.window.showErrorMessage(`Plu-Stan: ${formatError(error)}`);
    return undefined;
  }
}
function readSettings(folder: vscode.WorkspaceFolder): PluStanSettings {
  const config = vscode.workspace.getConfiguration("plustan", folder.uri);

  // VS Code does NOT expand `${workspaceFolder}` in arbitrary string settings
  // (only in launch.json/tasks.json), so the extension expands it itself.
  const rawBinaryPath = config.get<string>("binaryPath", "").trim();
  const binaryPath = rawBinaryPath.replace("${workspaceFolder}", folder.uri.fsPath);
  const configuredProjectDir = config.get<string>("projectDir", "").trim();
  const projectDir = configuredProjectDir
    ? resolveAgainst(folder.uri.fsPath, configuredProjectDir)
    : folder.uri.fsPath;

  const hieDir = config.get<string>("hieDir", ".hie");
  const extraArgs = config.get<string[]>("extraArgs", []);
  const showOutputChannel = config.get<boolean>("showOutputChannel", true);

  return {
    binaryPath,
    projectDir,
    hieDir,
    extraArgs,
    showOutputChannel
  };
}

function hasConfiguredBinaryPath(settings: PluStanSettings): boolean {
  return settings.binaryPath.trim().length > 0;
}

async function ensureBinaryConfigured(
  folder: vscode.WorkspaceFolder,
  provider: OnchainModulesProvider,
  resolveSettings: (f: vscode.WorkspaceFolder) => PluStanSettings
): Promise<PluStanSettings | undefined> {
  const settings = resolveSettings(folder);
  const configured = hasConfiguredBinaryPath(settings);
  provider.setBinaryConfigured(configured);
  if (configured) {
    return settings;
  }

  const choice = await vscode.window.showWarningMessage(
    "No plu-stan binary found. Download one or set `plustan.binaryPath` in settings.",
    "Download",
    "Open Settings"
  );
  if (choice === "Download") {
    await vscode.commands.executeCommand("plustan.checkForUpdates");
  } else if (choice === "Open Settings") {
    await vscode.commands.executeCommand("plustan.openSettings");
  }
  return undefined;
}

function resolveAgainst(baseDir: string, target: string): string {
  return path.isAbsolute(target) ? target : path.join(baseDir, target);
}

async function saveWorkspaceBeforeRun(): Promise<boolean> {
  const saved = await vscode.workspace.saveAll(false);
  if (saved) {
    return true;
  }

  vscode.window.showWarningMessage(
    "Plu-Stan: save failed for one or more files. Analysis was cancelled."
  );
  return false;
}

function toRelativePath(workspaceRoot: string, targetPath: string): string {
  const absoluteTarget = path.isAbsolute(targetPath)
    ? targetPath
    : path.join(workspaceRoot, targetPath);
  return path.relative(workspaceRoot, absoluteTarget) || targetPath;
}

function appendOnchainModulesSummary(
  payload: ListOnchainPayload,
  folder: vscode.WorkspaceFolder,
  output: vscode.OutputChannel
): void {
  output.appendLine(`Plu-Stan modules: ${payload.modules.length} found`);
  if (payload.modules.length === 0) {
    return;
  }

  for (const moduleInfo of payload.modules) {
    const relPath = toRelativePath(folder.uri.fsPath, moduleInfo.file);
    output.appendLine(`- ${moduleInfo.moduleName} (${moduleInfo.annotationSource}) ${relPath}`);
  }
}

function appendAnalyzeSummary(
  payload: AnalyzePayloadV2,
  folder: vscode.WorkspaceFolder,
  output: vscode.OutputChannel
): void {
  const observations = payload.observations;
  output.appendLine(
    `Plu-Stan analysis: runScope=${payload.runScope}, observations=${observations.length}`
  );

  if (observations.length === 0) {
    output.appendLine("No observations.");
    return;
  }

  const inspections = new Map(payload.inspections.map((inspection) => [inspection.id, inspection]));
  const maxLines = 200;
  const toShow = observations.slice(0, maxLines);

  for (const observation of toShow) {
    const relPath = toRelativePath(folder.uri.fsPath, observation.file);
    const inspection = inspections.get(observation.inspectionId);
    const nameSuffix = inspection ? ` ${inspection.name}` : "";
    output.appendLine(
      `[${observation.inspectionId}] ${relPath}:${observation.startLine}:${observation.startCol}${nameSuffix}`
    );
  }

  if (observations.length > maxLines) {
    output.appendLine(
      `... truncated ${observations.length - maxLines} additional observation(s). See Problems panel for full list.`
    );
  }
}

async function withUserProgress(
  title: string,
  action: (token: vscode.CancellationToken) => Promise<void>
): Promise<void> {
  try {
    await vscode.window.withProgress(
      {
        location: vscode.ProgressLocation.Notification,
        title,
        cancellable: true
      },
      async (_progress, token) => {
        await action(token);
      }
    );
  } catch (error) {
    vscode.window.showErrorMessage(`Plu-Stan: ${formatError(error)}`);
  }
}

function formatError(error: unknown): string {
  if (error instanceof Error) {
    return error.message;
  }
  return String(error);
}
