import * as path from "node:path";
import { spawn } from "node:child_process";
import * as vscode from "vscode";

type AnnotationSource = "hi" | "source" | "both";
type RunScope = "all" | "module";

interface OnchainModule {
  moduleName: string;
  file: string;
  annotationSource: AnnotationSource;
}

interface ListOnchainPayload {
  version: number;
  workspaceRoot: string;
  hieDir: string;
  modules: OnchainModule[];
}

interface Inspection {
  id: string;
  name: string;
  severity: string;
  description: string;
}

interface Observation {
  id: string;
  inspectionId: string;
  file: string;
  moduleName: string;
  startLine: number;
  startCol: number;
  endLine: number;
  endCol: number;
}

interface AnalysisSection {
  observations: Observation[];
}

interface AnalyzePayload {
  version: number;
  runScope: RunScope;
  targetModule: string | null;
  inspections: Inspection[];
  analysis: AnalysisSection;
}

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

class ActionItem extends vscode.TreeItem {
  constructor(
    label: string,
    description: string,
    commandId: string,
    title: string,
    iconId: string
  ) {
    super(label, vscode.TreeItemCollapsibleState.None);
    this.description = description;
    this.contextValue = "plustanAction";
    this.iconPath = new vscode.ThemeIcon(iconId);
    this.command = {
      command: commandId,
      title
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

type PluStanTreeItem = ActionItem | OnchainModuleItem | MessageItem;

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

    const actionItems: ActionItem[] = [
      new ActionItem("Run Workspace Analysis", "Run all onchain checks", "plustan.runWorkspace", "Run Workspace", "play-circle"),
      new ActionItem("Refresh Onchain Modules", "Rescan module annotations", "plustan.refreshOnchainModules", "Refresh Onchain Modules", "refresh"),
      new ActionItem("Clear Diagnostics", "Clear Problems panel entries", "plustan.clearDiagnostics", "Clear Diagnostics", "clear-all"),
      new ActionItem("Show Plu-Stan Output", "Open extension output logs", "plustan.openOutput", "Show Output", "output")
    ];

    const moduleItems = this.modules.map((moduleInfo) => new OnchainModuleItem(moduleInfo, this.workspaceRoot));
    return Promise.resolve([...actionItems, ...moduleItems]);
  }
}

export function activate(context: vscode.ExtensionContext): void {
  const output = vscode.window.createOutputChannel("Plu-Stan");
  const diagnostics = vscode.languages.createDiagnosticCollection("plu-stan");
  const provider = new OnchainModulesProvider();

  context.subscriptions.push(output, diagnostics);
  context.subscriptions.push(
    vscode.window.registerTreeDataProvider("plustanOnchainModules", provider)
  );

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
      const settings = await ensureBinaryConfigured(folder, provider);
      if (!settings) {
        return;
      }
      await withUserProgress("Refreshing onchain modules", async (token) => {
        const payload = await runListOnchain(folder, output, token);
        appendOnchainModulesSummary(payload, folder, output);
        provider.setData(payload.modules, payload.workspaceRoot);
        vscode.window.setStatusBarMessage(
          `Plu-Stan: loaded ${payload.modules.length} onchain module(s)`,
          3000
        );
      });
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("plustan.runWorkspace", async () => {
      if (!await saveWorkspaceBeforeRun()) {
        return;
      }
      const folder = getWorkspaceFolderOrNotify();
      if (!folder) {
        return;
      }
      const settings = await ensureBinaryConfigured(folder, provider);
      if (!settings) {
        return;
      }
      await withUserProgress("Running Plu-Stan on workspace", async (token) => {
        const payload = await runAnalyze(folder, output, token);
        appendAnalyzeSummary(payload, folder, output);
        publishDiagnostics(payload, folder, diagnostics);
        vscode.window.setStatusBarMessage(
          `Plu-Stan: ${payload.analysis.observations.length} observation(s)`,
          3000
        );
      });
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("plustan.runModule", async (item?: OnchainModuleItem) => {
      if (!await saveWorkspaceBeforeRun()) {
        return;
      }
      const folder = getWorkspaceFolderOrNotify();
      if (!folder) {
        return;
      }
      const settings = await ensureBinaryConfigured(folder, provider);
      if (!settings) {
        return;
      }
      await withUserProgress("Running Plu-Stan on module", async (token) => {
        const moduleName = item?.moduleInfo.moduleName ?? (await pickModuleName(provider, folder, output, token));
        if (!moduleName) {
          return;
        }
        const payload = await runAnalyze(folder, output, token, moduleName);
        appendAnalyzeSummary(payload, folder, output);
        publishDiagnostics(payload, folder, diagnostics);
        vscode.window.setStatusBarMessage(
          `Plu-Stan: ${moduleName} -> ${payload.analysis.observations.length} observation(s)`,
          3000
        );
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
    vscode.workspace.onDidChangeConfiguration(async (event) => {
      if (!event.affectsConfiguration("plustan.binaryPath")) {
        return;
      }
      try {
        const folder = getWorkspaceFolder();
        const settings = readSettings(folder);
        provider.setBinaryConfigured(hasConfiguredBinaryPath(settings));
      } catch {
        provider.setBinaryConfigured(false);
      }
    })
  );

  try {
    const folder = getWorkspaceFolder();
    const settings = readSettings(folder);
    provider.setBinaryConfigured(hasConfiguredBinaryPath(settings));
  } catch {
    provider.setBinaryConfigured(false);
  }
}

export function deactivate(): void {
  // no-op
}

async function pickModuleName(
  provider: OnchainModulesProvider,
  folder: vscode.WorkspaceFolder,
  output: vscode.OutputChannel,
  token: vscode.CancellationToken
): Promise<string | undefined> {
  let modules = provider.getData();
  if (modules.length === 0) {
    const payload = await runListOnchain(folder, output, token);
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

async function runListOnchain(
  folder: vscode.WorkspaceFolder,
  output: vscode.OutputChannel,
  token: vscode.CancellationToken
): Promise<ListOnchainPayload> {
  const settings = readSettings(folder);
  const payload = await runPluStanJson<ListOnchainPayload>(
    ["list-onchain", "--json", "--hiedir", settings.hieDir],
    settings,
    output,
    token
  );

  if (!Array.isArray(payload.modules)) {
    throw new Error("Invalid list-onchain payload: missing modules array");
  }

  return payload;
}

async function runAnalyze(
  folder: vscode.WorkspaceFolder,
  output: vscode.OutputChannel,
  token: vscode.CancellationToken,
  moduleName?: string
): Promise<AnalyzePayload> {
  const settings = readSettings(folder);
  const args = ["analyze", "--json", "--hiedir", settings.hieDir, ...settings.extraArgs];
  if (moduleName) {
    args.push("--module", moduleName);
  }

  const payload = await runPluStanJson<AnalyzePayload>(args, settings, output, token);
  if (!payload.analysis || !Array.isArray(payload.analysis.observations)) {
    throw new Error("Invalid analyze payload: missing analysis observations");
  }

  return payload;
}

function publishDiagnostics(
  payload: AnalyzePayload,
  folder: vscode.WorkspaceFolder,
  diagnostics: vscode.DiagnosticCollection
): void {
  diagnostics.clear();

  const inspections = new Map(payload.inspections.map((inspection) => [inspection.id, inspection]));
  const diagnosticsByFile = new Map<string, vscode.Diagnostic[]>();

  for (const observation of payload.analysis.observations) {
    const inspection = inspections.get(observation.inspectionId);
    const filePath = path.isAbsolute(observation.file)
      ? observation.file
      : path.join(folder.uri.fsPath, observation.file);

    const message = inspection
      ? `[${observation.inspectionId}] ${inspection.name}`
      : `[${observation.inspectionId}]`;

    const diagnostic = new vscode.Diagnostic(
      toRange(observation),
      message,
      mapSeverity(inspection?.severity)
    );
    diagnostic.source = "plu-stan";
    diagnostic.code = observation.inspectionId;

    const fileDiagnostics = diagnosticsByFile.get(filePath) ?? [];
    fileDiagnostics.push(diagnostic);
    diagnosticsByFile.set(filePath, fileDiagnostics);
  }

  diagnostics.set(
    [...diagnosticsByFile.entries()].map(([filePath, fileDiagnostics]) => [
      vscode.Uri.file(filePath),
      fileDiagnostics
    ])
  );
}

function toRange(observation: Observation): vscode.Range {
  const startLine = Math.max(0, observation.startLine - 1);
  const startCharacter = Math.max(0, observation.startCol - 1);
  const endLine = Math.max(startLine, observation.endLine - 1);

  const rawEndCharacter = Math.max(0, observation.endCol - 1);
  const endCharacter = endLine === startLine
    ? Math.max(startCharacter + 1, rawEndCharacter)
    : rawEndCharacter;

  return new vscode.Range(startLine, startCharacter, endLine, endCharacter);
}

function mapSeverity(severity: string | undefined): vscode.DiagnosticSeverity {
  switch (severity) {
    case "Error":
      return vscode.DiagnosticSeverity.Error;
    case "Warning":
    case "PotentialBug":
    case "Performance":
      return vscode.DiagnosticSeverity.Warning;
    default:
      return vscode.DiagnosticSeverity.Information;
  }
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

  const binaryPath = config.get<string>("binaryPath", "").trim();
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
  provider: OnchainModulesProvider
): Promise<PluStanSettings | undefined> {
  const settings = readSettings(folder);
  const configured = hasConfiguredBinaryPath(settings);
  provider.setBinaryConfigured(configured);
  if (configured) {
    return settings;
  }

  const choice = await vscode.window.showWarningMessage(
    "Set `plustan.binaryPath` in settings before running Plu-Stan commands.",
    "Open Settings"
  );
  if (choice) {
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


async function runPluStanJson<T>(
  args: string[],
  settings: PluStanSettings,
  output: vscode.OutputChannel,
  token: vscode.CancellationToken
): Promise<T> {
  if (settings.showOutputChannel) {
    output.show(true);
  }
  output.appendLine(`$ ${settings.binaryPath} ${args.join(" ")}`);
  output.appendLine(`cwd: ${settings.projectDir}`);

  let stdout = "";
  let stderr = "";
  let exitCode = 0;
  try {
    const result = await spawnCommand(settings.binaryPath, args, settings.projectDir, output, token);
    stdout = result.stdout;
    stderr = result.stderr;
    exitCode = result.exitCode;
  } catch (error) {
    if (isEnoentError(error)) {
      throw new Error(
        `Plu-Stan binary not found: ${settings.binaryPath}. ` +
        "Set `plustan.binaryPath` to an existing executable."
      );
    }
    throw error;
  }

  let parsed: unknown;
  try {
    parsed = parseJsonFromOutput(stdout);
  } catch (error) {
    const exitSuffix = exitCode !== 0 ? `\nExit code: ${exitCode}` : "";
    throw new Error(
      `Failed to parse plustan JSON output: ${formatError(error)}${exitSuffix}\n` +
      `stderr:\n${stderr}\nstdout:\n${stdout}`
    );
  }

  if (exitCode !== 0) {
    output.appendLine(`Plu-Stan exited with code ${exitCode}; using emitted JSON payload.`);
  }

  return parsed as T;
}

function parseJsonFromOutput(stdout: string): unknown {
  const lines = stdout
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter(Boolean);

  for (let i = lines.length - 1; i >= 0; i -= 1) {
    const line = lines[i];
    try {
      return JSON.parse(line);
    } catch {
      // Continue scanning earlier lines.
    }
  }

  return JSON.parse(stdout);
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
  payload: AnalyzePayload,
  folder: vscode.WorkspaceFolder,
  output: vscode.OutputChannel
): void {
  const observations = payload.analysis.observations;
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

function isEnoentError(error: unknown): boolean {
  if (!error || typeof error !== "object") {
    return false;
  }
  const maybeCode = (error as { code?: unknown }).code;
  return maybeCode === "ENOENT";
}

function spawnCommand(
  command: string,
  args: string[],
  cwd: string,
  output: vscode.OutputChannel,
  token: vscode.CancellationToken
): Promise<{ stdout: string; stderr: string; exitCode: number }> {
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, {
      cwd,
      env: process.env
    });

    let stdout = "";
    let stderr = "";

    const cancelSub = token.onCancellationRequested(() => {
      child.kill("SIGTERM");
    });

    child.stdout.on("data", (chunk: Buffer) => {
      stdout += chunk.toString("utf8");
    });

    child.stderr.on("data", (chunk: Buffer) => {
      const text = chunk.toString("utf8");
      stderr += text;
      output.append(text);
    });

    child.on("error", (error) => {
      cancelSub.dispose();
      const wrapped = new Error(`Failed to start plustan: ${formatError(error)}`) as Error & { code?: string };
      const errorCode = (error as NodeJS.ErrnoException).code;
      if (errorCode) {
        wrapped.code = errorCode;
      }
      reject(wrapped);
    });

    child.on("close", (code) => {
      cancelSub.dispose();
      resolve({ stdout, stderr, exitCode: code ?? 1 });
    });
  });
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
