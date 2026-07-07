import * as vscode from "vscode";
import { AnalyzerClient, AnalyzerError, AnalyzeScope } from "../analyzer/client";
import { InspectionV2, SchemaError } from "../core/schema";
import { initialSessionState, reduceSession, SessionState } from "../core/sessionState";
import { PendingRun, RunCoalescer } from "../core/runCoalescer";
import { DismissalsStore } from "./dismissalsStore";
import { PluStanStatusBar } from "../ui/statusBar";

const SESSION_STORAGE_KEY = "plustan.session.v1";
const SAVE_DEBOUNCE_MS = 500;

interface PersistedSession {
  state: SessionState;
  inspections: [string, InspectionV2][];
  moduleByFile: [string, string][];
}

export class ReviewController implements vscode.Disposable {
  private state: SessionState = initialSessionState;
  private inspections = new Map<string, InspectionV2>();
  private moduleByFile = new Map<string, string>(); // file path → module name
  private readonly queue = new RunCoalescer();
  private running = false;
  private buildFailed = false;
  private debounceTimer: NodeJS.Timeout | undefined;
  private abort: AbortController | undefined;
  private readonly disposables: vscode.Disposable[] = [];

  constructor(
    private readonly client: AnalyzerClient,
    private readonly dismissals: DismissalsStore,
    private readonly statusBar: PluStanStatusBar,
    private readonly workspaceState: vscode.Memento,
    private readonly onStateChange: (state: SessionState, inspections: Map<string, InspectionV2>) => void,
    private readonly output: vscode.OutputChannel
  ) {
    this.disposables.push(
      vscode.workspace.onDidSaveTextDocument((doc) => this.handleSave(doc)),
      // A finding is stale as soon as its file is *edited*, not only once saved.
      vscode.workspace.onDidChangeTextDocument((e) => {
        if (this.state.phase === "active" && e.contentChanges.length > 0) {
          const file = this.fileKeyForDocument(e.document);
          if (this.moduleForDocument(e.document)) {
            this.dispatch({ type: "fileEdited", file });
          }
        }
      })
    );
  }

  get sessionState(): SessionState { return this.state; }
  get inspectionDocs(): Map<string, InspectionV2> { return this.inspections; }

  restore(): void {
    const saved = this.workspaceState.get<PersistedSession>(SESSION_STORAGE_KEY);
    if (saved && saved.state.phase === "active") {
      this.state = saved.state;
      this.inspections = new Map(saved.inspections);
      this.moduleByFile = new Map(saved.moduleByFile);
      void vscode.commands.executeCommand("setContext", "plustan.sessionActive", true);
      this.publish();
    }
  }

  /**
   * Start a review. Scope: the whole workspace or a chosen set of onchain
   * modules. scopeArg === "all" (or ≤1 module) skips the picker — used
   * programmatically and by the integration test.
   */
  async startReview(scopeArg?: "all" | string[]): Promise<void> {
    let moduleByFile: Map<string, string>;
    try {
      await this.client.capabilities();
      const list = await this.client.listOnchain();
      moduleByFile = new Map(list.modules.map((m) => [m.file, m.moduleName]));
    } catch (error) {
      await this.explainStartFailure(error);
      return;
    }

    if (scopeArg === undefined && moduleByFile.size > 1) {
      const picks = await vscode.window.showQuickPick(
        [...moduleByFile.values()].sort().map((moduleName) => ({ label: moduleName, picked: true })),
        { canPickMany: true, placeHolder: "Modules to review (all preselected)" }
      );
      if (!picks) {
        return; // user cancelled
      }
      scopeArg = picks.map((p) => p.label);
    }
    if (Array.isArray(scopeArg)) {
      const wanted = new Set(scopeArg);
      moduleByFile = new Map([...moduleByFile.entries()].filter(([, m]) => wanted.has(m)));
    }
    this.moduleByFile = moduleByFile;

    this.dispatch({ type: "sessionStarted", startedAt: new Date().toISOString() });
    await vscode.commands.executeCommand("setContext", "plustan.sessionActive", true);
    this.queue.request({ kind: "workspace" });
    void this.pump();
  }

  async endReview(): Promise<void> {
    this.dispatch({ type: "sessionEnded" });
    await vscode.commands.executeCommand("setContext", "plustan.sessionActive", false);
    this.abort?.abort();
    const c = this.state.findings;
    this.output.appendLine(
      `Plu-Stan review ended: ${Object.values(c).filter((f) => f.status === "fixed").length} fixed, ` +
      `${Object.values(c).filter((f) => f.status === "open" || f.status === "stale").length} open, ` +
      `${Object.values(c).filter((f) => f.status === "dismissed").length} dismissed.`
    );
  }

  async dismiss(fingerprint: string, inspectionId: string, note?: string): Promise<void> {
    await this.dismissals.add({ fingerprint, inspectionId, note, dismissedAt: new Date().toISOString() });
    this.dispatch({ type: "findingDismissed", fingerprint });
  }

  async undismiss(fingerprint: string): Promise<void> {
    await this.dismissals.remove(fingerprint);
    this.dispatch({ type: "findingUndismissed", fingerprint });
  }

  private handleSave(doc: vscode.TextDocument): void {
    if (this.state.phase !== "active") {
      return;
    }
    const moduleName = this.moduleForDocument(doc);
    if (!moduleName) {
      return;
    }
    this.dispatch({ type: "fileEdited", file: this.fileKeyForDocument(doc) });
    if (this.debounceTimer) {
      clearTimeout(this.debounceTimer);
    }
    this.debounceTimer = setTimeout(() => {
      this.queue.request({ kind: "module", moduleName });
      void this.pump();
    }, SAVE_DEBOUNCE_MS);
  }

  /** The backend reports workspace-relative paths; match on suffix. */
  private moduleForDocument(doc: vscode.TextDocument): string | undefined {
    for (const [file, moduleName] of this.moduleByFile) {
      if (doc.fileName.endsWith(file)) {
        return moduleName;
      }
    }
    return undefined;
  }

  private fileKeyForDocument(doc: vscode.TextDocument): string {
    for (const [file] of this.moduleByFile) {
      if (doc.fileName.endsWith(file)) {
        return file;
      }
    }
    return doc.fileName;
  }

  private async pump(): Promise<void> {
    if (this.running) {
      return;
    }
    const next = this.queue.takeNext();
    if (!next) {
      return;
    }
    this.running = true;
    this.publish();
    try {
      await this.runOne(next);
      this.buildFailed = false;
    } catch (error) {
      if (error instanceof AnalyzerError && error.kind === "cancelled") {
        // A newer save superseded this run; not a failure — stay quiet.
      } else if (error instanceof AnalyzerError && error.kind === "build-failed") {
        this.buildFailed = true; // keep findings; status bar explains; next save retries
        this.output.appendLine(error.message);
      } else {
        this.output.appendLine(`Plu-Stan run failed: ${error instanceof Error ? error.message : String(error)}`);
        void vscode.window.showErrorMessage(`Plu-Stan: ${error instanceof Error ? error.message : String(error)}`);
      }
    } finally {
      this.running = false;
      this.publish();
      if (this.queue.size > 0) {
        void this.pump();
      }
    }
  }

  private async runOne(run: PendingRun): Promise<void> {
    this.abort = new AbortController();
    const scope: AnalyzeScope = run.kind === "workspace"
      ? { kind: "workspace" }
      : { kind: "module", moduleName: run.moduleName };
    const payload = await this.client.analyze(scope, this.abort.signal);

    for (const inspection of payload.inspections) {
      this.inspections.set(inspection.id, inspection);
    }
    const coveredFiles = run.kind === "workspace"
      ? [...this.moduleByFile.keys()]
      : [...this.moduleByFile.entries()].filter(([, m]) => m === run.moduleName).map(([f]) => f);
    // A module-scoped session still gets whole-project observations from a
    // workspace-kind run — keep only files inside the session scope.
    const covered = new Set(coveredFiles);
    const observations = payload.observations.filter((o) => covered.has(o.file));
    const dismissed = (await this.dismissals.load()).dismissals.map((d) => d.fingerprint);
    this.dispatch({
      type: "runCompleted",
      coveredFiles,
      observations,
      dismissedFingerprints: dismissed
    });
  }

  private dispatch(event: Parameters<typeof reduceSession>[1]): void {
    this.state = reduceSession(this.state, event);
    void this.workspaceState.update(SESSION_STORAGE_KEY, {
      state: this.state,
      inspections: [...this.inspections.entries()],
      moduleByFile: [...this.moduleByFile.entries()]
    } satisfies PersistedSession);
    this.publish();
  }

  private publish(): void {
    this.statusBar.update(this.state, this.running, this.buildFailed);
    this.onStateChange(this.state, this.inspections);
  }

  private async explainStartFailure(error: unknown): Promise<void> {
    if (error instanceof AnalyzerError && error.kind === "cancelled") {
      return; // start aborted; nothing to explain
    }
    const message = error instanceof SchemaError || error instanceof AnalyzerError
      ? error.message
      : `Plu-Stan handshake failed: ${error instanceof Error ? error.message : String(error)}`;
    const choice = await vscode.window.showErrorMessage(message, "Check for Updates");
    if (choice === "Check for Updates") {
      await vscode.commands.executeCommand("plustan.checkForUpdates");
    }
  }

  dispose(): void {
    this.abort?.abort();
    if (this.debounceTimer) {
      clearTimeout(this.debounceTimer);
    }
    for (const d of this.disposables) {
      d.dispose();
    }
  }
}
