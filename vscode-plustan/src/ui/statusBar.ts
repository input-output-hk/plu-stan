import * as vscode from "vscode";
import { countByStatus, SessionState } from "../core/sessionState";

export class PluStanStatusBar implements vscode.Disposable {
  private readonly item: vscode.StatusBarItem;

  constructor() {
    this.item = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 50);
    this.item.command = "plustanFindings.focus";
  }

  update(state: SessionState, running: boolean, buildFailed: boolean): void {
    if (state.phase === "idle") {
      this.item.hide();
      return;
    }
    const counts = countByStatus(state);
    const spinner = running ? "$(sync~spin) " : "";
    const failure = buildFailed ? " · build failed — results stale" : "";
    this.item.text = `${spinner}Plu-Stan: ${counts.open + counts.stale} open · ${counts.fixed} fixed${failure}`;
    this.item.tooltip = "Plu-Stan review session — click to open findings";
    this.item.show();
  }

  dispose(): void {
    this.item.dispose();
  }
}
