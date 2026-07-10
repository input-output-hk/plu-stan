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
    if (running) {
      this.item.text = "$(sync~spin) Plu-Stan: analyzing…";
      this.item.tooltip = "Plu-Stan is running an analysis…";
      this.item.show();
      return;
    }
    const counts = countByStatus(state);
    const failure = buildFailed ? " · $(warning) build failed — results stale" : "";
    this.item.text = `Plu-Stan: ${counts.open + counts.stale} open · ${counts.fixed} fixed${failure}`;
    this.item.tooltip = "Plu-Stan review session — click to open findings";
    this.item.show();
  }

  dispose(): void {
    this.item.dispose();
  }
}
