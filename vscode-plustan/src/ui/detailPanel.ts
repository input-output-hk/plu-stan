import * as vscode from "vscode";
import { InspectionV2 } from "../core/schema";
import { SessionFinding } from "../core/sessionState";

const RULES_URL = "https://github.com/input-output-hk/plu-stan/blob/main/RULES.md";

export class FindingDetailProvider implements vscode.WebviewViewProvider {
  static readonly viewId = "plustanFindingDetail";
  private view: vscode.WebviewView | undefined;
  private current: { finding: SessionFinding; inspection?: InspectionV2 } | undefined;

  constructor(
    private readonly onDismiss: (finding: SessionFinding) => void,
    private readonly onOpen: (finding: SessionFinding) => void
  ) {}

  resolveWebviewView(view: vscode.WebviewView): void {
    this.view = view;
    view.webview.options = { enableScripts: true };
    view.webview.onDidReceiveMessage((msg: { type: string }) => {
      if (!this.current) {
        return;
      }
      if (msg.type === "dismiss") {
        this.onDismiss(this.current.finding);
      } else if (msg.type === "open") {
        this.onOpen(this.current.finding);
      }
    });
    this.render();
  }

  showFinding(finding: SessionFinding, inspection: InspectionV2 | undefined): void {
    // Update the content but do NOT force the (collapsed-by-default) view open —
    // it only takes screen space when the user expands it themselves, at which
    // point resolveWebviewView renders whatever finding is currently selected.
    this.current = { finding, inspection };
    this.render();
  }

  clear(): void {
    this.current = undefined;
    this.render();
  }

  private render(): void {
    if (!this.view) {
      return;
    }
    this.view.webview.html = this.current
      ? findingHtml(this.current.finding, this.current.inspection)
      : emptyHtml();
  }
}

function esc(text: string): string {
  return text.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

function emptyHtml(): string {
  return wrap("<p class='muted'>Select a finding in the tree to see its explanation.</p>");
}

function findingHtml(f: SessionFinding, inspection: InspectionV2 | undefined): string {
  const name = inspection?.name ?? f.inspectionId;
  const severity = inspection?.severity ?? "";
  const why = inspection?.whyItMatters ?? inspection?.description ?? "";
  const solutions = (inspection?.solution ?? []).map((s) => `<li>${esc(s)}</li>`).join("");
  const docsLink = inspection?.docsAnchor
    ? `<a href="${RULES_URL}#${esc(inspection.docsAnchor)}">Rule documentation</a>`
    : "";
  const examples = inspection?.badExample && inspection?.goodExample
    ? `<h3>✗ Avoid</h3><pre>${esc(inspection.badExample)}</pre>
       <h3>✓ Prefer</h3><pre>${esc(inspection.goodExample)}</pre>`
    : "";
  const dismissalNote = f.status === "dismissed" && f.dismissalNote
    ? `<p class="muted">Dismissed — ${esc(f.dismissalNote)}</p>`
    : "";
  return wrap(`
    <h2>${esc(f.inspectionId)} · ${esc(name)}</h2>
    <p class="muted">${esc(severity)} · ${esc(f.file)}:${f.startLine}:${f.startCol} · status: ${esc(f.status)}</p>
    ${dismissalNote}
    ${why ? `<p>${esc(why)}</p>` : ""}
    ${examples}
    ${solutions ? `<h3>How to fix</h3><ul>${solutions}</ul>` : ""}
    <p>${docsLink}</p>
    <div class="actions">
      <button onclick="post('open')">Open file</button>
      <button onclick="post('dismiss')">Dismiss</button>
    </div>
  `);
}

function wrap(body: string): string {
  return `<!DOCTYPE html><html><head><meta charset="UTF-8">
    <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; script-src 'unsafe-inline';">
    <style>
      body { font-family: var(--vscode-font-family); font-size: 13px; padding: 12px 16px; line-height: 1.5; }
      h2 { margin: 0 0 4px; font-size: 1.15em; }
      h3 { margin: 16px 0 6px; font-size: 1em; }
      p { margin: 8px 0; }
      pre {
        background: var(--vscode-textCodeBlock-background);
        padding: 10px 12px;
        margin: 4px 0 12px;
        border-radius: 4px;
        line-height: 1.45;
        overflow-x: auto;
      }
      .muted { opacity: 0.7; margin-bottom: 12px; display: block; }
      .actions { margin-top: 16px; display: flex; gap: 8px; }
      button { padding: 4px 12px; }
      ul { margin: 6px 0; padding-left: 20px; }
      li { margin: 3px 0; }
    </style></head>
    <body>${body}
    <script>const vscode = acquireVsCodeApi(); function post(type) { vscode.postMessage({ type }); }</script>
    </body></html>`;
}
