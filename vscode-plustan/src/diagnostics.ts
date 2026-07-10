import * as vscode from "vscode";
import { InspectionV2 } from "./core/schema";
import { SessionFinding, SessionState } from "./core/sessionState";

export interface PluStanDiagnostic extends vscode.Diagnostic {
  plustanFingerprint?: string;
  plustanInspectionId?: string;
}

export function publishSessionDiagnostics(
  state: SessionState,
  inspections: Map<string, InspectionV2>,
  resolveFile: (file: string) => string,
  collection: vscode.DiagnosticCollection
): void {
  collection.clear();
  const byFile = new Map<string, PluStanDiagnostic[]>();

  for (const finding of Object.values(state.findings)) {
    if (finding.status !== "open" && finding.status !== "stale") {
      continue;
    }
    const inspection = inspections.get(finding.inspectionId);
    const filePath = resolveFile(finding.file);
    const stalePrefix = finding.status === "stale" ? "(stale) " : "";
    const summary = inspection ? `${inspection.name} — ${inspection.description}` : "";
    const diagnostic: PluStanDiagnostic = new vscode.Diagnostic(
      toRange(finding),
      `${stalePrefix}[${finding.inspectionId}] ${summary}`.trim(),
      mapSeverity(inspection?.severity)
    );
    diagnostic.source = "plu-stan";
    diagnostic.code = finding.inspectionId;
    diagnostic.plustanFingerprint = finding.fingerprint;
    diagnostic.plustanInspectionId = finding.inspectionId;
    const list = byFile.get(filePath) ?? [];
    list.push(diagnostic);
    byFile.set(filePath, list);
  }
  collection.set([...byFile.entries()].map(([f, ds]) => [vscode.Uri.file(f), ds]));
}

export function toRange(finding: SessionFinding): vscode.Range {
  const startLine = Math.max(0, finding.startLine - 1);
  const startCharacter = Math.max(0, finding.startCol - 1);
  const endLine = Math.max(startLine, finding.endLine - 1);
  const rawEnd = Math.max(0, finding.endCol - 1);
  const endCharacter = endLine === startLine ? Math.max(startCharacter + 1, rawEnd) : rawEnd;
  return new vscode.Range(startLine, startCharacter, endLine, endCharacter);
}

function mapSeverity(severity: string | undefined): vscode.DiagnosticSeverity {
  switch (severity) {
    case "Error": return vscode.DiagnosticSeverity.Error;
    case "Warning":
    case "PotentialBug":
    case "Performance": return vscode.DiagnosticSeverity.Warning;
    default: return vscode.DiagnosticSeverity.Information;
  }
}

export class DismissCodeActionProvider implements vscode.CodeActionProvider {
  provideCodeActions(
    _document: vscode.TextDocument,
    _range: vscode.Range,
    context: vscode.CodeActionContext
  ): vscode.CodeAction[] {
    const actions: vscode.CodeAction[] = [];
    for (const diagnostic of context.diagnostics as PluStanDiagnostic[]) {
      if (diagnostic.source !== "plu-stan" || !diagnostic.plustanFingerprint) {
        continue;
      }
      const action = new vscode.CodeAction("Plu-Stan: Dismiss this finding", vscode.CodeActionKind.QuickFix);
      action.diagnostics = [diagnostic];
      action.command = {
        command: "plustan.dismissFinding",
        title: "Dismiss",
        arguments: [{ fingerprint: diagnostic.plustanFingerprint, inspectionId: diagnostic.plustanInspectionId }]
      };
      actions.push(action);
    }
    return actions;
  }
}
