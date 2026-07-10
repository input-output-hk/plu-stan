import * as path from "node:path";
import * as vscode from "vscode";
import { InspectionV2 } from "../core/schema";
import { SessionFinding, SessionState, initialSessionState } from "../core/sessionState";

type Grouping = "severity" | "module";

const SEVERITY_ORDER = ["Error", "Warning", "PotentialBug", "Performance", "Style"];

class GroupItem extends vscode.TreeItem {
  constructor(
    label: string,
    readonly children: (GroupItem | FindingTreeItem)[],
    icon?: string,
    iconColor?: vscode.ThemeColor
  ) {
    super(label, vscode.TreeItemCollapsibleState.Expanded);
    if (icon) {
      this.iconPath = new vscode.ThemeIcon(icon, iconColor);
    }
  }
}

export class FindingTreeItem extends vscode.TreeItem {
  constructor(readonly finding: SessionFinding, inspection: InspectionV2 | undefined) {
    super(`${path.basename(finding.file)}:${finding.startLine}`, vscode.TreeItemCollapsibleState.None);
    this.description = inspection ? inspection.name : finding.inspectionId;
    this.tooltip = `[${finding.inspectionId}] ${finding.file}:${finding.startLine}:${finding.startCol}`;
    if (finding.status === "dismissed" && finding.dismissalNote) {
      this.tooltip += `\nDismissed: ${finding.dismissalNote}`;
    }
    this.contextValue = finding.status === "dismissed" ? "plustanDismissedFinding" : "plustanFinding";
    // Icon *shape* encodes status; icon *color* encodes severity. Active
    // findings (open/stale) get the severity colour so the tree reads as a
    // colour-coded list; resolved findings are neutral (green check / muted slash).
    const shape =
      finding.status === "stale" ? "history"
        : finding.status === "fixed" ? "check"
        : finding.status === "dismissed" ? "circle-slash"
        : "circle-filled";
    const color =
      finding.status === "fixed" ? new vscode.ThemeColor("charts.green")
        : finding.status === "dismissed" ? new vscode.ThemeColor("descriptionForeground")
        : severityColor(inspection?.severity);
    this.iconPath = new vscode.ThemeIcon(shape, color);
    if (finding.status === "stale") {
      this.description = `~ ${this.description ?? ""}`;
    }
    this.command = { command: "plustan.openFinding", title: "Open Finding", arguments: [this] };
  }
}

type Node = GroupItem | FindingTreeItem;

export class FindingsTreeProvider implements vscode.TreeDataProvider<Node> {
  private state: SessionState = initialSessionState;
  private inspections = new Map<string, InspectionV2>();
  private grouping: Grouping = "severity";
  private readonly emitter = new vscode.EventEmitter<Node | undefined>();
  readonly onDidChangeTreeData = this.emitter.event;

  setData(state: SessionState, inspections: Map<string, InspectionV2>): void {
    this.state = state;
    this.inspections = inspections;
    this.emitter.fire(undefined);
  }

  toggleGrouping(): void {
    this.grouping = this.grouping === "severity" ? "module" : "severity";
    this.emitter.fire(undefined);
  }

  getTreeItem(element: Node): vscode.TreeItem {
    return element;
  }

  getChildren(element?: Node): Node[] {
    if (element) {
      return element instanceof GroupItem ? element.children : [];
    }
    if (this.state.phase === "idle") {
      const idle = new vscode.TreeItem("No active review — press ▶ to start");
      return [idle as Node];
    }
    return this.buildRoots();
  }

  private buildRoots(): Node[] {
    const all = Object.values(this.state.findings);
    const active = all.filter((f) => f.status === "open" || f.status === "stale");
    const fixed = all.filter((f) => f.status === "fixed");
    const dismissed = all.filter((f) => f.status === "dismissed");

    const roots: Node[] = this.grouping === "severity"
      ? this.groupBySeverity(active)
      : this.groupByModule(active);

    if (fixed.length > 0) {
      const node = new GroupItem(`Fixed this session (${fixed.length})`, fixed.map((f) => this.item(f)), "check");
      node.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
      roots.push(node);
    }
    if (dismissed.length > 0) {
      const node = new GroupItem(`Dismissed (${dismissed.length})`, dismissed.map((f) => this.item(f)), "circle-slash");
      node.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
      roots.push(node);
    }
    return roots;
  }

  private groupBySeverity(findings: SessionFinding[]): Node[] {
    const severityOf = (f: SessionFinding): string => this.inspections.get(f.inspectionId)?.severity ?? "Warning";
    const severities = [...new Set(findings.map(severityOf))]
      .sort((a, b) => SEVERITY_ORDER.indexOf(a) - SEVERITY_ORDER.indexOf(b));
    return severities.map((severity) => {
      const inSeverity = findings.filter((f) => severityOf(f) === severity);
      const ruleIds = [...new Set(inSeverity.map((f) => f.inspectionId))].sort();
      const ruleNodes = ruleIds.map((ruleId) => {
        const inRule = inSeverity.filter((f) => f.inspectionId === ruleId).sort(bySpan);
        const name = this.inspections.get(ruleId)?.name ?? "";
        return new GroupItem(
          `${ruleId} ${name} (${inRule.length})`,
          inRule.map((f) => this.item(f)),
          "circle-filled",
          severityColor(severity)
        );
      });
      return new GroupItem(`${severity} (${inSeverity.length})`, ruleNodes, severityIcon(severity), severityColor(severity));
    });
  }

  private groupByModule(findings: SessionFinding[]): Node[] {
    const modules = [...new Set(findings.map((f) => f.moduleName))].sort();
    return modules.map((moduleName) => {
      const inModule = findings.filter((f) => f.moduleName === moduleName).sort(bySpan);
      return new GroupItem(`${moduleName} (${inModule.length})`, inModule.map((f) => this.item(f)), "symbol-module");
    });
  }

  private item(f: SessionFinding): FindingTreeItem {
    return new FindingTreeItem(f, this.inspections.get(f.inspectionId));
  }
}

function bySpan(a: SessionFinding, b: SessionFinding): number {
  return a.file.localeCompare(b.file) || a.startLine - b.startLine || a.startCol - b.startCol;
}

function severityIcon(severity: string): string {
  return severity === "Error" ? "error" : severity === "Performance" ? "zap" : "warning";
}

/**
 * Severity → theme colour for icon tinting. Uses the built-in `charts.*`
 * palette so it adapts to light/dark themes. `undefined` (unknown severity)
 * falls back to the icon's default colour.
 */
function severityColor(severity: string | undefined): vscode.ThemeColor | undefined {
  switch (severity) {
    case "Error": return new vscode.ThemeColor("charts.red");
    case "Warning": return new vscode.ThemeColor("charts.yellow");
    case "PotentialBug": return new vscode.ThemeColor("charts.orange");
    case "Performance": return new vscode.ThemeColor("charts.blue");
    case "Style": return new vscode.ThemeColor("charts.purple");
    default: return undefined;
  }
}
