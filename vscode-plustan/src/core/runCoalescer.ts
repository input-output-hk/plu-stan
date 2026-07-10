export type PendingRun = { kind: "workspace" } | { kind: "module"; moduleName: string };

/** Pending analysis runs: duplicates coalesce, workspace subsumes modules. */
export class RunCoalescer {
  private pending: PendingRun[] = [];

  request(run: PendingRun): void {
    if (this.pending.some((p) => p.kind === "workspace")) {
      return; // a pending workspace run already covers everything
    }
    if (run.kind === "workspace") {
      this.pending = [{ kind: "workspace" }];
      return;
    }
    if (!this.pending.some((p) => p.kind === "module" && p.moduleName === run.moduleName)) {
      this.pending.push(run);
    }
  }

  takeNext(): PendingRun | undefined {
    return this.pending.shift();
  }

  get size(): number {
    return this.pending.length;
  }
}
