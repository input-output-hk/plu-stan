import * as vscode from "vscode";
import { DismissalEntry, DismissalsFile, addDismissal, emptyDismissals, parseDismissals, removeDismissal, serializeDismissals } from "../core/dismissals";

const FILE_PATH = ".plustan/dismissals.json";

export class DismissalsStore {
  // Serializes mutations: each add/remove chains off the previous one so two
  // overlapping load-modify-write cycles can't clobber each other. The tail is
  // always kept in a resolved state (see `enqueue`) so a failed op never
  // poisons the chain and blocks later writes.
  private queue: Promise<unknown> = Promise.resolve();

  constructor(private readonly folder: vscode.WorkspaceFolder) {}

  private get uri(): vscode.Uri {
    return vscode.Uri.joinPath(this.folder.uri, FILE_PATH);
  }

  async load(): Promise<DismissalsFile> {
    try {
      const bytes = await vscode.workspace.fs.readFile(this.uri);
      return parseDismissals(Buffer.from(bytes).toString("utf8"));
    } catch (err) {
      // A missing file is the normal "no dismissals yet" case. Anything else
      // (permissions, transient I/O, path-is-a-directory) must propagate so
      // callers never trigger a save that overwrites an existing-but-unreadable
      // file and wipes real dismissals.
      if (err instanceof vscode.FileSystemError && err.code === "FileNotFound") {
        return emptyDismissals();
      }
      throw err;
    }
  }

  add(entry: DismissalEntry): Promise<DismissalsFile> {
    return this.enqueue(() => this.mutate((file) => addDismissal(file, entry)));
  }

  remove(fingerprint: string): Promise<DismissalsFile> {
    return this.enqueue(() => this.mutate((file) => removeDismissal(file, fingerprint)));
  }

  /**
   * Runs `op` after all previously enqueued ops have settled. The caller gets
   * the real result (or rejection) of their own op via the returned promise,
   * while the chain tail is sanitized to always resolve so a rejection here
   * cannot stall subsequent writes.
   */
  private enqueue<T>(op: () => Promise<T>): Promise<T> {
    const run = this.queue.then(() => op());
    this.queue = run.then(
      () => undefined,
      () => undefined
    );
    return run;
  }

  private async mutate(apply: (file: DismissalsFile) => DismissalsFile): Promise<DismissalsFile> {
    const next = apply(await this.load());
    await this.save(next);
    return next;
  }

  private async save(file: DismissalsFile): Promise<void> {
    await vscode.workspace.fs.createDirectory(vscode.Uri.joinPath(this.folder.uri, ".plustan"));
    await vscode.workspace.fs.writeFile(this.uri, Buffer.from(serializeDismissals(file), "utf8"));
  }
}
