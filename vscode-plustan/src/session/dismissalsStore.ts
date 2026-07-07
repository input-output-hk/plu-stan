import * as vscode from "vscode";
import { DismissalEntry, DismissalsFile, addDismissal, emptyDismissals, parseDismissals, removeDismissal, serializeDismissals } from "../core/dismissals";

const FILE_PATH = ".plustan/dismissals.json";

export class DismissalsStore {
  constructor(private readonly folder: vscode.WorkspaceFolder) {}

  private get uri(): vscode.Uri {
    return vscode.Uri.joinPath(this.folder.uri, FILE_PATH);
  }

  async load(): Promise<DismissalsFile> {
    try {
      const bytes = await vscode.workspace.fs.readFile(this.uri);
      return parseDismissals(Buffer.from(bytes).toString("utf8"));
    } catch {
      return emptyDismissals(); // file absent
    }
  }

  async add(entry: DismissalEntry): Promise<DismissalsFile> {
    const next = addDismissal(await this.load(), entry);
    await this.save(next);
    return next;
  }

  async remove(fingerprint: string): Promise<DismissalsFile> {
    const next = removeDismissal(await this.load(), fingerprint);
    await this.save(next);
    return next;
  }

  private async save(file: DismissalsFile): Promise<void> {
    await vscode.workspace.fs.createDirectory(vscode.Uri.joinPath(this.folder.uri, ".plustan"));
    await vscode.workspace.fs.writeFile(this.uri, Buffer.from(serializeDismissals(file), "utf8"));
  }
}
