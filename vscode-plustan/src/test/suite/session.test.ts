import * as assert from "node:assert";
import * as vscode from "vscode";

async function poll<T>(fn: () => T | undefined, timeoutMs = 30_000): Promise<T> {
  const start = Date.now();
  for (;;) {
    const value = fn();
    if (value !== undefined) {
      return value;
    }
    if (Date.now() - start > timeoutMs) {
      throw new Error("poll timed out");
    }
    await new Promise((r) => setTimeout(r, 250));
  }
}

describe("review session (integration)", () => {
  it("start review produces diagnostics from the stub binary", async () => {
    // "all" skips the module-scope QuickPick (which would block a headless test)
    await vscode.commands.executeCommand("plustan.startReview", "all");
    const diags = await poll(() => {
      const all = vscode.languages.getDiagnostics()
        .flatMap(([, ds]) => ds)
        .filter((d) => d.source === "plu-stan");
      return all.length >= 2 ? all : undefined;
    });
    assert.strictEqual(diags.length, 2);
    assert.ok(diags[0].message.includes("PLU-STAN-04"));
  });
});
