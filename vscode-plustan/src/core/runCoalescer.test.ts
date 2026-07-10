import * as assert from "node:assert";
import { RunCoalescer } from "./runCoalescer";

describe("RunCoalescer", () => {
  it("coalesces duplicate module requests", () => {
    const q = new RunCoalescer();
    q.request({ kind: "module", moduleName: "A" });
    q.request({ kind: "module", moduleName: "A" });
    q.request({ kind: "module", moduleName: "B" });
    assert.strictEqual(q.size, 2);
  });
  it("a workspace request subsumes all pending module requests", () => {
    const q = new RunCoalescer();
    q.request({ kind: "module", moduleName: "A" });
    q.request({ kind: "workspace" });
    assert.strictEqual(q.size, 1);
    assert.deepStrictEqual(q.takeNext(), { kind: "workspace" });
  });
  it("module requests while a workspace run is pending are absorbed", () => {
    const q = new RunCoalescer();
    q.request({ kind: "workspace" });
    q.request({ kind: "module", moduleName: "A" });
    assert.strictEqual(q.size, 1);
  });
  it("serves FIFO otherwise", () => {
    const q = new RunCoalescer();
    q.request({ kind: "module", moduleName: "A" });
    q.request({ kind: "module", moduleName: "B" });
    assert.deepStrictEqual(q.takeNext(), { kind: "module", moduleName: "A" });
    assert.deepStrictEqual(q.takeNext(), { kind: "module", moduleName: "B" });
    assert.strictEqual(q.takeNext(), undefined);
  });
});
