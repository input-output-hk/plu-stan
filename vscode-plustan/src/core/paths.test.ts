import * as assert from "node:assert";
import { resolveFindingPath } from "./paths";

// A fake `exists` that only knows about the paths we say are on disk.
const existsIn = (present: string[]) => (p: string) => present.includes(p);

describe("resolveFindingPath", () => {
  const file = "src/Cardano/Djed/OnChain/CustomRatio.hs";

  it("resolves the real-world broken config: projectDir unset, hieDir under a package subdir", () => {
    // The exact stablecoin case: projectDir defaults to the workspace root and
    // hieDir points at <workspaceRoot>/onchain/.hie. The file lives under
    // onchain/, so it must resolve via the dirname(hieDir) candidate.
    const ctx = {
      projectDir: "/ws",
      hieDir: "/ws/onchain/.hie",
      workspaceRoot: "/ws"
    };
    const resolved = resolveFindingPath(file, ctx, existsIn(["/ws/onchain/" + file]));
    assert.strictEqual(resolved, "/ws/onchain/src/Cardano/Djed/OnChain/CustomRatio.hs");
  });

  it("resolves the intended config: projectDir set to the package", () => {
    const ctx = { projectDir: "/ws/onchain", hieDir: ".hie", workspaceRoot: "/ws" };
    const resolved = resolveFindingPath(file, ctx, existsIn(["/ws/onchain/" + file]));
    assert.strictEqual(resolved, "/ws/onchain/src/Cardano/Djed/OnChain/CustomRatio.hs");
  });

  it("resolves a single-package project where package == workspace root", () => {
    const ctx = { projectDir: "/ws", hieDir: ".hie", workspaceRoot: "/ws" };
    const resolved = resolveFindingPath(file, ctx, existsIn(["/ws/" + file]));
    assert.strictEqual(resolved, "/ws/src/Cardano/Djed/OnChain/CustomRatio.hs");
  });

  it("returns an absolute file path unchanged", () => {
    const abs = "/somewhere/CustomRatio.hs";
    const ctx = { projectDir: "/ws/onchain", hieDir: ".hie", workspaceRoot: "/ws" };
    assert.strictEqual(resolveFindingPath(abs, ctx, existsIn([])), abs);
  });

  it("prefers projectDir when the file exists under multiple bases", () => {
    const ctx = { projectDir: "/ws/onchain", hieDir: "/ws/onchain/.hie", workspaceRoot: "/ws" };
    const resolved = resolveFindingPath(
      file,
      ctx,
      existsIn(["/ws/onchain/" + file, "/ws/" + file])
    );
    assert.strictEqual(resolved, "/ws/onchain/src/Cardano/Djed/OnChain/CustomRatio.hs");
  });

  it("falls back to projectDir when nothing exists on disk", () => {
    const ctx = { projectDir: "/ws/onchain", hieDir: ".hie", workspaceRoot: "/ws" };
    const resolved = resolveFindingPath(file, ctx, existsIn([]));
    assert.strictEqual(resolved, "/ws/onchain/src/Cardano/Djed/OnChain/CustomRatio.hs");
  });
});
