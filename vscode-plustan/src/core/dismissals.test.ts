import * as assert from "node:assert";
import { addDismissal, emptyDismissals, parseDismissals, removeDismissal, serializeDismissals } from "./dismissals";

describe("dismissals", () => {
  it("round-trips through serialize/parse", () => {
    const d1 = addDismissal(emptyDismissals(), {
      fingerprint: "f1", inspectionId: "PLU-STAN-04", note: "intentional", dismissedAt: "2026-07-06T10:00:00Z"
    });
    const d2 = parseDismissals(serializeDismissals(d1));
    assert.deepStrictEqual(d2, d1);
  });
  it("dedupes by fingerprint", () => {
    const base = addDismissal(emptyDismissals(), { fingerprint: "f1", inspectionId: "X", dismissedAt: "t" });
    const twice = addDismissal(base, { fingerprint: "f1", inspectionId: "X", dismissedAt: "t2" });
    assert.strictEqual(twice.dismissals.length, 1);
    assert.strictEqual(twice.dismissals[0].dismissedAt, "t2");
  });
  it("removes by fingerprint", () => {
    const base = addDismissal(emptyDismissals(), { fingerprint: "f1", inspectionId: "X", dismissedAt: "t" });
    assert.strictEqual(removeDismissal(base, "f1").dismissals.length, 0);
  });
  it("tolerates broken file content", () => {
    assert.deepStrictEqual(parseDismissals("not json {"), emptyDismissals());
    assert.deepStrictEqual(parseDismissals('{"version":1}'), emptyDismissals());
  });
  it("drops malformed entries but keeps valid ones", () => {
    const parsed = parseDismissals(
      '{"version":1,"dismissals":[{"fingerprint":"f1","inspectionId":"X","dismissedAt":"t"},{"note":"garbage, no fingerprint"}]}'
    );
    assert.strictEqual(parsed.dismissals.length, 1);
    assert.strictEqual(parsed.dismissals[0].fingerprint, "f1");
  });
  it("moves a re-added entry to the end", () => {
    let d = addDismissal(emptyDismissals(), { fingerprint: "f1", inspectionId: "X", dismissedAt: "t1" });
    d = addDismissal(d, { fingerprint: "f2", inspectionId: "X", dismissedAt: "t2" });
    d = addDismissal(d, { fingerprint: "f1", inspectionId: "X", dismissedAt: "t3" });
    assert.deepStrictEqual(d.dismissals.map((e) => e.fingerprint), ["f2", "f1"]);
  });
});
