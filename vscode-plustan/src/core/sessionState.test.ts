import * as assert from "node:assert";
import { countByStatus, initialSessionState, reduceSession, SessionState } from "./sessionState";
import { ObservationV2 } from "./schema";

const obs = (fingerprint: string, file = "src/V.hs", line = 3): ObservationV2 => ({
  id: "o", inspectionId: "PLU-STAN-04", fingerprint,
  file, moduleName: "V", startLine: line, startCol: 1, endLine: line, endCol: 10
});

const started = (): SessionState =>
  reduceSession(initialSessionState, { type: "sessionStarted", startedAt: "2026-07-06T10:00:00Z" });

const afterRun = (state: SessionState, observations: ObservationV2[], dismissed: string[] = []): SessionState =>
  reduceSession(state, { type: "runCompleted", coveredFiles: ["src/V.hs"], observations, dismissedFingerprints: dismissed });

describe("session reducer", () => {
  it("marks findings from a run as open", () => {
    const s = afterRun(started(), [obs("f1"), obs("f2", "src/V.hs", 7)]);
    assert.strictEqual(countByStatus(s).open, 2);
  });
  it("marks a finding fixed when a later run over its file no longer reports it", () => {
    const s1 = afterRun(started(), [obs("f1"), obs("f2", "src/V.hs", 7)]);
    const s2 = afterRun(s1, [obs("f2", "src/V.hs", 7)]);
    assert.strictEqual(s2.findings["f1"].status, "fixed");
    assert.strictEqual(s2.findings["f2"].status, "open");
  });
  it("does not touch findings in files not covered by the run", () => {
    const s1 = afterRun(started(), [obs("f1"), obs("g1", "src/Other.hs")]);
    const s2 = afterRun(s1, []); // covers only src/V.hs
    assert.strictEqual(s2.findings["f1"].status, "fixed");
    assert.strictEqual(s2.findings["g1"].status, "open");
  });
  it("marks open findings stale when their file is edited, and re-opens on re-run", () => {
    const s1 = afterRun(started(), [obs("f1")]);
    const s2 = reduceSession(s1, { type: "fileEdited", file: "src/V.hs" });
    assert.strictEqual(s2.findings["f1"].status, "stale");
    const s3 = afterRun(s2, [obs("f1")]);
    assert.strictEqual(s3.findings["f1"].status, "open");
  });
  it("applies and persists dismissals across runs", () => {
    const s1 = afterRun(started(), [obs("f1")], ["f1"]);
    assert.strictEqual(s1.findings["f1"].status, "dismissed");
    const s2 = afterRun(s1, [obs("f1")], ["f1"]);
    assert.strictEqual(s2.findings["f1"].status, "dismissed");
  });
  it("dismisses and undismisses interactively", () => {
    const s1 = afterRun(started(), [obs("f1")]);
    const s2 = reduceSession(s1, { type: "findingDismissed", fingerprint: "f1" });
    assert.strictEqual(s2.findings["f1"].status, "dismissed");
    const s3 = reduceSession(s2, { type: "findingUndismissed", fingerprint: "f1" });
    assert.strictEqual(s3.findings["f1"].status, "open");
  });
  it("ends the session back to idle but keeps findings for the summary", () => {
    const s = reduceSession(afterRun(started(), [obs("f1")]), { type: "sessionEnded" });
    assert.strictEqual(s.phase, "idle");
    assert.strictEqual(Object.keys(s.findings).length, 1);
  });

  // --- Extra tests pinning subtler reducer behavior ---

  it("does not resurrect a dismissed finding as fixed when a later covering run omits it", () => {
    const s1 = afterRun(started(), [obs("f1")], ["f1"]);
    assert.strictEqual(s1.findings["f1"].status, "dismissed");
    // Run again, still covering src/V.hs, but f1 is no longer reported at all.
    const s2 = afterRun(s1, []);
    assert.strictEqual(s2.findings["f1"].status, "dismissed");
  });

  it("re-opens a previously dismissed finding if it is re-reported without being re-dismissed", () => {
    const s1 = afterRun(started(), [obs("f1")], ["f1"]);
    assert.strictEqual(s1.findings["f1"].status, "dismissed");
    // The run report re-observes f1, but the dismissals file no longer lists it:
    // the dismissals file is the source of truth for each run, so f1 opens back up.
    const s2 = afterRun(s1, [obs("f1")], []);
    assert.strictEqual(s2.findings["f1"].status, "open");
  });

  it("fileEdited only stales open findings in the edited file", () => {
    const s1 = afterRun(started(), [obs("f1"), obs("g1", "src/Other.hs")]);
    // Manually drive f2 to fixed and f3 to dismissed within src/V.hs to check they aren't re-staled.
    const withExtra = afterRun(s1, [obs("f1"), obs("f3", "src/V.hs", 9)], ["f3"]);
    assert.strictEqual(withExtra.findings["f3"].status, "dismissed");
    const s2 = reduceSession(withExtra, { type: "fileEdited", file: "src/V.hs" });
    assert.strictEqual(s2.findings["f1"].status, "stale");
    assert.strictEqual(s2.findings["f3"].status, "dismissed", "dismissed findings are not re-staled");
    assert.strictEqual(s2.findings["g1"].status, "open", "findings in other files are untouched");
  });

  it("fileEdited does not re-stale an already-fixed finding", () => {
    const s1 = afterRun(started(), [obs("f1")]);
    const s2 = afterRun(s1, []); // f1 becomes fixed (no longer reported, but file still covered)
    assert.strictEqual(s2.findings["f1"].status, "fixed");
    const s3 = reduceSession(s2, { type: "fileEdited", file: "src/V.hs" });
    assert.strictEqual(s3.findings["f1"].status, "fixed");
  });

  it("counts totals across a mixed state", () => {
    const s1 = afterRun(started(), [obs("f1"), obs("f2", "src/V.hs", 7), obs("f3", "src/V.hs", 9)]);
    const s2 = reduceSession(s1, { type: "findingDismissed", fingerprint: "f3" });
    const s3 = reduceSession(s2, { type: "fileEdited", file: "src/V.hs" });
    // f1, f2 -> stale (were open); f3 -> dismissed (unaffected by edit)
    const s4 = afterRun(s3, [obs("f1")]); // f1 re-opens, f2 becomes fixed, f3 stays dismissed
    const counts = countByStatus(s4);
    assert.strictEqual(counts.open, 1);
    assert.strictEqual(counts.fixed, 1);
    assert.strictEqual(counts.dismissed, 1);
    assert.strictEqual(counts.stale, 0);
    assert.strictEqual(counts.open + counts.fixed + counts.dismissed + counts.stale, 3);
  });

  it("sessionStarted clears findings from a prior session", () => {
    const s1 = afterRun(started(), [obs("f1")]);
    assert.strictEqual(Object.keys(s1.findings).length, 1);
    const s2 = reduceSession(s1, { type: "sessionStarted", startedAt: "2026-07-06T11:00:00Z" });
    assert.strictEqual(Object.keys(s2.findings).length, 0);
    assert.strictEqual(s2.phase, "active");
    assert.strictEqual(s2.runCount, 0);
  });
});
