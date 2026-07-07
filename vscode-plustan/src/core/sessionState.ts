import { ObservationV2 } from "./schema";

export type FindingStatus = "open" | "stale" | "fixed" | "dismissed";

export interface SessionFinding extends ObservationV2 {
  status: FindingStatus;
  lastSeenRun: number;
}

export interface SessionState {
  phase: "idle" | "active";
  startedAt: string | null;
  runCount: number;
  findings: Record<string, SessionFinding>;
}

export type SessionEvent =
  | { type: "sessionStarted"; startedAt: string }
  | { type: "runCompleted"; coveredFiles: string[]; observations: ObservationV2[]; dismissedFingerprints: string[] }
  | { type: "fileEdited"; file: string }
  | { type: "findingDismissed"; fingerprint: string }
  | { type: "findingUndismissed"; fingerprint: string }
  | { type: "sessionEnded" };

export const initialSessionState: SessionState = {
  phase: "idle",
  startedAt: null,
  runCount: 0,
  findings: {}
};

export function reduceSession(state: SessionState, event: SessionEvent): SessionState {
  switch (event.type) {
    case "sessionStarted":
      return { phase: "active", startedAt: event.startedAt, runCount: 0, findings: {} };

    case "sessionEnded":
      return { ...state, phase: "idle" };

    case "runCompleted": {
      const covered = new Set(event.coveredFiles);
      const dismissed = new Set(event.dismissedFingerprints);
      const runCount = state.runCount + 1;
      const findings: Record<string, SessionFinding> = {};

      // Carry forward everything in files this run did not cover.
      for (const f of Object.values(state.findings)) {
        if (!covered.has(f.file)) {
          findings[f.fingerprint] = f;
        }
      }
      // Everything reported by this run is open (or dismissed).
      for (const o of event.observations) {
        findings[o.fingerprint] = {
          ...o,
          status: dismissed.has(o.fingerprint) ? "dismissed" : "open",
          lastSeenRun: runCount
        };
      }
      // Previously-known findings in covered files that were NOT re-reported: fixed.
      for (const f of Object.values(state.findings)) {
        if (covered.has(f.file) && findings[f.fingerprint] === undefined) {
          findings[f.fingerprint] =
            f.status === "open" || f.status === "stale" ? { ...f, status: "fixed" } : f;
        }
      }
      return { ...state, runCount, findings };
    }

    case "fileEdited": {
      const findings = { ...state.findings };
      for (const f of Object.values(findings)) {
        if (f.file === event.file && f.status === "open") {
          findings[f.fingerprint] = { ...f, status: "stale" };
        }
      }
      return { ...state, findings };
    }

    case "findingDismissed":
    case "findingUndismissed": {
      const f = state.findings[event.fingerprint];
      if (!f) {
        return state;
      }
      const status: FindingStatus = event.type === "findingDismissed" ? "dismissed" : "open";
      return { ...state, findings: { ...state.findings, [event.fingerprint]: { ...f, status } } };
    }
  }
}

export function countByStatus(state: SessionState): Record<FindingStatus, number> {
  const counts: Record<FindingStatus, number> = { open: 0, stale: 0, fixed: 0, dismissed: 0 };
  for (const f of Object.values(state.findings)) {
    counts[f.status] += 1;
  }
  return counts;
}
