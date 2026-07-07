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
  // `dismissedFingerprints` is the COMPLETE source of truth for this run: the
  // caller must pass the full current dismissal set every run, not a delta.
  // A re-reported finding whose fingerprint is absent from this set will reopen.
  // (This matches the design: the dismissals file is durable truth, read fresh
  // each run.)
  | { type: "runCompleted"; coveredFiles: string[]; observations: ObservationV2[]; dismissedFingerprints: string[] }
  | { type: "fileEdited"; file: string }
  | { type: "findingDismissed"; fingerprint: string }
  | { type: "findingUndismissed"; fingerprint: string }
  | { type: "sessionEnded" };

/**
 * Compile-time exhaustiveness guard: adding a SessionEvent variant without
 * handling it in reduceSession turns into a type error at the call to this
 * helper (its argument is `never` only when every variant is handled).
 */
function assertNever(event: never): never {
  throw new Error(`Unhandled SessionEvent: ${JSON.stringify(event)}`);
}

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
      let changed = false;
      const findings = { ...state.findings };
      for (const f of Object.values(state.findings)) {
        if (f.file === event.file && f.status === "open") {
          findings[f.fingerprint] = { ...f, status: "stale" };
          changed = true;
        }
      }
      // Return the SAME reference when nothing transitioned so callers can skip
      // re-persisting/re-publishing on no-op edits (e.g. every keystroke).
      return changed ? { ...state, findings } : state;
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

    default:
      return assertNever(event);
  }
}

export function countByStatus(state: SessionState): Record<FindingStatus, number> {
  const counts: Record<FindingStatus, number> = { open: 0, stale: 0, fixed: 0, dismissed: 0 };
  for (const f of Object.values(state.findings)) {
    counts[f.status] += 1;
  }
  return counts;
}
