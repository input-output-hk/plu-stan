# Plu-Stan VS Code Extension: Review Cockpit — Design

**Date:** 2026-07-06
**Status:** Approved design, pending implementation plan
**Scope:** Evolution of `vscode-plustan` (currently a v0.2.2 MVP CLI wrapper) plus the backend JSON contract changes it requires.

## Problem

The current extension is a thin CLI wrapper: the user manually clicks "Run", the extension spawns `plustan`, and findings land in the Problems panel as bare `[PLU-STAN-XX] Rule name` messages. There is no re-run loop, no explanation of what a rule means or why it matters on-chain, no way to dismiss a false positive that survives re-runs, no tracking of progress, and no staleness signal when code changes after a run.

## Product decisions (from brainstorm)

- **Usage model:** on-demand review (audit cockpit), not an always-on linter. The user deliberately reviews a contract at milestones.
- **Primary user:** the contract author self-reviewing before commit/deploy. Deliverable of a session is fixed code, not a report. Dismissals are informal (no justification workflow), but persist.
- **Fix loop:** rich in-editor explanations, quick-fix code actions where mechanical, one-click dismiss, and AI handoff are all in scope (phased).
- **Re-run model:** auto re-run on save, gated by an explicit **review session** — outside a session, no surprise builds ever.
- **Primary surface:** findings tree in the Plu-Stan sidebar + a detail panel for the selected finding. Squiggles/Problems remain secondary surfaces.
- **Architecture:** evolve the spawn-per-run CLI wrapper (Approach A), with the extension↔analyzer boundary shaped so a future daemon backend is a transport swap, not a redesign.

## Architecture

Two components, one repo:

- **TypeScript extension** — owns all session state, UX, dismissal storage, and run orchestration.
- **`plustan` CLI** — stays a stateless, spawn-per-request analyzer emitting JSON on stdout.

All extension↔analyzer traffic goes through one TS interface:

```ts
interface AnalyzerClient {
  capabilities(): Promise<Capabilities>;         // schema version + features
  listOnchain(): Promise<ListOnchainPayload>;
  analyze(scope: RunScope): Promise<AnalyzePayload>;  // scope: workspace | module
}
```

`SpawnAnalyzerClient` (spawn-per-run, today) is the first implementation; a future `DaemonAnalyzerClient` is the second. Nothing above this interface may assume a process-per-request model.

### Backend contract changes (JSON schema v2)

1. **Rich inspection docs.** Each inspection carries its full teaching content:
   `description`, `whyItMatters` (the Plinth/on-chain rationale), `badExample` and
   `goodExample` code blocks, `remediation` steps, and a docs anchor into the rules
   documentation. The analyzer binary is the single source of truth for rule
   content; the extension is a renderer. Extension and rule versions cannot drift.

2. **Stable finding fingerprints.** Current observation IDs embed line:col, so any
   edit above a finding changes its identity. New `fingerprint` field per
   observation: `hash(inspectionId, moduleName, sourceTextOfFlaggedSpan)`.
   Properties: survives edits elsewhere in the file; changes when the flagged
   expression itself changes (correct — a changed expression deserves re-triage).
   Fingerprints are the key for dismissals and for open/fixed tracking across runs.
   Duplicate fingerprints within one run (identical flagged text, same module, same
   rule) are disambiguated by an occurrence index appended in span order.

3. **Capabilities handshake.** New `plustan capabilities --json` subcommand
   returning `{schemaVersion, features: [...]}`. The extension calls it once per
   session; on schema mismatch it prompts to update the binary (existing download
   manager flow) instead of failing mid-run with a JSON parse error.

4. **Backend-computed fixes (Phase 2).** When a rule can produce a mechanically
   safe rewrite, its observation carries `fix: {span, replacement, title}`.
   Fixes are computed in Haskell (which has the AST) and applied in TypeScript as a
   `WorkspaceEdit`. The extension never rewrites Haskell source itself.

### Dismissal storage

`.plustan/dismissals.json` in the workspace root — extension-managed,
repo-committable:

```json
{ "version": 1,
  "dismissals": [
    { "fingerprint": "…", "inspectionId": "PLU-STAN-04",
      "note": "credential-only check is intentional here", "dismissedAt": "2026-07-06T10:00:00Z" }
  ] }
```

Phase 1: the extension filters dismissed findings client-side after each run.
Phase 3: the CLI learns to read the same file so CI runs agree with the editor.
Stan's native `.stan.toml` observation ignores are deliberately not used: they are
position-based and break on edit.

### Run orchestration

During an active session: file save → debounce ~500 ms → `analyze(module)` for the
saved module (the CLI auto-builds as it already does). A newer save for the same
scope cancels/coalesces the in-flight run. One run executes at a time; module runs
queue behind a workspace run. Outside a session, saves trigger nothing.

## Review session & UX components

### Session state machine

`idle → active → ended`. Started from the sidebar ("Start Review") or the command
palette; scoped to the workspace or a chosen set of onchain modules. While active:

- auto re-run on save is armed;
- each finding carries a status: **open**, **fixed** (present in a prior run,
  absent from the latest run of its module), **dismissed**, or **stale** (its file
  was edited since the run that produced it);
- session state (findings, statuses, run count, started-at) persists in VS Code
  workspace storage, so a window reload resumes the review.

A status bar item shows `Plu-Stan: 8 open · 3 fixed` with a spinner while a run is
in flight. Ending the session disarms auto-runs and writes a plain-text summary to
the output channel.

### Findings tree

Replaces the module list as the sidebar centerpiece (modules remain a secondary
collapsed section). Default grouping: severity → rule → finding (`file:line —
snippet`); one toggle regroups by module. Stale findings render dimmed with a `~`
marker. Fixed findings move to a collapsed "Fixed this session" node so progress is
visible. Inline per-finding icons: open file, dismiss, ask AI.

### Detail panel

A webview view docked in the Plu-Stan sidebar below the tree. Selecting a finding
(tree click, or the cursor landing on a plu-stan squiggle) renders: rule
ID/name/severity/category · why it matters on-chain · bad/good example blocks · the
flagged snippet · action buttons **[Quick Fix] [Dismiss] [Ask AI] [Rule docs]**.
The panel renders schema-v2 payload content only — it holds no rule knowledge of
its own.

### Diagnostics & code actions

Squiggles and the Problems panel stay, with the rule's one-line summary added to
the message. A `CodeActionProvider` attaches to every plu-stan diagnostic:

- *Dismiss this finding* — always offered; appends to `.plustan/dismissals.json`.
- *Ask AI* — always offered (see below).
- *Quick Fix: <title>* — offered when the observation carries a backend `fix`;
  applies it as a `WorkspaceEdit`. First candidate rule: PLU-STAN-08 (make
  non-strict binding strict with a bang pattern); further rules added only when the
  rewrite is provably mechanical.

### AI handoff

*Ask AI* assembles a self-contained markdown bundle: rule doc, flagged code with
`file:line`, surrounding function context, and an instruction to propose a fix
preserving validator semantics. Delivery adapters, tried in order:

1. Copilot Chat (`workbench.action.chat.open` with the bundle as query) when available;
2. Cursor chat when the Cursor environment is detected;
3. clipboard fallback (always works), with a toast confirming the copy;
4. a user-configurable command hook (`plustan.aiHandoffCommand`) for other assistants.

Future direction, explicitly out of scope for this spec: exposing plustan as an MCP
tool so agents can run analyses themselves (connects to the `plugins/` skill
experiments).

## Error handling

- **Broken builds are the normal case** in a fix loop. Build fails on save-triggered
  run → keep findings (marked stale), status bar shows `build failed — results
  stale`, compiler output goes to the output channel, next save retries. No modal
  errors during a session.
- **Schema drift** → caught by the capabilities handshake at session start, with an
  "update binary" prompt (existing download manager).
- **GHC/.hie mismatch, panics, no-JSON output** → existing classification in
  `describePluStanFailure` is kept.
- **Binary missing** → existing offer-download flow is kept.

## Testing

- **Extension unit tests (mocha, no VS Code host):** session reducer transitions,
  fingerprint matching (open/fixed/stale), dismissal filtering, JSON schema-v2
  parsing. These are pure functions by construction.
- **Extension integration tests (`@vscode/test-electron`):** run against the repo's
  existing `target/` fixture project — start session → run → dismiss a finding →
  edit a file → save → assert statuses and tree contents.
- **Backend (cabal test suite):** golden tests for schema-v2 JSON output, the
  capabilities payload, fingerprint stability under unrelated edits, and each
  backend-computed fix.

## Phasing

Three independently shippable phases; each is roughly one implementation plan.

1. **Phase 1 — the cockpit.** Backend: schema v2 (rich docs, fingerprints),
   capabilities handshake. Extension: `AnalyzerClient` refactor, session state
   machine, findings tree, detail panel, dismissals, staleness, auto-rerun-on-save,
   status bar. Delivers the core experience on its own.
2. **Phase 2 — quick fixes.** Backend `fix` emission for mechanically safe rules;
   extension code actions + `WorkspaceEdit` application.
3. **Phase 3 — AI handoff + CI parity.** Ask-AI adapters; CLI reads
   `.plustan/dismissals.json`.

## Out of scope

- Daemon backend / incremental analysis (revisit if save→results latency proves
  painful; the `AnalyzerClient` seam exists for it).
- LSP server / multi-editor support.
- Auditor-oriented triage states and exportable reports.
- MCP/agent integration.

## Known limitations (Phase 1)

- **Duplicate-fingerprint dismissal migration.** When two byte-identical findings
  of the same rule occur in one module, they share a base fingerprint and are
  disambiguated only by a run-local `#2`, `#3`… suffix assigned in span order
  (`uniquifyFingerprints`). Dismissals are keyed on the fingerprint string alone,
  so if the user dismisses the first occurrence (bare `FPR-…`) and then edits or
  fixes it, on the next run the surviving twin inherits the bare fingerprint and
  is silently auto-marked dismissed — a false negative for a finding that was
  never triaged. Trigger is narrow and it is recoverable (undismiss). Phase 2
  hardening: key dismissals on `inspectionId` + span (or anchor base fingerprints
  positionally) rather than the bare fingerprint. Surfaced by the whole-branch
  review, 2026-07-07.
