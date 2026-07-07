# Plu-Stan for VS Code / Cursor

Security and performance static analysis for Cardano smart contracts written in [Plinth](https://github.com/input-output-hk/plutus), delivered as a review cockpit in the **Plu-Stan** sidebar. It runs the local `plustan` CLI, surfaces findings as an interactive tree with per-rule teaching docs, and tracks them through your edit/fix/dismiss workflow.

It is a standard VS Code extension, so it works in both VS Code and Cursor.

## Review workflow

1. **Start Review** — from the ▶ icon in the Findings view title bar, or `Plu-Stan: Start Review` in the command palette. If the workspace has more than one onchain module, you're prompted to pick which ones to review (all preselected); otherwise it reviews everything.
2. Plu-Stan analyzes the selected scope and populates the **Findings** tree, grouped by **severity → rule** by default (toggle to grouping by **module** with `Plu-Stan: Toggle Findings Grouping`). Findings also appear as squiggles in the editor and entries in the Problems panel, and a status bar item shows an open/fixed count for the session.
3. Click a finding to open its location in the editor and populate the **Finding Detail** panel with why it matters, a ✗ bad-example / ✓ good-example pair, how-to-fix guidance, and a link to the full rule documentation (when the backend ships that data for the rule).
4. **Dismiss** a finding you've judged not applicable — from the tree's inline action or the detail panel. You can attach an optional note. Dismissals persist to `.plustan/dismissals.json` and the finding moves to a collapsed "Dismissed" group. **Restore Dismissed Finding** reopens it.
5. Editing a file with open findings marks them **stale** (a `~` prefix and history icon) immediately; saving a reviewed onchain module automatically re-runs analysis for that module and reconciles the findings — fixed ones move to a "Fixed this session" group, newly introduced ones appear as open.
6. **End Review** stops auto re-runs on save and prints a fixed/open/dismissed summary to the Plu-Stan output channel.

## Legacy one-shot commands

`Plu-Stan: Run Workspace` and `Plu-Stan: Run Module` still exist for a quick, session-free pass: they run `plustan` once and publish diagnostics, with no tracking, staleness, or auto re-run. They are **disabled while a review session is active** — use the Findings view, or run `Plu-Stan: End Review` first.

## Requirements

- A `plustan` binary. Set `plustan.binaryPath`, or run **Plu-Stan: Check for Updates** to download one matching your project's GHC (detected from your `.hie` files) via a `plustan capabilities` handshake.
- A buildable Haskell workspace producing `.hie`/`.hi` artifacts — the CLI auto-builds when needed.
- Extension 0.3.x requires `plustan` >= 0.2.5.0 (schema v2 JSON). On a version mismatch, Start Review shows an error with a "Check for Updates" action instead of silently misparsing output.

## Install In Cursor

1. Build extension assets:
```bash
npm install
npm run compile
```
2. Package as VSIX:
```bash
npm run package:vsix
```
3. In Cursor, run `Extensions: Install from VSIX...` and select the generated `.vsix`.

For development host mode (if your `cursor` CLI is installed), you can launch:
```bash
cursor --extensionDevelopmentPath=/path/to/plu-stan/vscode-plustan --disable-extensions /path/to/workspace
```

## Commands

- `Plu-Stan: Start Review`
- `Plu-Stan: End Review`
- `Plu-Stan: Toggle Findings Grouping`
- `Plu-Stan: Open Finding`
- `Plu-Stan: Dismiss Finding`
- `Plu-Stan: Restore Dismissed Finding`
- `Plu-Stan: Run Workspace`
- `Plu-Stan: Run Module`
- `Plu-Stan: Refresh Onchain Modules`
- `Plu-Stan: Clear Diagnostics`
- `Plu-Stan: Show Output`
- `Plu-Stan: Open Settings`
- `Plu-Stan: Check for Updates`

## Settings

- `plustan.binaryPath` — absolute path to the `plustan` executable. Supports a leading `${workspaceFolder}` token, which the extension expands itself (VS Code only expands this in `launch.json`/`tasks.json`, not arbitrary settings). Leave empty to let the extension manage a downloaded binary.
- `plustan.projectDir` — project directory; defaults to the active workspace folder.
- `plustan.hieDir` — directory containing `.hie`/`.hi` files, relative to `projectDir` unless absolute.
- `plustan.extraArgs` — additional CLI arguments appended to `plustan analyze` runs.
- `plustan.showOutputChannel` — show the Plu-Stan output channel automatically when running commands.

## `.plustan/dismissals.json`

Dismissals are written to `.plustan/dismissals.json` at the workspace root. Commit it to share reviewer decisions across your team, and (in a future phase) to gate CI on unresolved findings.
