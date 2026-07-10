# Plu-Stan

**Plu-Stan** is a static analysis tool for Cardano smart contracts written in [Plinth](https://github.com/input-output-hk/plutus). This extension turns it into a **review cockpit** right inside VS Code and Cursor: catch security and performance anti-patterns before they ship, with a per-rule explanation of *why* each one matters — no manual CLI invocation required.

## Features

- **Review sessions** — click ▶ **Start Review** to analyze your onchain modules and open a live findings workspace: a tree grouped by severity and rule (or by module), squiggles in the editor, and entries in the Problems panel
- **Finding Detail panel** — click any finding to see why it matters, a ✗ bad-example / ✓ good-example pair, concrete how-to-fix guidance, and a link to the full rule documentation
- **Persistent dismissals** — dismiss a finding you've judged not applicable (with an optional note); it's recorded in `.plustan/dismissals.json` so the decision survives restarts and can be committed and shared with your team
- **Staleness tracking** — editing a file marks its open findings as stale immediately, and saving an onchain module automatically re-runs analysis and reconciles what's fixed, what's new, and what's still open
- **Status bar at a glance** — live open/fixed counts for the active review session
- **Onchain module explorer** — tree view listing all modules annotated with `onchain-contract`, auto-discovered from your workspace
- **One-shot commands** — `Run Workspace` / `Run Module` for a quick, session-free pass
- **Works in Cursor** — fully compatible with the Cursor editor

## Requirements

- The `plustan` binary built from [input-output-hk/plu-stan](https://github.com/input-output-hk/plu-stan). Extension 0.3.x requires `plustan` >= 0.2.5.0; the extension checks this automatically via a `plustan capabilities` handshake and prompts you to update if there's a mismatch.
- A Haskell workspace compiled with `.hie`/`.hi` artifacts (Plu-Stan will trigger a build automatically if needed)
- A GHC the extension ships a prebuilt binary for. The `plustan` binary reads `.hie` files, whose on-disk format is tied to the GHC **major.minor series** that produced them — patch releases within a series share the format, so one binary handles any patch of its series (e.g. a 9.6 build reads any 9.6.x project). The extension fetches the binary matching your project's GHC. If your project's GHC series isn't shipped, build `plustan` with that GHC and set `plustan.binaryPath`.

## Getting Started

1. Open your Plinth project in VS Code or Cursor.
2. Open the **Plu-Stan** panel in the Activity Bar. If no binary is configured, you'll be offered a one-click download matched to your project's GHC (or run `Plu-Stan: Check for Updates` yourself). To point at a binary you built yourself, set `plustan.binaryPath`:
   ```json
   {
     "plustan.binaryPath": "/path/to/plustan"
   }
   ```
3. In the **Findings** view, click **Start Review** (▶), pick the modules to review (or accept all), and work the findings tree: click through to each issue, read the explanation, fix it or dismiss it.
4. Click **End Review** when you're done — it stops auto re-runs and logs a fixed/open/dismissed summary.

> **Monorepo / multi-package projects:** if your on-chain package lives in a subdirectory (e.g. `onchain/`), point `plustan.hieDir` at that package's `.hie` directory (e.g. `onchain/.hie`), or set `plustan.projectDir` to the package. Plu-Stan resolves each finding against the package that produced it, so either works.

## Commands

| Command | Description |
|---|---|
| `Plu-Stan: Start Review` | Begin a review session over the whole workspace or chosen modules |
| `Plu-Stan: End Review` | Stop auto re-runs and summarize the session |
| `Plu-Stan: Toggle Findings Grouping` | Switch the Findings tree between severity/rule and module grouping |
| `Plu-Stan: Open Finding` | Jump to a finding's location in the editor |
| `Plu-Stan: Dismiss Finding` | Mark a finding as not applicable, with an optional note |
| `Plu-Stan: Restore Dismissed Finding` | Reopen a previously dismissed finding |
| `Plu-Stan: Run Workspace` | One-shot analysis of the full workspace (disabled during a review session) |
| `Plu-Stan: Run Module` | One-shot analysis of the selected onchain module (disabled during a review session) |
| `Plu-Stan: Refresh Onchain Modules` | Re-scan the workspace for onchain modules |
| `Plu-Stan: Clear Diagnostics` | Clear all Plu-Stan findings from the Problems panel |
| `Plu-Stan: Show Output` | Open the Plu-Stan output channel |
| `Plu-Stan: Open Settings` | Jump to Plu-Stan settings |
| `Plu-Stan: Check for Updates` | Download the `plustan` binary matching your project's GHC |

## Settings

| Setting | Default | Description |
|---|---|---|
| `plustan.binaryPath` | `""` | Path to the `plustan` executable. Supports a leading `${workspaceFolder}` token. Leave empty to let the extension manage a downloaded binary |
| `plustan.projectDir` | `""` | Directory Plu-Stan runs the analyzer in. Defaults to the workspace folder; usually only needed to point at a package subdirectory in a monorepo |
| `plustan.hieDir` | `".hie"` | Directory containing `.hie`/`.hi` files. Relative to `projectDir`, or an absolute path |
| `plustan.extraArgs` | `[]` | Additional CLI arguments appended to `plustan analyze` runs |
| `plustan.showOutputChannel` | `true` | Automatically show the output channel when running commands |

## `.plustan/dismissals.json`

Every dismissal is recorded in `.plustan/dismissals.json` at your workspace root, with a timestamp and your optional note. Commit it so the whole team sees the same set of accepted findings — and so a future CI check can hold the line on anything left open.

## Rules

Plu-Stan checks for security and performance issues specific to Plinth on-chain code, including:

- Signature verification invariants (PLU-STAN-01)
- Unsafe `unsafeFromBuiltinData` usage (PLU-STAN-02)
- Optional types in on-chain code (PLU-STAN-03)
- Credential-only equality comparisons (PLU-STAN-04)
- Inefficient higher-order list helpers (PLU-STAN-05)
- Multiple list traversals (PLU-STAN-06)
- Guard syntax inefficiency (PLU-STAN-07)
- Non-strict let bindings (PLU-STAN-08)
- Unsafe `valueOf` comparisons (PLU-STAN-09)
- Unvalidated hashes from `BuiltinData` (PLU-STAN-10)
- `currencySymbolValueOf` misuse (PLU-STAN-11)
- Validity interval / POSIX time misuse (PLU-STAN-12)
- Division before multiplication precision loss (PLU-STAN-16)

Each rule's finding comes with a plain-language explanation, a bad/good example pair, and a fix suggestion right in the Finding Detail panel. For full rule documentation see the [plu-stan repository](https://github.com/input-output-hk/plu-stan/blob/main/RULES.md).

## License

[MPL-2.0](https://github.com/input-output-hk/plu-stan/blob/main/LICENSE)
