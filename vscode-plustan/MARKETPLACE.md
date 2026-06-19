# Plu-Stan

**Plu-Stan** is a static analysis tool for Cardano smart contracts written in [Plinth](https://github.com/input-output-hk/plutus). This extension surfaces Plu-Stan findings directly in VS Code and Cursor as diagnostics in the Problems panel — no manual CLI invocation required.

## Features

- **Onchain module explorer** — tree view listing all modules annotated with `onchain-contract`, auto-discovered from your workspace
- **Workspace analysis** — run Plu-Stan across your entire project with one click
- **Per-module analysis** — target a single onchain module from the tree view
- **VS Code Diagnostics integration** — findings appear in the Problems panel with severity, rule ID, and description
- **Works in Cursor** — fully compatible with the Cursor editor

## Requirements

- The `plustan` binary built from [input-output-hk/plu-stan](https://github.com/input-output-hk/plu-stan)
- A Haskell workspace compiled with `.hie`/`.hi` artifacts (Plu-Stan will trigger a build automatically if needed)
- A GHC the extension ships a prebuilt binary for (currently 9.6.7, 9.8.4, 9.10.3, 9.12.2). The `plustan` binary reads `.hie` files, whose format is locked to the **exact** GHC version that produced them, so the extension auto-downloads the binary matching your project's GHC. If your project uses a different GHC, build `plustan` with that GHC and set `plustan.binaryPath`.

## Getting Started

1. Build and install the `plustan` binary:
   ```bash
   cabal install exe:stan
   ```
2. Open your Plinth project in VS Code
3. Set `plustan.binaryPath` to the absolute path of the `plustan` executable (via Settings or `settings.json`):
   ```json
   {
     "plustan.binaryPath": "/path/to/plustan"
   }
   ```
4. Open the **Plu-Stan** panel in the Activity Bar and click **Run Workspace**

## Commands

| Command | Description |
|---|---|
| `Plu-Stan: Refresh Onchain Modules` | Re-scan the workspace for onchain modules |
| `Plu-Stan: Run Workspace` | Analyse the full workspace |
| `Plu-Stan: Run Module` | Analyse the selected onchain module |
| `Plu-Stan: Clear Diagnostics` | Clear all Plu-Stan findings from the Problems panel |
| `Plu-Stan: Show Output` | Open the Plu-Stan output channel |

## Settings

| Setting | Default | Description |
|---|---|---|
| `plustan.binaryPath` | `""` | Absolute path to the `plustan` executable (required) |
| `plustan.projectDir` | `""` | Project directory. Defaults to the active workspace folder |
| `plustan.hieDir` | `".hie"` | Directory containing `.hie`/`.hi` files, relative to `projectDir` |
| `plustan.extraArgs` | `[]` | Additional CLI arguments appended to `plustan analyze` runs |
| `plustan.showOutputChannel` | `true` | Automatically show the output channel when running commands |

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

For full rule documentation see the [plu-stan repository](https://github.com/input-output-hk/plu-stan/blob/main/rules.md).

## License

[MPL-2.0](https://github.com/input-output-hk/plu-stan/blob/main/LICENSE)
