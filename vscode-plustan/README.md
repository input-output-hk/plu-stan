# Plu-Stan VS Code/Cursor Extension (MVP)

This extension integrates the local `plustan` CLI.

It is implemented as a standard VS Code extension, so it can be used in both VS Code and Cursor.

## Features

- Explorer Tree View for modules annotated with `onchain-contract`.
- Large action rows at the top of the tree for refresh/run/clear/output actions.
- Run `plustan` for a selected onchain module.
- Run `plustan` for the full workspace.
- Publish findings to VS Code Diagnostics / Problems.

## Requirements

- `plustan.binaryPath` must be set to an absolute `plustan` executable path.
- A buildable Haskell workspace with `.hie`/`.hi` artifacts (the CLI will auto-build when needed).

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

- `Plu-Stan: Refresh Onchain Modules`
- `Plu-Stan: Run Workspace`
- `Plu-Stan: Run Module`
- `Plu-Stan: Clear Diagnostics`
- `Plu-Stan: Show Output`

## Settings

- `plustan.binaryPath`
- `plustan.projectDir`
- `plustan.hieDir`
- `plustan.extraArgs`
- `plustan.showOutputChannel`
