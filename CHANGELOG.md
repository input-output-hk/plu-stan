# Changelog

`stan` uses [PVP Versioning][1].
The change log is available [on GitHub][2].

## 1.0.0

* Add seven Plinth inspections mapped from the
  [Cardano-CWE-Research](https://github.com/input-output-hk/Cardano-CWE-Research)
  rule set:

    * `PLU-STAN-21` — credentials baked immutably into validators, both as
      top-level constants and specialised into compiled code via
      `applyCode` / `unsafeApplyCode` / `liftCode`.
    * `PLU-STAN-22` — TxOut validation that constrains other fields but
      never the output address, so the output can be paid anywhere.
    * `PLU-STAN-23` — `unstableMakeIsData`, whose positional constructor
      indices change the on-chain encoding when a type gains or reorders a
      constructor.
    * `PLU-STAN-24` — an empty-string literal standing in for ADA instead of
      `adaSymbol` / `adaToken`.
    * `PLU-STAN-25` — validation that depends on the transaction's other
      script inputs without inspecting a redeemer.
    * `PLU-STAN-26` — `zip` without comparing the two lists' lengths, which
      silently drops the tail of the longer one.
    * `PLU-STAN-27` — an input spent only to be recreated identically, where
      a reference input would do.

* Add `TRACEABILITY.csv` and `scripts/gen-traceability.py`: a matrix mapping
  all 23 upstream research rules to Plu-Stan inspections in both directions,
  with a divergence note and the tests backing each link. Inspection facts
  are derived from the source tree, so they cannot drift; the generator also
  renders the README matrix and fails if a registered inspection has no
  README row.

* Fix the release workflow: it created the GitHub release as a draft, which
  is untagged until published, so every binary upload failed to resolve the
  tag. Releases are now created as prereleases (published, tag resolvable,
  and excluded from `/releases/latest`) and promoted once assets exist.

## vscode-plustan 0.3.4

* Scope the binary-management messages to the CLI. Every notification and
  output line in `downloadManager.ts` now reads `Plu-Stan CLI:` rather than
  the generic `Plu-Stan:`, so "already up to date" and "download failed"
  clearly refer to the backend binary and not the extension itself. The
  extension's own messages (analysis results, session state, status bar) keep
  the plain `Plu-Stan:` prefix, making the two distinguishable at a glance.

  This matters because the update prompt fires from a once-a-day background
  check, so it can appear without the user having asked for anything.

## vscode-plustan 0.3.0

* Add the review-session cockpit: **Start Review** analyzes the chosen
  onchain modules (or the whole workspace) and opens a Findings tree grouped
  by severity/rule (or by module), with a Finding Detail panel that renders
  each rule's teaching docs — why it matters, a bad/good example pair, and a
  fix suggestion.

* Persist dismissals to `.plustan/dismissals.json` (with an optional note),
  so a reviewer's "not applicable" calls survive restarts and can be shared
  with the team by committing the file.

* Track finding staleness: editing a file marks its open findings stale
  immediately, and saving an onchain module auto re-runs analysis for it and
  reconciles fixed / new / still-open findings. A status bar item shows the
  live open/fixed counts for the session; **End Review** stops auto re-runs
  and logs a summary.

* Speak schema-v2 to the backend: a `plustan capabilities` handshake gates
  the session on a compatible binary and offers "Check for Updates" on a
  mismatch (extension 0.3.x requires `plustan` >= 0.2.5.0).

* Guard the legacy one-shot `Run Workspace` / `Run Module` commands: they
  are disabled while a review session is active, so they can no longer
  clobber session diagnostics.

* Expand a leading `${workspaceFolder}` token in `plustan.binaryPath`
  (VS Code only expands it in `launch.json`/`tasks.json`, not arbitrary
  settings, so the extension does it itself).

## 0.2.5.0

* Add a `plustan capabilities` subcommand: a machine-readable, project-free
  JSON handshake reporting `schemaVersion`, `ghcVersion`, and the set of
  supported `features`.

* Bump the `plustan analyze --json` payload to schema v2:

    * Each observation now carries a stable `fingerprint`.
    * Inspections include documentation fields (`whyItMatters`,
      `badExample`, `goodExample`, `docsAnchor`) when available.
    * Observations are now a top-level `observations` array instead of
      being nested inside an `analysis` field, which has been removed.

* `plustan list-onchain --json` now reports the same schema version as
  `analyze --json` instead of a hardcoded `1`.

## 0.2.1.0

* Fix high memory usage in finding Cabal files

  See:
  https://github.com/kowainik/stan/pull/586#issuecomment-2906713949

  Thanks to @0rphee

* Support `clay-0.16`

## 0.2.0.0

* Add the following inspections:

    * Partial instance for 'Scientific' method 'GHC.Real.fromRational' (`stan0022`)
    * Partial 'Scientific' function 'GHC.Real.realToFrac' (`stan0023`)
    * Partial instance for 'Scientific' method 'GHC.Real.recip' (`stan0024`)
    * Partial instance for 'Scientific' method 'GHC.Real.(/)' (`stan0025`)

  Since these change the behaviour of `stan` by default this is a
  major release.

* Support GHC 9.12 (thanks to @ncaq)

## 0.1.3.0

* Add prospective support for GHC 9.10

  * thanks to @philderbeast

## 0.1.2.1

* Support `clay-0.15` series.

* Support `base64-1.0` series.

## 0.1.2.0

* Added `runStan`, `getAnalysis`, `getStanConfig`

## 0.1.1.0

* Fix [bug #541](https://github.com/kowainik/stan/issues/541)
  "`nodeInfo`"

## 0.1.0.2

* Add prospective support for GHC 9.8

  * will only work with `Cabal` library version 3.10 -- if this causes
    problems for you please comment on the corresponding [`extensions`
    ticket](https://github.com/kowainik/extensions/issues/89)

  * thanks to @0rphee

  * we don't provide a binary release for 9.8.1 because `stan`'s
    dependencies have not yet caught up with 9.8.1.  If you want to
    use `stan` with 9.8.1 you can install it from Hackage with `cabal
    install stan --allow-newer`.

## 0.1.0.1

* Add support for GHC 9.6 (will only work with `Cabal` library version
  3.10 -- if this causes problems for you please comment on the
  corresponding [`extensions`
  ticket](https://github.com/kowainik/extensions/issues/89))

## 0.1.0.0

* Add support for GHCs 9.0, 9.2 and 9.4
* [#55](https://github.com/kowainik/stan/issues/55):
  Implement single-pass HIE AST traversal.
* [#348](https://github.com/kowainik/stan/issues/348):
  Compress binaries for GitHub releases.
* [#368](https://github.com/kowainik/stan/issues/368):
  Fix inspections for `unordered-containers` functions to support the
  latest package version.

## 0.0.1.0 — Jul 9, 2020

* [#320](https://github.com/kowainik/stan/issues/320):
  Add `-b|--browse` option to the `report` command.
* [#327](https://github.com/kowainik/stan/issues/327):
  When the generated HIE files are incomplete (missing the source code),
  print `<UNAVAILABLE>` as the source instead of failing.
* [#329](https://github.com/kowainik/stan/issues/329):
  Add GHC version to the `--version` output.
* [#326](https://github.com/kowainik/stan/issues/326):
  Handle constraints before constructors in `STAN-0206`.
* [#323](https://github.com/kowainik/stan/issues/323):
  Add `--json-output` option that output the results in machine readable JSON
  format instead. Also all other printing is turned off then.
* Minor documentation improvements.

## 0.0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/stan/releases
