# Working with observation-based tests

This repo has a set of “fixture” modules under `target/Target/*.hs` that are analysed by Stan, and a matching set of Hspec tests under `test/Test/Stan/Analysis/*` that assert observations at specific source locations (line/column spans).

## Add a new test case (end-to-end)

1. Add or edit a fixture in `target/Target/...` (e.g. `target/Target/PlutusTx.hs`).
   - Prefer appending new fixtures near the end of the file to minimize line-number churn.
   - Keep each fixture small and focused so it yields a single, unambiguous observation.

2. Add a matching spec in `test/Test/Stan/Analysis/...` (e.g. `test/Test/Stan/Analysis/PlutusTx.hs`).
   - Use `checkObservation` (via `observationAssert`) when you expect an observation.
   - Use `noObservationAssert` when you expect no observation on a given line.

3. Run tests:
   - `cabal test all --test-show-details=direct`
   - For faster iteration, rerun a single spec:
     - `cabal test all --test-show-details=direct --test-options='--match "/Static Analysis/Plutus-Tx/PLU-STAN-08: .../"'`

## Updating line numbers / spans when tests fail

Observation tests are location-sensitive. When you change a fixture file, you often need to update the expected line/column numbers in the corresponding spec.

### Find the correct line numbers

Use `nl` to get stable, 1-based line numbers:

- `nl -ba target/Target/PlutusTx.hs | sed -n 'START,ENDp'`

Update the `line` argument in the test to match the new location.

### Find the correct column span (start/end)

`observationAssert`/`checkObservation` expects:

- line number
- start column
- end column

Typical patterns:

- If the observation is reported on a specific identifier (e.g. an argument name), set the span to the identifier’s columns.
- If the observation is reported on a larger AST node, set the span to the node’s reported span in the failure output.

Tip: when a test fails, the failure message prints the “expected” and the “got”. Use the “got” `SrcSpanOneLine ... line start end` as the new truth.

### Avoid ambiguity on the same line

`observationAssert` intentionally matches observations by an ID prefix that is only line-specific. If multiple observations of the same inspection are emitted on the same line, the test can become flaky (it may match a different observation on that line).

Prefer writing fixtures so each inspection triggers at most once per line, or spread similar triggers across different lines.

## Common gotchas

- Tests are driven by `.hie` files under `.hie/`. `cabal test` will rebuild as needed; if you’re debugging locally, make sure fixture compilation succeeded so fresh `.hie` files exist.
- Many existing specs key off exact line numbers in `target/Target/PlutusTx.hs`; inserting code near the top of that file will cascade updates.
