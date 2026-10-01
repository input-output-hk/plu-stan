# Research-rule conformance

The acceptance target is **19 of 23 rules (82.6%)** against Cardano-CWE-Research commit `10eeea42c9b18d37cc2985c02b8b6987c0bb1c13`. All 23 rules remain eligible and in the denominator. A generated run report, not the README's declared mapping, establishes which contracts pass at a particular implementation revision.

This is implementation coverage of **representative static warning patterns**. It is not 80% vulnerability recall, a proof of contract safety, or an independent audit certification. The upstream rules README explicitly permits representative, non-exhaustive detection logic. The scope below is part of the claim and should be reviewed with the auditor.

## Reproduce

With the repository's native crypto dependencies and a compatible GHC installed:

```sh
cabal build all --enable-tests -ffixtures
cabal test all --test-show-details=direct
python3 scripts/gen-traceability.py
python3 scripts/check-cwe-conformance.py --no-build --output cwe-results.json
```

The last command runs the shipped `plustan analyze --json --module ...` executable, including its ordinary build refresh and on-chain module discovery. `Target.Research` and the applied-credential fixtures in `Target.PlutusTx` are annotated as on-chain. Missing/stale fixtures, disabled inspections, changed source snapshots, wrong analysis scope and malformed output are errors. A fixture may not suppress its own diagnostic. Other diagnostics are deliberately allowed: being valid for one research rule does not mean being valid for every rule.

`test/cwe-conformance.json` defines the positive and negative cases. Each expected result applies to the **whole function body**, not just its first line. Each rule counts once, only if it has both positive and negative cases and every declared case passes. Uncovered and partially passing rules earn no credit. The exit status fails if any declared case fails or fewer than 19 rules pass.

The report includes per-case actual observations, per-rule results, the research and implementation revisions, dirty-worktree status, a source digest, compiler version and executable hash. CI runs the check in its fixtures-enabled test job and uploads the report, manifest, traceability and scope for 90 days. Retain the artifact with the release/audit record before it expires. Requiring that existing CI check in branch protection is a repository-administration decision; adding this workflow step does not itself change branch protection.

## Source provenance and adaptations

`test/research-source/` contains the pinned rule documents and their upstream license. SHA-256 hashes in the manifest verify the snapshot; its filenames establish the complete denominator. The separate directory README is not a rule.

The research examples are often fragments or pseudocode. The executable corpus supplies types, parameters, imports and surrounding definitions. Necessary API adaptations include:

- `TokenName emptyByteString` and `CurrencySymbol emptyByteString` use constructors because the current lowercase smart constructors take ordinary `ByteString`, while `emptyByteString` is a `BuiltinByteString`.
- ADA amounts use the ledger's `Lovelace` type.
- Integer redeemer indices are converted to the `Int` accepted by the fixture's list indexing operator.
- Missing datum/context definitions are explicit function parameters; record datum examples use an explicit stable encoding except the intentionally unstable-splice case.
- The fixture module uses the ledger V2 API for `Value`-based mint examples. Existing V3/AsData regression fixtures remain in the normal test suite. The coverage claim does not promise identical detection across every version or representation.

The corpus includes both literal research patterns and equivalent small compiling examples, with additional comment, unused-binding, wrong-object, alias, bypass and endpoint mutations. It is maintained alongside the implementation and therefore is **not an independent benchmark**. An auditor should review the contracts and expected outcomes, and can add held-out cases.

## Analysis model and limits

The dedicated research inspections use resolved GHC names and expression structure from HIE. They expand acyclic aliases and simple helper calls within a module, preserve binder identity and tuple projections, and inspect expressions that contribute to the result. Comments, string contents and unused local definitions cannot serve as validation evidence. Recognized library identifiers are checked against package/module identity, not just spelling.

Expansion is bounded (80 normalization steps for function analysis; 60 for arithmetic expressions). Recursive calls, higher-order substitution, multi-clause functions, imported helper implementations, arbitrary custom operators and whole-program control/data flow are not exhaustively modeled. Unknown forms are not interpreted as proof of validation; triggers hidden entirely in unsupported forms may be missed. There is no SMT solver or proof that a recognized comparison is sufficient for a protocol invariant.

Conjunctions contribute required facts; disjunctions retain only shared facts. Supported conditional/case branches are analyzed structurally. An arm counts as rejecting only when its result is `False` or a throwing call (`traceError`/`error`), or every path within it rejects; a throwing call nested inside an otherwise accepting arm (such as an `else traceError` guarding a check) does not make that arm rejecting. This is bounded pattern analysis, not complete path-sensitive reasoning. For warnings about absent checks, helper predicates may need contextual review because another caller can perform additional validation.

Legacy inspections remain enabled and retain their own behavior and limitations. Several address the same concern using older, broader or narrower triggers, so multiple related warnings are possible. The acceptance manifest identifies the exact inspection that demonstrates each research contract; unrelated legacy warnings cannot satisfy a case.

## Accepted detection contracts

| Research rule | Acceptance inspection | Representative contract and material limits |
|---|---|---|
| PrecisionLoss | 16 | Multiplication of an expression containing division, including acyclic aliases. Multiply-first safe control and same-spelled independent binders. It does not prove arithmetic equivalence or quantify rounding loss. |
| EmptyStringADACheck | 24 | Empty string/empty-builtin construction of token names and currency symbols; dedicated ADA helpers pass. This is a style pattern and may also flag empty asset construction outside comparisons. |
| ImmutableCredential | 21 | Credential constants reachable from typed `ScriptContext -> ... -> Bool` validators, plus applyCode/unsafeApplyCode specialization through supported lifted helpers. CompiledCode/CompiledCodeIn and typed tuple projections distinguish a selected integer from a credential. Untyped entrypoints and arbitrary compilation metaprograms are outside the bounded contract. |
| UnstableMakeIsData | 23 | Recognizes the unstable declaration spelling; stable indexed derivation passes. HIE erases declaration splices, so this check masks comments and strings in source and preserves diagnostic positions. It is a spelling-based check, not full Template Haskell expansion analysis. |
| ZipWithoutLengthCheck | 26 | zip/zip3/zipWith without length-equality evidence for the actual participating lists. Supports equality chains, aliases, multiline calls and guarded branches. An unrelated length, dead binding, comment or bypassing OR is not a guard. Custom equal-length zip APIs are not modeled. |
| MissingAddressValidation | 28 | An identified output predicate omits an address constraint while checking other fields; explicit typed predicates that never mention the output are also recognized. A predicate that hands the output to a helper this module cannot expand (imported or multi-clause) is treated as unknown, not as empty. An address check on another output does not suffice. This does not infer every output selected through arbitrary custom abstractions. |
| MissingStakingValidation | 29 | Checks of only the payment part, or an address pattern discarding stake, warn. Full address equality, explicit stake accessor constraints and supported explicit-Nothing patterns pass. The protocol's desired delegation policy remains a review question. |
| UnvalidatedReferenceScript | 30 | Applicable output predicates omit reference-script constraints, including predicates below the old three-field gate. A comment, unused check or constraint on another output does not suffice. |
| UnvalidatedDatum | 31 | Identified output validation lacks datum equality/field constraints, including wildcard inline and hashed datum matches. Field validation can satisfy this rule; it does not satisfy PartialUnvalidatedDatum. Applicability to pubkey outputs is deliberately conservative and requires review. |
| TrashTokens | 32 | Missing value restrictions and recognized subset/asset-count comparisons without a bound on the same output's flattened token set. Exact value or an explicit same-output token-count bound is accepted by this pattern. The analysis does not prove that a chosen count/amount is a secure protocol bound. |
| UncheckedRedeemer | 33 | Script-input dependencies without a lookup/validation for the corresponding Spending input reference in the same transaction. Reference-input-only reads pass. Imported helpers, arbitrary purpose abstractions and full branch authorization are not proved. |
| ReadOnlySpend | 34 | Input/output datum equality, full output equality, or all locally declared datum-record fields compared between the same decoded input/output pair. Partial field equality does not suffice. Imported record schemas and arbitrary encodings are not modeled. A warning asks whether spending is necessary; other effects may justify it. |
| ValidityRangeBound | 35 | Use of a transaction validity range without a required upper-minus-lower comparison against a separate maximum. Endpoints must come from the same range. Finite-only, wrong-endpoint and unused checks warn. Bound positivity, acceptable duration and arbitrary equivalent arithmetic need review. |
| DatumComparisonOptimization | 36 | fromBuiltinData/unsafeFromBuiltinData-derived values used in field equality; direct encoded equality passes. This identifies optimization candidates, not proof that replacing a partial-field comparison with whole-datum equality preserves semantics. |
| IncompleteTokenValidation | 37 | Wildcard components in three-element token-tuple predicates over flattenValue using all/any/filter/foldl/foldr, for mint and output values. Tuple components bound by name but never meaningfully constrained require further analysis and are not exhaustively covered. |
| StrictValueEquality | 38 | Exact equality involving lovelaceValueOf (txOutValue ...), in either order and through aliases. Minimum comparisons and unused equalities pass. It does not decide whether an explicit protocol invariant justifies equality. |
| UnvalidatedInputIndex | 39 | Dynamic input/reference-input selection without an identity-token amount check on that selected input. Same-input valueOf/assetClassValueOf ==1 or >=1 forms pass. Constant indices are outside this contract. The rule conservatively covers dynamic parameter-derived indices; it does not prove redeemer provenance or authenticity of the expected token parameters. |
| HelperFunctions | 40 | Simple single-clause forwarding wrappers (including fixed arguments) and case-only projection helpers. More substantial logic passes. Arbitrary multi-equation helpers and compiler inlining decisions are not analyzed. |
| FixedStructureMap | 41 | Fixed string membership keys applied to a datum record-field map. Dynamic keys and typed-record use pass. This flags a pseudo-record candidate, not proof that every literal map key is a defect. |

The numeric inspection IDs above have the `PLU-STAN-` prefix. `TRACEABILITY.csv` additionally records legacy links, source implementations and regression/CLI case counts. Those counts describe evidence volume, not extra rule coverage.

## Four rules retained but not counted

- **PartialUnvalidatedDatum:** no claim that every required datum field is constrained.
- **ListUniqueness:** integer-index checks do not establish uniqueness of identity lists.
- **NoBurningLogic:** the separate unmerged implementation is not part of this change.
- **DoubleSatisfaction:** operation-to-payment attribution needs its own reviewed contract; output-datum uniqueness alone is insufficient assurance.

## Suggested audit statement

“At implementation commit [SHA], the attached reproducible CLI run passes all acceptance cases for the documented representative detection contracts of 19 of the 23 rules in Cardano-CWE-Research at commit 10eeea42c9b18d37cc2985c02b8b6987c0bb1c13: 82.6% rule coverage. All 23 rules remain in the denominator. The scope document identifies supported forms, limitations and the four unimplemented contracts. This measures research-pattern implementation, not arbitrary-program vulnerability recall.”
