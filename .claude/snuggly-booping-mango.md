# Implementation Plan: 13 Plinth Static Analysis Rules for plu-stan

## Overview

Implement all 13 security, performance, and code-quality rules from the plinth-static-analyzer-research repository into plu-stan. This includes enhancing 1 existing rule (PLU-STAN-04) and adding 10 new rules (PLU-STAN-10 through PLU-STAN-20).

### Rules Summary

| Rule ID | Name | Category | Severity | Complexity |
|---------|------|----------|----------|------------|
| PLU-STAN-04 (enhance) | Address/Staking Validation | Security | Error | Medium |
| PLU-STAN-10 | IncompleteTokenValidation | Security | Error | Medium-Complex |
| PLU-STAN-11 | ListUniqueness | Security | Warning | Complex |
| PLU-STAN-12 | PartialUnvalidatedDatum | Security | Error | Complex |
| PLU-STAN-13 | UnvalidatedDatum | Security | Error | Medium |
| PLU-STAN-14 | UnvalidatedInputIndex | Security | Error | Medium-Complex |
| PLU-STAN-15 | ValidityRangeBound | Security | Error | Medium |
| PLU-STAN-16 | StrictValueEquality | Security | Warning | Simple |
| PLU-STAN-17 | ReadOnlySpend | Performance | Performance | Complex |
| PLU-STAN-18 | UnvalidatedReferenceScript | Performance | Error | Complex |
| PLU-STAN-19 | PrecisionLoss | Code-Quality | Warning | Medium |
| PLU-STAN-20 | TrashTokens | Security+Performance | Error | Simple |

---

## Phase 1: Simple Pattern-Based Rules (Week 1)

These rules use FindAst pattern matching with straightforward AST patterns.

### 1.1 PLU-STAN-16: StrictValueEquality [SIMPLE]

**What it detects**: Exact ADA equality checks that can fail due to minUTxO changes

**Pattern to match**:
```haskell
lovelaceValueOf (txOutValue <var>) == <var>
```

**Implementation**:
- Use FindAst with opApp pattern
- Match `lovelaceValueOf` function from PlutusLedgerApi.V1.Value
- Detect equality operator on result

**Files to modify**:
- `src/Stan/Inspection/AntiPattern.hs`: Add `plustan16` function

**Testing**:
- Valid: `lovelaceValueOf (txOutValue out) >= 2_000_000` (minimum check)
- Invalid: `lovelaceValueOf (txOutValue out) == 2_000_000` (exact check)

---

### 1.2 PLU-STAN-19: PrecisionLoss [MEDIUM]

**What it detects**: Division before multiplication in integer arithmetic

**Pattern to match**:
```haskell
(a / b) * c
(a `div` b) * c
(a `quot` b) * c
```

**Implementation**:
- Use FindAst with nested opApp
- Match division operators: `/`, `div`, `quot`
- Match multiplication operators: `*`, `mul`
- Detect when division result is multiplied

**Files to modify**:
- `src/Stan/Inspection/AntiPattern.hs`: Add `plustan19` function

**Testing**:
- Valid: `(principal * elapsed) / total` (multiply first)
- Invalid: `(elapsed / total) * principal` (divide first)

---

### 1.3 PLU-STAN-20: TrashTokens [SIMPLE]

**What it detects**: Subset value comparisons (leq/geq) allowing arbitrary token injection

**Pattern to match**:
```haskell
value `leq` expectedValue
value `geq` expectedValue
assetClassValueOf v ac >= n
```

**Implementation**:
- Use FindAst with opApp pattern
- Match `leq`/`geq` from PlutusLedgerApi.V1.Value
- Match `>=`/`<=` operators with assetClassValueOf
- Type constraint: Value -> Value -> Bool

**Files to modify**:
- `src/Stan/Inspection/AntiPattern.hs`: Add `plustan20` function

**Testing**:
- Valid: `v == assetClassValue asset amt` (exact equality)
- Invalid: `assetClassValueOf v asset >= amt` (allows extra tokens)

---

## Phase 2: Medium Complexity Rules (Week 2)

These rules require custom analyzers or more sophisticated pattern matching.

### 2.1 PLU-STAN-04 Enhancement: MissingAddressValidation + MissingStakingValidation

**Current behavior**: Warns about comparing ScriptHash/PubKeyHash/Credential directly

**New behavior**: Also detect:
1. **MissingAddressValidation**: Absence of `txOutAddress` calls when validating outputs
2. **MissingStakingValidation**: Wildcard patterns in Address construction or absence of `addressStakingCredential` calls

**Implementation approach**:
- Keep existing FindAst pattern for credential equality
- Add custom analyzer to detect absence patterns
- Check for `Address (ScriptCredential _) _` wildcard pattern
- Check for missing `txOutAddress` and `addressStakingCredential` calls

**Files to modify**:
- `src/Stan/Inspection/AntiPattern.hs`: Enhance `plustan04`
- `src/Stan/Inspection.hs`: Add new InspectionAnalysis variant `MissingAddressStakingValidation`
- `src/Stan/Analysis/Analyser.hs`: Add `analyseMissingAddressStakingValidation` function

**Testing**:
- Valid: `txOutAddress out == expectedAddr && addressStakingCredential (txOutAddress out) == Just cred`
- Invalid: `Address (ScriptCredential sh) _` (wildcard staking)
- Invalid: Output selection without `txOutAddress` check

---

### 2.2 PLU-STAN-13: UnvalidatedDatum [MEDIUM]

**What it detects**: Script outputs created without datum validation

**Detection logic**:
1. Find functions working with script outputs
2. Check for absence of `txOutDatum` or `txOutDatumHash` calls
3. Detect wildcard patterns: `OutputDatum _`

**Implementation**:
- Custom analyzer similar to MissingAddressValidation
- Track script output contexts (ScriptCredential addresses)
- Check for datum validation presence

**Files to modify**:
- `src/Stan/Inspection/AntiPattern.hs`: Add `plustan13`
- `src/Stan/Inspection.hs`: Add `UnvalidatedDatum` analysis variant
- `src/Stan/Analysis/Analyser.hs`: Add `analyseUnvalidatedDatum` function

**Testing**:
- Valid: `case txOutDatum out of OutputDatum (Datum d) -> fromBuiltinData d`
- Invalid: `case txOutAddress out of Address (ScriptCredential _) _ -> True` (no datum check)

---

### 2.3 PLU-STAN-15: ValidityRangeBound [MEDIUM]

**What it detects**: Missing validity range length constraints in temporal checks

**Pattern to match**:
```haskell
-- Positive: (upper - lower) <= maxDuration
-- Negative: Uses txInfoValidRange without range length check
```

**Implementation**:
- Custom analyzer to find `txInfoValidRange` usage
- Track bound extractions (`ivFrom`, `ivTo`)
- Check for subtraction followed by comparison with constant
- Flag if no such constraint exists

**Files to modify**:
- `src/Stan/Inspection/AntiPattern.hs`: Add `plustan15`
- `src/Stan/Inspection.hs`: Add `ValidityRangeBound` analysis variant
- `src/Stan/Analysis/Analyser.hs`: Add `analyseValidityRangeBound` function

**Testing**:
- Valid: `let range = upper - lower in range <= maxDuration && lower >= deadline`
- Invalid: `case ivFrom (txInfoValidRange info) of LowerBound (Finite t) _ -> t >= deadline` (no length check)

---

### 2.4 PLU-STAN-14: UnvalidatedInputIndex [MEDIUM-COMPLEX]

**What it detects**: Using redeemer-provided indices without NFT validation

**Detection logic**:
1. Find index operations: `txInfoInputs !! idx` or `txInfoReferenceInputs !! idx`
2. Track the indexed input variable
3. Check for absence of NFT validation (`valueOf ... cs tn == 1`)

**Implementation**:
- Custom analyzer with data flow tracking
- Detect `!!` operator application
- Track result variable through `txInInfoResolved`
- Check for `valueOf` or `assetClassValueOf` on the indexed input

**Files to modify**:
- `src/Stan/Inspection/AntiPattern.hs`: Add `plustan14`
- `src/Stan/Inspection.hs`: Add `UnvalidatedInputIndex` analysis variant
- `src/Stan/Analysis/Analyser.hs`: Add `analyseUnvalidatedInputIndex` function

**Testing**:
- Valid: `let input = txInfoInputs info !! idx in valueOf (txOutValue (txInInfoResolved input)) cs tn == 1`
- Invalid: `let input = txInfoInputs info !! idx in validateDatum (txInInfoResolved input)` (no NFT check)

---

### 2.5 PLU-STAN-10: IncompleteTokenValidation [MEDIUM-COMPLEX]

**What it detects**: Incomplete validation of token tuples (cs, tn, quantity) in fold operations

**Pattern to match**:
```haskell
<fold-function> (\(_, tn, amt) -> ...) (flattenValue <var>)
<fold-function> (\(cs, _, amt) -> ...) (flattenValue <var>)
<fold-function> (\(cs, tn, _) -> ...) (flattenValue <var>)
```

**Implementation**:
- Custom analyzer (or complex FindAst) to detect lambda patterns
- Match fold functions: `all`, `any`, `filter`, etc.
- Check tuple destructuring for wildcards
- Verify the fold is over `flattenValue` result

**Files to modify**:
- `src/Stan/Inspection/AntiPattern.hs`: Add `plustan10`
- May need custom analyzer if FindAst insufficient
- `src/Stan/Analysis/Analyser.hs`: Add `analyseIncompleteTokenValidation` if needed

**Testing**:
- Valid: `all (\(cs, tn, amt) -> if cs == expectedCs && tn == expectedTn then amt == 1 else amt == 0) (flattenValue mint)`
- Invalid: `all (\(cs, _, _) -> cs == expectedCs) (flattenValue mint)` (ignores tn and amt)

---

## Phase 3: Complex Rules Requiring Advanced Analysis (Week 3)

These rules require sophisticated data flow analysis and context tracking.

### 3.1 PLU-STAN-11: ListUniqueness [COMPLEX]

**What it detects**: Lists of identity types (PubKeyHash, ValidatorHash, Address, Credential) without uniqueness validation

**Detection logic**:
1. Identify lists of identity types (by type or parameter names)
2. Check for absence of `nub` or `nubBy` application
3. Look for uniqueness validation patterns: `list == nub list` or `length list == length (nub list)`

**Implementation**:
- Custom analyzer with type-aware data flow
- Extract list variables and their types from HIE
- Track whether `nub`/`nubBy` is applied
- Flag if identity type lists lack uniqueness checks

**Files to modify**:
- `src/Stan/Inspection/AntiPattern.hs`: Add `plustan11`
- `src/Stan/Inspection.hs`: Add `ListUniqueness` analysis variant
- `src/Stan/Analysis/Analyser.hs`: Add `analyseListUniqueness` function
- May need helpers to identify identity types

**Testing**:
- Valid: `signers == nub signers && length signers >= 3`
- Invalid: `length signers >= 3 && all (`elem` txInfoSignatories info) signers` (no uniqueness check)

---

### 3.2 PLU-STAN-12: PartialUnvalidatedDatum [COMPLEX]

**What it detects**: Partial datum field validation (some fields checked, others ignored)

**Detection logic**:
1. Find datum deserialization: `fromBuiltinData`
2. Extract datum fields from pattern or record syntax
3. Track which fields appear in validation expressions
4. Compare accessed fields vs validated fields
5. Flag if mismatch exists

**Implementation**:
- Custom analyzer with field coverage tracking
- Requires type information from HIE
- Track field accesses vs field validations
- Distinguish mere access from actual validation

**Files to modify**:
- `src/Stan/Inspection/AntiPattern.hs`: Add `plustan12`
- `src/Stan/Inspection.hs`: Add `PartialUnvalidatedDatum` analysis variant
- `src/Stan/Analysis/Analyser.hs`: Add `analysePartialUnvalidatedDatum` function
- Helper functions for field tracking

**Testing**:
- Valid: `case fromBuiltinData d of Just (MyDatum f1 f2 f3) -> f1 == x && f2 > y && f3 <= z`
- Invalid: `case fromBuiltinData d of Just (MyDatum f1 f2 f3) -> f1 == x && f2 > y` (f3 never validated)

---

### 3.3 PLU-STAN-17: ReadOnlySpend [COMPLEX]

**What it detects**: Spending UTxOs only to recreate them identically (should use reference inputs)

**Detection logic**:
1. Find input/output comparisons
2. Check for equality on all three: `txOutAddress`, `txOutDatum`, `txOutValue`
3. Track through let bindings and variable flow

**Implementation**:
- Custom analyzer similar to `ValueOfInComparison` (PLU-STAN-09)
- Track equality checks on input/output fields
- Identify when all three properties are compared for equality
- Flag as performance issue

**Files to modify**:
- `src/Stan/Inspection/AntiPattern.hs`: Add `plustan17`
- `src/Stan/Inspection.hs`: Add `ReadOnlySpend` analysis variant
- `src/Stan/Analysis/Analyser.hs`: Add `analyseReadOnlySpend` function

**Testing**:
- Valid: `case find hasNFT (txInfoReferenceInputs info) of Just ref -> ...` (uses reference inputs)
- Invalid: `txOutAddress input == txOutAddress output && txOutDatum input == txOutDatum output && txOutValue input == txOutValue output` (recreates identical)

---

### 3.4 PLU-STAN-18: UnvalidatedReferenceScript [COMPLEX]

**What it detects**: Validators not constraining the `referenceScript` field of outputs

**Detection logic**:
1. Identify validator functions (return Bool, take ScriptContext)
2. Check if function uses outputs (`getContinuingOutputs`, `txInfoOutputs`)
3. Check for absence of `txOutReferenceScript` or `txOutReferenceScriptHash` calls
4. Flag if outputs used but reference script unconstrained

**Implementation**:
- Custom analyzer with function-level scope
- Detect validator function signatures
- Check for output usage
- Verify reference script validation presence

**Files to modify**:
- `src/Stan/Inspection/AntiPattern.hs`: Add `plustan18`
- `src/Stan/Inspection.hs`: Add `UnvalidatedReferenceScript` analysis variant
- `src/Stan/Analysis/Analyser.hs`: Add `analyseUnvalidatedReferenceScript` function

**Testing**:
- Valid: `case txOutReferenceScript out of Nothing -> True; _ -> False` (validates it's Nothing)
- Invalid: Uses `getContinuingOutputs` but never checks `txOutReferenceScript` field

---

## Implementation Sequence

### Week 1: Simple Rules (3 rules)
1. PLU-STAN-16: StrictValueEquality
2. PLU-STAN-19: PrecisionLoss
3. PLU-STAN-20: TrashTokens

**Goal**: Validate pattern matching approach, establish testing infrastructure

---

### Week 2: Medium Rules (5 rules + 1 enhancement)
4. PLU-STAN-04 (enhance): MissingAddressValidation + MissingStakingValidation
5. PLU-STAN-13: UnvalidatedDatum
6. PLU-STAN-15: ValidityRangeBound
7. PLU-STAN-14: UnvalidatedInputIndex
8. PLU-STAN-10: IncompleteTokenValidation

**Goal**: Build custom analyzer infrastructure, handle absence detection

---

### Week 3: Complex Rules (4 rules)
9. PLU-STAN-11: ListUniqueness
10. PLU-STAN-12: PartialUnvalidatedDatum
11. PLU-STAN-17: ReadOnlySpend
12. PLU-STAN-18: UnvalidatedReferenceScript

**Goal**: Implement sophisticated data flow analysis, field tracking

---

## Critical Files

### Files to Create
- `target/Target/PlinthRules.hs` - Test examples for all 11 new rules (~800-1000 lines)
- `test/Test/Stan/Analysis/PlinthRules.hs` - Test specifications (~400-500 lines)

### Files to Modify
- `src/Stan/Inspection/AntiPattern.hs` - Add plustan10-plustan20 definitions, enhance plustan04 (~600 new lines)
- `src/Stan/Inspection.hs` - Add 8 new InspectionAnalysis variants (~20 lines)
- `src/Stan/Analysis/Analyser.hs` - Add 8 custom analysis functions (~800 new lines)
- `src/Stan/NameMeta.hs` - Add ledgerApiNameFrom helper if needed (~10 lines)
- `test/Test/Stan/Analysis.hs` - Import and call analysisPlinthRulesSpec (~5 lines)
- `stan.cabal` - Add Target.PlinthRules and Test.Stan.Analysis.PlinthRules modules (~4 lines)

---

## Testing Strategy

### Test File Structure

**target/Target/PlinthRules.hs**:
```haskell
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# ANN module ("onchain-contract" :: String) #-}

module Target.PlinthRules where

import PlutusLedgerApi.V1 (...)
import qualified PlutusLedgerApi.V1.Value as Value

-- ============================================================================
-- PLU-STAN-10: Incomplete Token Validation
-- ============================================================================

-- Positive cases (should trigger)
incompleteTokenCs :: ScriptContext -> CurrencySymbol -> TokenName -> Bool  -- Line X
incompleteTokenTn :: ScriptContext -> CurrencySymbol -> Bool                -- Line Y
incompleteTokenAmt :: ScriptContext -> CurrencySymbol -> TokenName -> Bool  -- Line Z

-- Negative cases (should NOT trigger)
completeToken :: ScriptContext -> CurrencySymbol -> TokenName -> Bool       -- Line A

-- ============================================================================
-- PLU-STAN-11: List Uniqueness
-- ... repeat for all 11 rules
```

**test/Test/Stan/Analysis/PlinthRules.hs**:
```haskell
module Test.Stan.Analysis.PlinthRules (analysisPlinthRulesSpec) where

import Test.Hspec (Spec, describe, it)
import Stan.Analysis (Analysis)
import Test.Stan.Analysis.Common (noObservationAssert, observationAssert)
import qualified Stan.Inspection.AntiPattern as AntiPattern

analysisPlinthRulesSpec :: Analysis -> Spec
analysisPlinthRulesSpec analysis = describe "Plinth Rules" $ do
  let checkObs = observationAssert ["PlinthRules"] analysis
  let noObs = noObservationAssert ["PlinthRules"] analysis

  describe "PLU-STAN-10: Incomplete Token Validation" $ do
    it "triggers on wildcard currency symbol" $
      checkObs AntiPattern.plustan10 42 12 45
    it "does not trigger on full validation" $
      noObs AntiPattern.plustan10 75

  -- ... repeat for all 11 rules
```

### Test Coverage Requirements

For each rule:
- **Minimum 3 positive test cases** (different triggering patterns)
- **Minimum 2 negative test cases** (valid code that shouldn't trigger)
- **Edge cases**: ADA exceptions, legitimate patterns, reference inputs

### Validation Process

1. **Create target file** with annotated examples
2. **Compile** to generate .hie files: `cabal build target`
3. **Write test specs** with line/column assertions
4. **Run tests**: `cabal test`
5. **Verify** no false positives/negatives
6. **Iterate** on patterns if needed

---

## Common Infrastructure

### NameMeta Patterns Needed

```haskell
-- PlutusLedgerApi.V1.Value
flattenValueName :: NameMeta
txInfoMintName :: NameMeta
valueOfName :: NameMeta
assetClassValueOfName :: NameMeta
lovelaceValueOfName :: NameMeta
leqName :: NameMeta
geqName :: NameMeta

-- PlutusLedgerApi.V1.Tx
txOutAddressName :: NameMeta
txOutDatumName :: NameMeta
txOutValueName :: NameMeta
txOutReferenceScriptName :: NameMeta
txInfoOutputsName :: NameMeta
txInfoInputsName :: NameMeta
txInfoValidRangeName :: NameMeta

-- PlutusLedgerApi.V1.Interval
ivFromName :: NameMeta
ivToName :: NameMeta

-- PlutusTx functions
nubName :: NameMeta
nubByName :: NameMeta
```

### PatternType Helpers

```haskell
valuePattern :: PatternType
txOutPattern :: PatternType
scriptContextPattern :: PatternType
addressPattern :: PatternType
```

---

## Verification & End-to-End Testing

### After Implementation

1. **Build project**: `cabal build`
2. **Run full test suite**: `cabal test`
3. **Verify all 11 new rules**:
   - Each rule has passing positive tests
   - Each rule has passing negative tests
   - No false positives on valid Plutus code
4. **Test on real Plutus contracts**:
   - Run plu-stan on example contracts from plinth-static-analyzer-research/audits
   - Verify rules detect known vulnerabilities from audit reports
5. **Performance check**:
   - Ensure analysis doesn't significantly slow down
   - Profile if needed

### Success Criteria

- ✅ All 11 new rules implemented (PLU-STAN-10 through PLU-STAN-20)
- ✅ PLU-STAN-04 enhanced with address/staking validation
- ✅ All tests passing (300+ test cases total)
- ✅ No false positives on valid Plutus patterns
- ✅ Rules detect known vulnerabilities from audits
- ✅ Documentation updated with new rules
- ✅ Code follows existing plu-stan conventions

---

## Notes

- **Plutus version compatibility**: Ensure rules work with V1, V2, and V3 where applicable
- **ADA exceptions**: Many rules should allow ADA-specific patterns (adaSymbol, adaToken)
- **Reference inputs**: ReadOnlySpend should not trigger when using reference inputs (V2+)
- **False positive mitigation**: Custom analyzers must be carefully designed to avoid incorrect flags
- **Performance**: Complex analyzers should be optimized to not slow down analysis significantly

---

## Key Architectural Decisions

1. **Custom analyzers over FindAst**: For absence detection and data flow rules, custom analyzers are necessary
2. **Enhance PLU-STAN-04**: Rather than creating separate rules, extend existing credential validation rule
3. **Single test file**: Consolidate all test examples in Target/PlinthRules.hs for easier maintenance
4. **Phased implementation**: Start simple, build up to complex to validate approach iteratively
5. **Severity levels**: Error for security issues, Warning for code quality, Performance for optimizations
