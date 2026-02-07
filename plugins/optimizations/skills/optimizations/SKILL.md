# skills.md — Witness-Driven Complexity Collapse Finder for `onchain-contract` Modules

## Purpose

This skill scans the working directory for on-chain modules annotated with:

```haskell
{-# ANN module ("onchain-contract" :: String) #-}
```

For each annotated module, it identifies opportunities to reduce ExUnits by applying **witness-driven complexity collapse**:

> Instead of *solving* for a value on-chain (searching, scanning, matching, computing a “best/first/only” element),
> the redeemer (witness) *provides* the candidate solution, and the validator only **verifies** it.

### Key idea

Replace “searching for the right thing” with “point to the thing, then prove it’s right”.

This is typically a win when:
- the on-chain “solve” step requires scanning a list/map *and* running an expensive predicate on many candidates, and/or
- you do the same traversal multiple times, and/or
- you can enforce a tx-shape that keeps the searched structure small and predictable.

> Note: In Haskell/Plutus, list indexing (`!!` / `elemAt`) is still O(i) because lists are linked structures.
> The savings often comes from avoiding *re-running expensive predicates* across many candidates and from collapsing multiple traversals into one.

---

## Non-Goals

- Not a general security audit.
- Not an off-chain transaction builder review (except for describing what the redeemer should carry).
- Not a formal ExUnits measurement tool (qualitative impact is sufficient unless you have profiling).

---

## Inputs

- A project working directory containing Haskell/Plutus-style source files (e.g., `.hs`, `.lhs`).
- Modules may contain validators, minting policies, staking scripts, or helpers used by those.

---

## Outputs

A Markdown report (per module) listing:
- file/module identification
- “solve-on-chain” hotspots
- a witness-driven rewrite idea
- on-chain verification conditions that must remain binding
- ambiguity/uniqueness notes (if relevant)
- expected qualitative ExUnits impact

---

## High-Level Workflow

### 1) Discover annotated modules

Search all source files for the exact annotation:

```haskell
{-# ANN module ("onchain-contract" :: String) #-}
```

For each match:
- record file path
- record module name (if present)
- treat file as “on-chain relevant” for deeper analysis

### 2) Identify entrypoints and hot paths

Within each annotated module:
- locate validator/policy entrypoints and their transitive helpers.
Common names/patterns:
- `mkValidator`, `validator`, `typedValidator`
- `mkPolicy`, `policy`, `mintingPolicy`
- functions consuming `ScriptContext`, `TxInfo`, `TxOut`, `Value`, `Datum`, `Redeemer`

Prioritize code that:
- runs on every validation
- traverses ledger lists/maps
- decodes `BuiltinData` repeatedly
- performs `flattenValue` or repeated `valueOf`

### 3) Detect “solve-on-chain” patterns

Look for code that computes or searches for a “right” object, where the witness could instead point to the candidate.

Focus on:
- list searches (`find`, `filter`, folds that return `Maybe`)
- repeated traversals of the same structure
- “select special element” logic (continuing output, script input, unique datum, minted asset entry, etc.)
- expensive computations that could be verified by checking an equation/invariant

### 4) Propose witness-driven collapse

For each candidate hotspot:
- specify what the witness should provide (often an **index**, sometimes a stable identifier)
- replace the on-chain search/solve with:
  - one selection (indexing / pattern match)
  - one or more verification predicates
- preserve correctness by ensuring the predicates fully bind the intended semantics

### 5) Check ambiguity/disambiguation needs

If multiple candidates could satisfy your predicate, a witness can choose among them unless the script:
- enforces uniqueness/tx-shape constraints, or
- adds binding checks that fully determine the intended element

Prefer **tx-shape constraints** when possible (often cheaper than scanning to prove uniqueness).

---

## Pattern Catalog (What to Look For)

### A) List search / find (primary target)

**Indicators**
- `find p xs`
- `filter p xs` then `head`, `!! 0`, or `(x:_)`
- custom folds that return `Maybe` / “first match”
- repeated `any`/`all` after deriving subsets

**Witness-driven rewrite**
- Redeemer provides `i :: Integer` (or equivalent)
- On-chain:
  - select `x = xs !! i`
  - verify `p x` with a strong predicate

**Budget intuition**
- collapses “scan + expensive predicate per element” into “select + one predicate”
- especially useful when `p` is costly (datum decoding, value checks, hashing, map lookups, etc.)

---

### B) Repeated scanning over the same list

**Indicators**
- multiple scans over `txInfoInputs`, `txInfoOutputs`, `txInfoReferenceInputs`, etc.
- compute `xs = txInfoOutputs info` but traverse `xs` several times

**Witness-driven rewrite**
- redeemer supplies indices for each required “special” element:
  - `scriptInputIx`, `continuingOutputIx`, `refInputIx`, etc.
- on-chain:
  - select each once
  - verify each predicate
  - cache selections (avoid re-indexing + recomputation)

---

### C) Continuing output selection

**Indicators**
- scanning outputs to find the one that pays back to the script address / script hash
- matching datum/value conditions among outputs

**Witness-driven rewrite**
- redeemer supplies `continuingOutputIx`
- on-chain verifies:
  - output at that index is paid to the script
  - datum matches expected form/hash
  - value/state-transition constraints hold

---

### D) Script input selection (own input)

**Indicators**
- scanning inputs to find the input locked by this script
- matching `txOutAddress`, script hash, datum, etc.

**Witness-driven rewrite**
- redeemer supplies `scriptInputIx`
- on-chain verifies:
  - selected input is script-locked (correct address/script hash)
  - datum/redeemer constraints are met

---

### E) Datum / redeemer decoding hotspots

**Indicators**
- repeated `unsafeFromBuiltinData` / `fromBuiltinData` / `builtinDataToData` conversions
- decoding the same datum/redeemer multiple times in helper functions

**Witness-driven rewrite**
- decode once, pass decoded values through (or make helper functions accept decoded structures)
- if the script currently *searches* for a datum among datums, consider witness-provided index + verification of the referenced hash

---

### F) Value/token checks that flatten then search

**Indicators**
- `flattenValue` then search/filter for `(CurrencySymbol, TokenName, amount)`
- repeated `valueOf` calls on the same `Value` without caching

**Witness-driven rewrite**
- prefer direct queries (`valueOf`) when identifiers are fixed
- if you must traverse flattened entries, flatten once and reuse
- witness can provide which entry is relevant, but still verify identifiers and amount

---

## Rules of Thumb (Correctness + ExUnits)

### 1) Redeemer-provided indices are intended, not a vulnerability

Treat witness-driven indexing as a first-class optimization:
- witness points to the candidate
- validator verifies it is correct

### 2) Bounds checks are optional

Out-of-bounds indexing can be an intentional fast-fail. Only recommend explicit bounds checks if you want:
- clearer error messages, or
- a consistent “shape-first” validation style

### 3) Verification predicates must be strong

When collapsing a search, ensure the predicate binds the intended meaning:
- not just “has token X”
- but “is the continuing output with datum D and expected value transition”, etc.

### 4) Avoid cross-list positional coupling unless enforced

It’s fine to index within a single list and verify the element.
Do not assume that `inputs[i]` corresponds to `outputs[i]` unless you enforce a tx-shape 
that guarantees it by checking the correspondence  via applying a predicate to each corresponding 
`output[i]` to verify it does indeed satisfy the spending conditions of the corresponding `input[i]`.

### 5) Prefer tx-shape constraints over uniqueness scans

If ambiguity matters, enforce a transaction shape that removes it (often cheaper than scanning to prove uniqueness).

---

## Report Format

For each annotated module:

```md
## Module: <ModuleName>
**Path:** <file>

### Candidate: <short title>
**Location:** L<start>-L<end>
**Current pattern:** <e.g. find/filter/gcd/getContinuingOutputs>
**Why it’s expensive:** <e.g. repeated scan, predicate cost, repeated decoding>

**Witness-driven collapse:**
- Redeemer provides: <e.g. continuingOutputIx :: Integer>
- On-chain selects: <outputs !! continuingOutputIx>
- On-chain verifies: <predicate(s) that fully bind correctness>

**Notes:**
- Ambiguity risk: <none | explain>
- Tx-shape constraints recommended: <e.g. require exactly 1 unique continuing output for each corresponding input>
- Expected ExUnits impact: <qualitative>
```

---

## Examples of Witness-Driven Complexity Collapse

### Example 1 — Replace `find` with witness index + verification

**Before (solve on-chain)**

```haskell
case find p xs of
  Just x  -> ...
  Nothing -> traceError "not found"
```

**After (witness-driven)**

```haskell
let !x = xs !! ixFromRedeemer
in  if p x then ... else traceError "wrong element"
```

**What to verify**
- `p x` must fully bind the intended semantics (not a loose property)
- if ambiguity matters, enforce tx-shape constraints so only the intended element can satisfy `p`

---

### Example 2 — Continuing output: scan outputs → witness output index

**Before**

```haskell
...
let outs = txInfoOutputs info
    ownOutputs = filter isContinuing outs 
 in validateStateTransition ownInputs ownOutputs
```

**After**

```haskell
data OurRedeemer = OurRedeemer {ownOutputsStartIdx :: Integer, numExpectedOwnOutputs :: Integer}

ourValidator :: BuiltinData -> BI.BuiltinUnit
outValidator ctx = 
  ...
  let 
      outs = txInfoOutputsData info
      contOutsStart = BI.dropList ownOutputsStartIdx
      -- where tryTakeWithPredicate is similar to `take` except it applys a predicate check to 
      -- each element taken and errors if the predicate check fails
      -- ie: tryTakeWithPredicate :: Integer -> (a -> BuiltinBool) -> [a]
      !ownOutputs = tryTakeWithPredicate numExpectedOwnOutputs isContinuingOutput
  in  validateStateTransition ownInputs ownOutputs
```

**Typical verification**
- output address/script hash matches
- datum hash / inline datum matches expected
- value/state transition constraints hold

---

### Example 3 — Script input: scan inputs → witness input index

**Before**

```haskell
let ins = txInfoInputs info
case find isOwnScriptInput ins of
  Just ownIn -> ...
  Nothing    -> traceError "missing script input"
```

**After**

```haskell
let ins = txInfoInputs info
    !ownIn = ins !! scriptInputIx
in  traceIfFalse "not our input" (isOwnScriptInput ownIn)
    && ...
```

**Common verification**
- correct script address/hash
- datum correctness
- value constraints

---

### Example 4 — GCD: compute on-chain → witness gcd + Bézout coefficients

Sometimes you might compute `gcd(a,b)` on-chain (e.g., in math-heavy scripts). Instead:

**Witness provides**
- `g` (claimed gcd)
- coefficients `(x, y)` such that `a*x + b*y = g`

**On-chain verifies**
1) `a*x + b*y == g`  (Bézout identity)
2) `a % g == 0` and `b % g == 0` (g divides both; implement with your available primitives)
3) `g > 0` (if you want a canonical gcd)

**Why it works**
- Any Bézout combination `a*x + b*y` must be a multiple of the true gcd.
- If `g` also divides both `a` and `b`, then `g` and `gcd(a,b)` divide each other ⇒ `g == gcd(a,b)` (up to sign; enforce positivity if desired).

**ExUnits intuition**
- Verifying one equation + a couple divisibility checks can be cheaper than running Euclid’s algorithm, depending on your integer primitives and how often this is used.

---

### Example 5 — Modular inverse: extended Euclid on-chain → witness inverse + verification

Instead of computing `inv = a^{-1} mod m` on-chain:

**Witness provides**
- `inv`

**On-chain verifies**
- `(a * inv) % m == 1`

Optionally also verify:
- `0 <= inv < m` (canonical form)

**ExUnits intuition**
- One multiplication + one modulus check vs. a full extended gcd routine.

---

## Completion Criteria

This skill is complete when:
- every module with the `onchain-contract` ANN is enumerated
- each module has a pass for:
  - list scans
  - repeated traversals
  - “special element” selection (inputs/outputs/datum/value)
  - math/compute-heavy “solve” patterns that can be verified by equations/invariants
- each identified opportunity includes:
  - a concrete witness suggestion
  - a verification outline
  - ambiguity/shape notes
