# Plinth Smart Contract Development - Static Analysis Checks

Reference for common Plu-Stan checks and why they matter. Each item highlights the risk and safer patterns.

## Contents

- [Plinth Smart Contract Development - Static Analysis Checks](#plinth-smart-contract-development---static-analysis-checks)
  - [Contents](#contents)
  - [Data Handling \& Deserialization](#data-handling--deserialization)
  - [Value Handling](#value-handling)
  - [Equality](#equality)
  - [Optional Types](#optional-types)
  - [Higher-Order Functions](#higher-order-functions)
  - [Bindings](#bindings)
  - [Guards](#guards)
  - [Integers](#integers)
  - [Tooling](#tooling)
  - [Validity Interval / POSIX Time Misuse](#validity-interval--posix-time-misuse)

## Data Handling & Deserialization

- **Signature verification builtins:** Usage of `verifyEd25519Signature`, `verifyEcdsaSecp256k1Signature`, or `verifySchnorrSecp256k1Signature` must satisfy invariants. Ensure direct on-chain verification of message hash correspondence and include replay prevention mechanisms. (**Stan:** implemented via `PLU-STAN-01`)
<!-- - **`unsafeFromList` (AssocMap):** Using `AssocMap.unsafeFromList` without validation can cause script failure at runtime if the list contains duplicate keys. (**Stan:** not yet implemented) -->
- **`unsafeFromBuiltinData`:** Potential unbounded datum spam attack. Warn users and either: (**Stan:** implemented via `PLU-STAN-02`)
  1. Construct the expected output datum and assert the actual output datum matches it (enforces structural integrity by construction).
  2. Explicitly check structural integrity via a custom `checkIntegrity` that verifies field counts and per-field integrity in the underlying `BuiltinData`.
- **PubKeyHash/ScriptHash in fulfillment criteria without verifying ledger invariants:** Potential unsatisfiable constraints. (**Stan:** implemented via `PLU-STAN-10`)
  - *Example:* lender-chosen `repaymentAddress` in a lending validator.
  ```haskell
  lendingValidator :: ScriptContext -> ()
  lendingValidator ctx =
    ...
    case redeemer of
      RepayLoan ->
        let LoanDatum { repaymentAddress, loanAmount, loanEndDate, interestAPT } =
              getOwnInputDatum ownInput
        in if txOutAddress repaymentOutput == repaymentAddress && otherConditionsMet
             then ()
             else error "repayment output failed to satisfy loan terms"
      CreateLoan -> 
        let ownOutput = getOwnOutput txOutputs
            LoanDatum { repaymentAddress, loanAmount, loanEndDate, interestAPT } = unsafeFromBuiltinData $ getInlineDatum ownOutput
        in valueOf (txOutValue ownOutput) lendedCS lendedTokenName == loanAmount 
             && ...
  ```
  - **Issue:** The issue with the above is that because the lender is responsible for setting repaymentAddress (via `CreateLoan`), if the smart contract does not enforce proper constraints on PubKeyHash and ScriptHash (these constraints are not enforced by the types themselves) then the lender can set the repaymentAddress to Constr 0 [Constr 0 ["deadbeef"], Constr 1 []] although this does possess structural integrity, ie. it conforms to the BuiltinData representation of Address, but it doesn’t satisfy the invariants of the Address type from the ledger, and as such the ledger will reject any transaction which attempts to produce an output to this address, so the borrower will never be able to pay back their loan and thus is guaranteed to default and have their position liquidated. 

  So in addition to checking the BuiltinData encoding is correct, we must check that the ledger invariants are satisfied. The corrected variant of the above would do:
```haskell
-- | Checks if the builtin data corresponds to a pub key hash or script hash and satisfies 
-- the ledger invariant that requries pub key hash to be 28 bytes. 
-- An error is triggered when the builtin data is not well-formed.
{-# INLINABLE isBuiltinCredentialHash' #-}
isBuiltinCredentialHash' :: BI.BuiltinData -> BI.BuiltinBool
isBuiltinCredentialHash' b =
  traceIfFalse "isBuiltinPubKeyHash: invalid length !!!"
  (BI.equalsInteger (BI.lengthOfByteString (BI.unsafeDataAsB b)) 28)
  
-- | Checks if the builtin pair corresponds to a credential.
-- An error is triggered when the credential is not well-formed.
{-# INLINABLE isBuiltinCredential' #-}
isBuiltinCredential' :: BI.BuiltinPair BI.BuiltinInteger (BI.BuiltinList BI.BuiltinData) -> BI.BuiltinBool
isBuiltinCredential' b_constr =
  let !args = BI.snd b_constr
  in BI.ifThenElse (BI.equalsInteger (BI.fst b_constr) 0)
     (\_ ->
        builtinAnd
        (traceIfFalse "isBuiltinCredential': no additional fields expected !!!"
         (BI.null $ BI.tail args))
        (isBuiltinCredentialHash' (BI.head args))
     )
     (\_ -> BI.false)
     BI.unitval

-- | 
{-# INLINABLE isBuiltinAddress' #-}
isBuiltinAddress' :: BuiltinData -> BuiltinData
isBuiltinAddress' b =
  let !addr_pair = BI.unsafeDataAsConstr b
      !idx = BI.fst addr_pair
      !addr_l = BI.snd addr_pair
      !cred = BI.unsafeDataAsConstr $ BI.head addr_l
      !stake_l = BI.tail addr_l
      validCond =
        builtinAnd
        (traceIfFalse "isBuiltinAddress': Address tag construct expected !!!"
         (BI.equalsInteger idx 0)) $
        builtinAnd
        (traceIfFalse "isBuiltinAddress': ill-formed credential !!!"
         (isBuiltinCredential' cred)) $
        builtinAnd
        (traceIfFalse "isBuiltinAddress': ill-formed staking credential !!!"
         (isMaybeStakingCredential' (BI.head stake_l)))
        (traceIfFalse "isBuiltinAddress': no additional record fields expected !!!"
         (BI.null $ BI.tail stake_l))
  in validCond


-- | Checks if the builtin data corresponds to the encoding of a Maybe StakingCredential.
-- An error is triggered the builtin data is not well-formed.
{-# INLINABLE isMaybeStakingCredential' #-}
isMaybeStakingCredential' :: BI.BuiltinData -> BI.BuiltinBool
isMaybeStakingCredential' b =
  let !tup = BI.unsafeDataAsConstr b
      !just_l = BI.snd tup
      !idx = BI.fst tup
  in BI.ifThenElse (BI.equalsInteger idx 0)
     (\_ ->
          -- just case
          let !stakeCred = BI.unsafeDataAsConstr $ BI.head just_l
              !cred_l = BI.snd stakeCred
              !cred = BI.unsafeDataAsConstr $ BI.head cred_l
          in
            builtinAnd
            (traceIfFalse "isMaybeStakingCredential': no additional fields expected for Just !!!"
             (BI.null $ BI.tail just_l)) $
            builtinAnd
            (traceIfFalse "isMaybeStakingCredential': only staking hash expected !!!"
             (BI.equalsInteger (BI.fst stakeCred) 0)) $
            builtinAnd
            (traceIfFalse "isMaybeStakingCredential': only one credential expected !!!"
             (BI.null $ BI.tail cred_l)) (isBuiltinCredential' cred)
     )
     (\_ ->
        builtinAnd
        (traceIfFalse "isMaybeStakingCredential': Nothing construct expected !!!"
         (BI.equalsInteger idx 1))
        (traceIfFalse "isMaybeStakingCredential': no additional fields expected for Nothing !!!"
         (BI.null just_l))
     )
     BI.unitval

  lendingValidator :: ScriptContext -> ()
  lendingValidator ctx =
    ...
    case redeemer of
      RepayLoan ->
        let LoanDatum { repaymentAddress, loanAmount, loanEndDate, interestAPT } =
              getOwnInputDatum ownInput
        in if txOutAddress repaymentOutput == repaymentAddress && otherConditionsMet
             then ()
             else error "repayment output failed to satisfy loan terms"
      CreateLoan -> 
        let ownOutput = getOwnOutput txOutputs
            LoanDatum { repaymentAddress, loanAmount, loanEndDate, interestAPT } = unsafeFromBuiltinData $ getInlineDatum ownOutput
        in valueOf (txOutValue ownOutput) lendedCS lendedTokenName == loanAmount 
             && isBuiltinAddress repaymentAddress
             && ...
```

## Value Handling

- **`valueOf` instead of `valueEq` without limits:** Potential dust token attack when the number of unique tokens is unbounded. (**Stan:** partially implemented via `PLU-STAN-09` for `valueOf` in boolean comparisons)
- **`valueOf` with `adaSymbol` and `adaToken`:** Prefer extracting ADA directly. (**Stan:** not implemented)
  ```haskell
  -- BuiltinData variant
  outAda =
    BI.unsafeDataAsI
      $ BI.snd
      $ BI.head
      $ BI.unsafeDataAsMap
      $ BI.snd
      $ BI.head
      $ BI.unsafeDataAsMap outVal

  -- Or SOP-style
  outAda = snd $ head $ M.toList $ snd $ head (M.toList $ getValue outVal)
  ```
- **`currencySymbolValueOf` on minted value:** Potential unauthorized minting attack (common in `Burn` redeemers). (**Stan:** implemented via `PLU-STAN-11`)
  ```haskell
  ourMintingPolicy :: ScriptContext -> BuiltinUnit
  ourMintingPolicy ctx =
    case scriptContextRedeemer ctx of
      Mint -> ... -- deposit liquidity for LP tokens
      Burn -> currencySymbolValueOf mintedValue ownCS < 0
    where
      MintingScript ownCS = scriptContextScriptInfo ctx
      mintedValue = txInfoMint (scriptContextTxInfo ctx)
  ```
  - **Issue:** `currencySymbolValueOf` does not enforce that all entries in the value are strictly positive or negative. An attacker can burn and mint in the same transaction:
  ```haskell
  txInfoMint =
    PMap
      [ (PCurrencySymbol ourCS,
          PMap [ (PTokenName "TokenA", -2)
               , (PTokenName "TokenB", 1)
               ]
        )
      ]
  ```
  This passes the `Burn` check while still minting `TokenB`.

## Equality

- **Eq on `ScriptHash` / `PubKeyHash` / `PaymentCredential`:** Potential staking value theft. Prefer equality on full `Address`. (**Stan:** implemented via `PLU-STAN-04`)
- **Dangerous strict equalities:** Using strict integer equalities that attackers can manipulate.
  - *Example:*
  ```haskell
  -- minUTxOValue from testnet-shelley-genesis.json
  minTxOut :: Integer
  minTxOut = 2_000_000

  fee :: Integer
  fee = 300_000

  checkProposalValue proposalOutput fee =
    let proposalOutValue = txOutValue proposalOutput
        adaAmount = Value.lovelaceValueOf proposalOutValue
    in adaAmount #== minTxOut + fee
  ```
  - **Issue:** `minUTxOValue` is dynamic and set by ledger protocol parameters. Hard-coding it can strand funds if the required minimum rises above the enforced equality.

## Optional Types

- **Use of `Maybe`/`Either` in on-chain code:** Anti-pattern. Prefer fast-fail variants (for example, `tryFind` instead of `find`) or handle the other case via a continuation function. (**Stan:** partially implemented via `PLU-STAN-03` for `fromMaybe`)

## Higher-Order Functions

- **Higher-order helpers (`all`, `any`, etc.):** Anti-pattern in on-chain code; prefer specialized versions. (**Stan:** implemented via `PLU-STAN-05`)
  - *Example:*
  ```haskell
  findOwnInput :: ScriptContext -> Maybe TxInInfo
  findOwnInput ScriptContext{ scriptContextTxInfo = TxInfo{ txInfoInputs }
                            , scriptContextPurpose = Spending txOutRef
                            } =
    find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs
  findOwnInput _ = Nothing
  ```
  Rewritten with a specialized recursive function:
  ```haskell
  findOwnInput :: TxOutRef -> [TxInInfo] -> TxInInfo
  findOwnInput ownOutRef = go
    where
      go [] = error "findOwnInput: not found"
      go (i@TxInInfo{txInInfoOutRef} : rest) =
        if txInInfoOutRef == ownOutRef
          then i
          else go rest
  ```
- **Multiple/nested list traversals:** Composing list traversals (e.g., `map` over `filter`) forces multiple passes over the data, increasing execution costs. (**Stan:** implemented via `PLU-STAN-06`)
  - **Bad:**
  ```haskell
  processedList = map transform $ filter predicate inputList
  ```
  - **Good:**
  ```haskell
  processedList = go inputList
    where
      go [] = []
      go (x:xs) = if predicate x
                  then transform x : go xs
                  else go xs
  ```

## Bindings

- **Non-strict `let` bindings used multiple times:** Make them strict to avoid repeated evaluation. (**Stan:** implemented via `PLU-STAN-08`)
- **Immutable credentials baked into validator code:** Validator-reachable top-level `PubKeyHash` / `Credential` / `StakingCredential` / `Address` / `ScriptHash` bindings, or credential-like values specialized into validators with `applyCode` / `unsafeApplyCode`, cannot be rotated on-chain. Prefer storing mutable credentials in datum/state and document intentional immutability. (**Stan:** implemented via `PLU-STAN-21`)
- **Hardcoded values:** Most ledger-dependent values change across hardforks. Avoid hardcoding unless truly constant; prefer dynamic retrieval or explicit acknowledgment that the value is invariant.

## Guards

- **Guard syntax in on-chain code:** Anti-pattern that produces inefficient UPLC. Prefer `if-then-else` or lower-level conditionals. (**Stan:** implemented via `PLU-STAN-07`)
  ```haskell
  decimalLog2 :: Integer -> Integer
  decimalLog2 n
    | n == 0 = traceError "log2: -inf"
    | n == 1 = 0
    | n `modulo` 2 == 0 = 1 + decimalLog2 (n `divide` 2)
    | otherwise = traceError "log2: non-decimal output"
  ```

## Integers

- **Unconstrained integers:** Attackers can supply negative or out-of-range values. Add explicit range checks (for example, `x > 0`) and prefer newtypes that enforce invariants (such as `Natural`). (**Stan:** not implemented)
- **Division before multiplication:** Division before multiplication in integer arithmetic causes precision loss due to rounding. Always multiply first, then divide. (**Stan:** implemented via `PLU-STAN-16`)
  - **Bad:**
  ```haskell
  calculateFee amount rate =
    (amount `divide` 1000) * rate  -- Loses precision from early division
  ```
  - **Good:**
  ```haskell
  calculateFee amount rate =
    (amount * rate) `divide` 1000  -- Preserves precision by multiplying first
  ```

## Tooling

- **Codex integration:** Use Codex for vulnerability detection; similar to Slither (Ethereum static analysis).

## Validity Interval / POSIX Time Misuse

- **Detector:** Flags use of validity interval utilities (e.g. `from`, `to`, `interval`, `always`, `contains`, `member`) or any use of `txInfoValidRange` that does not ensure either the lower or upper bound is `Finite`. (**Stan:** implemented via `PLU-STAN-12`)
- **Risk:** Unbounded ranges can undermine intended timeboxing logic.

## Research conformance inspections

These complement the legacy inspections and are tested through the JSON CLI against the pinned research corpus.

### CWE research rule 28

**PLU-STAN-28: MissingAddressValidation.** An unconstrained destination lets a transaction pay the validated output to another address.

This is a bounded static warning. [Detection contract and limitations](docs/cwe-conformance.md).

```haskell
-- Suspicious
addressBad out d = txOutDatum out == OutputDatum d

-- Alternative
addressGood out d addr = txOutDatum out == OutputDatum d && txOutAddress out == addr
```

### CWE research rule 29

**PLU-STAN-29: MissingStakingValidation.** Payment credentials alone leave delegation and staking rewards unconstrained.

This is a bounded static warning. [Detection contract and limitations](docs/cwe-conformance.md).

```haskell
-- Suspicious
stakeBad out cred = addressCredential (txOutAddress out) == cred

-- Alternative
stakeGood out cred = addressCredential (txOutAddress out) == cred && addressStakingCredential (txOutAddress out) == Nothing
```

### CWE research rule 30

**PLU-STAN-30: UnvalidatedReferenceScript.** An attacker can attach a costly reference script to an otherwise valid output.

This is a bounded static warning. [Detection contract and limitations](docs/cwe-conformance.md).

```haskell
-- Suspicious
referenceBad out addr = txOutAddress out == addr

-- Alternative
referenceGood out addr = txOutAddress out == addr && txOutReferenceScript out == Nothing
```

### CWE research rule 31

**PLU-STAN-31: UnvalidatedDatum.** A script output with unconstrained datum content can corrupt state or become unspendable.

This is a bounded static warning. [Detection contract and limitations](docs/cwe-conformance.md).

```haskell
-- Suspicious
datumBad out addr = txOutAddress out == addr

-- Alternative
datumGood out addr d = txOutAddress out == addr && txOutDatum out == OutputDatum d
```

### CWE research rule 32

**PLU-STAN-32: TrashTokens.** Checking only required assets permits unrelated tokens and growing output costs.

This is a bounded static warning. [Detection contract and limitations](docs/cwe-conformance.md).

```haskell
-- Suspicious
trashSubset out expected = txOutValue out `V.geq` expected

-- Alternative
trashGood out expected = txOutValue out == expected
```

### CWE research rule 33

**PLU-STAN-33: UncheckedRedeemer.** A co-spent script input must perform the expected operation; check its corresponding redeemer.

This is a bounded static warning. [Detection contract and limitations](docs/cwe-conformance.md).

```haskell
-- Suspicious
redeemerBad info = any (\i -> case addressCredential (txOutAddress (txInInfoResolved i)) of
  ScriptCredential _ -> True
  _ -> False) (txInfoInputs info)

-- Alternative
redeemerReferences info = any (\i -> case addressCredential (txOutAddress (txInInfoResolved i)) of
  ScriptCredential _ -> True
  _ -> False) (txInfoReferenceInputs info)
```

### CWE research rule 34

**PLU-STAN-34: ReadOnlySpend.** Recreating an unchanged datum can cause unnecessary spending and contention. Review whether a reference input suffices.

This is a bounded static warning. [Detection contract and limitations](docs/cwe-conformance.md).

```haskell
-- Suspicious
readOnlyFull i out = txOutAddress (txInInfoResolved i) == txOutAddress out && txOutValue (txInInfoResolved i) == txOutValue out && txOutDatum (txInInfoResolved i) == txOutDatum out && txOutReferenceScript (txInInfoResolved i) == txOutReferenceScript out

-- Alternative
readOnlyChanged i out next = txOutDatum out == OutputDatum next && txOutValue out == txOutValue (txInInfoResolved i)
```

### CWE research rule 35

**PLU-STAN-35: ValidityRangeBound.** Finite endpoints alone allow arbitrarily long validity windows. Bound their difference.

This is a bounded static warning. [Detection contract and limitations](docs/cwe-conformance.md).

```haskell
-- Suspicious
timeFiniteBad info deadline = case I.ivFrom (txInfoValidRange info) of
  I.LowerBound (I.Finite lo) _ -> lo >= deadline
  _ -> False

-- Alternative
timeGood info maxDuration = case (I.ivFrom (txInfoValidRange info), I.ivTo (txInfoValidRange info)) of
  (I.LowerBound (I.Finite lo) _, I.UpperBound (I.Finite hi) _) -> hi - lo <= maxDuration
  _ -> False
```

### CWE research rule 36

**PLU-STAN-36: DatumComparisonOptimization.** Decoding a datum only to compare fields can cost more than comparing its encoded representation.

This is a bounded static warning. [Detection contract and limitations](docs/cwe-conformance.md).

```haskell
-- Suspicious
decodeCompare d expected = case Tx.fromBuiltinData d of
  Just (TestDatum n pkh) -> n == amount expected && pkh == owner expected
  Nothing -> False

-- Alternative
encodeCompare d expected = d == Tx.toBuiltinData expected
```

### CWE research rule 37

**PLU-STAN-37: IncompleteTokenValidation.** Ignoring a currency symbol, token name or amount can authorize an unintended token.

This is a bounded static warning. [Detection contract and limitations](docs/cwe-conformance.md).

```haskell
-- Suspicious
tokenWildcard0 info symbol name = all (\(_, tn, q) -> tn == name && q == 1) (V.flattenValue (txInfoMint info))

-- Alternative
tokenGood info symbol name = all (\(cs, tn, q) -> cs == symbol && tn == name && q == 1) (V.flattenValue (txInfoMint info))
```

### CWE research rule 38

**PLU-STAN-38: StrictValueEquality.** Exact ADA equality can conflict with minimum-output requirements. Review an appropriate lower bound.

This is a bounded static warning. [Detection contract and limitations](docs/cwe-conformance.md).

```haskell
-- Suspicious
strictAda out n = V.lovelaceValueOf (txOutValue out) == n

-- Alternative
minimumAda out n = V.lovelaceValueOf (txOutValue out) >= n
```

### CWE research rule 39

**PLU-STAN-39: UnvalidatedInputIndex.** A dynamic index selects a position, not the intended UTxO. Check the selected input identity.

This is a bounded static warning. [Detection contract and limitations](docs/cwe-conformance.md).

```haskell
-- Suspicious
indexBad info r addr = let selected = txInfoInputs info !! fromInteger (inputIndex r) in txOutAddress (txInInfoResolved selected) == addr

-- Alternative
indexGood info r cs tn = let selected = txInfoInputs info !! fromInteger (inputIndex r) in V.valueOf (txOutValue (txInInfoResolved selected)) cs tn == 1
```

### CWE research rule 40

**PLU-STAN-40: HelperFunctions.** A trivial helper may be inlined to reduce overhead. Measure generated code before changing it.

This is a bounded static warning. [Detection contract and limitations](docs/cwe-conformance.md).

```haskell
-- Suspicious
forwardHelper pkh info = txSignedBy info pkh

-- Alternative
substantialHelper x y = x > 0 && y > x
```

### CWE research rule 41

**PLU-STAN-41: FixedStructureMap.** Fixed string keys in a datum map can hide missing fields. A typed record makes structure explicit.

This is a bounded static warning. [Detection contract and limitations](docs/cwe-conformance.md).

```haskell
-- Suspicious
mapBad d = M.member "fee" (extraInfo d) && M.member "owner" (extraInfo d)

-- Alternative
mapDynamic key d = M.member key (extraInfo d)
```
