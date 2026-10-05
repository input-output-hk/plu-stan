# Unvalidated Datum [SECURITY]

## Description

Validators must validate the datum of outputs they create at script addresses. When a validator creates or continues outputs without validating the datum field, attackers can:

- Inject arbitrary or malicious datum data into protocol UTxOs
- Create unspendable UTxOs if the datum format is incompatible with the validator
- Corrupt protocol state by modifying datum fields that affect subsequent operations
- Break invariants that other operations depend on

This rule applies when outputs are sent to script addresses (not user pubkey addresses). The validator should either:

1. Validate that the output datum equals an expected datum
2. Validate specific critical fields of the output datum

**Note:** This pattern covers cases where no datum validation occurs. For cases where only some fields are validated, see [PARTIAL-UNVALIDATED-DATUM].

## Detection Logic

### Missing validation

Absence of calls to `txOutDatum` or `txOutDatumHash` when validator logic includes output creation or validation at script addresses.

Identifiable by checking `txOutAddress` against script credentials:

```hs
(txOutAddress <var> == scriptHashAddress <var>)
```

or

```hs
case txOutAddress <var> of
    (ScriptCredential scriptHash) _ -> scriptHash
```

It could also be the case that `txOutDatum` or `txOutDatumHash` is called, but the datum is discarded as a wildcard.

```hs
case txOutDatum <var> of
    OutputDatum _ -> ...
```

or

```hs
case txOutDatum <var> of
    OutputDatumHash _ -> ...
```

## Examples

### Valid case

```hs
-- Checks if the UTxO is being sent to the script address, and validates its datum
validContinuingOutput :: Integer -> Bool
validContinuingOutput newVal =
    case getContinuingOutputs ctx of
        [out] ->
            (txOutAddress out == scriptHashAddress ownHash) &&

            case txOutDatum out of
                OutputDatum (Datum d) ->
                    case PlutusTx.fromBuiltinData d of
                        Just (MyDatum val) -> val == newVal
                        Nothing -> False
                _ -> False
        _ -> False
```

### Invalid cases

```hs
-- Validates address but ignores datum completely
hasContinuingOutput :: Bool
hasContinuingOutput =
    any (\out -> txOutAddress out == scriptHashAddress ownHash)
        (txInfoOutputs info)

```

```hs
-- From: Cerra P2P Lending
-- Searches for "the only input with any datum" instead of filtering by specific datum type and address
contractInput :: TxInInfo
!contractInput = case [i | <- txInputs, scriptDatumExists (txInInfoResolved i)] of
	[i] -> i
	_ -> debugError “E15” True

scriptDatumExists :: TxOut -> Bool
scriptDatumExists output = case txOutDatum output of
    OutputDatum _ -> True
    OutputDatumHash _ -> True
    NoOutputDatum -> False
```
