# Unvalidated Reference Script [PERFORMANCE]

## Description

Validators should explicitly constrain the `referenceScript` field of outputs they create or continue. A validator that ignores the reference script field allows attackers to:

- Attach arbitrarily large reference scripts
- Permanently inflate min-ADA requirements
- Increase future transaction fees
- Make protocol UTxOs economically unspendable

When a validator creates or validates continuation of UTxOs, it must check that no reference scripts are attached (or validate specific allowed reference scripts if that's part of the protocol design). Without this validation, an attacker can attach large scripts to protocol-controlled UTxOs, effectively griefing the protocol by making those UTxOs expensive to spend in future transactions.

### Detection Logic

Absence of calls to:

- `txOutReferenceScript`
- `txOutReferenceScriptHash`

## Examples

### Valid case

```hs
-- Validating no reference scripts in transaction
{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidator _datum _redeemer ctx = traceIfFalse "Script references not allowed" noScriptRefs
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    noScriptRefs :: Bool
    noScriptRefs = null (txInfoReferenceInputs info)

-- Validating specific reference script hash is used
{-# INLINABLE mkValidatorWithRefScript #-}
mkValidatorWithRefScript :: ScriptHash -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidatorWithRefScript allowedScriptHash _datum _redeemer ctx =
    traceIfFalse "Invalid reference script" validRefScript
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validRefScript :: Bool
    validRefScript = all checkRefScript (txInfoReferenceInputs info)

    checkRefScript :: TxInInfo -> Bool
    checkRefScript txInInfo =
        case txOutReferenceScript (txInInfoResolved txInInfo) of
            Just (ScriptHash sh) -> sh == allowedScriptHash
            Nothing              -> True

validator :: ScriptHash -> Validator
validator allowedHash = mkValidatorScript
    $$(PlutusTx.compile [|| mkValidatorWithRefScript ||])
    `PlutusTx.applyCode` PlutusTx.liftCode allowedHash
```

### Invalid case

```hs
-- Only validates value
{-# INLINABLE mkValidatorUnsafe1 #-}
mkValidatorUnsafe1 :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidatorUnsafe1 _datum _redeemer ctx = traceIfFalse "Invalid value" validValue
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validValue :: Bool
    validValue = all (\out -> valueOf (txOutValue out) adaSymbol adaToken >= 2_000_000)
                     (getContinuingOutputs ctx)

-- Only validates datum
{-# INLINABLE mkValidatorUnsafe2 #-}
mkValidatorUnsafe2 :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidatorUnsafe2 _datum _redeemer ctx = traceIfFalse "Missing datum" hasDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasDatum :: Bool
    hasDatum = all checkDatum (getContinuingOutputs ctx)

    checkDatum :: TxOut -> Bool
    checkDatum txOut = case txOutDatum txOut of
        OutputDatum _ -> True
        _             -> False
```
