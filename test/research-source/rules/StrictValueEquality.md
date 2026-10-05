# Strict value equality [SECURITY]

## Description

Validators could become unsatisfiable when enforcing exact equality on ADA or full output values, since it could fail under:

- minUTxO rules
- Datum size changes
- Reference script additions
- Token additions that increase UTxO size

Exact value equality is almost always incorrect for ADA in Plutus V2. Validators should enforce minimums, not exact amounts, unless there is a very strong invariant requiring exact equality.

### Detection Logic

```hs
lovelaceValueOf (txOutValue <var>) == <var>
```

## Examples

### Valid case

```hs
-- Validating token amount with minimum ADA
{-# INLINABLE mkValidatorWithToken #-}
mkValidatorWithToken :: CurrencySymbol -> TokenName -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidatorWithToken cs tn _datum _redeemer ctx =
    traceIfFalse "Invalid output" validOutput
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validOutput :: Bool
    validOutput = all checkOutput (getContinuingOutputs ctx)

    checkOutput :: TxOut -> Bool
    checkOutput out =
        valueOf (txOutValue out) cs tn == 1 &&
        lovelaceValueOf (txOutValue out) >= 2_000_000

validator :: CurrencySymbol -> TokenName -> Validator
validator cs tn = mkValidatorScript
    $$(PlutusTx.compile [|| mkValidatorWithToken ||])
    `PlutusTx.applyCode` PlutusTx.liftCode cs
    `PlutusTx.applyCode` PlutusTx.liftCode tn
```

### Invalid case

```hs
-- Enforces exact ADA amount (will fail if minUTxO requirements change)
{-# INLINABLE mkValidatorUnsafe1 #-}
mkValidatorUnsafe1 :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidatorUnsafe1 _datum _redeemer ctx = traceIfFalse "Invalid ADA amount" exactAda
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    exactAda :: Bool
    exactAda = all (\out -> lovelaceValueOf (txOutValue out) == 2_000_000)
                   (getContinuingOutputs ctx)
```
