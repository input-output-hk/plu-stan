# Trash Tokens [PERFORMANCE] [SECURITY]

## Description

Validators should fully constrain the value of created or continuing outputs, instead of merely checking that required assets are present.
Validators that only enforce subset inclusion allow attackers to inject arbitrary tokens into script-controlled UTxOs, and this can:

- Permanently lock funds
- Inflate min-ADA requirements
- Cause transaction size / execution failures

If the transaction size reaches to a point where the funds are permanently locked, this could be considered a security issue.

### Detection Logic

**Pattern 1 - Subset value comparisons:**

```hs
<value> `leq` <value>
<value> `geq` <value>

assetClassValueOf <var> <var> >= <var>
assetClassValueOf <var> <var> <= <var>
```

Where value could be the result of calling `TxOutValue` with an output or `assetClassValue` with an asset class and an amount.

**Pattern 2 - Missing restriction of value:**

```hs
length ((flattenValue . txOutValue) <var>) <= <var>
```

## Examples

### Valid case

```hs
-- Exact value equality
validateExact :: Value -> AssetClass -> Integer -> Bool
validateExact v asset amount =
    v == assetClassValue asset amount <> lovelaceValueOf minAda

-- Filter with length restriction
validateNoTrash :: Value -> AssetClass -> Integer -> Bool
validateNoTrash v asset amount =
    let flattened = flattenValue v
        validTokens = filter isValid flattened
    in length flattened == length validTokens

-- Length cap to prevent bloat
checkForTokens :: TxOut -> Bool
checkForTokens utxo =
    length (flattenValue $ txOutValue utxo) <= 3
```

### Invalid case

```hs
-- Subset check allows trash tokens
invalidSubSet :: Value -> Asset -> Integer -> Bool
invalidSubset v asset amount =
    assetClassValue asset amount `leq` v

-- Filter without count validation
invalidFilter :: Value -> CurrencySymbol -> Bool
invalidFilter v cs =
    let filtered = filter (\(cs', _, _) -> cs' == cs) (flattenValue v)
    in not (null filtered)

-- Amount check without value restriction
invalidAmount :: Value -> Asset -> Integer -> Bool
invalidAmount v asset amount =
    assetClassValueOf v asset >= amount

-- From AADA: Checks only over elements of a filtered list
factoryNFT :: [(CurrencySymbol, TokenName, Integer)] -> AssetClass -> AssetClass
factoryNFT flattenVal asset = case [c | c <- flattenValue var,
  adaSymbol /= getCS c
  && fst (unAssetClass asset) /= getCS c
  && getAmount c == 1] of
    [c] -> assetClass (getCS c) (getTN c)
    _ -> debugError "E19" True

```

