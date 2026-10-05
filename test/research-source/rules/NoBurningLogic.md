# No Burning Logic [CODE-QUALITY] [SECURITY]

## Description

Minting policies should include logic to allow burning tokens when the protocol design requires tokens to be destroyed.

When minting policies only implement minting logic without corresponding burning logic, this can prevent protocol operations that depend on token destruction, such as withdrawing funds that require burning authorization tokens or unlocking collateral that requires burning loan tokens. In this case, it could become a security issue.

While having a burning mechanism is generally recommended, the absence of one may not always be strictly problematic depending on the protocol's specific use case.

## Detection Logic

**Pattern 1 - Using valueOf without burn validation:**

Presence of minting validation:

```hs
valueOf (txInfoMint (scriptContextTxInfo <var>)) <cs> <tn> > 0
valueOf (txInfoMint (scriptContextTxInfo <var>)) <cs> <tn> == <positive-literal>
```

And absence of corresponding burning validation for the same `<cs>` and `<tn>`:

```hs
valueOf (txInfoMint (scriptContextTxInfo <var>)) <cs> <tn> < 0
valueOf (txInfoMint (scriptContextTxInfo <var>)) <cs> <tn> == <negative-literal>
```

Where `<positive-literal>` is any positive integer literal and `<negative-literal>` is any negative integer literal.

**Pattern 2 - Using flattenValue with case matching:**

```hs
case flattenValue (txInfoMint (scriptContextTxInfo <var>)) of
    [(<cs>, <tn>, <amount>)] -> <logic>
    ...
```

Where `<logic>` could be any validation logic that checks only positive `<amount>`:

```hs
<amount> > 0
<amount> == <positive-literal>
```

And does not contain validation for negative `<amount>`:

```hs
<amount> < 0
<amount> == <negative-literal>
```

## Examples

### Valid case

```hs
-- Pattern 1: valueOf with both minting and burning validation
mkPolicy :: ScriptContext -> Bool
mkPolicy ctx =
    let info = scriptContextTxInfo ctx
        mintedAmount = valueOf (txInfoMint info) ownCurrencySymbol tokenName

        -- Allows positive amounts (minting)
        validMint = mintedAmount > 0 && validateMintConditions ctx

        -- Also allows negative amounts (burning)
        validBurn = mintedAmount < 0 && validateBurnConditions ctx

    in validMint || validBurn

-- Pattern 2: flattenValue with both positive and negative validation
mkPolicy2 :: TokenName -> ScriptContext -> Bool
mkPolicy2 tn ctx =
  case flattenValue (txInfoMint $ scriptContextTxInfo ctx) of
    [(_, tn', amt)] ->
      tn' == tn && (amt == 1 || amt == (-1)) -- Allows for both positive and negative amounts
    _ -> False
  where
    info = scriptContextTxInfo ctx
```

### Invalid case

```hs
-- Pattern 1: valueOf with only minting validation
mkPolicyUnsafe :: ScriptContext -> Bool
mkPolicyUnsafe ctx =
    let info = scriptContextTxInfo ctx
        mintedAmount = valueOf (txInfoMint info) ownCurrencySymbol tokenName
    in mintedAmount > 0 && validateMintConditions ctx

-- Pattern 2: flattenValue with only positive amount validation
mkPolicyUnsafe2 :: TokenName -> ScriptContext -> Bool
mkPolicyUnsafe2 tn ctx =
  case flattenValue (txInfoMint info) of
    [(_, tn', amount)] ->
      tn' == tn && amount == 1
    _ -> False
  where
    info = scriptContextTxInfo ctx
```
