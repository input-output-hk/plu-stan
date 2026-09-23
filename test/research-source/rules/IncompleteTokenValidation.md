# Incomplete Token Validation [SECURITY]

## Description
Any validator or minting policy that relies on token based authorization must fully validate the token tuple (currency symbol, token name, quantity) and must restrict minting/burning and additional tokens under the same policy where relevant. This rule is intended to prevent authorization bypass, invariant breaks and protocol state corruption. The rule should detect situations where the on-chain logic has an incomplete validation of token tuple components (cs, tn, n), for example:
- Accepts any token under a policy
- Accepts any token under certain token name
- Accepts any quantity
- Does not restrict minting/burning to the intended policy

### Detection Logic

**For minting cases**:
```hs
<fold-function> (\(<var>, <var>, <var>) -> <logic>) (flattenValue $ txInfoMint (scriptContextTxInfo <var>))
```

**For continuing output cases**:
```hs
<fold-function> (\(<var>, <var>, <var>) -> <logic>) (flattenValue <var>)
```

Where `<fold-function>` could be any function that iterates over the elements of a list such as `filter`, `all`, etc.
And `<logic>` could be any validation that has at least one of the variables in the tuple as wildcard.

## Examples

### Valid case

```hs
-- Checks for cs, tn, quantity and no extra tokens minting allowed
validNftMint :: ScriptContext -> CurrencySymbol -> TokenName -> Bool
validNftMint ctx cs tn =
 all
    (\(cs', tn', amt) ->
        if cs' == cs && tn' == tn
        then amt == 1
        else amt == 0
    )
    (flattenValue $ txInfoMint (scriptContextTxInfo ctx))

-- Validates token name and currency symbol  matches
validation1 :: Value -> CurrencySymbol -> TokenName -> Bool
validation1 v cs tk am =
    all
        (\(cs',tk',am') ->
            (cs' == cs && tk' == tk && am' <= am) || (cs' == Ada.adaSymbol  && tk' == Ada.adaToken))
    (flattenValue v)

{- From AADA. This pattern may be obscured since the checks are performed through helper functions,
    requiring a deeper analysis to detect -}
mkPolicy tn ctx = validate
  where
    mintFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintFlattened = flattenValue $ txInfoMint (scriptContextTxInfo ctx)

    ownMintedValue :: [(CurrencySymbol, TokenName, Integer)]
    ownMintedValue = filter (\(cs, _tn, _n) -> cs == ownCurrencySymbol ctx) mintFlattened

    singleTokenName :: Bool
    singleTokenName = all (\(_cs, tn', _n) -> tn == tn') ownMintedValue

    burn :: Bool
    burn = valueOf (txInfoMint (U.info ctx)) (ownCurrencySymbol ctx) tn < 0

    validate = singleTokenName && burn
```

### Invalid case

```hs
-- Checks for cs in minting while discarding token name and amount
invalidNftMint :: ScriptContext -> CurrencySymbol -> TokenName -> Bool
invalidNftMint ctx cs =
 all (\(cs', _, _) -> cs' == cs ) (flattenValue $ txInfoMint (scriptContextTxInfo ctx))

-- Validates token name and currency symbol  matches
validation2 :: Value -> CurrencySymbol -> TokenName -> Bool
validation2 v cs tk =
    all
        (\(cs',tk', _) ->
            (cs' == cs && tk' == tk ) || (cs' == Ada.adaSymbol  && tk' == Ada.adaToken))
    (flattenValue v)
```
