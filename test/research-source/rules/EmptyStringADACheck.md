# Empty String ADA Check [CODE-QUALITY]

## Description

This pattern was identified in Aiken audit finding **ORD-02** (MinSwap Dex v2 reaudit), where empty strings are used to verify if assets are ADA instead of using a dedicated helper function.

Using empty string comparison to check if an asset is ADA is less clear and maintainable than using a dedicated helper function. While ADA's token name is indeed an empty `ByteString`, direct string comparison makes the code less readable and more prone to errors if the check logic needs to be updated.

## Detection Logic

**Pattern - Empty string comparison for ADA:**

```hs
<token-name> == tokenName emptyByteString
<token-name> == tokenName ""

tokenName emptyByteString == <token-name>
tokenName "" == <token-name>
```

Where `<token-name>` is of type `TokenName`.

or:

```hs
<currency-symbol> == currencySymbol emptyByteString
<currency-symbol> == currencySymbol ""

currencySymbol emptyByteString == <currency-symbol>
currencySymbol "" == <currency-symbol>
```

Where `<currency-symbol>` is of type `CurrencySymbol`.

**Recommended alternative:**

Create a helper function:

```hs
isAda :: CurrencySymbol -> TokenName -> Bool
isAda cs tn = cs == adaSymbol && tn == adaToken
```

## Examples

### Valid case

```hs
-- Uses explicit helper function
isAda :: CurrencySymbol -> TokenName -> Bool
isAda cs tn = cs == adaSymbol && tn == adaToken

validateAsset :: CurrencySymbol -> TokenName -> Bool
validateAsset cs tn =
    if isAda cs tn
    then
         True
    else
         False
```

### Invalid case

```hs
-- Uses empty string comparison
validateAsset :: CurrencySymbol -> TokenName -> Bool
validateAsset cs tn =
    if tn == tokenName emptyByteString
    then
         True
    else False
```
