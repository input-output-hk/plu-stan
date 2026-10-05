# Unvalidated Input Index [SECURITY]

## Description

Validators that use redeemer-provided indices to select specific inputs must validate that the selected input contains the expected identifying token (NFT). When validators trust redeemer indices without verification, attackers can:

- Substitute the expected input with a fake UTxO at the same address
- Corrupt protocol invariants by processing incorrect inputs

**Note:** There could be other predicates for what constitutes a "valid" UTxO, although checking for a NFT is the standard.

## Detection logic

**Pattern - Input/reference input selection by redeemer index**

```hs
<list> !! <redeemer-field>
```

Where `<list>` is one of:

- `txInfoInputs <var>`
- `txInfoReferenceInputs <var>`

And `<redeemer-field>` is a field extracted from the redeemer parameter.

**Missing validation**

After pattern `<var> = <list> !! <redeemer-field>`, absence of:

```hs
valueOf (txOutValue (txInInfoResolved <var>)) <currency-symbol> <token-name> == 1
valueOf (txOutValue (txInInfoResolved <var>)) <currency-symbol> <token-name> >= 1
```

```hs
assetClassValueOf (txOutValue (txInInfoResolved <var>)) <asset-class> == 1
assetClassValueOf (txOutValue (txInInfoResolved <var>)) <asset-class> >= 1
```

Where `<var>` is the variable bound to the result of the index operation (type TxInInfo).

**Note:** These are examples of how the NFT could be checked, but there could be other ways to do it in Plinth.

## Examples

### Valid cases

```hs
-- Validates NFT presence at input provided by the redeemer
validSettingsUpdate :: Integer -> Integer -> Bool
validSettingsUpdate newFee idx =
    let inputs = txInfoInputs info

        -- Get input at redeemer-provided index
        settingsInput = inputs !! idx
        settingsOutput = txInInfoResolved settingsInput

        -- Important: Verify NFT is present at this input
        hasNFT = assetClassValueOf (txOutValue settingsOutput) settingsNFT == 1

        -- Extract and validate settings datum
        validFee = case txOutDatum settingsOutput of
            OutputDatum (Datum d) ->
                case PlutusTx.fromBuiltinData d of
                    Just (SettingsDatum maxFee) -> newFee <= maxFee
                    Nothing -> False
            _ -> False

    in hasNFT && validFee
```

```hs
validateNFT :: Redeemer -> ScriptContext -> Bool
validateNFT red ctx =
  let
    info = scriptContextTxInfo ctx
    poolInput = txInfoInputs info !! (poolIndex red)
    value = txOutValue (txInInfoResolved poolInput)
  in
    valueOf value poolPolicyId poolNFTName == 1
```

### Invalid cases

```hs
-- Uses redeemer index without validating NFT presence
unsafeSettingsUpdate :: Integer -> Integer -> Bool
unsafeSettingsUpdate newFee idx =
    let inputs = txInfoInputs info

        -- Gets input at redeemer-provided index
        -- but doesn't verify it contains the settings NFT
        settingsInput = inputs !! idx
        settingsOutput = txInInfoResolved settingsInput

        -- Trusts datum from unverified input
        -- Attacker can provide any UTxO at the settings address with malicious datum
        validFee = case txOutDatum settingsOutput of
            OutputDatum (Datum d) ->
                case PlutusTx.fromBuiltinData d of
                    Just (SettingsDatum maxFee) -> newFee <= maxFee
                    Nothing -> False
            _ -> False

    in validFee

-- Selects input only by index from redeemer
validate :: Redeemer -> ScriptContext -> Bool
validate red ctx =
  let
    info = scriptContextTxInfo ctx
    poolInput = txInfoInputs info !! (poolIndex red)
    poolDatum = getDatum poolInput
  in
    validatePoolUpdate poolDatum
```
