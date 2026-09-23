# Unchecked Redeemer [SECURITY]

## Description

Validators must verify redeemers when validation logic depends on other script inputs or when specific operations require specific redeemer types. When validators check for the presence of script inputs without verifying which redeemer was used, or when they ignore redeemer fields that should be validated, attackers can bypass authorization by using unexpected redeemers or manipulating unchecked redeemer data.

## Detection Logic

**Pattern - Script input dependency without redeemer verification:**

Validation depends on other script inputs:

```hs
filter <is-script-input> (txInfoInputs <var>)
any <is-script-input> (txInfoInputs <var>)
```

Where `<is-script-input>` checks if an input is from a script (e.g., checking for `ScriptCredential`).

Absence of redeemer validation:

```hs
lookup <script-purpose> (txInfoRedeemers <var>)
case lookup <script-purpose> (txInfoRedeemers <var>) of
```

Where `<script-purpose>` could be identified by `Spending`, `Minting`, `Rewarding`, or `Certifying` combined with the relevant identifier.

## Examples

### Valid case

```hs
-- Verifies redeemer of other script inputs
validatePoolUpdate :: TxInfo -> Bool
validatePoolUpdate info =
    let scriptInputs = filter isPoolScript (txInfoInputs info)
        hasPoolInput = not (null scriptInputs)
        -- Important: Verify the pool input uses the correct redeemer
        correctRedeemer = case scriptInputs of
            [txIn] -> case lookup (Spending (txInInfoOutRef txIn)) (txInfoRedeemers info) of
                Just (Redeemer r) -> case PlutusTx.fromBuiltinData r of
                    Just PoolUpdate -> True
                    _ -> False
                Nothing -> False
            _ -> False
    in hasPoolInput && correctRedeemer
  where
    isPoolScript txIn =
        case addressCredential (txOutAddress (txInInfoResolved txIn)) of
            ScriptCredential _ -> True
            _ -> False
```

### Invalid case

```hs
-- Checks for script input presence without verifying redeemer
validatePoolUpdateUnsafe :: TxInfo -> Bool
validatePoolUpdateUnsafe info =
    let scriptInputs = filter isPoolScript (txInfoInputs info)
        -- Only checks that a pool script input exists
        -- Doesn't verify which redeemer was used
        -- Attacker can use any redeemer (e.g., Cancel instead of Update)
    in not (null scriptInputs)
  where
    isPoolScript txIn =
        case addressCredential (txOutAddress (txInInfoResolved txIn)) of
            ScriptCredential _ -> True
            _ -> False
```
