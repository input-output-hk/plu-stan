# Partial Unvalidated Datum [SECURITY]

## Description

Validators must validate all fields when creating or updating datum outputs at script addresses. When validators only validate some datum fields while ignoring others, attackers can inject malicious values into unchecked fields which might be unrealistic, invalid or even break invariants from the protocol.

This differs from [UNVALIDATED-DATUM] where no validation occurs. Here, some fields are validated but others are left unchecked, creating partial validation gaps.

## Detection Logic

**Pattern - Partial datum field validation:**

Presence of datum extraction and SOME field validation:

```hs
<field-accessor-1> <datum> <comparison> <var> &&
<field-accessor-2> <datum> <comparison> <var>
```

Where only a subset of datum fields are validated (not all fields of the datum type appear in validation logic).

`<datum>` could be identified by calling `txOutDatum` on an output, followed by `PlutusTx.fromBuiltinData`.

## Examples

### Valid case

```hs
-- Validates all critical datum fields on creation
data PoolDatum = PoolDatum
    { fee       :: Integer
    , reserveA  :: Integer
    , reserveB  :: Integer
    , timestamp :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''PoolDatum

validatePoolCreation :: Bool
validatePoolCreation =
    let poolOutput = case find isPoolOutput (txInfoOutputs info) of
            Just out -> out
            Nothing -> traceError "Pool output not found"

        validDatum = case txOutDatum poolOutput of
            OutputDatum (Datum d) ->
                case PlutusTx.fromBuiltinData d of
                    Just (PoolDatum fee reserveA reserveB timestamp) ->
                        -- Validate all fields
                        fee >= 0 && fee <= 10000 &&
                        reserveA > 0 &&
                        reserveB > 0 &&
                        timestamp > 0
                    Nothing -> False
            _ -> False

    in validDatum
```

### Invalid case 1

```hs
-- Only validates some datum fields, leaves others unchecked
data PoolDatum = PoolDatum
    { fee       :: Integer
    , reserveA  :: Integer
    , reserveB  :: Integer
    , timestamp :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''PoolDatum

validatePoolCreationUnsafe :: Bool
validatePoolCreationUnsafe =
    let poolOutput = case find isPoolOutput (txInfoOutputs info) of
            Just out -> out
            Nothing -> traceError "Pool output not found"

        validDatum = case txOutDatum poolOutput of
            OutputDatum (Datum d) ->
                case PlutusTx.fromBuiltinData d of
                    Just (PoolDatum fee reserveA reserveB timestamp) ->
                        -- Only validates fee is present
                        -- reserveA, reserveB, timestamp are never validated
                        -- Attacker can set reserves to 0 or negative,
                        -- and timestamp to an arbitrary value
                        fee >= 0 && fee <= 10000
                    Nothing -> False
            _ -> False

    in validDatum
```
