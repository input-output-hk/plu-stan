# Immutable Credential [CODE-QUALITY] [SECURITY]

## Description

Validators should store credentials (admin keys, staking keys, or authorized signers) in mutable datum fields rather than hardcoded in script parameters or addresses. When credentials are hardcoded, they cannot be rotated or updated if compromised, lost, or when governance changes are needed. If a credential is critical to security and is embedded in script parameters, compiled into the validator, or not changeable through datum, then compromise, key loss, or protocol evolution cannot be handled on-chain.

While some protocols may intentionally use immutable credentials for specific security properties, mutable credential storage is generally recommended for operational flexibility.

## Detection Logic

**Pattern 1 - Credentials hardcoded as top-level constants:**

```hs
<var> :: <credential-type>
<var> = ...
```

Where `<credential-type>` is `PubKeyHash`, `ValidatorHash`, `StakingCredential`, `Credential`, or `Address`.

**Pattern 2 - Credentials applied as immutable parameters:**

Credentials applied to compiled validators via `PlutusTx.applyCode`:

```hs
$$(PlutusTx.compile [|| mkValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode <credential>
```

Where `<credential>` could be of type `PubKeyHash`, `ValidatorHash`, `StakingCredential`, `Credential`, or `Address`.

## Examples

### Valid case

```hs
-- Credential stored in mutable datum
data ProtocolDatum = ProtocolDatum
    { stakingCred :: StakingCredential
    , adminKey :: PubKeyHash
    }

mkValidator :: ProtocolDatum -> () -> ScriptContext -> Bool
mkValidator datum _ ctx =
    ownStakingCredential ctx == Just (stakingCred datum)
```

### Invalid case

```hs
-- Pattern 1: Staking credential hardcoded as top-level constant
adminKey :: PubKeyHash
adminKey = "a1b2c3d4..."  -- Immutable, cannot be changed

stakeCred :: StakingCredential
stakeCred = StakingHash (PubKeyCredential adminKey)

mkValidator :: ScriptContext -> Bool
mkValidator ctx =
    ownStakingCredential ctx == Just stakeCred  -- Uses hardcoded credential

-- Pattern 2: Credential applied as immutable parameter via applyCode
mkValidator :: PubKeyHash -> ProtocolDatum -> ProtocolRedeemer -> ScriptContext -> Bool
mkValidator adminKey datum redeemer ctx =
    case redeemer of
        AdminAction ->
            traceIfFalse "Not signed by admin" $
                txSignedBy (scriptContextTxInfo ctx) adminKey
        _ -> True

validator :: PubKeyHash -> Validator
validator adminKey = mkValidatorScript
    $$(PlutusTx.compile [|| mkValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode adminKey  -- Admin key baked into script
```
