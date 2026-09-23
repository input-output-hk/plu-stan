# Missing Stake Validation [SECURITY]

## Description

Validators and minting policies should construct script addresses with a staking credential.
Or validate the staking credential of continuing outputs, in case the UTxO is not being created at that point.

When the protocol locks ADA or creates long-lived UTxOs. Not doing this could cause:

- Loss of staking rewards
- Delegation hijacking
- Off-chain discoverability issues
- Protocol-level economic leakage

## Detection Logic

**Pattern 1 - No calls of addressStakingCredential:**

Output creation at script addresses:

```hs
txOutAddress <var> == scriptHashAddress <var>
```

```hs
case txOutAddress <var> of
    (ScriptCredential scriptHash) _ -> scriptHash
```

Combined with absence of staking credential validation:

```hs
addressStakingCredential (txOutAddress <var>)
```

Where `stakingCredential` extracts and validates the staking part of the address:

```hs
addressStakingCredential (txOutAddress <var>) == Just <expected-credential>
addressStakingCredential (txOutAddress <var>) == Nothing
```

**Pattern 2 - Explicit construction of output address, discarding staking credential:**
The output address is checked only against the script hash, while the staking credential is discarded as a wildcard.

```hs
txOutAddress <var> == Address (ScriptCredential <var>) _
```

instead of pattern matching it:

```hs
txOutAddress <var> == Address (ScriptCredential <var>) Nothing
```

```hs
txOutAddress <var> == Address (ScriptCredential <var>) (Just <expected-credential>)
```

## Examples

### Valid case

```hs
-- Validates staking credential in output
validateOutput :: Address -> Bool
validateOutput expectedAddr =
    let outputs = txInfoOutputs info

        scriptOutput = case find isScriptOutput outputs of
            Just out -> out
            Nothing -> traceError "Script output not found"

        -- Validate payment credential
        validAddress = txOutAddress scriptOutput == expectedAddr

        -- Important: Validate staking credential
        validStaking = case addressStakingCredential (txOutAddress scriptOutput) of
            Nothing -> True  -- No staking credential is acceptable
            _ -> False

    in validAddress && validStaking
```

### Invalid case

```hs
-- Creates output without validating staking credential
validateOutputUnsafe :: Address -> Bool
validateOutputUnsafe expectedAddr =
    let outputs = txInfoOutputs info

        scriptOutput = case find isScriptOutput outputs of
            Just out -> out
            Nothing -> traceError "Script output not found"

        -- Only validates payment credential
        -- Staking credential is never checked
        validAddress = txOutAddress scriptOutput == Address (ScriptCredential ownScriptHash) _

    in validAddress
```
