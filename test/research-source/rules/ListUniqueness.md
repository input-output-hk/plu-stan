# List Uniqueness [SECURITY]

## Description

Validators should validate that lists of identity types contain unique elements when duplicates would compromise security or protocol invariants. When lists allow duplicate entries for credentials or identifiers, attackers can:

- Amplify voting power or authorization weight by including the same credential multiple times
- Increase risk if a compromised key appears multiple times in authorization lists

## Detection Logic

### Missing validation

Absence of uniqueness check of the form:

```hs
<list> == nub <list>
<list> == nubBy <func> <list>
```

Where `nub` removes duplicate elements using `(==)`, and `nubBy` uses a custom comparison function.

And `<list>` contains identity types such as `PubKeyHash`, `ValidatorHash`, `Address`, or `Credential`.

The check could also be in the form of:

```hs
length <list> == length (nub <list>)
length <list> == length (nubBy <func> <list>)

```

although it would be more performant to directly compare the lists.

## Examples

### Valid cases

```hs
validate :: [PubKeyHash] -> Bool
validate signers =
  signers == nub signers
```

```hs
validSignersUpdate :: [PubKeyHash] -> Bool
validSignersUpdate newSigners =
    let hasMinSigners = length newSigners >= 3

        -- Validate no duplicate signers
        noDuplicates = length newSigners == length (nub newSigners)

        -- Validate all new signers are present
        allSigned = all (\pkh -> elem pkh (txInfoSignatories info)) newSigners

    in hasMinSigners && noDuplicates && allSigned
```

### Invalid case

```hs
validate :: [PubKeyHash] -> ScriptContext -> Bool
validate signers ctx =
  length signers >= 3 &&
  all (`elem` txInfoSignatories info) signers
  where
    info = scriptContextTxInfo ctx

```

```hs
-- No uniqueness validation on signers list
unsafeSignersUpdate :: [PubKeyHash] -> Bool
unsafeSignersUpdate newSigners =
    let hasMinSigners = length newSigners >= 3

        -- Validates all new signers are present,
        -- but doesn't check if any of them is counted twice
        allSigned = all (\pkh -> elem pkh (txInfoSignatories info)) newSigners

    in hasMinSigners && allSigned
```
