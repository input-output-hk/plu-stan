# UnstableMakeIsData [SECURITY]

## Description

This pattern was identified in Plinth audit finding **MSW-B01** (MuesliSwap).

Using `unstableMakeIsData` to derive `ToData` and `FromData` instances assigns constructor indices based on declaration order in the source code. When constructors are reordered, added, or removed in future code changes, the on-chain serialization format changes, breaking compatibility with already-deployed contracts that expect the original constructor indices.

**Recommendation:** Use `makeIsDataIndexed` instead, which allows explicit specification of constructor indices that remain stable across code changes.

## Detection Logic

**Pattern - Unstable constructor indexing:**

```hs
PlutusTx.unstableMakeIsData ''<type>
```

Where `<type>` is any custom data type.

**Recommended alternative:**

```hs
PlutusTx.makeIsDataIndexed ''<type>
    [ ('<constructor1>', 0)
    , ('<constructorN>', N)
    ]
```

Where constructor indices are explicitly specified and remain stable.

## Examples

### Valid case

```hs
-- Explicitly specifies constructor indices
data MyRedeemer
    = Action1
    | Action2
    | Action3

PlutusTx.makeIsDataIndexed ''MyRedeemer
    [ ('Action1, 0)
    , ('Action2, 1)
    , ('Action3, 2)
    ]
```

### Invalid case

```hs
-- Uses unstable automatic indexing
data MyRedeemer
    = Action1  -- Implicitly gets index 0
    | Action2  -- Implicitly gets index 1
    | Action3  -- Implicitly gets index 2

PlutusTx.unstableMakeIsData ''MyRedeemer
```
