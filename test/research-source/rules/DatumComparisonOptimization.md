# Datum Comparison Optimization [PERFORMANCE]

## Description

This pattern was identified in Aiken audit findings **ID-302** (Private Audit #03) and **STF-301** (Forwards), where datum equality checks upcast output data to specific type then compare, instead of downcasting expected datum to Data for direct comparison.

When comparing datums for equality, it is more efficient to convert the expected datum to `BuiltinData` (downcast) rather than converting the output datum from `BuiltinData` to a specific type (upcast). The upcast approach requires pattern matching and field extraction, while downcast with direct equality check is faster.

## Detection Logic

**Pattern - Upcast comparison (inefficient):**

```hs
case fromBuiltinData <datum> of
    Just (<constructor> <fields>) -> <field-comparisons>
    Nothing -> False
```

Where:

- `<datum>` is of type `BuiltinData`
- `<constructor>` is a data constructor pattern
- `<fields>` are pattern-matched field variables
- `<field-comparisons>` are individual field equality checks

**Note:** `fromBuiltinData` could also be a call of `unsafeFromBuiltinData`.

**Efficient alternative - Downcast comparison:**

```hs
<datum> == toBuiltinData <expected-datum>
```

Where `<datum>` is of type `BuiltinData`, and `<expected-datum>` is the expected datum value constructed directly.

## Examples

### Valid case

```hs
-- Compares datums directly after downcast
validateDatum :: CustomDatum -> BuiltinData -> Bool
validateDatum expected outputDatum =
    -- Important: Convert expected to BuiltinData and compare directly
    outputDatum == toBuiltinData expected
```

### Invalid case

```hs
-- Upcasts output datum then compares fields individually
validateDatumSlow :: Integer -> Integer -> BuiltinData -> Bool
validateDatumSlow expectedX expectedY outputDatum =
    case fromBuiltinData outputDatum of
        Just (MyDatum x y) ->
            -- Pattern match and field-by-field comparison
            x == expectedX && y == expectedY
        Nothing -> False
```
