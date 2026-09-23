# Precision Loss [CODE-QUALITY]

## Description

Validators performing arithmetic operations with integers must order operations carefully to minimize precision loss. When division occurs before multiplication in integer arithmetic, intermediate rounding can cause significant precision loss and systematic underpayment or overpayment.

One common scenario where this occurs could be interest calculations, where dividing first causes precision loss (`(elapsed / total) * amount`).
The correct approach is to multiply first, then divide: `(elapsed * amount) / total`, which delays rounding until the final step and preserves maximum precision.

## Detection Logic

**Pattern - Division before multiplication:**

```hs
(<expr1> <division> <expr2>) <multiplication> <expr3>

```

**Correct pattern (multiply first, divide last):**

```hs
(<expr1> <multiplication> <expr3>) <division> <expr2>
```

Where:

- `<expr1>`, `<expr2>` and `<expr3>` could be any mathematical expression
- `<division>` could be identified by `/`, `div` or `quot`
- `<multiplication>` could be identified by `*` or `mul`

## Examples

### Valid case

```hs
-- Calculates interest with maximum precision
calculateInterest :: Integer -> Integer -> Integer -> Integer
calculateInterest principal elapsedTime totalTime =
    -- Multiply first, divide last to preserve precision
    (principal * elapsedTime) `div` totalTime
```

### Invalid case 1

```hs
-- From AADA: Division before multiplication loses precision
calculateInterestUnsafe :: Integer -> Integer -> Integer -> Integer
calculateInterestUnsafe principal elapsedTime totalTime =
    (elapsedTime `div` totalTime) * principal
```

### Invalid case 2

```hs
-- Precision loss compounds over multiple calculations
calculateCompoundInterest :: Integer -> [Integer] -> Integer -> Integer
calculateCompoundInterest principal periods totalTime =
    -- Each period calculation loses precision independently
    -- Losses accumulate across all periods
    let periodInterests = map (\period ->
            (period `div` totalTime) * principal  -- Precision loss here
        ) periods
    in sum periodInterests
    -- Should be: sum (map (\period -> (principal * period) `div` totalTime) periods)
```
