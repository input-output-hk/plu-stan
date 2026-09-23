# Zip Without Length Check [CODE-QUALITY] [SECURITY]

## Description

This pattern was identified in Plinth audit finding **TRS-103** (Treasury Contracts).

Every time a zip is used, length from both lists should be validated to be the same.

## Detection logic

```hs
zip <varA> <varB>
```

And absence of length call on `<varA>` and `<varB>`.

## Examples

### Valid example

```hs
validatePayouts :: [Payout] -> [Bool] -> Bool
validatePayouts payouts statuses =
  length payouts == length statuses &&
  all (\(p,s) -> check p s) (zip payouts statuses)
```

### Invalid example

```hs
validatePayouts :: [Payout] -> [Bool] -> Bool
validatePayouts payouts statuses =
  all (\(p,s) -> check p s) (zip payouts statuses)
```
