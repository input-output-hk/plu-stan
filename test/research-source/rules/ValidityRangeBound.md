# Validity Range Bound [SECURITY]

## Description

Validators that perform temporal checks (deadlines, time-based conditions, interest calculations) must validate the validity range length of the transaction. When validators use `txInfoValidRange` bounds for temporal logic without restricting the range length, attackers can:

- Set artificially long validity ranges to manipulate time-sensitive calculations
- Use validity ranges with past lower bounds to bypass deadline checks

The validity range (`lowerBound`, `upperBound`) defines the time window when a transaction is considered valid. Attackers can manipulate this window by:

- Setting lowerBound far in the past while executing the transaction later
- Setting upperBound far in the future to pass future deadline checks
- Creating very long ranges (e.g., years) to distort duration-based calculations

## Detection Logic

**Missing validation**

Absence of range length constraint of the form:

```hs
(<upper-bound> - <lower-bound>) <= <max-duration>
(<upper-bound> - <lower-bound>) < <max-duration>
```

Where `<upper-bound>` and `<lower-bound>` can be extracted from `ivTo` and `ivFrom`.

For example:

```hs
case ivFrom (txInfoValidRange info) of
    LowerBound (Finite lower) _ -> lower

case ivTo (txInfoValidRange info) of
    UpperBound (Finite upper) _ -> upper
```

## Examples

### Valid cases

```hs
-- Restricts validity range length of the transaction
validDeadlineCheck :: MyDatum -> Bool
validDeadlineCheck MyDatum{deadline} =
    let validRange = txInfoValidRange info

        lowerBound = case ivFrom validRange of
            LowerBound (Finite t) _ -> t
            _ -> traceError "Invalid lower bound"

        upperBound = case ivTo validRange of
            UpperBound (Finite t) _ -> t
            _ -> traceError "Invalid upper bound"

        -- Important: Restrict validity range length
        -- Only allow ranges up to 2 hours (7_200_000 milliseconds)
        maxRangeLength :: POSIXTime
        maxRangeLength = POSIXTime 7_200_000

        rangeLength :: POSIXTime
        rangeLength = upperBound - lowerBound

        now :: POSIXTime
        now = lowerBound

    in rangeLength <= maxRangeLength &&
       now >= deadline
```

```hs
validate :: POSIXTime -> ScriptContext -> Bool
validate deadline ctx =
  let
    range = txInfoValidRange info
  in
    contains (from deadline) range &&
    ivTo range - ivFrom range <= maxAllowedRange
  where
    info = scriptContextTxInfo ctx
```

### Invalid cases

```hs
-- Uses validity bound without restricting range length
unsafeDeadlineCheck :: MyDatum -> Bool
unsafeDeadlineCheck MyDatum{deadline} =
    let validRange = txInfoValidRange info
        (lowerBound, _) = case ivFrom validRange of
            LowerBound (Finite t) c -> (t, c)
            _ -> traceError "Invalid lower bound"

        -- Uses lower bound as "now" without validating range length
        -- Attacker can set lowerBound = deadline while actual execution time is much later
        now :: POSIXTime
        now = lowerBound

    in now >= deadline
```

```hs
validate :: POSIXTime -> ScriptContext -> Bool
validate maturity ctx =
  contains (from maturity) (txInfoValidRange $ scriptContextTxInfo ctx)
```
