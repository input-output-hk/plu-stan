# Double Satisfaction [SECURITY]

## Description

Validators must ensure that value payments are uniquely attributed to specific operations when aggregating across multiple inputs or outputs. When validators filter outputs by address without verifying datum uniqueness, attackers can batch multiple operations expecting payment to the same address and satisfy all with a single payment.

**Note:** For datum uniqueness checks to be effective, the datum must contain a unique identifier (such as a `TxOutRef`) that distinguishes each operation.

## Detection Logic

**Pattern - Value aggregation by address without datum uniqueness:**
The pattern is filtering outputs by address and aggregating their values.

```hs
<aggregate-function> (map (\<var> -> txOutValue <var>)
                     ((filter (\<var> -> txOutAddress <var> == <target-address>) (txInfoOutputs <var>))))
```

or, using list comprehension:

```hs
<aggregate-function> [ txOutvalue <var>
                     | <var> <- (txInfoOutputs <var>)
                     , txOutAddress <var> == <target-address>
                     ]
```

Where:

- `<aggregate-function>` could be either `mconcat`, `sum`, or any function that aggregates lists of monoids.
- `<target-address>` could be identified by `pubKeyHashAddress`, `scriptHashAddress`, or direct `Address` values

And absence of datum uniqueness check on filtered outputs:

```hs
length <filtered-outputs> == length (nub (map txOutDatum <filtered-outputs>))
```

or:

```hs
map txOutDatum <filtered-outputs> ==  nub (map txOutDatum <filtered-outputs>)
```

Where `<filtered-outputs>` is the result of filtering outputs by address. This check ensures that each output has a unique datum, preventing multiple script inputs from being satisfied by the same output.

## Examples

### Valid case

```hs
-- Prevents double satisfaction by checking datum uniqueness
validLoanPayment :: LoanDatum -> TxInfo -> Bool
validLoanPayment LoanDatum{borrower} info =
    let borrowerAddress = pubKeyHashAddress borrower Nothing
        borrowerOutputs = filter (\out -> txOutAddress out == borrowerAddress)
                                 (txInfoOutputs info)
        totalPaid = sum $ map (\out -> txOutValue out) borrowerOutputs
        validAmount = totalPaid >= loanAmount
        -- Important: Verify datum uniqueness to prevent double satisfaction
        -- The datum should have some unique information (such as TxOutRef) for this to work
        outputDatums = map txOutDatum borrowerOutputs
        noDuplicateDatums = length outputDatums == length (nub outputDatums)
    in validAmount && noDuplicateDatums
```

### Invalid case

```hs
-- Allows double satisfaction through duplicate payments
unsafeLoanPayment :: LoanDatum -> TxInfo -> Bool
unsafeLoanPayment LoanDatum{borrower} info =
    let borrowerAddress = pubKeyHashAddress borrower Nothing
        borrowerOutputs = filter (\out -> txOutAddress out == borrowerAddress)
                                 (txInfoOutputs info)
        totalPaid = sum $ map (\out -> txOutValue out) borrowerOutputs
        -- Only validates amount, doesn't check datum uniqueness
        -- Attacker can batch multiple loan requests from the same borrower and satisfy all with a single payment
    in totalPaid >= loanAmount
```
