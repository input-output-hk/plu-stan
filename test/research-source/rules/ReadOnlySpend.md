# Read Only Spend [PERFORMANCE] [SECURITY]

## Description

Validators should use reference inputs for read-only access to UTxOs instead of spending and recreating them identically. When validators spend UTxOs only to recreate them with the same datum and value at the same address, this causes:

- Unnecessary UTxO congestion and transaction conflicts
- Wasted transaction fees for recreating unchanged UTxOs
- DoS vectors where attackers can block legitimate operations, in this case it could be considered a security issue.

Reference inputs (Plutus V2+) allow validators to read UTxO data without spending them, enabling parallel access. Spending should only occur when the UTxO's datum or value actually changes.

## Detection Logic

**Pattern 1 - Explicit address, datum and value check:**

```hs
txOutAddress         <input> == txOutAddress         <output> &&
txOutDatum           <input> == txOutDatum           <output> &&
txOutValue           <input> == txOutValue           <output> &&
txOutReferenceScript <input> == txOutReferenceScript <output>
```

The validator explictly checks that the address, datum, value and reference script of the UTxO being spent is not changed.

`<input>` could be identified by `txInInfoResolved`, and `<output>` could be identified by `getContinuingOutputs`

It could also be the case that only the datum is explictly checked to be equal to the output:

```hs
let <input-datum> = txOutDatum (txInInfoResolved <var2>)
in case getContinuingOutputs <script-context> of
    <output> -> txOutDatum <output> == <input-datum>
```

There could still be changes in either the value or address of the UTxO, requiring it to be spent. But it is unusual to make changes in the value of a UTxO without reflecting any changes in the datum. A warning could be triggered when this pattern is identified.

**Pattern 2 - All datum fields compared for equality:**

```hs
<field-accessor-1> <input-datum> == <field-accessor-1> <output-datum>
<field-accessor-2> <input-datum> == <field-accessor-2> <output-datum>
...
<field-accessor-n> <input-datum> == <field-accessor-n> <output-datum>
```

Where **all** fields of the datum type are compared for equality between input datum and output datum.

## Examples

### Valid case

```hs
-- Uses reference input for read-only access
validateRefInput :: ScriptContext -> Bool
validateRefInput ctx =
    let info = scriptContextTxInfo ctx
        refInputs = txInfoReferenceInputs info

        -- Access UTxO via reference input (read-only)
        refInput = case find hasNFT refInputs of
            Just input -> input
            Nothing -> traceError "Reference input not found"

        utxo = txInInfoResolved refInput

--- ... validation continues
```

### Invalid case 1

```hs
-- Spends and recreates UTxO with identical datum, value and adress
validateInput :: Integer -> Bool
validateInput newFee =
    let -- Extract the script input being spent
        ownInput = case find isOwnInput (txInfoInputs info) of
            Just input -> input
            Nothing -> traceError "Own input not found"

        ownOutput = txInInfoResolved ownInput
        inputDatum = txOutDatum ownOutput
        inputValue = txOutValue ownOutput
        inputAddress = txOutAddress ownOutput

        validContinuation = case getContinuingOutputs ctx of
            [out] ->
                -- Both datum, value and address are unchanged
                txOutDatum out == inputDatum &&
                txOutValue out == inputValue &&
                txOutAddress out == inputAddress
            _ -> False

    in validContinuation
```

### Invalid case 2

```hs
-- Spends and recreates UTxO with identical datum, value and adress
validateInput2 :: ScriptContext -> Bool
validateInput2 ctx =
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Find oracle input
    oracleIn :: TxOut
    oracleIn =
      case findOracleInput info of
        Just i  -> txInInfoResolved i
        Nothing -> traceError "oracle input missing"

    -- Read oracle datum
    oracleDatum :: Price
    oracleDatum =
      case txOutDatum oracleIn of
        OutputDatum (Datum d) -> unsafeFromBuiltinData d
        _ -> traceError "oracle datum missing"

    -- Find oracle output (continuing)
    oracleOut :: TxOut
    oracleOut =
      case findContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"
  in
    -- Read-only use of oracle datum
    oracleDatum > 0

    -- Recreate oracle UTxO unchanged
    && txOutAddress oracleOut == txOutAddress oracleIn
    && txOutDatum oracleOut   == txOutDatum oracleIn
    && txOutValue oracleOut   == txOutValue oracleIn
```

### Invalid case 3

```hs
-- From Cerra P2P Lending: Spends input to validate unchanged fields
validateMintAccept :: TokenName -> TxInfo -> Bool
validateMintAccept borrowerTokenName info =
    let contractInput = getContractInput info
        scriptOutput = getContractOutput info

        positionDatumIn = mustFindScriptDatum @LendingDatum contractInput info
        lenderNFTIn               = scLenderNFT positionDatumIn
        oracleAddressLoanIn       = scOracleAddressLoan positionDatumIn
        oracleAddressCollateralIn = scOracleAddressCollateral positionDatumIn
        loanAssetIn               = scLoanAsset positionDatumIn
        loanAmountIn              = scLoanAmount positionDatumIn
        collateralAssetIn         = scCollateralAsset positionDatumIn
        collateralAmountIn        = scCollateralAmount positionDatumIn

        positionDatumOut = mustFindScriptDatum @LendingDatum scriptOutput info
        lenderNFTOut               = scLenderNFT positionDatumOut
        oracleAddressLoanOut       = scOracleAddressLoan positionDatumOut
        oracleAddressCollateralOut = scOracleAddressCollateral positionDatumOut
        loanAssetOut               = scLoanAsset positionDatumOut
        loanAmountOut              = scLoanAmount positionDatumOut
        collateralAssetOut         = scCollateralAsset positionDatumOut
        collateralAmountOut        = scCollateralAmount positionDatumOut

     in
        fromJustCustom lenderNFTIn == fromJustCustom lenderNFTOut
        && oracleAddressLoanIn == oracleAddressLoanOut
        && oracleAddressCollateralIn == oracleAddressCollateralOut
        && loanAssetIn == loanAssetOut
        && loanAmountIn == loanAmountOut
        && collateralAssetIn == collateralAssetOut
        && collateralAmountIn == collateralAmountOut
```
