# Missing Address Validation [SECURITY]

## Description
Validators and minting policies should explicitly verify the destination address (payment + staking credentials) of:
- continuing outputs
- newly created outputs
- outputs receiving new minted tokens
Selecting outputs only by datum, token presence or value shape should be warned.

### Detection Logic

No calling of `txOutAddress`

## Examples

### Valid case

```hs
-- From AADA: Checks the output address is the same as the interest source
destinationIsToInterestSc :: TxOut -> Bool
destinationIsToInterestSc txo = txOutAddress txo == interestSc

-- From Lending: Retrieves the outputs at ownAddress if there is only one
ownContractOutput :: TxInfo -> Address -> TxOut
ownContractOutput info ownAddress = ownOutput
  where
    txOutputs :: [TxOut]
    !txOutputs = txInfoOutputs info

    ownOutput :: TxOut
    !ownOutput = case [o | o <- txOutputs, ownAddress == txOutAddress o] of
      [o] -> o
      _ -> debugError "E23" True

-- From Lending: Gets the continuing contract output if it is the only output with script datum
getContinuingContractOutput :: TxInfo -> TxOut -> TxOut
getContinuingContractOutput info input = if (txOutAddress contractOutput) == (txOutAddress input)
  then contractOutput
  else debugError "E28" True
  where
    txOutputs :: [TxOut]
    txOutputs = txInfoOutputs info

    contractOutput :: TxOut
    !contractOutput = case [o | o <- txOutputs, scriptDatumExists o] of
      [o] -> o
      _ -> debugError "E18" True
```

### Invalid case

```hs
-- No validation for destination of minted tokens
mkPolicy :: ScriptContext -> Bool
mkPolicy ctx =
    let info = scriptContextTxInfo ctx
        mintedValue = txInfoMint info
        outputs = txInfoOutputs info

        isContinuingOutput = any
            (\o -> mintedValue `leq` txOutValue o)
            outputs

    in isContinuingOutput

-- Selects output based on NFT presence without address check
invalidContinue :: TxInfo -> TxOut
invalidContinue info =
    case [o | o <- txInfoOutputs info, hasNFT o] of
        [o] -> o
        _ -> traceError "E"
```
