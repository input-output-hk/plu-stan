{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Target.PlutusTx where

import PlutusTx qualified as Tx
import PlutusTx.Builtins.HasOpaque qualified as BI
import PlutusTx.Builtins.Internal qualified as BIInternal
import PlutusTx.Foldable qualified as TxFoldable
import PlutusTx.Maybe qualified as Maybe
import PlutusTx.List qualified as TxList
import PlutusTx.Prelude qualified as P

-- Place for future imports
import PlutusLedgerApi.V1 (Credential (..), POSIXTimeRange, PubKeyHash (..), ScriptHash (..))
import PlutusLedgerApi.V1.Contexts qualified as V1
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V3 (
  Address (..),
  BuiltinData,
  Datum (..),
  OutputDatum (..),
  ScriptContext (..),
  TxInfo (..),
  TxOut (..),
  txOutAddress,
  )
import PlutusLedgerApi.V3.MintValue qualified as MintValue
{-# ANN module ("onchain-contract" :: String) #-}
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
verifyEd25519SignatureUsage :: Bool
verifyEd25519SignatureUsage =
  -- stan-ignore: PLU-STAN-01
  P.verifyEd25519Signature
    (BI.stringToBuiltinByteStringHex "deadbeef")
    (BI.stringToBuiltinByteStringHex "deadbeef")
    (BI.stringToBuiltinByteStringHex "deadbeef")

verifyEcdsaSecp256k1SignatureUsage :: Bool
verifyEcdsaSecp256k1SignatureUsage =
  P.verifyEcdsaSecp256k1Signature
    (BI.stringToBuiltinByteStringHex "deadbeef")
    (BI.stringToBuiltinByteStringHex "deadbeef")
    (BI.stringToBuiltinByteStringHex "deadbeef")

verifySchnorrSecp256k1SignatureUsage :: Bool
verifySchnorrSecp256k1SignatureUsage =
  P.verifySchnorrSecp256k1Signature
    (BI.stringToBuiltinByteStringHex "deadbeef")
    (BI.stringToBuiltinByteStringHex "deadbeef")
    (BI.stringToBuiltinByteStringHex "deadbeef")

unsafeFromBuiltinData :: Integer
unsafeFromBuiltinData =
  Tx.unsafeFromBuiltinData (Tx.toBuiltinData (42 :: Integer))

usageOfPTxMaybe :: Integer
usageOfPTxMaybe = let
  x = Maybe.fromMaybe 0 (Maybe.Just 1)
  in x

pubKeyHashEq :: Bool
pubKeyHashEq = pubKeyHash == pubKeyHash
  where
    pubKeyHash :: PubKeyHash
    pubKeyHash = PubKeyHash (BI.stringToBuiltinByteStringHex "deadbeef")

scriptHashEq :: Bool
scriptHashEq = scriptHash == scriptHash
  where
    scriptHash :: ScriptHash
    scriptHash = ScriptHash (BI.stringToBuiltinByteStringHex "deadbeef")

credentialHashEq :: Bool
credentialHashEq = credentialHash == credentialHash
  where
    credentialHash :: Credential
    credentialHash = PubKeyCredential (PubKeyHash (BI.stringToBuiltinByteStringHex "deadbeef"))

credentialHashLe :: Bool
credentialHashLe = credentialHash < credentialHash
  where
    credentialHash :: Credential
    credentialHash = PubKeyCredential (PubKeyHash (BI.stringToBuiltinByteStringHex "deadbeef"))

hoListFilter :: [Integer]
hoListFilter =
  TxList.filter ((>) 0) [1, 2, 3]

hoFoldableLength :: Integer
hoFoldableLength =
  TxFoldable.length ([1, 2, 3] :: [Integer])

nestedMapFilter :: [Integer]
nestedMapFilter =
  let exList = [1, 2, 3, 4]
  in TxList.map ((+) 100) $ TxList.filter ((>) 0) exList

guardedLog2 :: Integer -> Integer
guardedLog2 n
  | n == 0 = 0
  | n == 1 = 1
  | n `mod` 2 == 0 = 1 + guardedLog2 (n `div` 2)
  | otherwise = n

nonStrictLetTwice :: Integer
nonStrictLetTwice =
  let tup = (1, 2)
  in fst tup + snd tup

strictLetTwice :: Integer
strictLetTwice =
  let !tup = (1, 2)
  in fst tup + snd tup

nonStrictLetUsedInBindings :: Integer
nonStrictLetUsedInBindings =
  let tup = (1, 2)
      a = fst tup
      b = snd tup
  in a + b

nonStrictLetUsedInBindingAndBody :: Integer
nonStrictLetUsedInBindingAndBody =
  let tup = (1, 2)
      a = fst tup
  in a + snd tup

valueOfEqInput :: Bool
valueOfEqInput =
  Value.valueOf inputValue adaCS adaToken == 5000
  where
    inputValue :: Value.Value
    inputValue = Value.singleton adaCS adaToken 5000
    adaCS :: Value.CurrencySymbol
    adaCS = Value.adaSymbol
    adaToken :: Value.TokenName
    adaToken = Value.adaToken

valueOfEqLet :: Bool
valueOfEqLet =
  let adaAmountInput = Value.valueOf inputValue adaCS adaToken
      adaAmountOutput = Value.valueOf outputValue adaCS adaToken
  in adaAmountInput == adaAmountOutput
  where
    inputValue :: Value.Value
    inputValue = Value.singleton adaCS adaToken 5000
    outputValue :: Value.Value
    outputValue = Value.singleton adaCS adaToken 5000
    adaCS :: Value.CurrencySymbol
    adaCS = Value.adaSymbol
    adaToken :: Value.TokenName
    adaToken = Value.adaToken

valueOfEqReversed :: Bool
valueOfEqReversed =
  5000 == Value.valueOf inputValue adaCS adaToken
  where
    inputValue :: Value.Value
    inputValue = Value.singleton adaCS adaToken 5000
    adaCS :: Value.CurrencySymbol
    adaCS = Value.adaSymbol
    adaToken :: Value.TokenName
    adaToken = Value.adaToken

valueOfEqPrefix :: Bool
valueOfEqPrefix =
  (==) (Value.valueOf inputValue adaCS adaToken) 5000
  where
    inputValue :: Value.Value
    inputValue = Value.singleton adaCS adaToken 5000
    adaCS :: Value.CurrencySymbol
    adaCS = Value.adaSymbol
    adaToken :: Value.TokenName
    adaToken = Value.adaToken

valueOfEqSection :: Bool
valueOfEqSection =
  (== 5000) (Value.valueOf inputValue adaCS adaToken)
  where
    inputValue :: Value.Value
    inputValue = Value.singleton adaCS adaToken 5000
    adaCS :: Value.CurrencySymbol
    adaCS = Value.adaSymbol
    adaToken :: Value.TokenName
    adaToken = Value.adaToken

currencySymbolValueOfMintedValue :: V1.ScriptContext -> Bool
currencySymbolValueOfMintedValue ctx =
  let cs = Value.adaSymbol
  in Value.currencySymbolValueOf (V1.txInfoMint $ V1.scriptContextTxInfo ctx) cs < 0

unvalidatedPubKeyHashEqFromBuiltinData :: Bool
unvalidatedPubKeyHashEqFromBuiltinData =
  Tx.unsafeFromBuiltinData (BIInternal.mkB (BI.stringToBuiltinByteStringHex "deadbeef")) == pubKeyHash
  where
    pubKeyHash :: PubKeyHash
    pubKeyHash = PubKeyHash (BI.stringToBuiltinByteStringHex "deadbeef")

unvalidatedScriptHashEqFromBuiltinData :: Bool
unvalidatedScriptHashEqFromBuiltinData =
  Tx.unsafeFromBuiltinData (BIInternal.mkB (BI.stringToBuiltinByteStringHex "deadbeef")) == scriptHash
  where
    scriptHash :: ScriptHash
    scriptHash = ScriptHash (BI.stringToBuiltinByteStringHex "deadbeef")


-- | Realistic lending validator example demonstrating PLU-STAN-10 vulnerability.
-- This mimics the pattern from RULES.md where:
-- 1. A LoanDatum contains a repaymentPkh (PubKeyHash for repayment)
-- 2. The datum is deserialized using unsafeFromBuiltinData
-- 3. The repaymentPkh is compared without validating ledger invariants
--
-- The vulnerability: an attacker can set repaymentPkh to an invalid PubKeyHash
-- (e.g., wrong length) that will never match any real pubKeyHash, making the loan
-- impossible to repay.
data LoanDatum = LoanDatum
  { repaymentPkh :: PubKeyHash
  , loanAmount :: Integer
  }

Tx.makeIsDataIndexed ''LoanDatum [('LoanDatum, 0)]

lendingValidatorCreateLoan :: ScriptContext -> Bool
lendingValidatorCreateLoan ctx =
  let ownOutput = head $ txInfoOutputs $ scriptContextTxInfo ctx
      datumData = getDatumData $ txOutDatum ownOutput
      outputPkh = getOutputPkh (txOutAddress ownOutput)
      -- VULNERABILITY: The comparison directly uses unsafeFromBuiltinData result
      -- without validating ledger invariants. The repaymentPkh could be an
      -- invalid PubKeyHash (wrong length) that will never match any real address,
      -- making the loan impossible to repay.
  in repaymentPkh (Tx.unsafeFromBuiltinData datumData) == outputPkh
       && Value.valueOf (txOutValue ownOutput) Value.adaSymbol Value.adaToken
            >= loanAmount (Tx.unsafeFromBuiltinData datumData)
  where
    getDatumData :: OutputDatum -> BuiltinData
    getDatumData (OutputDatum (Datum d)) = d
    getDatumData _ = error "expected inline datum"

    txOutValue :: TxOut -> Value.Value
    txOutValue (TxOut _ v _ _) = v

    getOutputPkh :: Address -> PubKeyHash
    getOutputPkh (Address (PubKeyCredential pkh) _) = pkh
    getOutputPkh _ = error "expected pubkey address"

-- Test that it still triggers on the same pattern, but with a more idiomatic pattern matching.
lendingValidatorCreateLoan_2 :: ScriptContext -> Bool
lendingValidatorCreateLoan_2 ctx =
  let ownOutput = head $ txInfoOutputs $ scriptContextTxInfo ctx
      LoanDatum { repaymentPkh = mustRepayToPkh, loanAmount = amountMustBeRepaid } = Tx.unsafeFromBuiltinData $ getDatumData $ txOutDatum ownOutput
      outputPkh = getOutputPkh (txOutAddress ownOutput)
      -- VULNERABILITY: The comparison directly uses unsafeFromBuiltinData result
      -- without validating ledger invariants. The repaymentPkh could be an
      -- invalid PubKeyHash (wrong length) that will never match any real address,
      -- making the loan impossible to repay.
  in mustRepayToPkh == outputPkh
       && Value.valueOf (txOutValue ownOutput) Value.adaSymbol Value.adaToken
            >= amountMustBeRepaid
  where
    getDatumData :: OutputDatum -> BuiltinData
    getDatumData (OutputDatum (Datum d)) = d
    getDatumData _ = error "expected inline datum"

    txOutValue :: TxOut -> Value.Value
    txOutValue (TxOut _ v _ _) = v

    getOutputPkh :: Address -> PubKeyHash
    getOutputPkh (Address (PubKeyCredential pkh) _) = pkh
    getOutputPkh _ = error "expected pubkey address"

-- Test case 1: Intermediate variable binding
-- The datum is bound first, then pattern matched separately
-- PLU-STAN-10 might NOT trigger because "unsafeFromBuiltinData" isn't in repaymentPkh's binding span
lendingValidatorIntermediateBinding :: ScriptContext -> Bool
lendingValidatorIntermediateBinding ctx =
  let ownOutput = head $ txInfoOutputs $ scriptContextTxInfo ctx
      datum = Tx.unsafeFromBuiltinData $ getDatumData $ txOutDatum ownOutput
      LoanDatum { repaymentPkh = mustRepayToPkh, loanAmount = amountMustBeRepaid } = datum
      outputPkh = getOutputPkh (txOutAddress ownOutput)
  in mustRepayToPkh == outputPkh
       && Value.valueOf (txOutValue ownOutput) Value.adaSymbol Value.adaToken
            >= amountMustBeRepaid
  where
    getDatumData :: OutputDatum -> BuiltinData
    getDatumData (OutputDatum (Datum d)) = d
    getDatumData _ = error "expected inline datum"

    txOutValue :: TxOut -> Value.Value
    txOutValue (TxOut _ v _ _) = v

    getOutputPkh :: Address -> PubKeyHash
    getOutputPkh (Address (PubKeyCredential pkh) _) = pkh
    getOutputPkh _ = error "expected pubkey address"

-- Test case 2: Case expression pattern
-- PLU-STAN-10 might NOT trigger because case bindings use MatchBind context
lendingValidatorCasePattern :: ScriptContext -> Bool
lendingValidatorCasePattern ctx =
  let ownOutput = head $ txInfoOutputs $ scriptContextTxInfo ctx
      outputPkh = getOutputPkh (txOutAddress ownOutput)
  in case Tx.unsafeFromBuiltinData $ getDatumData $ txOutDatum ownOutput of
       LoanDatum { repaymentPkh = mustRepayToPkh, loanAmount = amountMustBeRepaid } ->
         mustRepayToPkh == outputPkh
           && Value.valueOf (txOutValue ownOutput) Value.adaSymbol Value.adaToken
                >= amountMustBeRepaid
  where
    getDatumData :: OutputDatum -> BuiltinData
    getDatumData (OutputDatum (Datum d)) = d
    getDatumData _ = error "expected inline datum"

    txOutValue :: TxOut -> Value.Value
    txOutValue (TxOut _ v _ _) = v

    getOutputPkh :: Address -> PubKeyHash
    getOutputPkh (Address (PubKeyCredential pkh) _) = pkh
    getOutputPkh _ = error "expected pubkey address"

-- Test case 3: Record field accessor function
-- PLU-STAN-10 might NOT trigger because repaymentPkh is used as a function, not a bound variable
lendingValidatorFieldAccessor :: ScriptContext -> Bool
lendingValidatorFieldAccessor ctx =
  let ownOutput = head $ txInfoOutputs $ scriptContextTxInfo ctx
      datum = Tx.unsafeFromBuiltinData $ getDatumData $ txOutDatum ownOutput
      outputPkh = getOutputPkh (txOutAddress ownOutput)
  in repaymentPkh datum == outputPkh
       && Value.valueOf (txOutValue ownOutput) Value.adaSymbol Value.adaToken
            >= loanAmount datum
  where
    getDatumData :: OutputDatum -> BuiltinData
    getDatumData (OutputDatum (Datum d)) = d
    getDatumData _ = error "expected inline datum"

    txOutValue :: TxOut -> Value.Value
    txOutValue (TxOut _ v _ _) = v

    getOutputPkh :: Address -> PubKeyHash
    getOutputPkh (Address (PubKeyCredential pkh) _) = pkh
    getOutputPkh _ = error "expected pubkey address"

-- Test case 5: Where clause binding
-- PLU-STAN-10 should trigger if where bindings are handled like let bindings
lendingValidatorWhereClause :: ScriptContext -> Bool
lendingValidatorWhereClause ctx = mustRepayToPkh == outputPkh
       && Value.valueOf (txOutValue ownOutput) Value.adaSymbol Value.adaToken
            >= amountMustBeRepaid
  where
    ownOutput = head $ txInfoOutputs $ scriptContextTxInfo ctx
    LoanDatum { repaymentPkh = mustRepayToPkh, loanAmount = amountMustBeRepaid } =
      Tx.unsafeFromBuiltinData $ getDatumData $ txOutDatum ownOutput
    outputPkh = getOutputPkh (txOutAddress ownOutput)

    getDatumData :: OutputDatum -> BuiltinData
    getDatumData (OutputDatum (Datum d)) = d
    getDatumData _ = error "expected inline datum"

    txOutValue :: TxOut -> Value.Value
    txOutValue (TxOut _ v _ _) = v

    getOutputPkh :: Address -> PubKeyHash
    getOutputPkh (Address (PubKeyCredential pkh) _) = pkh
    getOutputPkh _ = error "expected pubkey address"

mintCheckViaHelperLet1 :: V1.ScriptContext -> Bool
mintCheckViaHelperLet1 ctx =
  let mint = V1.txInfoMint $ V1.scriptContextTxInfo ctx
      sym = Value.adaSymbol
  in checkBurn1 mint sym

checkBurn1 :: Value.Value -> Value.CurrencySymbol -> Bool
checkBurn1 x y = Value.currencySymbolValueOf x y < 0

mintCheckViaHelperLet2 :: V1.ScriptContext -> Bool
mintCheckViaHelperLet2 ctx =
  let mint = V1.txInfoMint $ V1.scriptContextTxInfo ctx
      sym = Value.adaSymbol
  in checkBurn2 mint sym

checkBurn2 :: Value.Value -> Value.CurrencySymbol -> Bool
checkBurn2 a b = Value.currencySymbolValueOf a b < 0

nonMintedCurrencySymbolValueOf :: Bool
nonMintedCurrencySymbolValueOf =
  let val = Value.singleton Value.adaSymbol Value.adaToken 1
  in Value.currencySymbolValueOf val Value.adaSymbol > 0

validRangeUtilityFrom :: V1.ScriptContext -> Bool
validRangeUtilityFrom ctx =
  Interval.contains
    (Interval.from 10)
    (V1.txInfoValidRange $ V1.scriptContextTxInfo ctx)

validRangeUtilityAlways :: V1.ScriptContext -> Bool
validRangeUtilityAlways ctx =
  Interval.contains Interval.always (V1.txInfoValidRange $ V1.scriptContextTxInfo ctx)

validRangeNoFiniteCheck :: V1.ScriptContext -> POSIXTimeRange
validRangeNoFiniteCheck ctx =
  V1.txInfoValidRange $ V1.scriptContextTxInfo ctx

validRangeFiniteCheck :: V1.ScriptContext -> Bool
validRangeFiniteCheck ctx =
  case Interval.lowerBound (V1.txInfoValidRange $ V1.scriptContextTxInfo ctx) of
    Interval.LowerBound (Interval.Finite _) _ -> True
    _ -> False
    
_plutusTxFixtureBindings :: [()]
_plutusTxFixtureBindings =
  [ validRangeUtilityFrom `seq` ()
  , validRangeUtilityAlways `seq` ()
  , validRangeNoFiniteCheck `seq` ()
  , validRangeFiniteCheck `seq` ()
  , currencySymbolValueOfMintedValueRewrapped `seq` ()
  , currencySymbolValueOfMintedValueToMap `seq` ()
  , currencySymbolValueOfMintedValueLetPattern `seq` ()
  , mintCheckViaHelperLet1 `seq` ()
  , checkBurn1 `seq` ()
  , mintCheckViaHelperLet2 `seq` ()
  , checkBurn2 `seq` ()
  , nonMintedCurrencySymbolValueOf `seq` ()
  ]

currencySymbolValueOfMintedValueRewrapped :: ScriptContext -> Bool
currencySymbolValueOfMintedValueRewrapped ctx =
  let mintValue = txInfoMint $ scriptContextTxInfo ctx
      mintMap = case mintValue of
        MintValue.UnsafeMintValue valueMap -> valueMap
      rewrapped = Value.Value mintMap
  in Value.currencySymbolValueOf rewrapped Value.adaSymbol < 0

currencySymbolValueOfMintedValueToMap :: ScriptContext -> Bool
currencySymbolValueOfMintedValueToMap ctx =
  let mintValue = txInfoMint $ scriptContextTxInfo ctx
      mintMap = MintValue.mintValueToMap mintValue
      rewrapped = Value.Value mintMap
  in Value.currencySymbolValueOf rewrapped Value.adaSymbol < 0

currencySymbolValueOfMintedValueLetPattern :: ScriptContext -> Bool
currencySymbolValueOfMintedValueLetPattern ctx =
  let mintValue = txInfoMint $ scriptContextTxInfo ctx
      MintValue.UnsafeMintValue mintMap = mintValue
      rewrapped = Value.Value mintMap
  in Value.currencySymbolValueOf rewrapped Value.adaSymbol < 0

precisionLossValid :: Integer -> Integer -> Integer -> Integer
precisionLossValid principal elapsedTime totalTime =
  (principal * elapsedTime) `div` totalTime

precisionLossInvalid1 :: Integer -> Integer -> Integer -> Integer
precisionLossInvalid1 principal elapsedTime totalTime =
  elapsedTime `div` totalTime * principal

precisionLossInvalid2 :: Integer -> [Integer] -> Integer -> Integer
precisionLossInvalid2 principal periods totalTime =
  let periodInterests =
        map (\period -> period `div` totalTime * principal) periods
  in sum periodInterests

precisionLossLetBlock :: Integer -> Integer -> Integer -> Integer
precisionLossLetBlock a b c =
  let divResult = a `div` b
      multResult = divResult * c
  in multResult

precisionLossNestedLet :: Integer -> Integer -> Integer -> Integer
precisionLossNestedLet a b c =
  let divResult = a `div` b
  in let multResult = divResult * c
     in multResult

precisionLossWhere :: Integer -> Integer -> Integer -> Integer
precisionLossWhere a b c = multResult
  where
    divResult = a `div` b
    multResult = divResult * c
