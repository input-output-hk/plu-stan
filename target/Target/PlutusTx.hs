{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}

module Target.PlutusTx where

import PlutusTx qualified as Tx
import PlutusTx.Builtins.HasOpaque qualified as BI
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Foldable qualified as TxFoldable
import PlutusTx.Maybe qualified as Maybe
import PlutusTx.List qualified as TxList
import PlutusTx.Prelude qualified as P

-- Place for future imports
import PlutusLedgerApi.V1 (Credential (..), POSIXTime, POSIXTimeRange, PubKeyHash (..), ScriptHash (..))
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
  txOutAddress, getRedeemer,
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
  Tx.unsafeFromBuiltinData (BI.mkB (BI.stringToBuiltinByteStringHex "deadbeef")) == pubKeyHash
  where
    pubKeyHash :: PubKeyHash
    pubKeyHash = PubKeyHash (BI.stringToBuiltinByteStringHex "deadbeef")

unvalidatedScriptHashEqFromBuiltinData :: Bool
unvalidatedScriptHashEqFromBuiltinData =
  Tx.unsafeFromBuiltinData (BI.mkB (BI.stringToBuiltinByteStringHex "deadbeef")) == scriptHash
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

strictBangArgMultiUseWhere :: Integer
strictBangArgMultiUseWhere = go ([1, 2, 3] :: [Integer])
  where
    go :: [Integer] -> Integer
    go !xs = TxFoldable.length xs + TxFoldable.length xs

multiUseLazyArgBindingWhere :: Integer
multiUseLazyArgBindingWhere = go ([1, 2, 3] :: [Integer])
  where
    go :: [Integer] -> Integer
    go xs = TxFoldable.length xs + TxFoldable.length xs

pluStan08OracleLikeCredStrictLet :: Integer
pluStan08OracleLikeCredStrictLet = go 1
  where
    go :: Integer -> Integer
    go x =
      let !cred = (x, x + 1)
      in if fst cred == 1 then fst cred else snd cred

pluStan08OracleLikeCredLazyLet :: Integer
pluStan08OracleLikeCredLazyLet = go 1
  where
    go :: Integer -> Integer
    go !x =
      let !cred = (x, x + 1)
      in if fst cred == 1 then fst cred else snd cred

{-# INLINE plutStan08GetOracle #-}
plutStan08GetOracle ::
  BI.BuiltinByteString
  -- oracle minting currency symbol
  -> BuiltinData 
  -> BI.BuiltinList BuiltinData
  -> BuiltinData 
plutStan08GetOracle b_oracleSym tref = go
  where
    go :: BI.BuiltinList BuiltinData -> BuiltinData
    go l =
      let !infoFields = BI.snd $ BI.unsafeDataAsConstr (BI.head l) -- error if l is null
          tInfoRef = BI.head infoFields
          r_txout = BI.head $ BI.tail infoFields
          !txout_fields = BI.snd $ BI.unsafeDataAsConstr r_txout
      in BI.ifThenElse (BI.equalsData tInfoRef tref)
         (\_ ->
             let r_v = BI.head $ BI.tail txout_fields
                 r_addr = BI.head txout_fields
                 !cred = BI.unsafeDataAsConstr $ BI.head $ BI.snd $ BI.unsafeDataAsConstr r_addr
             in BI.ifThenElse (BI.equalsInteger (BI.fst cred) 1)
                (\_ -> BI.head $ BI.snd cred)
                (\_ -> error "retrieveOracleHash: only one script input expected or NFT token missing !!!")
                BI.unitval
         )
         (\_ -> go (BI.tail l))
         BI.unitval

{-# INLINE plutStan08GetOracleTrigger #-}
plutStan08GetOracleTrigger ::
  BI.BuiltinByteString
  -- oracle minting currency symbol
  -> BuiltinData 
  -> BI.BuiltinList BuiltinData
  -> BuiltinData 
plutStan08GetOracleTrigger b_oracleSym tref = go
  where
    go :: BI.BuiltinList BuiltinData -> BuiltinData
    go l =
      let !infoFields = BI.snd $ BI.unsafeDataAsConstr (BI.head l) -- error if l is null
          tInfoRef = BI.head infoFields
          r_txout = BI.head $ BI.tail infoFields
          !txout_fields = BI.snd $ BI.unsafeDataAsConstr r_txout
      in BI.ifThenElse (BI.equalsData tInfoRef tref)
         (\_ ->
             let r_v = BI.head $ BI.tail txout_fields
                 r_addr = BI.head txout_fields
                 cred = BI.unsafeDataAsConstr $ BI.head $ BI.snd $ BI.unsafeDataAsConstr r_addr
             in BI.ifThenElse (BI.equalsInteger (BI.fst cred) 1)
                (\_ -> BI.head $ BI.snd cred)
                (\_ -> error "retrieveOracleHash: only one script input expected or NFT token missing !!!")
                BI.unitval
         )
         (\_ -> go (BI.tail l))
         BI.unitval

-- PLU-STAN-17 fixtures (Redeemer-supplied indices must be unique)
--
-- The functions below are intentionally small and are analysed by Stan's
-- `PLU-STAN-17` inspection. Each one documents whether it should trigger
-- a warning, and why.
--
-- Local indexing helpers used by the PLU-STAN-17 fixtures. The detector identifies
-- indexing-like sinks by type shape (Integer + []/BuiltinList args), not by name.
{-# INLINABLE elemAt #-}
elemAt :: Integer -> [a] -> a
elemAt n = go n
  where
    go i xs =
      if i == 0
        then case xs of
          x : _ -> x
          [] -> error "elemAt: empty list"
        else case xs of
          _ : ys -> go (i - 1) ys
          [] -> error "elemAt: index too large"

{-# INLINE elemAt' #-}
elemAt' :: Integer -> BI.BuiltinList a -> a
elemAt' !n xs =
  BI.ifThenElse (BI.equalsInteger n 0)
  (\_ -> BI.head xs)
  (\_ -> elemAt' (n - 1) (BI.tail xs))
  BI.unitval

{-# INLINE elemAtFast #-}
elemAtFast :: Integer -> BI.BuiltinList a -> a
elemAtFast !n xs =
  BI.ifThenElse (BI.lessThanInteger 10 n)
  (\_ -> elemAtFast (n - 10) (BI.tail $ BI.tail $ BI.tail $ BI.tail $ BI.tail $ BI.tail $ BI.tail $ BI.tail $ BI.tail $ BI.tail xs))
  (\_ -> elemAtFast2 n xs)
  BI.unitval

{-# INLINE elemAtFast2 #-}
elemAtFast2 :: Integer -> BI.BuiltinList a -> a
elemAtFast2 !n xs =
  BI.ifThenElse (BI.lessThanInteger 5 n)
  (\_ -> elemAtFast2 (n - 5) (BI.tail $ BI.tail $ BI.tail $ BI.tail $ BI.tail xs))
  (\_ -> elemAt' n xs)
  BI.unitval

plutStan17UnsafeRedeemerIndicesList :: ScriptContext -> BuiltinData -> Integer
-- PLU-STAN-17 (should trigger): indexing a ScriptContext-derived outputs list
-- using a redeemer-supplied indices list, without any uniqueness enforcement.
plutStan17UnsafeRedeemerIndicesList ctx redeemer =
  let outs = txInfoOutputs $ scriptContextTxInfo ctx
      out = elemAt (elemAt 0 (Tx.unsafeFromBuiltinData redeemer :: [Integer])) outs
  in case txOutAddress out of
    Address _ _ -> 0

plutStan17SafeRedeemerIndicesList :: ScriptContext -> BuiltinData -> Integer
-- PLU-STAN-17 (should NOT trigger): same as the unsafe case, but we add the
-- `plutstan uniqueness enforced` marker on the line above the indexing site.
plutStan17SafeRedeemerIndicesList ctx redeemer =
  let outs = txInfoOutputs $ scriptContextTxInfo ctx
      -- PluTStAn   Uniqueness    Enforced
      out = elemAt (elemAt 0 (Tx.unsafeFromBuiltinData redeemer :: [Integer])) outs
  in case txOutAddress out of
    Address _ _ -> 0

plutStan17UnsafeRedeemerIndicesBytestring :: ScriptContext -> BuiltinData -> BuiltinData
-- PLU-STAN-17 (should trigger): indexing a ScriptContext-derived list using an
-- index obtained from a redeemer-supplied bytestring (via `indexByteString`),
-- without any uniqueness enforcement.
plutStan17UnsafeRedeemerIndicesBytestring ctx redeemer =
  elemAtFast (BI.indexByteString (Tx.unsafeFromBuiltinData redeemer :: BI.BuiltinByteString) 0) (BI.unsafeDataAsList $ BI.head $ BI.snd $ BI.unsafeDataAsConstr $ BI.head $ BI.snd $ BI.unsafeDataAsConstr $ Tx.toBuiltinData ctx)

plutStan17UnsafeIxBindingList :: ScriptContext -> BuiltinData -> Integer
-- PLU-STAN-17 (should trigger): the indices list and the final index are bound
-- to intermediate variables (`idxs`, `ix`) before being used to index into the
-- ScriptContext-derived outputs list. The analysis should be transitive.
plutStan17UnsafeIxBindingList ctx redeemer =
  let outs = txInfoOutputs $ scriptContextTxInfo ctx
      idxs = Tx.unsafeFromBuiltinData redeemer :: [Integer]
      ix = elemAt 0 idxs
      out = elemAt ix outs
  in case txOutAddress out of
    Address _ _ -> 0

plutStan17SafeMarkerSameLine :: ScriptContext -> BuiltinData -> Integer
-- PLU-STAN-17 (should NOT trigger): same as the unsafe ix-binding case, but
-- the marker is on the SAME line as the indexing site (case/whitespace
-- insensitive), so it should be suppressed.
plutStan17SafeMarkerSameLine ctx redeemer =
  let outs = txInfoOutputs $ scriptContextTxInfo ctx
      idxs = Tx.unsafeFromBuiltinData redeemer :: [Integer]
      ix = elemAt 0 idxs
      out = elemAt ix outs -- plutSTAN    uniqueness enforced
  in case txOutAddress out of
    Address _ _ -> 0

plutStan17UnsafeIxBindingBytestring :: BuiltinData -> BuiltinData -> BuiltinData
-- PLU-STAN-17 (should trigger): index is extracted from a redeemer-supplied
-- bytestring into an intermediate variable (`ix`) and then used to index a
-- ScriptContext-derived list.
plutStan17UnsafeIxBindingBytestring ctx redeemer =
  let inputs =
        BI.unsafeDataAsList $
          BI.head $
            BI.snd $
              BI.unsafeDataAsConstr $
                BI.head $
                  BI.snd $
                    BI.unsafeDataAsConstr ctx
      ix = BI.indexByteString (Tx.unsafeFromBuiltinData redeemer :: BI.BuiltinByteString) 0
  in elemAtFast ix inputs

plutStan17UnsafeMapIndices :: ScriptContext -> BuiltinData -> Integer
-- PLU-STAN-17 (should trigger): mapping over a redeemer-supplied indices list.
-- The indexing happens inside the lambda, so the detector must look through
-- `map` and still connect indices -> indexing into a ScriptContext-derived list.
plutStan17UnsafeMapIndices ctx redeemer =
  let outs = txInfoOutputs $ scriptContextTxInfo ctx
      idxs = Tx.unsafeFromBuiltinData redeemer :: [Integer]
      selected = map (\i -> elemAt i outs) idxs
  in case selected of
    [] -> 0
    _ -> 1

plutStan17UnsafeListArg :: [TxOut] -> BuiltinData -> Integer
-- PLU-STAN-17 (should trigger): the list being indexed is a function argument.
-- Stan cannot know at this call site whether the caller supplied a ScriptContext-
-- derived list (e.g. `txInfoOutputs`) or any other attacker-influenced list, so
-- the rule flags this conservatively.
plutStan17UnsafeListArg outs redeemer =
  let idxs = Tx.unsafeFromBuiltinData redeemer :: [Integer]
      out = elemAt (elemAt 0 idxs) outs
  in case txOutAddress out of
    Address _ _ -> 0

plutStan17UnsafeCtxListTransitive :: ScriptContext -> BuiltinData -> Integer
-- PLU-STAN-17 (should trigger): ScriptContext-derived list is passed through a
-- transitive alias (`outs0` -> `outs`) before being indexed.
plutStan17UnsafeCtxListTransitive ctx redeemer =
  let outs0 = txInfoOutputs $ scriptContextTxInfo ctx
      outs = outs0
      idxs = Tx.unsafeFromBuiltinData redeemer :: [Integer]
      out = elemAt (elemAt 0 idxs) outs
  in case txOutAddress out of
    Address _ _ -> 0

plutStan17UnsafeIndexValueTransitive :: ScriptContext -> BuiltinData -> Integer
-- PLU-STAN-17 (should trigger): redeemer-derived index is passed through a
-- transitive alias (`ix0` -> `ix`) before being used to index a ScriptContext-
-- derived list.
plutStan17UnsafeIndexValueTransitive ctx redeemer =
  let outs = txInfoOutputs $ scriptContextTxInfo ctx
      idxs = Tx.unsafeFromBuiltinData redeemer :: [Integer]
      ix0 = elemAt 0 idxs
      ix = ix0
      out = elemAt ix outs
  in case txOutAddress out of
    Address _ _ -> 0

plutStan17UnsafeBangBangOperator :: ScriptContext -> BuiltinData -> Integer
-- PLU-STAN-17 (should trigger): indexing a list using `!!` where the index is
-- derived from a redeemer-supplied indices list, without uniqueness enforcement.
plutStan17UnsafeBangBangOperator ctx redeemer =
  let outs = txInfoOutputs $ scriptContextTxInfo ctx
      idxs = Tx.unsafeFromBuiltinData redeemer :: [Integer]
      out = outs TxList.!! elemAt 0 idxs
  in case txOutAddress out of
    Address _ _ -> 0

plutStan17SafeBangBangOperator :: ScriptContext -> BuiltinData -> Integer
-- PLU-STAN-17 (should NOT trigger): same as the unsafe `!!` case, but we mark
-- the indexing site as having uniqueness enforced.
plutStan17SafeBangBangOperator ctx redeemer =
  let outs = txInfoOutputs $ scriptContextTxInfo ctx
      idxs = Tx.unsafeFromBuiltinData redeemer :: [Integer]
      -- plutstan uniqueness enforced
      out = outs TxList.!! elemAt 0 idxs
  in case txOutAddress out of
    Address _ _ -> 0

plutStan17UnsafeDrop :: ScriptContext -> BI.BuiltinUnit
-- PLU-STAN-17 (should trigger): using an index-based traversal helper (`drop`)
-- with a redeemer-derived index should still be considered unsafe without a
-- uniqueness check on the indices list.
plutStan17UnsafeDrop ctx =
  let outs = txInfoOutputs $ scriptContextTxInfo ctx
      idxs = Tx.unsafeFromBuiltinData (getRedeemer $ scriptContextRedeemer ctx) :: [Integer]
      getScriptOutputs :: [TxOut] -> [Integer] -> [TxOut]
      getScriptOutputs outs indices = 
        case indices of
          [] -> []
          x : xs -> TxList.head (TxList.drop x outs) : getScriptOutputs outs xs
      rest = getScriptOutputs outs idxs
  in case rest of
      [] -> error "plutStan17UnsafeDrop: no script output found"
      _ -> BI.unitval

-- PLU-STAN-18 fixtures (Avoid lazy (&&) in on-chain code)
--
-- PLU-STAN-18 warns about using (&&) in on-chain code. For now, the rule is
-- only triggers when (&&) appears in the predicate of a branching statement
-- (`if` or `BI.ifThenElse`) and one of the branches contains a failure
-- (`error` or `traceError`).

plutStan18TriggerIfElseError :: Bool
-- PLU-STAN-18 (should trigger): (&&) is used in the predicate of an `if`, and
-- the else branch fails via `traceError`.
plutStan18TriggerIfElseError =
  if True && False
    then True
    else P.traceError "boom"

plutStan18TriggerIfThenError :: Bool
-- PLU-STAN-18 (should trigger): (&&) is used in the predicate of an `if`, and
-- the then branch fails via `traceError`.
plutStan18TriggerIfThenError =
  if True && False
    then P.traceError "boom"
    else False

plutStan18NoTriggerNoError :: Bool
-- PLU-STAN-18 (should NOT trigger): predicate uses (&&) but neither branch
-- fails, so no warning is emitted.
plutStan18NoTriggerNoError =
  if True && False
    then True
    else False

plutStan18NoTriggerAndInBranch :: Bool
-- PLU-STAN-18 (should NOT trigger): (&&) appears in a branch body, not in the
-- predicate.
plutStan18NoTriggerAndInBranch =
  if True
    then True && False
    else P.traceError "boom"

plutStan18TriggerBuiltinIfThenElse :: Bool
-- PLU-STAN-18 (should trigger): (&&) is used in the predicate of
-- `BI.ifThenElse` and a branch fails.
plutStan18TriggerBuiltinIfThenElse =
  BI.ifThenElse (True && False) (P.traceError "boom") False

plutStan18TriggerOutermostOnly :: Bool
-- PLU-STAN-18 (should trigger ONCE): nested (&&) inside the predicate should
-- only report the outermost operator occurrence.
plutStan18TriggerOutermostOnly =
  if True &&
     (False && True)
    then P.traceError "boom"
    else False

pluStan18BooleanCondition :: Integer -> Integer -> Bool 
pluStan18BooleanCondition x y =
  x > 5 
   && y < 10
   && x < y 

-- PLU-STAN-18 (should trigger): boolean condition uses (&&) and one of the branches fails.
pluStan18BooleanConditionTrigger :: Bool 
pluStan18BooleanConditionTrigger = 
  if pluStan18BooleanCondition 6 9 
    then True
    else error "pluStan18BooleanConditionTrigger: condition failed"

pluStan08TuplePatternMultiUse :: Integer
-- PLU-STAN-08 (should trigger): tuple pattern is non-strict; x is used twice.
pluStan08TuplePatternMultiUse =
  let (x, y) = (1 :: Integer, 2 :: Integer)
  in x + x + y

-- Additional PLU-STAN-12 test cases (only those with passing tests)

-- Test: Using both upper and lower bound checks (should NOT trigger)
validRangeWithBothBoundChecks :: V1.ScriptContext -> Bool
validRangeWithBothBoundChecks ctx =
  case V1.txInfoValidRange $ V1.scriptContextTxInfo ctx of
    Interval.Interval (Interval.LowerBound (Interval.Finite _) _) (Interval.UpperBound (Interval.Finite _) _) -> True
    _ -> False

-- Test: Using interval utilities on custom intervals (not txInfoValidRange)
customIntervalWithUtility :: Bool
customIntervalWithUtility =
  let customInterval = Interval.Interval (Interval.LowerBound (Interval.Finite (100 :: Integer)) True)
                                         (Interval.UpperBound (Interval.Finite (200 :: Integer)) False)
  in Interval.contains (Interval.from (50 :: Integer)) customInterval

-- Test: Using interval utilities on literal intervals
literalIntervalWithAlways :: Bool
literalIntervalWithAlways =
  let time = 100 :: POSIXTime
  in Interval.contains Interval.always (Interval.singleton time)

-- Test: Bounded interval operations (singleton is always finite)
boundedIntervalSingleton :: V1.ScriptContext -> Bool
boundedIntervalSingleton ctx =
  let time = 100 :: POSIXTime
  in Interval.contains (Interval.singleton time) (V1.txInfoValidRange $ V1.scriptContextTxInfo ctx)

-- Test: Intersection with finite interval bounds the result
intersectionWithFiniteBounds :: V1.ScriptContext -> Bool
intersectionWithFiniteBounds ctx =
  let txRange = V1.txInfoValidRange $ V1.scriptContextTxInfo ctx
      finiteRange = Interval.Interval (Interval.LowerBound (Interval.Finite (0 :: POSIXTime)) True)
                                       (Interval.UpperBound (Interval.Finite (1000 :: POSIXTime)) True)
      bounded = Interval.intersection txRange finiteRange
  in not $ Interval.isEmpty bounded

checkRangeHelper :: POSIXTimeRange -> Bool
checkRangeHelper range = not $ Interval.isEmpty range

-- Test: Helper function that validates before using
checkedHelperFunction :: V1.ScriptContext -> Bool
checkedHelperFunction ctx =
  let txRange = V1.txInfoValidRange $ V1.scriptContextTxInfo ctx
  in case (Interval.lowerBound txRange, Interval.upperBound txRange) of
       (Interval.LowerBound (Interval.Finite _) _, Interval.UpperBound (Interval.Finite _) _) ->
         checkRangeHelper txRange
       _ -> False

-- Test: Pattern matching extracts bounds then checks them
extractAndCheckBounds :: V1.ScriptContext -> Bool
extractAndCheckBounds ctx =
  let Interval.Interval lb ub = V1.txInfoValidRange $ V1.scriptContextTxInfo ctx
  in case (lb, ub) of
       (Interval.LowerBound (Interval.Finite l) _, Interval.UpperBound (Interval.Finite u) _) ->
         l < u
       _ -> False

-- Edge case: Custom utility that wraps finite bound check
isFiniteLowerBound :: POSIXTimeRange -> Bool
isFiniteLowerBound range =
  case Interval.lowerBound range of
    Interval.LowerBound (Interval.Finite _) _ -> True
    _ -> False

isFiniteUpperBound :: POSIXTimeRange -> Bool
isFiniteUpperBound range =
  case Interval.upperBound range of
    Interval.UpperBound (Interval.Finite _) _ -> True
    _ -> False

-- Test: Using custom utilities for full validation (both bounds)
customFullValidation :: V1.ScriptContext -> Bool
customFullValidation ctx =
  let txRange = V1.txInfoValidRange $ V1.scriptContextTxInfo ctx
  in if isFiniteLowerBound txRange && isFiniteUpperBound txRange
     then not $ Interval.isEmpty txRange
     else False
