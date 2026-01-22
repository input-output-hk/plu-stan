{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{-# ANN module ("onchain-contract" :: String) #-}

module Target.PlinthRules where

import PlutusLedgerApi.V1
    ( Address (..)
    , Credential (..)
    , ScriptContext (..)
    , ScriptHash
    , TxInInfo (..)
    , TxInfo (..)
    , TxOut (..)
    , Value
    )
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusTx.Prelude

-- ============================================================================
-- PLU-STAN-16: StrictValueEquality
-- Tests for exact ADA equality that can fail due to minUTxO changes
-- ============================================================================

-- SHOULD TRIGGER: Exact ADA equality in output validation
strictAdaEquality1 :: TxOut -> Bool
strictAdaEquality1 out =
    Value.lovelaceValueOf (txOutValue out) == 2_000_000

-- SHOULD TRIGGER: Exact ADA equality in list validation
strictAdaEquality2 :: [TxOut] -> Bool
strictAdaEquality2 outputs =
    all (\out -> Value.lovelaceValueOf (txOutValue out) == 5_000_000) outputs

-- SHOULD TRIGGER: Exact ADA equality with variable
strictAdaEquality3 :: TxOut -> Integer -> Bool
strictAdaEquality3 out expectedAda =
    Value.lovelaceValueOf (txOutValue out) == expectedAda

-- SHOULD NOT TRIGGER: Minimum ADA check (valid)
validMinimumAda :: TxOut -> Bool
validMinimumAda out =
    Value.lovelaceValueOf (txOutValue out) >= 2_000_000

-- SHOULD NOT TRIGGER: Greater than check (valid)
validGreaterThanAda :: TxOut -> Bool
validGreaterThanAda out =
    Value.lovelaceValueOf (txOutValue out) > 1_500_000

-- SHOULD NOT TRIGGER: Less than or equal check (valid)
validLessThanAda :: TxOut -> Bool
validLessThanAda out =
    Value.lovelaceValueOf (txOutValue out) <= 10_000_000

-- ============================================================================
-- PLU-STAN-19: PrecisionLoss
-- Tests for division before multiplication in integer arithmetic
-- ============================================================================

-- SHOULD TRIGGER: Division before multiplication (loses precision)
precisionLoss1 :: Integer -> Integer -> Integer -> Integer
precisionLoss1 principal total elapsed =
    (elapsed / total) * principal

-- SHOULD TRIGGER: Using div operator (loses precision)
precisionLoss2 :: Integer -> Integer -> Integer -> Integer
precisionLoss2 amount divisor multiplier =
    (amount `div` divisor) * multiplier

-- SHOULD TRIGGER: Using quot operator (loses precision)
precisionLoss3 :: Integer -> Integer -> Integer -> Integer
precisionLoss3 x y z =
    (x `quot` y) * z

-- SHOULD TRIGGER: Complex expression with division first
precisionLoss4 :: Integer -> Integer -> Integer -> Integer -> Integer
precisionLoss4 a b c d =
    ((a / b) * c) + d

-- SHOULD NOT TRIGGER: Multiplication before division (correct order)
correctPrecision1 :: Integer -> Integer -> Integer -> Integer
correctPrecision1 principal total elapsed =
    (principal * elapsed) / total

-- SHOULD NOT TRIGGER: Using mul and div in correct order
correctPrecision2 :: Integer -> Integer -> Integer -> Integer
correctPrecision2 amount multiplier divisor =
    (amount * multiplier) `div` divisor

-- SHOULD NOT TRIGGER: Using quot in correct order
correctPrecision3 :: Integer -> Integer -> Integer -> Integer
correctPrecision3 x y z =
    (x * z) `quot` y

-- SHOULD NOT TRIGGER: Just division without multiplication
justDivision :: Integer -> Integer -> Integer
justDivision x y =
    x / y

-- SHOULD NOT TRIGGER: Just multiplication without division
justMultiplication :: Integer -> Integer -> Integer
justMultiplication x y =
    x * y

-- ============================================================================
-- PLU-STAN-20: TrashTokens
-- Tests for subset value comparisons allowing arbitrary token injection
-- ============================================================================

-- SHOULD TRIGGER: Using leq operator (allows extra tokens)
trashTokens1 :: Value -> Value -> Bool
trashTokens1 actualValue expectedValue =
    actualValue `Value.leq` expectedValue

-- SHOULD TRIGGER: Using geq operator (allows extra tokens)
trashTokens2 :: Value -> Value -> Bool
trashTokens2 actualValue requiredValue =
    actualValue `Value.geq` requiredValue

-- SHOULD TRIGGER: Using >= with assetClassValueOf (allows trash tokens)
trashTokens3 :: Value -> Value.AssetClass -> Integer -> Bool
trashTokens3 v asset minAmount =
    Value.assetClassValueOf v asset >= minAmount

-- SHOULD TRIGGER: Using <= with assetClassValueOf (allows trash tokens)
trashTokens4 :: Value -> Value.AssetClass -> Integer -> Bool
trashTokens4 v asset maxAmount =
    Value.assetClassValueOf v asset <= maxAmount

-- SHOULD TRIGGER: Using > with assetClassValueOf (allows trash tokens)
trashTokens5 :: Value -> Value.AssetClass -> Integer -> Bool
trashTokens5 v asset minAmount =
    Value.assetClassValueOf v asset > minAmount

-- SHOULD NOT TRIGGER: Exact value equality (correct)
correctValueCheck1 :: Value -> Value -> Bool
correctValueCheck1 actualValue expectedValue =
    actualValue == expectedValue

-- SHOULD NOT TRIGGER: Exact token amount with equality
correctValueCheck2 :: Value -> Value.AssetClass -> Integer -> Bool
correctValueCheck2 v asset expectedAmount =
    Value.assetClassValueOf v asset == expectedAmount

-- SHOULD NOT TRIGGER: Using valueOf with exact equality
correctValueCheck3 :: Value -> Value.CurrencySymbol -> Value.TokenName -> Integer -> Bool
correctValueCheck3 v cs tn expectedAmount =
    Value.valueOf v cs tn == expectedAmount

-- SHOULD NOT TRIGGER: Not related to Value comparisons
unrelatedComparison :: Integer -> Integer -> Bool
unrelatedComparison x y =
    x >= y
