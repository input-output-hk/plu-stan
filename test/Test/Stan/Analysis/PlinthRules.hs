{-# LANGUAGE OverloadedStrings #-}

module Test.Stan.Analysis.PlinthRules
    ( analysisPlinthRulesSpec
    ) where

import Test.Hspec (Spec, describe, it)

import Stan.Analysis (Analysis)
import Test.Stan.Analysis.Common (noObservationAssert, observationAssert)

import qualified Stan.Inspection.AntiPattern as AntiPattern


analysisPlinthRulesSpec :: Analysis -> Spec
analysisPlinthRulesSpec analysis = describe "Plinth Security & Performance Rules" $ do
    let checkObs = observationAssert ["PlinthRules"] analysis
    let noObs = noObservationAssert ["PlinthRules"] analysis

    strictValueEqualitySpec checkObs noObs
    precisionLossSpec checkObs noObs
    trashTokensSpec checkObs noObs


strictValueEqualitySpec
    :: (forall ins . Show ins => ins -> Int -> Int -> Int -> IO ())
    -> (forall ins . Show ins => ins -> Int -> IO ())
    -> Spec
strictValueEqualitySpec checkObs noObs =
    describe "PLU-STAN-16: Strict Value Equality" $ do
        it "triggers on exact ADA equality in output validation" $
            checkObs AntiPattern.plustan16 33 5 55

        it "triggers on exact ADA equality in list validation" $
            checkObs AntiPattern.plustan16 38 10 71

        it "triggers on exact ADA equality with variable" $
            checkObs AntiPattern.plustan16 43 5 55

        it "does not trigger on minimum ADA check" $
            noObs AntiPattern.plustan16 48

        it "does not trigger on greater than check" $
            noObs AntiPattern.plustan16 53

        it "does not trigger on less than or equal check" $
            noObs AntiPattern.plustan16 58


precisionLossSpec
    :: (forall ins . Show ins => ins -> Int -> Int -> Int -> IO ())
    -> (forall ins . Show ins => ins -> Int -> IO ())
    -> Spec
precisionLossSpec checkObs noObs =
    describe "PLU-STAN-19: Precision Loss" $ do
        it "triggers on division before multiplication" $
            checkObs AntiPattern.plustan19 69 5 29

        it "triggers on div operator before multiplication" $
            checkObs AntiPattern.plustan19 74 5 41

        it "triggers on quot operator before multiplication" $
            checkObs AntiPattern.plustan19 79 5 27

        it "triggers on complex expression with division first" $
            checkObs AntiPattern.plustan19 84 5 17

        it "does not trigger on multiplication before division" $
            noObs AntiPattern.plustan19 89

        it "does not trigger on mul and div in correct order" $
            noObs AntiPattern.plustan19 94

        it "does not trigger on mult and quot in correct order" $
            noObs AntiPattern.plustan19 99

        it "does not trigger on just division" $
            noObs AntiPattern.plustan19 104

        it "does not trigger on just multiplication" $
            noObs AntiPattern.plustan19 109


trashTokensSpec
    :: (forall ins . Show ins => ins -> Int -> Int -> Int -> IO ())
    -> (forall ins . Show ins => ins -> Int -> IO ())
    -> Spec
trashTokensSpec checkObs noObs =
    describe "PLU-STAN-20: Trash Tokens" $ do
        it "triggers on leq operator" $
            checkObs AntiPattern.plustan20 120 5 44

        it "triggers on geq operator" $
            checkObs AntiPattern.plustan20 125 5 44

        it "triggers on >= with assetClassValueOf" $
            checkObs AntiPattern.plustan20 130 5 46

        it "triggers on <= with assetClassValueOf" $
            checkObs AntiPattern.plustan20 135 5 46

        it "triggers on > with assetClassValueOf" $
            checkObs AntiPattern.plustan20 140 5 45

        it "does not trigger on exact value equality" $
            noObs AntiPattern.plustan20 145

        it "does not trigger on exact token amount with equality" $
            noObs AntiPattern.plustan20 150

        it "does not trigger on valueOf with exact equality" $
            noObs AntiPattern.plustan20 155

        it "does not trigger on unrelated comparison" $
            noObs AntiPattern.plustan20 160
