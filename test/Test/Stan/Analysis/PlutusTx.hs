module Test.Stan.Analysis.PlutusTx
    ( analysisPlutusTxSpec
    ) where

import Test.Hspec (Spec, describe, it, xit)

import Stan.Analysis (Analysis)
import Test.Stan.Analysis.Common (observationAssert)

import qualified Stan.Inspection.AntiPattern as AntiPattern


analysisPlutusTxSpec :: Analysis -> Spec
analysisPlutusTxSpec analysis = describe "Plutus-Tx" $ do
    let checkObservation = observationAssert ["PlutusTx"] analysis

    --it "PLU-STAN-0X: no variable named foo" $
        --checkObservation AntiPattern.dummyFooStan01 37 3 6

    it "PLU-STAN-01: PlutusTx.AssocMap unsafeFromList" $
        checkObservation AntiPattern.plustan01 35 12 35

    it "PLU-STAN-02: PlutusTx.UnsafeFromData unsafeFromBuiltinData" $
        checkObservation AntiPattern.plustan02 39 3 27

    it "PLU-STAN-03: PlutusTx.Maybe fromMaybe" $
        checkObservation AntiPattern.plustan03 43 7 22


