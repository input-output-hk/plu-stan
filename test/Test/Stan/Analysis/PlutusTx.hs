module Test.Stan.Analysis.PlutusTx (
  analysisPlutusTxSpec,
) where

import Test.Hspec (Spec, describe, it)

import Stan.Analysis (Analysis)
import Test.Stan.Analysis.Common (noObservationAssert, observationAssert)

import qualified Stan.Inspection.AntiPattern as AntiPattern

analysisPlutusTxSpec :: Analysis -> Spec
analysisPlutusTxSpec analysis = describe "Plutus-Tx" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "PLU-STAN-01: PlutusTx.AssocMap unsafeFromList" $
    checkObservation AntiPattern.plustan01 58 12 35

  it "PLU-STAN-02: PlutusTx.UnsafeFromData unsafeFromBuiltinData" $
    checkObservation AntiPattern.plustan02 62 3 27

  it "PLU-STAN-03: No usage of Optional types in on-chain code" $
    checkObservation AntiPattern.plustan03 66 7 22

  it "PLU-STAN-04: == on pubKeyHash" $
    checkObservation AntiPattern.plustan04 70 27 29

  it "PLU-STAN-04: == on scriptHash" $
    checkObservation AntiPattern.plustan04 76 27 29

  it "PLU-STAN-04: == on credentialHash" $
    checkObservation AntiPattern.plustan04 82 35 37

  it "PLU-STAN-04: < on credentialHash" $
    checkObservation AntiPattern.plustan04 88 35 36

  it "PLU-STAN-12: validity interval utility usage (from)" $
    checkObservation AntiPattern.plustan12 398 6 22

  it "PLU-STAN-12: validity interval utility usage (always)" $
    checkObservation AntiPattern.plustan12 403 3 36

  it "PLU-STAN-12: txInfoValidRange without finite bound check" $
    checkObservation AntiPattern.plustan12 407 3 51

  it "PLU-STAN-12: txInfoValidRange with finite bound check does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 409

  it "PLU-STAN-05: higher-order list helpers" $
    checkObservation AntiPattern.plustan05 95 3 16

  it "PLU-STAN-06: nested list traversals" $
    checkObservation AntiPattern.plustan06 104 6 57

  it "PLU-STAN-07: guard syntax in on-chain code" $
    checkObservation AntiPattern.plustan07 108 5 11

  it "PLU-STAN-08: non-strict let binding used multiple times" $
    checkObservation AntiPattern.plustan08 115 7 19

  it "PLU-STAN-08: strict let binding does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 118

  it "PLU-STAN-08: non-strict binding used in sibling let bindings" $
    checkObservation AntiPattern.plustan08 125 7 19

  it "PLU-STAN-08: non-strict binding used in a let binding and the body" $
    checkObservation AntiPattern.plustan08 132 7 19

  it "PLU-STAN-09: valueOf compared directly" $
    checkObservation AntiPattern.plustan09 138 3 50

  it "PLU-STAN-09: valueOf compared via let bindings" $
    checkObservation AntiPattern.plustan09 151 6 39

  it "PLU-STAN-09: valueOf compared with literal on the left" $
    checkObservation AntiPattern.plustan09 164 3 50

  it "PLU-STAN-09: valueOf compared via prefix (==)" $
    checkObservation AntiPattern.plustan09 175 3 54

  it "PLU-STAN-09: valueOf compared via section (== 5000)" $
    checkObservation AntiPattern.plustan09 186 3 54

  it "PLU-STAN-11: currencySymbolValueOf on minted value" $
    checkObservation AntiPattern.plustan11 198 6 78

  it "PLU-STAN-11: should catch minted value passed to helper (case 1, currently missed)" $
    checkObservation AntiPattern.plustan11 379 18 47

  it "PLU-STAN-11: should catch minted value passed to helper (case 2, currently missed)" $
    checkObservation AntiPattern.plustan11 388 18 47

  it "PLU-STAN-11: should catch minted value rewrapped from map" $
    checkObservation AntiPattern.plustan11 437 6 43

  it "PLU-STAN-11: should catch minted value rewrapped from mintValueToMap" $
    checkObservation AntiPattern.plustan11 444 6 43

  it "PLU-STAN-11: should catch minted value rewrapped via let pattern" $
    checkObservation AntiPattern.plustan11 451 6 43

  it "PLU-STAN-11: should not flag non-minted currencySymbolValueOf" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan11 390

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData used in comparison" $
    checkObservation AntiPattern.plustan10 202 3 103

  it "PLU-STAN-10: ScriptHash from unsafeFromBuiltinData used in comparison" $
    checkObservation AntiPattern.plustan10 209 3 103

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData in lending validator" $
    checkObservation AntiPattern.plustan10 240 6 68

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData via pattern binding" $
    checkObservation AntiPattern.plustan10 265 6 33

  -- Test case 1: Intermediate variable binding - triggers via transitive tracking:
  -- datum = unsafeFromBuiltinData x, then mustRepayToPkh is bound from datum
  it "PLU-STAN-10: Intermediate variable binding" $
    checkObservation AntiPattern.plustan10 289 6 33

  it "PLU-STAN-10: Record field accessor function" $
    checkObservation AntiPattern.plustan10 334 6 37

  it "PLU-STAN-10: Case expression pattern binding" $
    checkObservation AntiPattern.plustan10 312 10 37

  it "PLU-STAN-10: Where clause binding" $
    checkObservation AntiPattern.plustan10 352 35 62
