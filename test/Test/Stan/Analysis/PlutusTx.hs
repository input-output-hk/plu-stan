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

  it "PLU-STAN-01: verifyEd25519Signature usage ignored via inline comment" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan01 59

  it "PLU-STAN-01: verifyEcdsaSecp256k1Signature usage" $
    checkObservation AntiPattern.plustan01 66 3 34

  it "PLU-STAN-01: verifySchnorrSecp256k1Signature usage" $
    checkObservation AntiPattern.plustan01 73 3 36

  it "PLU-STAN-02: PlutusTx.UnsafeFromData unsafeFromBuiltinData" $
    checkObservation AntiPattern.plustan02 80 3 27

  it "PLU-STAN-03: No usage of Optional types in on-chain code" $
    checkObservation AntiPattern.plustan03 84 7 22

  it "PLU-STAN-04: == on pubKeyHash" $
    checkObservation AntiPattern.plustan04 88 27 29

  it "PLU-STAN-04: == on scriptHash" $
    checkObservation AntiPattern.plustan04 94 27 29

  it "PLU-STAN-04: == on credentialHash" $
    checkObservation AntiPattern.plustan04 100 35 37

  it "PLU-STAN-04: < on credentialHash" $
    checkObservation AntiPattern.plustan04 106 35 36

  it "PLU-STAN-12: validity interval utility usage (from)" $
    checkObservation AntiPattern.plustan12 416 6 22

  it "PLU-STAN-12: validity interval utility usage (always)" $
    checkObservation AntiPattern.plustan12 421 3 36

  it "PLU-STAN-12: txInfoValidRange without finite bound check" $
    checkObservation AntiPattern.plustan12 425 3 51

  it "PLU-STAN-12: txInfoValidRange with finite bound check does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 427

  it "PLU-STAN-05: higher-order list helpers" $
    checkObservation AntiPattern.plustan05 113 3 16

  it "PLU-STAN-06: nested list traversals" $
    checkObservation AntiPattern.plustan06 122 6 57

  it "PLU-STAN-07: guard syntax in on-chain code" $
    checkObservation AntiPattern.plustan07 126 5 11

  it "PLU-STAN-08: non-strict let binding used multiple times" $
    checkObservation AntiPattern.plustan08 133 7 19

  it "PLU-STAN-08: strict let binding does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 136

  it "PLU-STAN-08: non-strict binding used in sibling let bindings" $
    checkObservation AntiPattern.plustan08 143 7 19

  it "PLU-STAN-08: non-strict binding used in a let binding and the body" $
    checkObservation AntiPattern.plustan08 150 7 19

  it "PLU-STAN-09: valueOf compared directly" $
    checkObservation AntiPattern.plustan09 156 3 50

  it "PLU-STAN-09: valueOf compared via let bindings" $
    checkObservation AntiPattern.plustan09 169 6 39

  it "PLU-STAN-09: valueOf compared with literal on the left" $
    checkObservation AntiPattern.plustan09 182 3 50

  it "PLU-STAN-09: valueOf compared via prefix (==)" $
    checkObservation AntiPattern.plustan09 193 3 54

  it "PLU-STAN-09: valueOf compared via section (== 5000)" $
    checkObservation AntiPattern.plustan09 204 3 54

  it "PLU-STAN-11: currencySymbolValueOf on minted value" $
    checkObservation AntiPattern.plustan11 216 6 78

  it "PLU-STAN-11: should catch minted value passed to helper (case 1, currently missed)" $
    checkObservation AntiPattern.plustan11 397 18 47

  it "PLU-STAN-11: should catch minted value passed to helper (case 2, currently missed)" $
    checkObservation AntiPattern.plustan11 406 18 47

  it "PLU-STAN-11: should catch minted value rewrapped from map" $
    checkObservation AntiPattern.plustan11 455 6 43

  it "PLU-STAN-11: should catch minted value rewrapped from mintValueToMap" $
    checkObservation AntiPattern.plustan11 462 6 43

  it "PLU-STAN-11: should catch minted value rewrapped via let pattern" $
    checkObservation AntiPattern.plustan11 469 6 43

  it "PLU-STAN-11: should not flag non-minted currencySymbolValueOf" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan11 408

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData used in comparison" $
    checkObservation AntiPattern.plustan10 220 3 103

  it "PLU-STAN-10: ScriptHash from unsafeFromBuiltinData used in comparison" $
    checkObservation AntiPattern.plustan10 227 3 103

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData in lending validator" $
    checkObservation AntiPattern.plustan10 258 6 68

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData via pattern binding" $
    checkObservation AntiPattern.plustan10 283 6 33

  -- Test case 1: Intermediate variable binding - triggers via transitive tracking:
  -- datum = unsafeFromBuiltinData x, then mustRepayToPkh is bound from datum
  it "PLU-STAN-10: Intermediate variable binding" $
    checkObservation AntiPattern.plustan10 307 6 33

  it "PLU-STAN-10: Record field accessor function" $
    checkObservation AntiPattern.plustan10 352 6 37

  it "PLU-STAN-10: Case expression pattern binding" $
    checkObservation AntiPattern.plustan10 330 10 37

  it "PLU-STAN-10: Where clause binding" $
    checkObservation AntiPattern.plustan10 370 35 62

  it "PLU-STAN-16: valid multiply-before-divide does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan16 473

  it "PLU-STAN-16: division before multiplication (simple)" $
    checkObservation AntiPattern.plustan16 477 3 42

  it "PLU-STAN-16: division before multiplication (lambda)" $
    checkObservation AntiPattern.plustan16 482 25 59

  it "PLU-STAN-16: division before multiplication (let-bound)" $
    checkObservation AntiPattern.plustan16 488 20 33

  it "PLU-STAN-16: division before multiplication (nested let)" $
    checkObservation AntiPattern.plustan16 494 23 36

  it "PLU-STAN-16: division before multiplication (where)" $
    checkObservation AntiPattern.plustan16 501 18 31
