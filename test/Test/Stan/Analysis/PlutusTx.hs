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
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan01 60

  it "PLU-STAN-01: verifyEcdsaSecp256k1Signature usage" $
    checkObservation AntiPattern.plustan01 67 3 34

  it "PLU-STAN-01: verifySchnorrSecp256k1Signature usage" $
    checkObservation AntiPattern.plustan01 74 3 36

  it "PLU-STAN-02: PlutusTx.UnsafeFromData unsafeFromBuiltinData" $
    checkObservation AntiPattern.plustan02 81 3 27

  it "PLU-STAN-03: No usage of Optional types in on-chain code" $
    checkObservation AntiPattern.plustan03 85 7 22

  it "PLU-STAN-04: == on pubKeyHash" $
    checkObservation AntiPattern.plustan04 89 27 29

  it "PLU-STAN-04: == on scriptHash" $
    checkObservation AntiPattern.plustan04 95 27 29

  it "PLU-STAN-04: == on credentialHash" $
    checkObservation AntiPattern.plustan04 101 35 37

  it "PLU-STAN-04: < on credentialHash" $
    checkObservation AntiPattern.plustan04 107 35 36

  it "PLU-STAN-12: validity interval utility usage (from)" $
    checkObservation AntiPattern.plustan12 417 5 23

  it "PLU-STAN-12: validity interval utility usage (always)" $
    checkObservation AntiPattern.plustan12 422 37 87

  it "PLU-STAN-12: txInfoValidRange passed around does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 425

  it "PLU-STAN-12: txInfoValidRange used in contains without finite bound check" $
    checkObservation AntiPattern.plustan12 418 5 55

  it "PLU-STAN-12: txInfoValidRange with finite bound check does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 427

  it "PLU-STAN-05: higher-order list helpers" $
    checkObservation AntiPattern.plustan05 114 3 16

  it "PLU-STAN-06: nested list traversals" $
    checkObservation AntiPattern.plustan06 123 6 57

  it "PLU-STAN-07: guard syntax in on-chain code" $
    checkObservation AntiPattern.plustan07 127 5 11

  it "PLU-STAN-08: non-strict let binding used multiple times" $
    checkObservation AntiPattern.plustan08 134 7 19

  it "PLU-STAN-08: strict let binding does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 136

  it "PLU-STAN-08: non-strict binding used in sibling let bindings" $
    checkObservation AntiPattern.plustan08 144 7 19

  it "PLU-STAN-08: non-strict binding used in a let binding and the body" $
    checkObservation AntiPattern.plustan08 151 7 19

  it "PLU-STAN-08: bang-pattern function argument does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 508

  it "PLU-STAN-08: lazy function argument binding used multiple times" $
    checkObservation AntiPattern.plustan08 514 8 10

  it "PLU-STAN-08: oracle-like strict let still triggers on arg (x) multi-use" $
    checkObservation AntiPattern.plustan08 520 8 9

  it "PLU-STAN-08: oracle-like strict let does not flag strict cred binding" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 521

  it "PLU-STAN-08: oracle-like bang arg (x) does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 528

  it "PLU-STAN-08: oracle-like bang cred does not flag strict cred binding" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 529

  it "PLU-STAN-08: plutStan08GetOracle does not trigger on strict cred" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 551

  it "PLU-STAN-08: plutStan08GetOracleTrigger triggers on cred binding" $
    checkObservation AntiPattern.plustan08 579 18 96

  it "PLU-STAN-09: valueOf compared directly" $
    checkObservation AntiPattern.plustan09 157 3 50

  it "PLU-STAN-09: valueOf compared via let bindings" $
    checkObservation AntiPattern.plustan09 170 6 39

  it "PLU-STAN-09: valueOf compared with literal on the left" $
    checkObservation AntiPattern.plustan09 183 3 50

  it "PLU-STAN-09: valueOf compared via prefix (==)" $
    checkObservation AntiPattern.plustan09 194 3 54

  it "PLU-STAN-09: valueOf compared via section (== 5000)" $
    checkObservation AntiPattern.plustan09 205 3 54

  it "PLU-STAN-11: currencySymbolValueOf on minted value" $
    checkObservation AntiPattern.plustan11 217 6 78

  it "PLU-STAN-11: should catch minted value passed to helper (case 1, currently missed)" $
    checkObservation AntiPattern.plustan11 398 18 47

  it "PLU-STAN-11: should catch minted value passed to helper (case 2, currently missed)" $
    checkObservation AntiPattern.plustan11 407 18 47

  it "PLU-STAN-11: should catch minted value rewrapped from map" $
    checkObservation AntiPattern.plustan11 456 6 43

  it "PLU-STAN-11: should catch minted value rewrapped from mintValueToMap" $
    checkObservation AntiPattern.plustan11 463 6 43

  it "PLU-STAN-11: should catch minted value rewrapped via let pattern" $
    checkObservation AntiPattern.plustan11 470 6 43

  it "PLU-STAN-11: should not flag non-minted currencySymbolValueOf" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan11 412

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData used in comparison" $
    checkObservation AntiPattern.plustan10 221 3 95

  it "PLU-STAN-10: ScriptHash from unsafeFromBuiltinData used in comparison" $
    checkObservation AntiPattern.plustan10 228 3 95

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData in lending validator" $
    checkObservation AntiPattern.plustan10 259 6 68

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData via pattern binding" $
    checkObservation AntiPattern.plustan10 284 6 33

  -- Test case 1: Intermediate variable binding - triggers via transitive tracking:
  -- datum = unsafeFromBuiltinData x, then mustRepayToPkh is bound from datum
  it "PLU-STAN-10: Intermediate variable binding" $
    checkObservation AntiPattern.plustan10 308 6 33

  it "PLU-STAN-10: Record field accessor function" $
    checkObservation AntiPattern.plustan10 353 6 37

  it "PLU-STAN-10: Case expression pattern binding" $
    checkObservation AntiPattern.plustan10 331 10 37

  it "PLU-STAN-10: Where clause binding" $
    checkObservation AntiPattern.plustan10 371 35 62

  it "PLU-STAN-16: valid multiply-before-divide does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan16 474

  it "PLU-STAN-16: division before multiplication (simple)" $
    checkObservation AntiPattern.plustan16 478 3 42

  it "PLU-STAN-16: division before multiplication (lambda)" $
    checkObservation AntiPattern.plustan16 483 25 59

  it "PLU-STAN-16: division before multiplication (let-bound)" $
    checkObservation AntiPattern.plustan16 489 20 33

  it "PLU-STAN-16: division before multiplication (nested let)" $
    checkObservation AntiPattern.plustan16 495 23 36

  it "PLU-STAN-16: division before multiplication (where)" $
    checkObservation AntiPattern.plustan16 502 18 31
