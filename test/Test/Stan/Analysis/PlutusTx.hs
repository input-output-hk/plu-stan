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
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan01 62

  it "PLU-STAN-01: verifyEcdsaSecp256k1Signature usage" $
    checkObservation AntiPattern.plustan01 69 3 34

  it "PLU-STAN-01: verifySchnorrSecp256k1Signature usage" $
    checkObservation AntiPattern.plustan01 76 3 36

  it "PLU-STAN-02: PlutusTx.UnsafeFromData unsafeFromBuiltinData" $
    checkObservation AntiPattern.plustan02 83 3 27

  it "PLU-STAN-03: No usage of Optional types in on-chain code" $
    checkObservation AntiPattern.plustan03 87 7 22

  it "PLU-STAN-04: == on pubKeyHash" $
    checkObservation AntiPattern.plustan04 91 27 29

  it "PLU-STAN-04: == on scriptHash" $
    checkObservation AntiPattern.plustan04 97 27 29

  it "PLU-STAN-04: == on credentialHash" $
    checkObservation AntiPattern.plustan04 103 35 37

  it "PLU-STAN-04: < on credentialHash" $
    checkObservation AntiPattern.plustan04 109 35 36

  it "PLU-STAN-12: validity interval utility usage (from)" $
    checkObservation AntiPattern.plustan12 419 5 23

  it "PLU-STAN-12: validity interval utility usage (always)" $
    checkObservation AntiPattern.plustan12 424 37 87

  it "PLU-STAN-12: txInfoValidRange passed around does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 427

  it "PLU-STAN-12: txInfoValidRange used in contains without finite bound check" $
    checkObservation AntiPattern.plustan12 420 5 55

  it "PLU-STAN-12: txInfoValidRange with finite bound check does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 429

  it "PLU-STAN-05: higher-order list helpers" $
    checkObservation AntiPattern.plustan05 116 3 16

  it "PLU-STAN-06: nested list traversals" $
    checkObservation AntiPattern.plustan06 125 6 57

  it "PLU-STAN-07: guard syntax in on-chain code" $
    checkObservation AntiPattern.plustan07 129 5 11

  it "PLU-STAN-08: non-strict let binding used multiple times" $
    checkObservation AntiPattern.plustan08 136 7 19

  it "PLU-STAN-08: strict let binding does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 138

  it "PLU-STAN-08: non-strict binding used in sibling let bindings" $
    checkObservation AntiPattern.plustan08 146 7 19

  it "PLU-STAN-08: non-strict binding used in a let binding and the body" $
    checkObservation AntiPattern.plustan08 153 7 19

  it "PLU-STAN-08: bang-pattern function argument does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 510

  it "PLU-STAN-08: lazy function argument binding used multiple times" $
    checkObservation AntiPattern.plustan08 516 8 10

  it "PLU-STAN-08: oracle-like strict let still triggers on arg (x) multi-use" $
    checkObservation AntiPattern.plustan08 522 8 9

  it "PLU-STAN-08: oracle-like strict let does not flag strict cred binding" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 523

  it "PLU-STAN-08: oracle-like bang arg (x) does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 530

  it "PLU-STAN-08: oracle-like bang cred does not flag strict cred binding" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 531

  it "PLU-STAN-08: plutStan08GetOracle does not trigger on strict cred" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 553

  it "PLU-STAN-08: plutStan08GetOracleTrigger triggers on cred binding" $
    checkObservation AntiPattern.plustan08 581 18 96

  it "PLU-STAN-08: tuple pattern binding used multiple times" $
    checkObservation AntiPattern.plustan08 856 7 44

  it "PLU-STAN-09: valueOf compared directly" $
    checkObservation AntiPattern.plustan09 159 3 50

  it "PLU-STAN-09: valueOf compared via let bindings" $
    checkObservation AntiPattern.plustan09 172 6 39

  it "PLU-STAN-09: valueOf compared with literal on the left" $
    checkObservation AntiPattern.plustan09 185 3 50

  it "PLU-STAN-09: valueOf compared via prefix (==)" $
    checkObservation AntiPattern.plustan09 196 3 54

  it "PLU-STAN-09: valueOf compared via section (== 5000)" $
    checkObservation AntiPattern.plustan09 207 3 54

  it "PLU-STAN-11: currencySymbolValueOf on minted value" $
    checkObservation AntiPattern.plustan11 219 6 78

  it "PLU-STAN-11: should catch minted value passed to helper (case 1, currently missed)" $
    checkObservation AntiPattern.plustan11 400 18 47

  it "PLU-STAN-11: should catch minted value passed to helper (case 2, currently missed)" $
    checkObservation AntiPattern.plustan11 409 18 47

  it "PLU-STAN-11: should catch minted value rewrapped from map" $
    checkObservation AntiPattern.plustan11 458 6 43

  it "PLU-STAN-11: should catch minted value rewrapped from mintValueToMap" $
    checkObservation AntiPattern.plustan11 465 6 43

  it "PLU-STAN-11: should catch minted value rewrapped via let pattern" $
    checkObservation AntiPattern.plustan11 472 6 43

  it "PLU-STAN-11: should not flag non-minted currencySymbolValueOf" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan11 414

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData used in comparison" $
    checkObservation AntiPattern.plustan10 223 3 95

  it "PLU-STAN-10: ScriptHash from unsafeFromBuiltinData used in comparison" $
    checkObservation AntiPattern.plustan10 230 3 95

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData in lending validator" $
    checkObservation AntiPattern.plustan10 261 6 68

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData via pattern binding" $
    checkObservation AntiPattern.plustan10 286 6 33

  -- Test case 1: Intermediate variable binding - triggers via transitive tracking:
  -- datum = unsafeFromBuiltinData x, then mustRepayToPkh is bound from datum
  it "PLU-STAN-10: Intermediate variable binding" $
    checkObservation AntiPattern.plustan10 310 6 33

  it "PLU-STAN-10: Record field accessor function" $
    checkObservation AntiPattern.plustan10 355 6 37

  it "PLU-STAN-10: Case expression pattern binding" $
    checkObservation AntiPattern.plustan10 333 10 37

  it "PLU-STAN-10: Where clause binding" $
    checkObservation AntiPattern.plustan10 373 35 62

  it "PLU-STAN-16: valid multiply-before-divide does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan16 476

  it "PLU-STAN-16: division before multiplication (simple)" $
    checkObservation AntiPattern.plustan16 480 3 42

  it "PLU-STAN-16: division before multiplication (lambda)" $
    checkObservation AntiPattern.plustan16 485 25 59

  it "PLU-STAN-16: division before multiplication (let-bound)" $
    checkObservation AntiPattern.plustan16 491 20 33

  it "PLU-STAN-16: division before multiplication (nested let)" $
    checkObservation AntiPattern.plustan16 497 23 36

  it "PLU-STAN-16: division before multiplication (where)" $
    checkObservation AntiPattern.plustan16 504 18 31

  it "PLU-STAN-17: redeemer-supplied indices list without uniqueness check" $
    checkObservation AntiPattern.plustan17 640 13 84

  it "PLU-STAN-17: redeemer-supplied indices list suppressed via marker" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan17 650

  it "PLU-STAN-17: redeemer-supplied indices bytestring without uniqueness check" $
    checkObservation AntiPattern.plustan17 659 3 227

  it "PLU-STAN-17: intermediate ix binding from redeemer indices list" $
    checkObservation AntiPattern.plustan17 669 13 27

  it "PLU-STAN-17: marker on same line suppresses" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan17 681

  it "PLU-STAN-17: intermediate ix binding from redeemer indices bytestring" $
    checkObservation AntiPattern.plustan17 699 6 26

  it "PLU-STAN-17: map over redeemer indices triggers on indexing" $
    checkObservation AntiPattern.plustan17 708 29 42

  it "PLU-STAN-17: flags indexing on list argument conservatively" $
    checkObservation AntiPattern.plustan17 720 13 40

  it "PLU-STAN-17: transitive list binding still triggers" $
    checkObservation AntiPattern.plustan17 731 13 40

  it "PLU-STAN-17: transitive index value binding still triggers" $
    checkObservation AntiPattern.plustan17 744 13 27

  it "PLU-STAN-17: indexing via (!!) operator triggers" $
    checkObservation AntiPattern.plustan17 754 13 41

  it "PLU-STAN-17: (!!) operator suppressed via marker" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan17 765

  it "PLU-STAN-17: drop-style indexing triggers" $
    checkObservation AntiPattern.plustan17 780 34 52

  it "PLU-STAN-18: flags (&&) in if predicate with failing else" $
    checkObservation AntiPattern.plustan18 797 11 13

  it "PLU-STAN-18: flags (&&) in if predicate with failing then" $
    checkObservation AntiPattern.plustan18 805 11 13

  it "PLU-STAN-18: does not flag when branches do not fail" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan18 813

  it "PLU-STAN-18: does not flag (&&) inside branch body" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan18 824

  it "PLU-STAN-18: flags (&&) in BI.ifThenElse predicate with failing branch" $
    checkObservation AntiPattern.plustan18 829 23 25

  it "PLU-STAN-18: nested (&&) only flags the outermost" $ do
    checkObservation AntiPattern.plustan18 835 11 13
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan18 836

  it "PLU-STAN-18: flags (&&) inside predicate helper with failing branch" $
    checkObservation AntiPattern.plustan18 843 4 6
