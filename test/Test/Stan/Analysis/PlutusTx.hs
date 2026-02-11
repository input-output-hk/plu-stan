module Test.Stan.Analysis.PlutusTx (
  analysisPlutusTxSpec,
  plustan01Spec,
  plustan02Spec,
  plustan03Spec,
  plustan04Spec,
  plustan05Spec,
  plustan06Spec,
  plustan07Spec,
  plustan08Spec,
  plustan09Spec,
  plustan10Spec,
  plustan11Spec,
  plustan12Spec,
  plustan16Spec,
  plustan17Spec,
  plustan18Spec,
) where

import Test.Hspec (Spec, describe, it)

import Stan.Analysis (Analysis)
import Test.Stan.Analysis.Common (noObservationAssert, observationAssert)

import qualified Stan.Inspection.AntiPattern as AntiPattern

analysisPlutusTxSpec :: Analysis -> Spec
analysisPlutusTxSpec analysis = describe "Plutus-Tx" $ do
  plustan01Spec analysis
  plustan02Spec analysis
  plustan03Spec analysis
  plustan04Spec analysis
  plustan05Spec analysis
  plustan06Spec analysis
  plustan07Spec analysis
  plustan08Spec analysis
  plustan09Spec analysis
  plustan10Spec analysis
  plustan11Spec analysis
  plustan12Spec analysis
  plustan16Spec analysis
  plustan17Spec analysis
  plustan18Spec analysis

plustan01Spec :: Analysis -> Spec
plustan01Spec analysis = describe "PLU-STAN-01" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "verifyEd25519Signature usage ignored via inline comment" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan01 62

  it "verifyEcdsaSecp256k1Signature usage" $
    checkObservation AntiPattern.plustan01 69 3 34

  it "verifySchnorrSecp256k1Signature usage" $
    checkObservation AntiPattern.plustan01 76 3 36

plustan02Spec :: Analysis -> Spec
plustan02Spec analysis = describe "PLU-STAN-02" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "PlutusTx.UnsafeFromData unsafeFromBuiltinData" $
    checkObservation AntiPattern.plustan02 83 3 27

plustan03Spec :: Analysis -> Spec
plustan03Spec analysis = describe "PLU-STAN-03" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "No usage of Optional types in on-chain code" $
    checkObservation AntiPattern.plustan03 87 7 22

plustan04Spec :: Analysis -> Spec
plustan04Spec analysis = describe "PLU-STAN-04" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "== on pubKeyHash" $
    checkObservation AntiPattern.plustan04 91 27 29

  it "== on scriptHash" $
    checkObservation AntiPattern.plustan04 97 27 29

  it "== on credentialHash" $
    checkObservation AntiPattern.plustan04 103 35 37

  it "< on credentialHash" $
    checkObservation AntiPattern.plustan04 109 35 36

plustan05Spec :: Analysis -> Spec
plustan05Spec analysis = describe "PLU-STAN-05" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "higher-order list helpers" $
    checkObservation AntiPattern.plustan05 116 3 16

plustan06Spec :: Analysis -> Spec
plustan06Spec analysis = describe "PLU-STAN-06" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "nested list traversals" $
    checkObservation AntiPattern.plustan06 125 6 57

plustan07Spec :: Analysis -> Spec
plustan07Spec analysis = describe "PLU-STAN-07" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "guard syntax in on-chain code" $
    checkObservation AntiPattern.plustan07 129 5 11

plustan08Spec :: Analysis -> Spec
plustan08Spec analysis = describe "PLU-STAN-08" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "non-strict let binding used multiple times" $
    checkObservation AntiPattern.plustan08 136 7 19

  it "strict let binding does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 138

  it "non-strict binding used in sibling let bindings" $
    checkObservation AntiPattern.plustan08 146 7 19

  it "non-strict binding used in a let binding and the body" $
    checkObservation AntiPattern.plustan08 153 7 19

  it "bang-pattern function argument does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 510

  it "lazy function argument binding used multiple times" $
    checkObservation AntiPattern.plustan08 516 8 10

  it "oracle-like strict let still triggers on arg (x) multi-use" $
    checkObservation AntiPattern.plustan08 522 8 9

  it "oracle-like strict let does not flag strict cred binding" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 523

  it "oracle-like bang arg (x) does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 530

  it "oracle-like bang cred does not flag strict cred binding" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 531

  it "plutStan08GetOracle does not trigger on strict cred" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 553

  it "plutStan08GetOracleTrigger triggers on cred binding" $
    checkObservation AntiPattern.plustan08 581 18 96

  it "tuple pattern binding used multiple times" $
    checkObservation AntiPattern.plustan08 856 7 44

plustan09Spec :: Analysis -> Spec
plustan09Spec analysis = describe "PLU-STAN-09" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "valueOf compared directly" $
    checkObservation AntiPattern.plustan09 159 3 50

  it "valueOf compared via let bindings" $
    checkObservation AntiPattern.plustan09 172 6 39

  it "valueOf compared with literal on the left" $
    checkObservation AntiPattern.plustan09 185 3 50

  it "valueOf compared via prefix (==)" $
    checkObservation AntiPattern.plustan09 196 3 54

  it "valueOf compared via section (== 5000)" $
    checkObservation AntiPattern.plustan09 207 3 54

plustan10Spec :: Analysis -> Spec
plustan10Spec analysis = describe "PLU-STAN-10" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "PubKeyHash from unsafeFromBuiltinData used in comparison" $
    checkObservation AntiPattern.plustan10 223 3 95

  it "ScriptHash from unsafeFromBuiltinData used in comparison" $
    checkObservation AntiPattern.plustan10 230 3 95

  it "PubKeyHash from unsafeFromBuiltinData in lending validator" $
    checkObservation AntiPattern.plustan10 261 6 68

  it "PubKeyHash from unsafeFromBuiltinData via pattern binding" $
    checkObservation AntiPattern.plustan10 286 6 33

  it "Intermediate variable binding" $
    checkObservation AntiPattern.plustan10 310 6 33

  it "Record field accessor function" $
    checkObservation AntiPattern.plustan10 355 6 37

  it "Case expression pattern binding" $
    checkObservation AntiPattern.plustan10 333 10 37

  it "Where clause binding" $
    checkObservation AntiPattern.plustan10 373 35 62

plustan11Spec :: Analysis -> Spec
plustan11Spec analysis = describe "PLU-STAN-11" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "currencySymbolValueOf on minted value" $
    checkObservation AntiPattern.plustan11 219 6 78

  it "should catch minted value passed to helper (case 1, currently missed)" $
    checkObservation AntiPattern.plustan11 400 18 47

  it "should catch minted value passed to helper (case 2, currently missed)" $
    checkObservation AntiPattern.plustan11 409 18 47

  it "should catch minted value rewrapped from map" $
    checkObservation AntiPattern.plustan11 458 6 43

  it "should catch minted value rewrapped from mintValueToMap" $
    checkObservation AntiPattern.plustan11 465 6 43

  it "should catch minted value rewrapped via let pattern" $
    checkObservation AntiPattern.plustan11 472 6 43

  it "should not flag non-minted currencySymbolValueOf" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan11 414

plustan12Spec :: Analysis -> Spec
plustan12Spec analysis = describe "PLU-STAN-12" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "validity interval utility usage (from)" $
    checkObservation AntiPattern.plustan12 419 5 23

  it "validity interval utility usage (always)" $
    checkObservation AntiPattern.plustan12 424 37 87

  it "txInfoValidRange passed around does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 427

  it "txInfoValidRange used in contains without finite bound check" $
    checkObservation AntiPattern.plustan12 420 5 55

  it "txInfoValidRange with finite bound check does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 429

  it "both bounds checked does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 864

  it "custom intervals with utilities should not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 870

  it "literal intervals with always should not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 877

  it "singleton (bounded) interval should not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 883

  it "intersection with finite bounds should not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 889

  it "helper with validation should not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 901

  it "extract and check bounds should not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 910

  it "custom full validation (both bounds) should not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan12 932

plustan16Spec :: Analysis -> Spec
plustan16Spec analysis = describe "PLU-STAN-16" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "valid multiply-before-divide does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan16 476

  it "division before multiplication (simple)" $
    checkObservation AntiPattern.plustan16 480 3 42

  it "division before multiplication (lambda)" $
    checkObservation AntiPattern.plustan16 485 25 59

  it "division before multiplication (let-bound)" $
    checkObservation AntiPattern.plustan16 491 20 33

  it "division before multiplication (nested let)" $
    checkObservation AntiPattern.plustan16 497 23 36

  it "division before multiplication (where)" $
    checkObservation AntiPattern.plustan16 504 18 31

plustan17Spec :: Analysis -> Spec
plustan17Spec analysis = describe "PLU-STAN-17" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "redeemer-supplied indices list without uniqueness check" $
    checkObservation AntiPattern.plustan17 640 13 84

  it "redeemer-supplied indices list suppressed via marker" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan17 650

  it "redeemer-supplied indices bytestring without uniqueness check" $
    checkObservation AntiPattern.plustan17 659 3 227

  it "intermediate ix binding from redeemer indices list" $
    checkObservation AntiPattern.plustan17 669 13 27

  it "marker on same line suppresses" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan17 681

  it "intermediate ix binding from redeemer indices bytestring" $
    checkObservation AntiPattern.plustan17 699 6 26

  it "map over redeemer indices triggers on indexing" $
    checkObservation AntiPattern.plustan17 708 29 42

  it "flags indexing on list argument conservatively" $
    checkObservation AntiPattern.plustan17 720 13 40

  it "transitive list binding still triggers" $
    checkObservation AntiPattern.plustan17 731 13 40

  it "transitive index value binding still triggers" $
    checkObservation AntiPattern.plustan17 744 13 27

  it "indexing via (!!) operator triggers" $
    checkObservation AntiPattern.plustan17 754 13 41

  it "(!!) operator suppressed via marker" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan17 765

  it "drop-style indexing triggers" $
    checkObservation AntiPattern.plustan17 780 34 52

plustan18Spec :: Analysis -> Spec
plustan18Spec analysis = describe "PLU-STAN-18" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "flags (&&) in if predicate with failing else" $
    checkObservation AntiPattern.plustan18 797 11 13

  it "flags (&&) in if predicate with failing then" $
    checkObservation AntiPattern.plustan18 805 11 13

  it "does not flag when branches do not fail" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan18 813

  it "does not flag (&&) inside branch body" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan18 824

  it "flags (&&) in BI.ifThenElse predicate with failing branch" $
    checkObservation AntiPattern.plustan18 829 23 25

  it "nested (&&) only flags the outermost" $ do
    checkObservation AntiPattern.plustan18 835 11 13
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan18 836

  it "flags (&&) inside predicate helper with failing branch" $
    checkObservation AntiPattern.plustan18 843 4 6
