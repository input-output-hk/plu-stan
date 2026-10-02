module Test.Stan.Analysis.Research (analysisResearchSpec) where

import Stan.Analysis (Analysis)
import qualified Stan.Inspection.AntiPattern as AntiPattern
import Test.Hspec (Spec, describe, it)
import Test.Stan.Analysis.Common (observationAssert, noObservationAssert)

analysisResearchSpec :: Analysis -> Spec
analysisResearchSpec analysis = describe "Research accepting paths" $ do
    it "reviewReferenceBypass" $
        observationAssert ["Research"] analysis AntiPattern.plustan30 546 1 22
    it "reviewReferenceCaseGood" $
        noObservationAssert ["Research"] analysis AntiPattern.plustan30 590
    it "reviewReferenceRejectingOr" $
        noObservationAssert ["Research"] analysis AntiPattern.plustan30 595
    it "reviewDatumBypass" $
        observationAssert ["Research"] analysis AntiPattern.plustan31 551 1 18
    it "reviewDatumCaseGood" $
        noObservationAssert ["Research"] analysis AntiPattern.plustan31 600
    it "reviewStakeBypass" $
        observationAssert ["Research"] analysis AntiPattern.plustan29 558 1 18
    it "reviewStakeCaseGood" $
        noObservationAssert ["Research"] analysis AntiPattern.plustan29 607
    it "reviewSubtractWrong" $
        observationAssert ["Research"] analysis AntiPattern.plustan35 564 1 20
    it "reviewSubtractGood" $
        noObservationAssert ["Research"] analysis AntiPattern.plustan35 569
    it "reviewCaseGuards" $
        observationAssert ["Research"] analysis AntiPattern.plustan35 575 1 17
    it "reviewCaseGuardsGood" $
        noObservationAssert ["Research"] analysis AntiPattern.plustan35 612
    it "reviewCaseRejectingGuard" $
        noObservationAssert ["Research"] analysis AntiPattern.plustan35 619
    it "reviewRedeemerWrongBranch" $
        observationAssert ["Research"] analysis AntiPattern.plustan33 583 1 26
    it "reviewRedeemerBranchGood" $
        noObservationAssert ["Research"] analysis AntiPattern.plustan33 626
    it "reviewRedeemerGuardBypass" $
        observationAssert ["Research"] analysis AntiPattern.plustan33 631 1 26
    it "reviewRedeemerRejectScript" $
        noObservationAssert ["Research"] analysis AntiPattern.plustan33 638
