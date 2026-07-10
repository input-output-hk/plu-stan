module Test.Stan.Observation
    ( observationSpec
    ) where

import Stan.Ghc.Compat (mkRealSrcLoc, mkRealSrcSpan)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

import Stan.Core.Id (Id (..))
import Stan.Inspection (inspectionId)
import Stan.Inspection.Partial (stan0001)
import Stan.Observation (Observation (..), mkObservationId, observationFingerprint,
                         observationSpanText)

import qualified Data.ByteString.Char8 as BS8


observationSpec :: Spec
observationSpec = describe "Observation" $ do
    it "calculates Observation Id properly" $
        testObservationId `shouldBe` Id "OBS-STAN-0001-bbjjJQ-10:42"

    describe "observationSpanText" $ do
        -- line 2 is "bad expr here"; cols 5–14 (GHC end col is exclusive)
        -- select the 9 characters "expr here"
        it "extracts a single-line span, column-trimmed" $
            observationSpanText (mkObs 2 5 14 sampleContent)
                `shouldBe` "expr here"
        it "extracts a multi-line span" $
            observationSpanText (mkObs2 2 5 3 6 sampleContent)
                `shouldBe` "expr here\ny = 2"
        it "trims a single-line span ending mid-line (exclusive end col)" $
            observationSpanText (mkObs 2 5 9 sampleContent)
                `shouldBe` "expr"
        it "trims a multi-line span ending mid-line" $
            observationSpanText (mkObs2 2 5 3 2 sampleContent)
                `shouldBe` "expr here\ny"

    describe "observationFingerprint" $ do
        it "is stable when the same flagged text moves to another line" $
            observationFingerprint (mkObs 2 5 14 sampleContent)
                `shouldBe` observationFingerprint (mkObs 3 5 14 shiftedContent)
        it "changes when the flagged text changes" $
            observationFingerprint (mkObs 2 5 14 sampleContent)
                `shouldNotBe` observationFingerprint (mkObs 2 5 14 editedContent)
  where
    testObservationId :: Id Observation
    testObservationId = mkObservationId
        (inspectionId stan0001)
        "Test.Module.Name"
        $ mkRealSrcSpan
            (mkRealSrcLoc "src/Test/Module/Name.hs" 10 42)
            (mkRealSrcLoc "src/Test/Module/Name.hs" 10 42)

    -- line 1: "x = 1", line 2: "bad expr here", line 3: "y = 2"
    sampleContent, shiftedContent, editedContent :: ByteString
    sampleContent  = BS8.pack "x = 1\nbad expr here\ny = 2\n"
    -- same flagged text, one line lower (a comment was added above)
    shiftedContent = BS8.pack "-- c\nx = 1\nbad expr here\ny = 2\n"
    -- the flagged text itself changed
    editedContent  = BS8.pack "x = 1\nbad EXPR here\ny = 2\n"

    mkObs :: Int -> Int -> Int -> ByteString -> Observation
    mkObs line startC endC = mkObs2 line startC line endC

    mkObs2 :: Int -> Int -> Int -> Int -> ByteString -> Observation
    mkObs2 startL startC endL endC content = Observation
        { observationId = Id "test-obs"
        , observationInspectionId = inspectionId stan0001
        , observationSrcSpan = mkRealSrcSpan
            (mkRealSrcLoc "src/T.hs" startL startC)
            (mkRealSrcLoc "src/T.hs" endL endC)
        , observationFile = "src/T.hs"
        , observationModuleName = "Test.Module.Name"
        , observationFileContent = content
        }
