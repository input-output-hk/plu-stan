module Test.Stan.Plinth
    ( plinthSpec
    ) where

import Data.Aeson.Micro (encode, object, (.=))
import Data.List (isInfixOf)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

import Stan.Core.Id (Id (..))
import Stan.Ghc.Compat (mkRealSrcLoc, mkRealSrcSpan)
import Stan.Inspection (inspectionId)
import Stan.Inspection.All (inspectionsMap)
import Stan.Inspection.Partial (stan0001)
import Stan.Observation (Observation (..))
import Stan.Plinth.Docs (InspectionDocs (..), lookupDocs, plinthDocsMap)
import Stan.Plinth.Payload (mkAnalyzePayload, mkCapabilitiesPayload, uniquifyFingerprints)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text


plinthSpec :: Spec
plinthSpec = docsSpec >> payloadSpec

docsSpec :: Spec
docsSpec = describe "Stan.Plinth.Docs" $ do
    it "covers every PLU-STAN inspection" $ do
        let plinthIds = filter (Text.isPrefixOf "PLU-STAN" . unId) (HM.keys inspectionsMap)
        let missing = filter (isNothing . lookupDocs) plinthIds
        missing `shouldBe` []
    it "has no stale docs for removed or renamed inspections" $ do
        let stale = filter (\insId -> not (HM.member insId inspectionsMap))
                (HM.keys plinthDocsMap)
        stale `shouldBe` []
    it "has non-empty teaching content everywhere" $ do
        let offenders =
                [ unId insId
                | (insId, InspectionDocs{..}) <- HM.toList plinthDocsMap
                , Text.null docsWhyItMatters
                    || Text.null docsBadExample
                    || Text.null docsGoodExample
                ]
        offenders `shouldBe` []

payloadSpec :: Spec
payloadSpec = describe "Stan.Plinth.Payload" $ do
    it "capabilities carries schemaVersion 2" $
        mkCapabilitiesPayload "9.6.6" `shouldBe` object
            [ "schemaVersion" .= (2 :: Int)
            , "ghcVersion" .= ("9.6.6" :: Text)
            , "features" .= (["list-onchain", "analyze", "fingerprints", "inspection-docs"] :: [Text])
            ]
    it "suffixes duplicate fingerprints in span order" $ do
        let content = BS8.pack "f x\nf x\n"      -- identical flagged text on 2 lines
        let o1 = mkPayloadObs 1 content
        let o2 = mkPayloadObs 2 content
        let fps = map snd (uniquifyFingerprints [o2, o1])   -- input deliberately unordered
        case fps of
            [fp1, fp2] -> fp2 `shouldBe` (fp1 <> "#2")
            _ -> expectationFailure "expected exactly two fingerprints"
    it "analyze payload is version 2 with top-level observations" $ do
        let payloadText = LBS8.unpack (encode (mkAnalyzePayload Nothing [] []))
        payloadText `shouldSatisfy` isInfixOf "\"version\":2"
        payloadText `shouldSatisfy` isInfixOf "\"observations\":[]"
  where
    mkPayloadObs :: Int -> ByteString -> Observation
    mkPayloadObs line content = Observation
        { observationId = Id ("obs-" <> show line)
        , observationInspectionId = inspectionId stan0001
        , observationSrcSpan = mkRealSrcSpan
            (mkRealSrcLoc "src/T.hs" line 1)
            (mkRealSrcLoc "src/T.hs" line 4)
        , observationFile = "src/T.hs"
        , observationModuleName = "Test.Module.Name"
        , observationFileContent = content
        }
