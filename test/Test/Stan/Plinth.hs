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
import Stan.Inspection.AntiPattern (plustan04)
import Stan.Inspection.Partial (stan0001)
import Stan.Observation (Observation (..), observationFingerprint)
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
            [fp1, fp2] -> do
                fp1 `shouldBe` observationFingerprint o1
                fp2 `shouldBe` (fp1 <> "#2")
            _ -> expectationFailure "expected exactly two fingerprints"
    it "dedupes observations that report the same rule at the exact same span" $ do
        let content = BS8.pack "f x\n"
        let o1 = mkPayloadObs 1 content
        let o2 = o1 { observationId = Id "obs-1-again" }
        let o3 = o1 { observationId = Id "obs-1-yet-again" }
        let pairs = uniquifyFingerprints [o1, o2, o3]
        case pairs of
            [(o, fp)] -> do
                o `shouldBe` o1
                fp `shouldBe` observationFingerprint o1
            _ -> expectationFailure ("expected exactly one deduped pair, got " <> show (length pairs))
    it "analyze payload is version 2 with top-level observations" $ do
        let payloadText = LBS8.unpack (encode (mkAnalyzePayload Nothing [] []))
        payloadText `shouldSatisfy` isInfixOf "\"version\":2"
        payloadText `shouldSatisfy` isInfixOf "\"observations\":[]"
    it "golden: non-empty analyze payload encoding is byte-stable" $ do
        let content = BS8.pack "f x\nf x\n"
        let obs = mkPayloadObs 1 content
        let payloadText = LBS8.unpack (encode
                (mkAnalyzePayload (Just "Test.Module.Name") [stan0001] [obs]))
        payloadText `shouldBe` goldenAnalyzePayload
    it "enriches PLU-STAN inspections with teaching docs fields" $ do
        let payloadText = LBS8.unpack (encode (mkAnalyzePayload Nothing [plustan04] []))
        payloadText `shouldSatisfy` isInfixOf "\"whyItMatters\":"
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

    -- Derived from a reference run of microaeson's canonical (alphabetically
    -- keyed) encoding; pins the schema-v2 wire contract byte for byte.
    goldenAnalyzePayload :: String
    goldenAnalyzePayload =
        "{\"inspections\":[{\"category\":[\"Partial\",\"List\"],\"description\":\"Usage of partial \
        \function 'head' for lists\",\"id\":\"STAN-0001\",\"name\":\"Partial: base/head\",\"severit\
        \y\":\"Warning\",\"solution\":[\"Replace list with 'NonEmpty' from 'Data.List.NonEmpty'\",\
        \\"Use explicit pattern-matching over lists\"]}],\"observations\":[{\"endCol\":4,\"endLine\
        \\":1,\"file\":\"src/T.hs\",\"fingerprint\":\"FPR-STAN-0001-bbjjJQ-NmTLVWB5C9\",\"id\":\"ob\
        \s-1\",\"inspectionId\":\"STAN-0001\",\"moduleName\":\"Test.Module.Name\",\"startCol\":1,\"\
        \startLine\":1}],\"runScope\":\"module\",\"targetModule\":\"Test.Module.Name\",\"version\":\
        \2}"
