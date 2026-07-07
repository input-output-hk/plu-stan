module Test.Stan.Plinth
    ( plinthSpec
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Stan.Core.Id (Id (..))
import Stan.Inspection.All (inspectionsMap)
import Stan.Plinth.Docs (InspectionDocs (..), lookupDocs, plinthDocsMap)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text


plinthSpec :: Spec
plinthSpec = describe "Stan.Plinth.Docs" $ do
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
