module Test.Stan.Plinth
    ( plinthSpec
    ) where

import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

import Stan.Core.Id (Id (..))
import Stan.Inspection.All (inspectionsMap)
import Stan.Plinth.Docs (InspectionDocs (..), lookupDocs)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text


plinthSpec :: Spec
plinthSpec = describe "Stan.Plinth.Docs" $ do
    it "covers every PLU-STAN inspection" $ do
        let plinthIds = filter (Text.isPrefixOf "PLU-STAN" . unId) (HM.keys inspectionsMap)
        let missing = filter (isNothing . lookupDocs) plinthIds
        missing `shouldBe` []
    it "has non-empty teaching content everywhere" $
        case lookupDocs (Id "PLU-STAN-04") of
            Nothing -> expectationFailure "no docs for PLU-STAN-04"
            Just InspectionDocs{..} -> do
                docsWhyItMatters `shouldSatisfy` (not . Text.null)
                docsBadExample `shouldSatisfy` (not . Text.null)
                docsGoodExample `shouldSatisfy` (not . Text.null)
