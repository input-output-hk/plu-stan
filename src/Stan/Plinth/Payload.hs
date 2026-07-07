{- |
Copyright: (c) 2026 IOHK
SPDX-License-Identifier: MPL-2.0

Assembly of the machine-readable (schema v2) payloads emitted by the
@plustan@ CLI for the VS Code extension. Kept in the library so the
JSON shape is golden-testable.
-}

module Stan.Plinth.Payload
    ( analyzeSchemaVersion
    , mkAnalyzePayload
    , mkCapabilitiesPayload
    , uniquifyFingerprints
    ) where

import Data.Aeson.Micro (Value, object, (.=))

import Stan.Core.ModuleName (ModuleName)
import Stan.Ghc.Compat (srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine)
import Stan.Inspection (Inspection (..))
import Stan.Observation (Observation (..), observationFingerprint)
import Stan.Plinth.Docs (InspectionDocs (..), lookupDocs)

import qualified Data.HashMap.Strict as HM


-- | Version of the JSON contract between the CLI and the extension.
analyzeSchemaVersion :: Int
analyzeSchemaVersion = 2

mkCapabilitiesPayload :: Text -> Value
mkCapabilitiesPayload ghcVersion = object
    [ "schemaVersion" .= analyzeSchemaVersion
    , "ghcVersion" .= ghcVersion
    , "features" .= (["list-onchain", "analyze", "fingerprints", "inspection-docs"] :: [Text])
    ]

mkAnalyzePayload :: Maybe ModuleName -> [Inspection] -> [Observation] -> Value
mkAnalyzePayload targetModule inspections observations = object
    [ "version" .= analyzeSchemaVersion
    , "runScope" .= maybe ("all" :: Text) (const "module") targetModule
    , "targetModule" .= targetModule
    , "inspections" .= map inspectionToJson inspections
    , "observations" .= map observationToJson (uniquifyFingerprints observations)
    ]

{- | Pair each observation with a run-unique fingerprint: duplicates
(same rule, same module, same flagged text) get @#2@, @#3@… suffixes
in span order, so dismissing one of two identical findings never
dismisses both.
-}
uniquifyFingerprints :: [Observation] -> [(Observation, Text)]
uniquifyFingerprints observations =
    reverse $ fst $ foldl' step ([], HM.empty) sorted
  where
    sorted :: [Observation]
    sorted = sortOn spanKey observations

    spanKey :: Observation -> (FilePath, Int, Int)
    spanKey o =
        ( observationFile o
        , srcSpanStartLine (observationSrcSpan o)
        , srcSpanStartCol (observationSrcSpan o)
        )

    step
        :: ([(Observation, Text)], HashMap Text Int)
        -> Observation
        -> ([(Observation, Text)], HashMap Text Int)
    step (acc, seen) o =
        let base = observationFingerprint o
            n = HM.lookupDefault 0 base seen + 1
            fp = if n == 1 then base else base <> "#" <> show n
        in ((o, fp) : acc, HM.insert base n seen)

inspectionToJson :: Inspection -> Value
inspectionToJson Inspection{..} = object $
    [ "id" .= inspectionId
    , "name" .= inspectionName
    , "description" .= inspectionDescription
    , "solution" .= inspectionSolution
    , "category" .= toList inspectionCategory
    , "severity" .= inspectionSeverity
    ] <> docsPairs
  where
    docsPairs = case lookupDocs inspectionId of
        Nothing -> []
        Just InspectionDocs{..} ->
            [ "whyItMatters" .= docsWhyItMatters
            , "badExample" .= docsBadExample
            , "goodExample" .= docsGoodExample
            , "docsAnchor" .= docsAnchor
            ]

observationToJson :: (Observation, Text) -> Value
observationToJson (Observation{..}, fingerprint) = object
    [ "id" .= observationId
    , "inspectionId" .= observationInspectionId
    , "fingerprint" .= fingerprint
    , "startLine" .= srcSpanStartLine observationSrcSpan
    , "startCol" .= srcSpanStartCol observationSrcSpan
    , "endLine" .= srcSpanEndLine observationSrcSpan
    , "endCol" .= srcSpanEndCol observationSrcSpan
    , "file" .= toText observationFile
    , "moduleName" .= observationModuleName
    ]
