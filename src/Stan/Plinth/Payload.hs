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
    , dedupeObservations
    ) where

import Data.Aeson.Micro (Value, object, (.=))

import Stan.Core.Id (unId)
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

Observations are first deduplicated (see 'dedupeObservations') and
sorted by a total order (file, full span, inspection id), so both the
suffix assignment and the payload's observation order are canonical
regardless of upstream traversal order.
-}
uniquifyFingerprints :: [Observation] -> [(Observation, Text)]
uniquifyFingerprints observations =
    reverse $ fst $ foldl' step ([], HM.empty) (dedupeObservations observations)
  where
    step
        :: ([(Observation, Text)], HashMap Text Int)
        -> Observation
        -> ([(Observation, Text)], HashMap Text Int)
    step (acc, seen) o =
        let base = observationFingerprint o
            n = HM.lookupDefault 0 base seen + 1
            fp = if n == 1 then base else base <> "#" <> show n
        in ((o, fp) : acc, HM.insert base n seen)

{- | Sort observations into the canonical @(file, span, inspection id)@
order, then collapse observations that report the exact same rule for
the exact same source span down to one.

Several upstream inspections walk the HIE AST in a way that visits the
same syntactic construct more than once (e.g. once while matching the
whole rewrite pattern and again while re-examining one of its
sub-terms, or once per overlapping traversal branch that both happen
to land on the same node) and end up calling 'mkObservation' several
times for an identical @(inspectionId, file, span)@ triple. That is
one finding reported redundantly, not several distinct ones, so only
the first occurrence — the one that sorts first in the canonical order
— is kept.

This must run /before/ fingerprint-suffix assignment in
'uniquifyFingerprints': two observations that flag the same text but
sit at genuinely different locations have different spans, so
'dedupeObservations' leaves both, and they still get distinct @#2@,
@#3@… suffixes downstream.
-}
dedupeObservations :: [Observation] -> [Observation]
dedupeObservations = dedupAdjacent . sortOn spanKey
  where
    dedupAdjacent :: [Observation] -> [Observation]
    dedupAdjacent [] = []
    dedupAdjacent (o : os) = o : go (spanKey o) os
      where
        go :: (FilePath, Int, Int, Int, Int, Text) -> [Observation] -> [Observation]
        go _ [] = []
        go prevKey (x : xs)
            | spanKey x == prevKey = go prevKey xs
            | otherwise = x : go (spanKey x) xs

spanKey :: Observation -> (FilePath, Int, Int, Int, Int, Text)
spanKey o =
    ( observationFile o
    , srcSpanStartLine (observationSrcSpan o)
    , srcSpanStartCol (observationSrcSpan o)
    , srcSpanEndLine (observationSrcSpan o)
    , srcSpanEndCol (observationSrcSpan o)
    , unId (observationInspectionId o)
    )

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
