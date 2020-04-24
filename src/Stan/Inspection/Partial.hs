{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains all 'Inspection's for partial functions.
-}

module Stan.Inspection.Partial
    ( -- * Partial 'Inspection's
      -- *** Partial 'GHC.List.head'
      stan0001
      -- *** Partial 'GHC.List.tail'
    , stan0002
      -- *** Partial 'GHC.List.init'
    , stan0003
      -- *** Partial 'GHC.List.last'
    , stan0004
      -- *** Partial 'GHC.List.!!'
    , stan0005
      -- *** Partial 'GHC.List.cycle'
    , stan0006
      -- *** Partial 'Data.OldList.genericIndex'
    , stan0007
      -- *** Partial 'Data.Maybe.fromJust'
    , stan0008
      -- *** Partial 'Text.Read.read'
    , stan0009

      -- * List of all partial 'Inspection's
    , partialInspectionsMap
    ) where

import Relude.Extra.Tuple (mapToFst)

import Stan.Category (partial)
import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..), InspectionAnalysis (..), InspectionsMap)
import Stan.NameMeta (NameMeta (..), mkBaseListMeta, mkBaseOldListMeta)
import Stan.Severity (Severity (..))


-- | All partial 'Inspection's.
partialInspectionsMap :: InspectionsMap
partialInspectionsMap = fromList $ map (mapToFst inspectionId)
    [ stan0001
    , stan0002
    , stan0003
    , stan0004
    , stan0005
    , stan0006
    , stan0007
    , stan0008
    , stan0009
    ]

-- | Smart constructor to create partial 'Inspection'.
mkPartialInspection :: Id Inspection -> NameMeta -> Inspection
mkPartialInspection insId nameMeta@NameMeta{..} = Inspection
    { inspectionId = insId
    , inspectionName = "Partial: " <> nameMetaPackage <> "/" <> nameMetaName
    , inspectionDescription = "Usage of partial function '" <> nameMetaName <> "' for lists"
    , inspectionSolution = []
    , inspectionCategory = one partial
    , inspectionSeverity = Warning
    , inspectionAnalysis = FindName nameMeta
    }

{- | Smart constructor to create partial 'Inspection' for functions
that work with lists.
-}
mkPartialInspectionList :: Id Inspection -> NameMeta -> Inspection
mkPartialInspectionList insId nameMeta = (mkPartialInspection insId nameMeta)
    { inspectionSolution =
        [ "Replace list with 'NonEmpty' from 'Data.List.NonEmpty'"
        , "Use explicit pattern-matching over lists"
        ]
    }

-- | 'Inspection' for 'stan0001' — partial 'GHC.List.head' @STAN-0001@.
stan0001 :: Inspection
stan0001 = mkPartialInspectionList (Id "STAN-0001") (mkBaseListMeta "head")

-- | 'Inspection' for 'stan0002' — partial 'GHC.List.tail' @STAN-0002@.
stan0002 :: Inspection
stan0002 = mkPartialInspectionList (Id "STAN-0002") (mkBaseListMeta "tail")

-- | 'Inspection' for 'stan0003' — partial 'GHC.List.init' @STAN-0003@.
stan0003 :: Inspection
stan0003 = mkPartialInspectionList (Id "STAN-0003") (mkBaseListMeta "init")

-- | 'Inspection' for 'stan0004' — partial 'GHC.List.last' @STAN-0004@.
stan0004 :: Inspection
stan0004 = mkPartialInspectionList (Id "STAN-0004") (mkBaseListMeta "last")

-- | 'Inspection' for 'stan0005' — partial 'GHC.List.!!' @STAN-0005@.
stan0005 :: Inspection
stan0005 = mkPartialInspection (Id "STAN-0005") (mkBaseListMeta "!!")

-- | 'Inspection' for 'stan0006' — partial 'GHC.List.cycle' @STAN-0006@.
stan0006 :: Inspection
stan0006 = mkPartialInspectionList (Id "STAN-0006") (mkBaseListMeta "cycle")

-- | 'Inspection' for 'stan0007' — partial 'Data.OldList.genericIndex' @STAN-0007@.
stan0007 :: Inspection
stan0007 = mkPartialInspection (Id "STAN-0007") (mkBaseOldListMeta "genericIndex")

-- | 'Inspection' for 'stan0008' — partial 'Data.Maybe.fromJust' @STAN-0008@.
stan0008 :: Inspection
stan0008 = Inspection
    { inspectionId = Id "STAN-0008"
    , inspectionName = "Partial: " <> nameMetaPackage <> "/" <> nameMetaName
    , inspectionDescription = "Usage of partial function '" <> nameMetaName <> "' for 'Maybe'"
    , inspectionSolution =
        [ "Use explicit pattern-matching over Maybe"
        , "Use one of the standard functions: 'maybe', 'fromMaybe'"
        ]
    , inspectionCategory = one partial
    , inspectionSeverity = Warning
    , inspectionAnalysis = FindName nameMeta
    }
  where
    nameMeta :: NameMeta
    nameMeta@NameMeta{..} = NameMeta
        { nameMetaName       = "fromJust"
        , nameMetaModuleName = "Data.Maybe"
        , nameMetaPackage    = "base"
        }

-- | 'Inspection' for 'stan0008' — partial 'Data.Maybe.fromJust' @STAN-0008@.
stan0009 :: Inspection
stan0009 = Inspection
    { inspectionId = Id "STAN-0009"
    , inspectionName = "Partial: " <> nameMetaPackage <> "/" <> nameMetaName
    , inspectionDescription = "Usage of partial parsing function '" <> nameMetaName <> "'"
    , inspectionSolution =
        [ "Use 'readMaybe' or 'readEither' to handle failed parsing"
        ]
    , inspectionCategory = one partial
    , inspectionSeverity = Warning
    , inspectionAnalysis = FindName nameMeta
    }
  where
    nameMeta :: NameMeta
    nameMeta@NameMeta{..} = NameMeta
        { nameMetaName       = "read"
        , nameMetaModuleName = "Text.Read"
        , nameMetaPackage    = "base"
        }
