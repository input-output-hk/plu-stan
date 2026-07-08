{-# LANGUAGE CPP #-}

module Stan.Hie.Compat904
#if __GLASGOW_HASKELL__ == 904 || __GLASGOW_HASKELL__ == 906 || __GLASGOW_HASKELL__ == 908 || __GLASGOW_HASKELL__== 910 || __GLASGOW_HASKELL__== 912
    ( -- * Main HIE types
      ContextInfo (..)
    , HieArgs (..)
    , HieAST (..)
    , HieASTs (..)
    , HieFile (..)
    , HieType (..)
    , HieTypeFlat
    , IEType (..)
    , Identifier
    , IdentifierDetails (..)
    , NodeInfo (..)
    , TypeIndex
    , Stan.Hie.Compat904.DeclType (..)
    , hFunTy2
    , conDec
    , eqDeclType
    , Stan.Hie.Compat904.NodeAnnotation
    , mkNodeAnnotation
    , toNodeAnnotation

      -- * Binary interface to hie files
    , HieFileResult (hie_file_result)
    , readHieFileWithNameCache
    , nodeInfo
    ) where

import GHC.Iface.Ext.Binary (HieFileResult (hie_file_result), HieHeader, readHieFileWithVersion)
import GHC.Iface.Ext.Types
                 (ContextInfo (..), DeclType (..), HieAST (..), HieASTs (..), HieArgs (..),
                 HieFile (..), HieType (..), HieTypeFlat, IEType (..), Identifier,
                 IdentifierDetails (..), NodeInfo (..), TypeIndex,
                 getSourcedNodeInfo, NodeAnnotation(..))
import GHC.Iface.Ext.Utils (emptyNodeInfo)
import GHC.Types.Name.Cache (initNameCache)
import GHC.Data.FastString (FastString)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

import Data.Version (showVersion)
import System.Info (compilerVersion)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import qualified Data.Set as S

import Text.Show (show)

-- This is a direct copy of GHC.Iface.Ext.Utils.emptyNodeInfo except
-- we're using our own redefined combineNodeInfo.
nodeInfo :: Ord a => HieAST a -> NodeInfo a
nodeInfo = foldl' combineNodeInfo emptyNodeInfo . getSourcedNodeInfo . sourcedNodeInfo

-- This is a direct copy of GHC.Iface.Ext.Utils.combineNodeInfo except
-- we use compare rather than nonDetCmpType.
combineNodeInfo :: Ord a => NodeInfo a -> NodeInfo a -> NodeInfo a
(NodeInfo as ai ad) `combineNodeInfo` (NodeInfo bs bi bd) =
  NodeInfo (S.union as bs) (mergeSorted ai bi) (Map.unionWith (<>) ad bd)
  where
    mergeSorted :: Ord b => [b] -> [b] -> [b]
    mergeSorted lc@(c:cs) ld@(d:ds) = case compare c d of
                                        LT -> c : mergeSorted cs ld
                                        EQ -> c : mergeSorted cs ds
                                        GT -> d : mergeSorted lc ds
    mergeSorted cs [] = cs
    mergeSorted [] ds = ds

mkNodeAnnotation :: FastString
                 -> FastString
                 -> Stan.Hie.Compat904.NodeAnnotation
mkNodeAnnotation f1 f2 =
  Stan.Hie.Compat904.NodeAnnotation (GHC.Iface.Ext.Types.NodeAnnotation f1 f2)

newtype NodeAnnotation = NodeAnnotation GHC.Iface.Ext.Types.NodeAnnotation
  deriving stock (Eq, Ord)

instance Show Stan.Hie.Compat904.NodeAnnotation where
  show
    (Stan.Hie.Compat904.NodeAnnotation (GHC.Iface.Ext.Types.NodeAnnotation a1 a2)) =
    Text.Show.show (a1, a2)

toNodeAnnotation :: GHC.Iface.Ext.Types.NodeAnnotation
                 -> Stan.Hie.Compat904.NodeAnnotation
toNodeAnnotation = Stan.Hie.Compat904.NodeAnnotation

-- For forward compatibility: the two-argument function type
-- constructor.
hFunTy2 :: HieType b -> Maybe (b, b)
hFunTy2 t = case t of
  HFunTy _multiplicity i1 i2 -> Just (i1, i2)
  _ -> Nothing

readHieFileWithNameCache :: IO (FilePath -> IO HieFileResult)
readHieFileWithNameCache = do
    nameCache <- initNameCache 'z' []
    pure $ \file -> do
        result <- readHieFileWithVersion isCompatibleHieVersion nameCache file
        case result of
            Right hieFileResult -> pure hieFileResult
            Left (fileHieVersion, fileGhcVersion) -> error $ toText $ mconcat
                [ "Stan cannot read the .hie file '", file, "': it was produced by GHC "
                , BS8.unpack fileGhcVersion, " (.hie format version "
                , Text.Show.show fileHieVersion
                , "), which belongs to a different GHC major.minor series than the compiler"
                , " Stan was built with (GHC ", showVersion compilerVersion
                , "). Rebuild Stan with a GHC from the same series to analyse these files."
                ]

-- | Predicate for 'readHieFileWithVersion': accept a @.hie@ file when the GHC
-- that produced it shares the major.minor series of the compiler that built
-- Stan (e.g. a 9.6.x build reads any 9.6.y file).
--
-- GHC bumps the integer @.hie@-file version on /every/ release, patch releases
-- included — @hieVersion = read (cProjectVersionInt ++ cProjectPatchLevel)@ in
-- "GHC.Iface.Ext.Types" — even when the on-disk format is unchanged. The stock
-- 'GHC.Iface.Ext.Binary.readHieFile' compares that integer exactly, so a build
-- made with GHC 9.6.6 refuses a GHC-9.6.7 @.hie@ file. Patch releases do not
-- change the on-disk @.hie@ format, so matching on the major.minor series lets
-- one Stan build read every patch release in its series while still rejecting
-- files from other series, whose format genuinely may differ.
isCompatibleHieVersion :: HieHeader -> Bool
isCompatibleHieVersion (_fileHieVersion, fileGhcVersion) =
    majorMinor (BS8.unpack fileGhcVersion) == majorMinor (showVersion compilerVersion)
  where
    -- First two version components, so "9.6.7" and "9.6" both give ["9","6"].
    -- String-based to stay robust to multi-digit patch levels.
    majorMinor :: String -> [String]
    majorMinor = take 2 . splitOnDot

    splitOnDot :: String -> [String]
    splitOnDot s = case break (== '.') s of
        (component, '.' : rest) -> component : splitOnDot rest
        (component, _)          -> [component]

newtype DeclType = DeclType GHC.Iface.Ext.Types.DeclType
  deriving stock Eq

instance Show Stan.Hie.Compat904.DeclType where
  show (DeclType d) = Text.Show.show (showSDocUnsafe (ppr d))

conDec :: Stan.Hie.Compat904.DeclType
conDec = DeclType ConDec

eqDeclType :: Stan.Hie.Compat904.DeclType -> GHC.Iface.Ext.Types.DeclType -> Bool
eqDeclType (DeclType d1) d2 = d1 == d2
#else
  () where
#endif
