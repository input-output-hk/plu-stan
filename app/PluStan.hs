module Main (main) where

import Colourista (errorMessage, infoMessage, successMessage, warningMessage)
import Control.Exception (SomeException, handle)
import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, mapMaybe)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.Process (callProcess)
import Trial (withTag, whenResult_)

import Stan (getAnalysis)
import Stan.Analysis (Analysis (..))
import Stan.Analysis.Pretty (isPlinthObservation, prettyShowAnalysis)
import Stan.Browse (openBrowser)
import Stan.Cabal (usedCabalFiles)
import Stan.Cli (StanArgs (..))
import Stan.Config (Check (..), CheckFilter (..), CheckType (..), Config, ConfigP (..),
                    PartialConfig, Scope (..), finaliseConfig)
import Stan.EnvVars (envVarsToText, getEnvVars)
import Stan.Hie (readHieFiles)
import Stan.Hie.Compat (HieFile (..))
import Stan.Info (ProjectInfo (..), StanEnv (..))
import Stan.Inspection (Inspection (..))
import Stan.Inspection.All (getInspectionById)
import Stan.FileInfo (FileInfo (..))
import Stan.Observation (Observation (..))
import Stan.Report (generateReport)
import Stan.Report.Settings (OutputSettings (..), ToggleSolution (..), Verbosity (..))
import Stan.Severity (Severity (..))

import qualified Stan.Category as Category
import qualified Slist as Slist
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import GHC (getSessionDynFlags, runGhc, setSessionDynFlags)
import GHC.Driver.Session (targetProfile)
import GHC.Iface.Binary (CheckHiWay (IgnoreHiWay), TraceBinIFace (QuietBinIFace), readBinIface)
import GHC.Iface.Syntax (IfaceAnnotation (..))
import GHC.Paths (libdir)
import GHC.Platform.Profile (Profile)
import GHC.Serialized (deserializeWithData, fromSerialized)
import GHC.Types.Annotations (AnnTarget (..))
import GHC.Types.Name.Cache (NameCache, initNameCache)
import GHC.Unit.Module.ModIface (ModIface, mi_anns, mi_module)


main :: IO ()
main = setLocaleEncoding utf8 >> runPluStan

runPluStan :: IO ()
runPluStan =
  whenResult_ (finaliseConfig pluStanConfig) $ \warnings config -> do
    cli <- parsePluStanArgs
    let hieDir = plustanHieDir cli
    ensureHieFiles hieDir
    hieFiles <- readHieFiles hieDir
    when (null hieFiles) $ do
      warningMessage "No .hie files found after build. Ensure the project is compiled with -fwrite-ide-info and -hiedir=.hie."
      exitFailure
    analysis <- getAnalysis (stanArgs hieDir) True config hieFiles >>= removeOffchain hieDir hieFiles
    let observations = analysisObservations analysis
    if null observations
      then successMessage "All clean! Plu-Stan did not find any observations at the moment."
      else warningMessage "Plu-Stan found the following observations:\n"
    Text.putStrLn $ prettyShowAnalysis analysis outputSettings
    when (plustanReport cli) $
      generatePluStanReport cli config warnings hieFiles analysis
    when (any ((>= Error) . getObservationSeverity) observations) exitFailure
  where
    outputSettings :: OutputSettings
    outputSettings = OutputSettings
      { outputSettingsVerbosity = Verbose
      , outputSettingsSolutionVerbosity = ShowSolution
      }

    stanArgs :: FilePath -> StanArgs
    stanArgs hieDir = StanArgs
      { stanArgsHiedir = hieDir
      , stanArgsCabalFilePath = []
      , stanArgsOutputSettings = outputSettings
      , stanArgsReport = Nothing
      , stanArgsUseDefaultConfigFile = withTag "PluStan" (pure False)
      , stanArgsConfigFile = Nothing
      , stanArgsConfig = pluStanConfig
      , stanArgsJsonOut = False
      }

    getObservationSeverity :: Observation -> Severity
    getObservationSeverity = inspectionSeverity . getInspectionById . observationInspectionId

generatePluStanReport
  :: PluStanArgs
  -> Config
  -> [Text]
  -> [HieFile]
  -> Analysis
  -> IO ()
generatePluStanReport PluStanArgs{..} config warnings hieFiles analysis = do
  env <- getEnvVars
  seCliArgs <- getArgs
  piName <- takeFileName <$> getCurrentDirectory
  piCabalFiles <- usedCabalFiles []
  let piHieDir = plustanHieDir
  let piFileNumber = length hieFiles
  let stanEnv = StanEnv
        { seEnvVars = envVarsToText env
        , seTomlFiles = []
        , ..
        }
  generateReport analysis config warnings stanEnv ProjectInfo{..}
  infoMessage "Report is generated here -> stan.html"
  when plustanBrowse $ openBrowser "stan.html"

data PluStanArgs = PluStanArgs
  { plustanReport :: Bool
  , plustanBrowse :: Bool
  , plustanHieDir :: FilePath
  }

parsePluStanArgs :: IO PluStanArgs
parsePluStanArgs = do
  args <- getArgs
  case go defaultArgs args of
    Left err -> do
      errorMessage $ Text.pack err
      Text.putStrLn usage
      exitFailure
    Right parsed -> pure parsed
  where
    defaultArgs = PluStanArgs False False ".hie"
    usage = Text.unlines
      [ "Usage: plustan [--report] [--browse] [--hiedir DIR]"
      , "  --report      Generate stan.html report"
      , "  --browse      Open report in browser (implies --report)"
      , "  --hiedir DIR  Directory with .hie/.hi files (default: .hie)"
      ]

    go :: PluStanArgs -> [String] -> Either String PluStanArgs
    go acc [] = Right acc
    go acc ("--report":xs) = go acc { plustanReport = True } xs
    go acc ("--browse":xs) = go acc { plustanReport = True, plustanBrowse = True } xs
    go acc ("--hiedir":dir:xs) = go acc { plustanHieDir = dir } xs
    go acc (arg:xs)
      | "--hiedir=" `isPrefixOf` arg =
          let prefix = "--hiedir=" :: String
          in go acc { plustanHieDir = drop (length prefix) arg } xs
      | otherwise = Left ("Unknown argument: " <> arg)

ensureHieFiles :: FilePath -> IO ()
ensureHieFiles hieDir = do
  hasHie <- hasFilesWithExt hieDir ".hie"
  hasHi <- hasFilesWithExt hieDir ".hi"
  when (not (hasHie && hasHi)) $ do
    infoMessage "Missing .hie/.hi files. Running cabal build to generate artifacts..."
    callProcess "cabal"
      [ "build"
      , "all"
      , "--disable-tests"
      , "--ghc-options=-fwrite-ide-info"
      , "--ghc-options=-hiedir=" <> hieDir
      , "--ghc-options=-hidir=" <> hieDir
      ]

hasFilesWithExt :: FilePath -> String -> IO Bool
hasFilesWithExt dir ext = do
  files <- listFilesWithExt dir ext
  pure (not (null files))

listFilesWithExt :: FilePath -> String -> IO [FilePath]
listFilesWithExt dir ext = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else go dir
  where
    go :: FilePath -> IO [FilePath]
    go path = do
      entries <- listDirectory path
      concat <$> traverse (entryFiles path) entries

    entryFiles :: FilePath -> FilePath -> IO [FilePath]
    entryFiles path entry = do
      let fullPath = path </> entry
      isDir <- doesDirectoryExist fullPath
      if isDir
        then go fullPath
        else pure [fullPath | takeExtension fullPath == ext]

readModIface :: Profile -> NameCache -> FilePath -> IO (Maybe ModIface)
readModIface profile nameCache hiPath =
  handle @SomeException (\_ -> pure Nothing) $
    Just <$> readBinIface profile nameCache IgnoreHiWay QuietBinIFace hiPath

hasOnchainAnnotation :: ModIface -> Bool
hasOnchainAnnotation iface = any isOnchainAnn (mi_anns iface)
  where
    isOnchainAnn :: IfaceAnnotation -> Bool
    isOnchainAnn IfaceAnnotation{..} =
      case ifAnnotatedTarget of
        ModuleTarget _ -> case fromSerialized deserializeWithData ifAnnotatedValue of
          Just (s :: String) -> s == "onchain-contract"
          Nothing -> False
        _ -> False

onchainFiles :: FilePath -> [HieFile] -> IO (Set.Set FilePath)
onchainFiles hieDir hieFiles =
  handle @SomeException (\_ -> pure Set.empty) $ runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags dflags
    hiFiles <- liftIO $ listFilesWithExt hieDir ".hi"
    nameCache <- liftIO $ initNameCache 'z' []
    let profile = targetProfile dflags
    let moduleToFile = Map.fromList [(hie_module, hie_hs_file) | HieFile{..} <- hieFiles]
    annotatedModules <- liftIO $ fmap catMaybes $ forM hiFiles $ \hiPath -> do
      iface <- readModIface profile nameCache hiPath
      pure $ iface >>= \modIface -> if hasOnchainAnnotation modIface
        then Just (mi_module modIface)
        else Nothing
    pure $ Set.fromList $ mapMaybe (`Map.lookup` moduleToFile) annotatedModules

isOnchainObservations :: Set.Set FilePath -> Observation -> Bool
isOnchainObservations files obs = Set.member (observationFile obs) files

onchainCondition :: Set.Set FilePath -> Observation -> Bool
onchainCondition contracts obs = not (isPlinthObservation obs) || isOnchainObservations contracts obs

filterForOnchain :: Set.Set FilePath -> FileInfo -> FileInfo
filterForOnchain contracts info@FileInfo{..}= info {
  fileInfoObservations = Slist.filter (onchainCondition contracts) fileInfoObservations }

removeOffchain :: FilePath -> [HieFile] -> Analysis -> IO Analysis
removeOffchain hieDir hieFiles analysis = do
  contracts <- onchainFiles hieDir hieFiles
  pure analysis {
          analysisObservations = Slist.filter (onchainCondition contracts) (analysisObservations analysis)
        , analysisFileMap = fmap (filterForOnchain contracts) (analysisFileMap analysis)
      }

pluStanConfig :: PartialConfig
pluStanConfig = ConfigP
  { configChecks = withTag "PluStan" $ pure
      [ Check
          { checkType = Exclude
          , checkFilter = CheckAll
          , checkScope = ScopeAll
          }
      , Check
          { checkType = Include
          , checkFilter = CheckCategory Category.plutus
          , checkScope = ScopeAll
          }
      ]
  , configRemoved = withTag "PluStan" $ pure []
  , configIgnored = withTag "PluStan" $ pure []
  }
