module Main (main) where

import Colourista (errorMessage, infoMessage, successMessage, warningMessage)
import Control.Exception (SomeException, handle, try)
import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Micro (ToJSON (..), encode, object, (.=))
import Data.Foldable (toList, traverse_)
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Directory (findExecutable)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.Info (compilerVersion)
import System.IO (stderr)
import System.Process (CreateProcess (std_err, std_out), StdStream (UseHandle), callProcess,
                       createProcess, proc, waitForProcess)
import Data.Version (showVersion)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Trial (withTag, whenResult_)

import Stan (getAnalysis)
import Stan.Analysis (Analysis (..))
import Stan.Analysis.Pretty (isPlinthObservation, prettyShowAnalysis)
import Stan.Browse (openBrowser)
import Stan.Cabal (usedCabalFiles)
import Stan.Cli (StanArgs (..))
import Stan.Config (Check (..), CheckFilter (..), CheckType (..), Config, ConfigP (..),
                    PartialConfig, Scope (..), finaliseConfig)
import Stan.Core.ModuleName (ModuleName (..), fromGhcModule)
import Stan.EnvVars (envVarsToText, getEnvVars)
import Stan.FileInfo (FileInfo (..))
import Stan.Hie (readHieFiles)
import Stan.Hie.Compat (HieFile (..))
import Stan.Info (ProjectInfo (..), StanEnv (..))
import Stan.Inspection (Inspection (..))
import Stan.Inspection.All (getInspectionById)
import Stan.Observation (Observation (..))
import Stan.Report (generateReport)
import Stan.Report.Settings (OutputSettings (..), ToggleSolution (..), Verbosity (..))
import Stan.Severity (Severity (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Stan.Category as Category
import qualified Slist as Slist

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
runPluStan = do
  args <- getArgs
  case parsePluStanCommand args of
    Left err -> do
      if err == "help"
        then Text.putStrLn usage
        else do
          errorMessage (Text.pack err)
          Text.putStrLn usage
          exitFailure
    Right command -> case command of
      CommandAnalyze analyzeArgs -> runAnalyze analyzeArgs
      CommandListOnchain listArgs -> runListOnchain listArgs

data PluStanCommand
  = CommandAnalyze AnalyzeArgs
  | CommandListOnchain ListOnchainArgs

data AnalyzeArgs = AnalyzeArgs
  { analyzeReport :: Bool
  , analyzeBrowse :: Bool
  , analyzeProjectDir :: Maybe FilePath
  , analyzeHieDir :: FilePath
  , analyzeJson :: Bool
  , analyzeModule :: Maybe Text
  }

data ListOnchainArgs = ListOnchainArgs
  { listOnchainProjectDir :: Maybe FilePath
  , listOnchainHieDir :: FilePath
  , listOnchainJson :: Bool
  }

data OnchainAnnotationSource
  = AnnotationHi
  | AnnotationSource
  | AnnotationBoth
  deriving stock (Show, Eq)

instance ToJSON OnchainAnnotationSource where
  toJSON = \case
    AnnotationHi -> toJSON ("hi" :: Text)
    AnnotationSource -> toJSON ("source" :: Text)
    AnnotationBoth -> toJSON ("both" :: Text)

data OnchainModule = OnchainModule
  { onchainModuleName :: ModuleName
  , onchainModuleFile :: FilePath
  , onchainAnnotationSource :: OnchainAnnotationSource
  } deriving stock (Show, Eq)

instance ToJSON OnchainModule where
  toJSON OnchainModule{..} = object
    [ "moduleName" .= onchainModuleName
    , "file" .= Text.pack onchainModuleFile
    , "annotationSource" .= onchainAnnotationSource
    ]

data ListOnchainJsonPayload = ListOnchainJsonPayload
  { listPayloadVersion :: Int
  , listPayloadWorkspaceRoot :: FilePath
  , listPayloadHieDir :: FilePath
  , listPayloadModules :: [OnchainModule]
  }

instance ToJSON ListOnchainJsonPayload where
  toJSON ListOnchainJsonPayload{..} = object
    [ "version" .= listPayloadVersion
    , "workspaceRoot" .= Text.pack listPayloadWorkspaceRoot
    , "hieDir" .= Text.pack listPayloadHieDir
    , "modules" .= listPayloadModules
    ]

data AnalyzeJsonPayload = AnalyzeJsonPayload
  { analyzePayloadVersion :: Int
  , analyzePayloadRunScope :: Text
  , analyzePayloadTargetModule :: Maybe ModuleName
  , analyzePayloadInspections :: [Inspection]
  , analyzePayloadAnalysis :: Analysis
  }

instance ToJSON AnalyzeJsonPayload where
  toJSON AnalyzeJsonPayload{..} = object
    [ "version" .= analyzePayloadVersion
    , "runScope" .= analyzePayloadRunScope
    , "targetModule" .= analyzePayloadTargetModule
    , "inspections" .= analyzePayloadInspections
    , "analysis" .= analyzePayloadAnalysis
    ]

runAnalyze :: AnalyzeArgs -> IO ()
runAnalyze AnalyzeArgs{..} =
  whenResult_ (finaliseConfig pluStanConfig) $ \warnings config -> do
    let notJson = not analyzeJson
    enterProjectDir analyzeProjectDir notJson
    let hieDir = analyzeHieDir
    ensureFreshHieFiles hieDir notJson
    hieFiles <- readHieFilesOrRebuild hieDir notJson
    when (null hieFiles) $ do
      reportWarning notJson "No .hie files found after build. Ensure the project is compiled with -fwrite-ide-info and -hiedir=.hie."
      exitFailure

    onchainModules <- discoverOnchainModules hieDir hieFiles
    targetModule <- resolveTargetModule analyzeModule onchainModules notJson
    let targetFiles = maybe Set.empty (Set.singleton . onchainModuleFile) targetModule
    let filteredHieFiles =
          if Set.null targetFiles
            then hieFiles
            else filter (\HieFile{..} -> Set.member hie_hs_file targetFiles) hieFiles

    when (null filteredHieFiles) $ do
      let moduleLabel = maybe "<unknown>" (Text.unpack . unModuleName . onchainModuleName) targetModule
      reportError notJson $ "No HIE module found for target module: " <> Text.pack moduleLabel
      exitFailure

    analysis <- getAnalysis (stanArgs hieDir) notJson config filteredHieFiles
      >>= pure . filterAnalysisToContracts (Set.fromList $ map onchainModuleFile onchainModules)
      >>= pure . maybe id (limitAnalysisToTarget . onchainModuleFile) targetModule

    let observations = analysisObservations analysis
    let usedInspections = sortOn inspectionId $ map getInspectionById (toList $ analysisInspections analysis)

    if analyzeJson
      then do
        putJson AnalyzeJsonPayload
          { analyzePayloadVersion = 1
          , analyzePayloadRunScope = maybe "all" (const "module") targetModule
          , analyzePayloadTargetModule = onchainModuleName <$> targetModule
          , analyzePayloadInspections = usedInspections
          , analyzePayloadAnalysis = analysis
          }
      else do
        if null observations
          then successMessage "All clean! Plu-Stan did not find any observations at the moment."
          else warningMessage "Plu-Stan found the following observations:\n"
        Text.putStrLn $ prettyShowAnalysis analysis outputSettings

    when (analyzeReport && not analyzeJson) $
      generatePluStanReport AnalyzeArgs{..} config warnings filteredHieFiles analysis

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

runListOnchain :: ListOnchainArgs -> IO ()
runListOnchain ListOnchainArgs{..} = do
  let notJson = not listOnchainJson
  enterProjectDir listOnchainProjectDir notJson
  let hieDir = listOnchainHieDir
  ensureFreshHieFiles hieDir notJson
  hieFiles <- readHieFilesOrRebuild hieDir notJson
  modules <- discoverOnchainModules hieDir hieFiles
  currentDir <- getCurrentDirectory
  if listOnchainJson
    then putJson ListOnchainJsonPayload
      { listPayloadVersion = 1
      , listPayloadWorkspaceRoot = currentDir
      , listPayloadHieDir = hieDir
      , listPayloadModules = modules
      }
    else printOnchainModules modules

printOnchainModules :: [OnchainModule] -> IO ()
printOnchainModules modules =
  if null modules
    then infoMessage "No onchain modules were found."
    else traverse_ printLine modules
  where
    printLine :: OnchainModule -> IO ()
    printLine OnchainModule{..} =
      Text.putStrLn $ unModuleName onchainModuleName <> " -> " <> Text.pack onchainModuleFile

usage :: Text
usage = Text.unlines
  [ "Usage:"
  , "  plustan [--report] [--browse] [--json] [--module MODULE] [--project DIR] [--hiedir DIR] [PROJECT_DIR]"
  , "  plustan analyze [--report] [--browse] [--json] [--module MODULE] [--project DIR] [--hiedir DIR]"
  , "  plustan list-onchain [--json] [--project DIR] [--hiedir DIR]"
  , ""
  , "Options:"
  , "  --report        Generate stan.html report (analyze only)"
  , "  --browse        Open report in browser (implies --report)"
  , "  --json          Output machine-readable JSON"
  , "  --module NAME   Analyze a single onchain module by GHC module name"
  , "  --project DIR   Change into project DIR before running"
  , "  --hiedir DIR    Directory with .hie/.hi files (default: .hie)"
  ]

parsePluStanCommand :: [String] -> Either String PluStanCommand
parsePluStanCommand = \case
  [] -> CommandAnalyze <$> parseAnalyzeArgs True defaultAnalyzeArgs []
  "analyze":rest -> CommandAnalyze <$> parseAnalyzeArgs False defaultAnalyzeArgs rest
  "list-onchain":rest -> CommandListOnchain <$> parseListOnchainArgs defaultListOnchainArgs rest
  "help":_ -> Left "help"
  "--help":_ -> Left "help"
  "-h":_ -> Left "help"
  args -> CommandAnalyze <$> parseAnalyzeArgs True defaultAnalyzeArgs args

parseAnalyzeArgs :: Bool -> AnalyzeArgs -> [String] -> Either String AnalyzeArgs
parseAnalyzeArgs allowLegacyPositional = go
  where
    go :: AnalyzeArgs -> [String] -> Either String AnalyzeArgs
    go acc [] = Right acc
    go acc ("--report":xs) = go acc { analyzeReport = True } xs
    go acc ("--browse":xs) = go acc { analyzeReport = True, analyzeBrowse = True } xs
    go acc ("--json":xs) = go acc { analyzeJson = True } xs
    go acc ("--project":dir:xs) = go acc { analyzeProjectDir = Just dir } xs
    go acc ("--hiedir":dir:xs) = go acc { analyzeHieDir = dir } xs
    go acc ("--module":modName:xs) = go acc { analyzeModule = Just (Text.pack modName) } xs
    go acc (arg:xs)
      | "--project=" `isPrefixOf` arg =
          let prefix = "--project=" :: String
          in go acc { analyzeProjectDir = Just (drop (length prefix) arg) } xs
      | "--hiedir=" `isPrefixOf` arg =
          let prefix = "--hiedir=" :: String
          in go acc { analyzeHieDir = drop (length prefix) arg } xs
      | "--module=" `isPrefixOf` arg =
          let prefix = "--module=" :: String
          in go acc { analyzeModule = Just (Text.pack $ drop (length prefix) arg) } xs
      | "-" `isPrefixOf` arg = Left ("Unknown argument: " <> arg)
      | allowLegacyPositional =
          case analyzeProjectDir acc of
            Nothing -> go acc { analyzeProjectDir = Just arg } xs
            Just _ -> Left ("Unexpected extra positional argument: " <> arg)
      | otherwise = Left ("Unexpected positional argument: " <> arg)

defaultAnalyzeArgs :: AnalyzeArgs
defaultAnalyzeArgs = AnalyzeArgs
  { analyzeReport = False
  , analyzeBrowse = False
  , analyzeProjectDir = Nothing
  , analyzeHieDir = ".hie"
  , analyzeJson = False
  , analyzeModule = Nothing
  }

parseListOnchainArgs :: ListOnchainArgs -> [String] -> Either String ListOnchainArgs
parseListOnchainArgs = go
  where
    go :: ListOnchainArgs -> [String] -> Either String ListOnchainArgs
    go acc [] = Right acc
    go acc ("--json":xs) = go acc { listOnchainJson = True } xs
    go acc ("--project":dir:xs) = go acc { listOnchainProjectDir = Just dir } xs
    go acc ("--hiedir":dir:xs) = go acc { listOnchainHieDir = dir } xs
    go acc (arg:xs)
      | "--project=" `isPrefixOf` arg =
          let prefix = "--project=" :: String
          in go acc { listOnchainProjectDir = Just (drop (length prefix) arg) } xs
      | "--hiedir=" `isPrefixOf` arg =
          let prefix = "--hiedir=" :: String
          in go acc { listOnchainHieDir = drop (length prefix) arg } xs
      | "-" `isPrefixOf` arg = Left ("Unknown argument: " <> arg)
      | otherwise =
          case listOnchainProjectDir acc of
            Nothing -> go acc { listOnchainProjectDir = Just arg } xs
            Just _ -> Left ("Unexpected extra positional argument: " <> arg)

defaultListOnchainArgs :: ListOnchainArgs
defaultListOnchainArgs = ListOnchainArgs
  { listOnchainProjectDir = Nothing
  , listOnchainHieDir = ".hie"
  , listOnchainJson = False
  }

resolveTargetModule :: Maybe Text -> [OnchainModule] -> Bool -> IO (Maybe OnchainModule)
resolveTargetModule Nothing _ _ = pure Nothing
resolveTargetModule (Just moduleName) modules notJson =
  case filter ((== moduleName) . unModuleName . onchainModuleName) modules of
    (mod':_) -> pure (Just mod')
    [] -> do
      let candidates = map (unModuleName . onchainModuleName) modules
      let hint =
            if null candidates
              then "No onchain modules were discovered in this project."
              else "Known onchain modules: " <> Text.intercalate ", " candidates
      reportError notJson $ "Unknown onchain module: " <> moduleName
      reportError notJson hint
      exitFailure

putJson :: ToJSON a => a -> IO ()
putJson = LBS8.putStrLn . encode

enterProjectDir :: Maybe FilePath -> Bool -> IO ()
enterProjectDir Nothing _ = pure ()
enterProjectDir (Just dir) notJson = do
  exists <- doesDirectoryExist dir
  if exists
    then setCurrentDirectory dir
    else do
      reportError notJson $ "Project directory does not exist: " <> Text.pack dir
      exitFailure

reportError :: Bool -> Text -> IO ()
reportError notJson msg =
  if notJson
    then errorMessage msg
    else Text.hPutStrLn stderr msg

reportWarning :: Bool -> Text -> IO ()
reportWarning notJson msg =
  if notJson
    then warningMessage msg
    else Text.hPutStrLn stderr msg

ensureFreshHieFiles :: FilePath -> Bool -> IO ()
ensureFreshHieFiles hieDir notJson = do
  hasHie <- hasFilesWithExt hieDir ".hie"
  hasHi <- hasFilesWithExt hieDir ".hi"
  if not (hasHie && hasHi)
    then do
      when notJson $ infoMessage "Missing .hie/.hi files. Running full cabal build to generate artifacts..."
      buildHieFiles hieDir notJson True
    else do
      when notJson $ infoMessage "Refreshing .hie/.hi files for changed sources..."
      buildHieFiles hieDir notJson False

readHieFilesOrRebuild :: FilePath -> Bool -> IO [HieFile]
readHieFilesOrRebuild hieDir notJson = do
  res <- try (readHieFiles hieDir)
  case res of
    Right hieFiles -> pure hieFiles
    Left (_ :: SomeException) -> do
      reportWarning notJson "Failed to read .hie files (possibly built by a different GHC). Rebuilding..."
      buildHieFiles hieDir notJson True
      readHieFiles hieDir

buildHieFiles :: FilePath -> Bool -> Bool -> IO ()
buildHieFiles hieDir notJson forceRecomp = do
  let ghcVer = "ghc-" <> showVersion compilerVersion
  ghc <- maybe "ghc" id <$> findExecutable ghcVer
  let forceRecompArg =
        if forceRecomp
          then ["--ghc-options=-fforce-recomp"]
          else []
  let buildArgs =
        [ "build"
        , "all"
        , "--disable-tests"
        , "-w"
        , ghc
        ]
        <> forceRecompArg
        <>
        [ "--ghc-options=-fwrite-ide-info"
        , "--ghc-options=-hiedir=" <> hieDir
        , "--ghc-options=-hidir=" <> hieDir
        ]
  if notJson
    then callProcess "cabal" buildArgs
    else do
      (_, _, _, processHandle) <- createProcess (proc "cabal" buildArgs)
        { std_out = UseHandle stderr
        , std_err = UseHandle stderr
        }
      waitForProcess processHandle >>= \case
        ExitSuccess -> pure ()
        ExitFailure code -> exitWith (ExitFailure code)

hasFilesWithExt :: FilePath -> String -> IO Bool
hasFilesWithExt dir ext = do
  files <- listFilesWithExt dir ext
  pure (not $ null files)

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

discoverOnchainModules :: FilePath -> [HieFile] -> IO [OnchainModule]
discoverOnchainModules hieDir hieFiles =
  handle @SomeException (\_ -> pure []) $ runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags dflags
    hiFiles <- liftIO $ listFilesWithExt hieDir ".hi"
    nameCache <- liftIO $ initNameCache 'z' []
    let profile = targetProfile dflags

    let moduleToFile = Map.fromList [(hie_module, hie_hs_file) | HieFile{..} <- hieFiles]
    annotatedFromHi <- liftIO $ fmap catMaybes $ forM hiFiles $ \hiPath -> do
      iface <- readModIface profile nameCache hiPath
      pure $ iface >>= \modIface ->
        if hasOnchainAnnotation modIface
          then Just (mi_module modIface)
          else Nothing

    let fromHiSet = Set.fromList annotatedFromHi
    let fromSourceSet = Set.fromList
          [ hie_module
          | HieFile{..} <- hieFiles
          , hasOnchainAnnotationInSource hie_hs_src
          ]
    let allOnchainModules = Set.toList (Set.union fromHiSet fromSourceSet)

    pure $ sortOn (unModuleName . onchainModuleName) $
      mapMaybe (toOnchainModule moduleToFile fromHiSet fromSourceSet) allOnchainModules
  where
    toOnchainModule moduleToFile fromHiSet fromSourceSet ghcModule = do
      filePath <- Map.lookup ghcModule moduleToFile
      let inHi = Set.member ghcModule fromHiSet
      let inSource = Set.member ghcModule fromSourceSet
      annotationSource <- case (inHi, inSource) of
        (True, True) -> Just AnnotationBoth
        (True, False) -> Just AnnotationHi
        (False, True) -> Just AnnotationSource
        (False, False) -> Nothing
      pure OnchainModule
        { onchainModuleName = fromGhcModule ghcModule
        , onchainModuleFile = filePath
        , onchainAnnotationSource = annotationSource
        }

hasOnchainAnnotationInSource :: BS8.ByteString -> Bool
hasOnchainAnnotationInSource src =
  any lineLooksLikeOnchainAnnotation (BS8.lines src)
  where
    lineLooksLikeOnchainAnnotation :: BS8.ByteString -> Bool
    lineLooksLikeOnchainAnnotation line =
      "{-# ANN module" `BS8.isInfixOf` line
        && "onchain-contract" `BS8.isInfixOf` line

isOnchainObservation :: Set.Set FilePath -> Observation -> Bool
isOnchainObservation files obs = Set.member (observationFile obs) files

onchainCondition :: Set.Set FilePath -> Observation -> Bool
onchainCondition contracts obs = not (isPlinthObservation obs) || isOnchainObservation contracts obs

filterForOnchain :: Set.Set FilePath -> FileInfo -> FileInfo
filterForOnchain contracts info@FileInfo{..} = info
  { fileInfoObservations = Slist.filter (onchainCondition contracts) fileInfoObservations
  }

filterAnalysisToContracts :: Set.Set FilePath -> Analysis -> Analysis
filterAnalysisToContracts contracts analysis = analysis
  { analysisObservations = Slist.filter (onchainCondition contracts) (analysisObservations analysis)
  , analysisFileMap = fmap (filterForOnchain contracts) (analysisFileMap analysis)
  }

limitAnalysisToTarget :: FilePath -> Analysis -> Analysis
limitAnalysisToTarget targetFile analysis = analysis
  { analysisObservations = Slist.filter ((== targetFile) . observationFile) (analysisObservations analysis)
  , analysisFileMap = Map.filterWithKey (\filePath _ -> filePath == targetFile) (analysisFileMap analysis)
  }

generatePluStanReport
  :: AnalyzeArgs
  -> Config
  -> [Text]
  -> [HieFile]
  -> Analysis
  -> IO ()
generatePluStanReport AnalyzeArgs{..} config warnings hieFiles analysis = do
  env <- getEnvVars
  seCliArgs <- getArgs
  piName <- takeFileName <$> getCurrentDirectory
  piCabalFiles <- usedCabalFiles []
  let piHieDir = analyzeHieDir
  let piFileNumber = length hieFiles
  let stanEnv = StanEnv
        { seEnvVars = envVarsToText env
        , seTomlFiles = []
        , ..
        }
  generateReport analysis config warnings stanEnv ProjectInfo{..}
  infoMessage "Report is generated here -> stan.html"
  when analyzeBrowse $ openBrowser "stan.html"

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
