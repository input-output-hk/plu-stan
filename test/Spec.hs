module Main (main) where

import Stan.Hie.Compat (HieFile (..))
import System.Directory (doesFileExist, getModificationTime)
import System.FilePath ((</>))
import System.Process (callProcess)
import Test.Hspec (hspec)

import Stan.Hie (readHieFiles)
import Test.Stan.Analysis (analysisSpec)
import Test.Stan.Cli (cliSpec)
import Test.Stan.Config (configSpec)
import Test.Stan.Number (linesOfCodeSpec, modulesNumSpec)
import Test.Stan.Observation (observationSpec)
import Test.Stan.Toml (tomlSpec)

import Control.Monad (when)


main :: IO ()
main = do
    ensureHieFiles ".hie"
    hieFiles <- readHieFiles ".hie"
    case filter isTargetFile hieFiles of
        [] -> do
            putStrLn "FAILED: target/ files are not found"
            exitFailure
        testHies -> do
            Just exampleHie <- pure $
                find ((==) ("target" </> "Target" </> "Partial.hs") . hie_hs_file) testHies
            hspec $ do
                linesOfCodeSpec exampleHie
                modulesNumSpec $ length hieFiles
                cliSpec
                tomlSpec
                configSpec
                observationSpec
                analysisSpec testHies

isTargetFile :: HieFile -> Bool
isTargetFile HieFile{..} = "target" `isPrefixOf` hie_hs_file

ensureHieFiles :: FilePath -> IO ()
ensureHieFiles hieDir = do
    needsRebuild <- hieFilesStale hieDir
    when needsRebuild $ do
        putStrLn "Rebuilding to generate fresh .hie files for tests..."
        callProcess "cabal"
            [ "build"
            , "all"
            , "--disable-tests"
            , "--ghc-options=-fwrite-ide-info"
            , "--ghc-options=-hiedir=" <> hieDir
            , "--ghc-options=-hidir=" <> hieDir
            ]

hieFilesStale :: FilePath -> IO Bool
hieFilesStale hieDir = do
    let src = "target" </> "Target" </> "PlutusTx.hs"
    let hie = hieDir </> "Target" </> "PlutusTx.hie"
    hasHie <- doesFileExist hie
    if not hasHie
        then pure True
        else do
            srcTime <- getModificationTime src
            hieTime <- getModificationTime hie
            pure (srcTime > hieTime)
