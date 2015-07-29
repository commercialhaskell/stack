{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
-- | Generate HPC (Haskell Program Coverage) reports
module Stack.Build.Coverage (generateHpcReport) where

import           Control.Applicative            ((<$>))
import           Control.Exception.Lifted
import           Control.Monad.Catch            (MonadCatch, MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader           (MonadReader, asks)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8          as S8
import           Data.Foldable                  (forM_)
import           Data.Function
import           Data.List
import qualified Data.Map.Strict                as Map
import           Data.Maybe
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Path
import           Path.IO
import           Prelude                        hiding (FilePath, writeFile)
import           Stack.Constants
import           Stack.Types
import           System.Process.Read

-- | Generates the HTML coverage report and shows a textual coverage
-- summary.
generateHpcReport :: (MonadIO m,MonadReader env m,HasConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,HasEnvConfig env)
                  => Path Abs Dir -> Text -> Text -> Text -> m ()
generateHpcReport pkgDir pkgName pkgId testName = do
    let whichTest = pkgName <> "'s test-suite \"" <> testName <> "\""
    -- Compute destination directory.
    installDir <- installationRootLocal
    testNamePath <- parseRelDir (T.unpack testName)
    pkgIdPath <- parseRelDir (T.unpack pkgId)
    let destDir = installDir </> hpcDirSuffix </> pkgIdPath </> testNamePath
    -- Directories for .mix files.
    hpcDir <- hpcDirFromDir pkgDir
    hpcRelDir <- (</> dotHpc) <$> hpcRelativeDir
    -- Compute arguments used for both "hpc markup" and "hpc report".
    pkgDirs <- Map.keys . bcPackages <$> asks getBuildConfig
    let args =
            -- Use index files from all packages (allows cross-package
            -- coverage results).
            concatMap (\x -> ["--srcdir", toFilePath x]) pkgDirs ++
            -- Look for index files in the correct dir (relative to
            -- each pkgdir).
            ["--hpcdir", toFilePath hpcRelDir, "--reset-hpcdirs"
            -- Restrict to just the current library code (see #634 -
            -- this will likely be customizable in the future)
            ,"--include", T.unpack (pkgId <> ":")]
    -- If a .tix file exists, generate an HPC report for it.
    tixFile <- parseRelFile (T.unpack testName ++ ".tix")
    let tixFileAbs = hpcDir </> tixFile
    tixFileExists <- fileExists tixFileAbs
    if not tixFileExists
        then $logError $ T.concat
            [ "Didn't find .tix coverage file for "
            , whichTest
            , " - expected to find it at "
            , T.pack (toFilePath tixFileAbs)
            , "."
            ]
        else (`onException` $logError ("Error occurred while producing coverage report for " <> whichTest)) $ do
            menv <- getMinimalEnvOverride
            $logInfo $ "Generating HTML coverage report for " <> whichTest
            _ <- readProcessStdout (Just hpcDir) menv "hpc"
                ("markup" : toFilePath tixFileAbs : ("--destdir=" ++ toFilePath destDir) : args)
            output <- readProcessStdout (Just hpcDir) menv "hpc"
                ("report" : toFilePath tixFileAbs : args)
            -- Print output, stripping @\r@ characters because
            -- Windows.
            forM_ (S8.lines output) ($logInfo . T.decodeUtf8 . S8.filter (not . (=='\r')))
            $logInfo
                ("The HTML coverage report for " <> whichTest <> " is available at " <>
                 T.pack (toFilePath (destDir </> $(mkRelFile "hpc_index.html"))))
