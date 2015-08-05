{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
-- | Generate HPC (Haskell Program Coverage) reports
module Stack.Build.Coverage
    ( generateHpcReport
    , generateHpcMarkupIndex
    ) where

import           Control.Applicative            ((<$>))
import           Control.Exception.Lifted
import           Control.Monad                  (liftM)
import           Control.Monad.Catch            (MonadCatch)
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
import qualified Data.Text.IO                   as T
import qualified Data.Text.Lazy                 as LT
import           Data.Traversable               (forM)
import           Path
import           Path.IO
import           Prelude                        hiding (FilePath, writeFile)
import           Stack.Constants
import           Stack.Types
import           System.Process.Read
import           Text.Hastache                  (htmlEscape)

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
    pkgDirs <- Map.keys . envConfigPackages <$> asks getEnvConfig
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

generateHpcMarkupIndex :: (MonadIO m,MonadReader env m,MonadLogger m,MonadCatch m,HasEnvConfig env)
                       => m ()
generateHpcMarkupIndex = do
    installDir <- installationRootLocal
    let markupDir = installDir </> hpcDirSuffix
        outputFile = markupDir </> $(mkRelFile "index.html")
    (dirs, _) <- listDirectory markupDir
    rows <- liftM (catMaybes . concat) $ forM dirs $ \dir -> do
        (subdirs, _) <- listDirectory dir
        forM subdirs $ \subdir -> do
            let indexPath = subdir </> $(mkRelFile "hpc_index.html")
            exists <- fileExists indexPath
            if not exists then return Nothing else do
                relPath <- stripDir markupDir indexPath
                let package = dirname dir
                    testsuite = dirname subdir
                return $ Just $ T.concat
                  [ "<tr><td>"
                  , pathToHtml package
                  , "</td><td><a href=\""
                  , pathToHtml relPath
                  , "\">"
                  , pathToHtml testsuite
                  , "</a></td></tr>"
                  ]
    liftIO $ T.writeFile (toFilePath outputFile) $ T.concat $
        [ "<html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">"
        -- Part of the css from HPC's output HTML
        , "<style type=\"text/css\">"
        , "table.dashboard { border-collapse: collapse; border: solid 1px black }"
        , ".dashboard td { border: solid 1px black }"
        , ".dashboard th { border: solid 1px black }"
        , "</style>"
        , "</head>"
        , "<body>"
        ] ++
        (if null rows
            then
                [ "<b>No hpc_index.html files found in \""
                , pathToHtml markupDir
                , "\".</b>"
                ]
            else
                [ "<table class=\"dashboard\" width=\"100%\" boder=\"1\"><tbody>"
                , "<p><b>NOTE: This is merely a listing of the html files found in the coverage reports directory.  Some of these reports may be old.</b></p>"
                , "<tr><th>Package</th><th>TestSuite</th></tr>"
                ] ++
                rows ++
                ["</tbody></table>"]) ++
        ["</body></html>"]
    $logInfo $ "\nAn index of the generated HTML coverage reports is available at " <>
        T.pack (toFilePath outputFile)

pathToHtml :: Path b t -> Text
pathToHtml = T.dropWhileEnd (=='/') . LT.toStrict . htmlEscape . LT.pack . toFilePath
