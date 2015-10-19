{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
-- | Generate HPC (Haskell Program Coverage) reports
module Stack.Build.Coverage
    ( updateTixFile
    , generateHpcReport
    , generateHpcUnifiedReport
    , generateHpcMarkupIndex
    ) where

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Monad                  (liftM, when, void)
import           Control.Monad.Catch            (MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader           (MonadReader, asks)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8          as S8
import           Data.Foldable                  (forM_, asum)
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
import           Trace.Hpc.Tix
import           Path
import           Path.IO
import           Prelude                        hiding (FilePath, writeFile)
import           Stack.Constants
import           Stack.Package
import           Stack.Types
import           System.Process.Read
import           Text.Hastache                  (htmlEscape)

-- | Move a tix file into a sub-directory of the hpc report directory. Deletes the old one if one is
-- present.
updateTixFile :: (MonadIO m,MonadReader env m,HasConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,HasEnvConfig env)
            => Path Abs File -> String -> m ()
updateTixFile tixSrc pkgId = do
    exists <- fileExists tixSrc
    when exists $ do
        outputDir <- hpcReportDir
        pkgIdRel <- parseRelDir pkgId
        let tixDest = outputDir </> pkgIdRel </> filename tixSrc
        removeFileIfExists tixDest
        createTree (parent tixDest)
        renameFile tixSrc tixDest

-- | Get the tix file location, given the name of the file (without extension), and the package
-- identifier string.
tixFilePath ::  (MonadIO m,MonadReader env m,HasConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,HasEnvConfig env)
            => String -> String ->  m (Path Abs File)
tixFilePath pkgId tixName = do
    outputDir <- hpcReportDir
    pkgIdRel <- parseRelDir pkgId
    tixRel <- parseRelFile (tixName ++ ".tix")
    return (outputDir </> pkgIdRel </> tixRel)

-- | Generates the HTML coverage report and shows a textual coverage summary for a package.
generateHpcReport :: (MonadIO m,MonadReader env m,HasConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,HasEnvConfig env)
                  => Path Abs Dir -> Package -> [Text] -> m ()
generateHpcReport pkgDir package tests = do
    -- If we're using > GHC 7.10, the hpc 'include' parameter must specify a ghc package key. See
    -- https://github.com/commercialhaskell/stack/issues/785
    let pkgName = packageNameText (packageName package)
        pkgId = packageIdentifierString (packageIdentifier package)
    compilerVersion <- asks (envConfigCompilerVersion . getEnvConfig)
    eincludeName <-
        -- Pre-7.8 uses plain PKG-version in tix files.
        if getGhcVersion compilerVersion < $(mkVersion "7.10") then return $ Right $ Just pkgId
        -- We don't expect to find a package key if there is no library.
        else if not (packageHasLibrary package) then return $ Right Nothing
        -- Look in the inplace DB for the package key.
        -- See https://github.com/commercialhaskell/stack/issues/1181#issuecomment-148968986
        else do
            mghcPkgKey <- findPackageKeyForBuiltPackage pkgDir (packageIdentifier package)
            case mghcPkgKey of
                Nothing -> do
                    let msg = "Failed to find GHC package key for " <> pkgName
                    $logError msg
                    return $ Left msg
                Just ghcPkgKey -> return $ Right $ Just $ T.unpack ghcPkgKey
    forM_ tests $ \testName -> do
        tixSrc <- tixFilePath pkgId (T.unpack testName)
        subdir <- parseRelDir (T.unpack testName)
        let report = "coverage report for " <> pkgName <> "'s test-suite \"" <> testName <> "\""
            reportDir = parent tixSrc </> subdir
        case eincludeName of
            Left err -> generateHpcErrorReport reportDir (sanitize (T.unpack err))
            -- Restrict to just the current library code, if there is a library in the package (see
            -- #634 - this will likely be customizable in the future)
            Right mincludeName -> do
                let extraArgs = case mincludeName of
                        Just includeName -> ["--include", includeName ++ ":"]
                        Nothing -> []
                generateHpcReportInternal tixSrc reportDir report extraArgs extraArgs

generateHpcReportInternal :: (MonadIO m,MonadReader env m,HasConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,HasEnvConfig env)
                          => Path Abs File -> Path Abs Dir -> Text -> [String] -> [String] -> m ()
generateHpcReportInternal tixSrc reportDir report extraMarkupArgs extraReportArgs = do
    -- If a .tix file exists, move it to the HPC output directory and generate a report for it.
    tixFileExists <- fileExists tixSrc
    if not tixFileExists
        then $logError $ T.concat
            [ "Didn't find .tix for "
            , report
            , " - expected to find it at "
            , T.pack (toFilePath tixSrc)
            , "."
            ]
        else (`catch` \err -> generateHpcErrorReport reportDir $ sanitize $ show (err :: ReadProcessException)) $
             (`onException` $logError ("Error occurred while producing " <> report)) $ do
            -- Directories for .mix files.
            hpcRelDir <- (</> dotHpc) <$> hpcRelativeDir
            -- Compute arguments used for both "hpc markup" and "hpc report".
            pkgDirs <- Map.keys . envConfigPackages <$> asks getEnvConfig
            let args =
                    -- Use index files from all packages (allows cross-package coverage results).
                    concatMap (\x -> ["--srcdir", toFilePath x]) pkgDirs ++
                    -- Look for index files in the correct dir (relative to each pkgdir).
                    ["--hpcdir", toFilePath hpcRelDir, "--reset-hpcdirs"]
            menv <- getMinimalEnvOverride
            $logInfo $ "Generating " <> report
            outputLines <- liftM S8.lines $ readProcessStdout Nothing menv "hpc"
                ( "report"
                : toFilePath tixSrc
                : (args ++ extraReportArgs)
                )
            if all ("(0/0)" `S8.isSuffixOf`) outputLines
                then do
                    let msg html = T.concat
                            [ "Error: The "
                            , report
                            , " did not consider any code. One possible cause of this is"
                            , " if your test-suite builds the library code (see stack "
                            , if html then "<a href='https://github.com/commercialhaskell/stack/issues/1008'>" else ""
                            , "issue #1008"
                            , if html then "</a>" else ""
                            , "). It may also indicate a bug in stack or"
                            , " the hpc program. Please report this issue if you think"
                            , " your coverage report should have meaningful results."
                            ]
                    $logError (msg False)
                    generateHpcErrorReport reportDir (msg True)
                else do
                    -- Print output, stripping @\r@ characters because Windows.
                    forM_ outputLines ($logInfo . T.decodeUtf8 . S8.filter (not . (=='\r')))
                    $logInfo
                        ("The " <> report <> " is available at " <>
                         T.pack (toFilePath (reportDir </> $(mkRelFile "hpc_index.html"))))
                    -- Generate the markup.
                    void $ readProcessStdout Nothing menv "hpc"
                        ( "markup"
                        : toFilePath tixSrc
                        : ("--destdir=" ++ toFilePath reportDir)
                        : (args ++ extraMarkupArgs)
                        )

generateHpcUnifiedReport :: (MonadIO m,MonadReader env m,HasConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,HasEnvConfig env)
                     => m ()
generateHpcUnifiedReport = do
    outputDir <- hpcReportDir
    createTree outputDir
    (dirs, _) <- listDirectory outputDir
    tixFiles <- liftM concat $ forM dirs $ \dir -> do
        (_, files) <- listDirectory dir
        return (filter ((".tix" `isSuffixOf`) . toFilePath) files)
    if length tixFiles < 2
        then $logInfo $ T.concat $
            [ if null tixFiles then "No tix files" else "Only one tix file"
            , " found in "
            , T.pack (toFilePath outputDir)
            , ", so not generating a unified coverage report."
            ]
        else do
            tixes <- mapM (liftM (fmap removeExeModules) . readTixOrLog) tixFiles
            let (errs, tix) = unionTixes (catMaybes tixes)
            when (not (null errs)) $ $logWarn $ T.concat $
                "The following modules are left out of the unified report due to version mismatches: " :
                intersperse ", " (map T.pack errs)
            let tixDest = outputDir </> $(mkRelFile "unified/unified.tix")
            createTree (parent tixDest)
            liftIO $ writeTix (toFilePath tixDest) tix
            generateHpcReportInternal tixDest (outputDir </> $(mkRelDir "unified/unified")) "unified report" [] []

readTixOrLog :: (MonadLogger m, MonadIO m) => Path b File -> m (Maybe Tix)
readTixOrLog path = do
    mtix <- liftIO $ readTix (toFilePath path)
    when (isNothing mtix) $
        $logError $ "Failed to read tix file " <> T.pack (toFilePath path)
    return mtix

-- | Module names which contain '/' have a package name, and so they weren't built into the
-- executable.
removeExeModules :: Tix -> Tix
removeExeModules (Tix ms) = Tix (filter (\(TixModule name _ _ _) -> '/' `elem` name) ms)

unionTixes :: [Tix] -> ([String], Tix)
unionTixes tixes = (Map.keys errs, Tix (Map.elems outputs))
  where
    (errs, outputs) = Map.mapEither id $ Map.unionsWith merge $ map toMap tixes
    toMap (Tix ms) = Map.fromList (map (\x@(TixModule k _ _ _) -> (k, Right x)) ms)
    merge (Right (TixModule k hash1 len1 tix1))
          (Right (TixModule _ hash2 len2 tix2))
        | hash1 == hash2 && len1 == len2 = Right (TixModule k hash1 len1 (zipWith (+) tix1 tix2))
    merge _ _ = Left ()

generateHpcMarkupIndex :: (MonadIO m,MonadReader env m,MonadLogger m,MonadCatch m,HasEnvConfig env)
                       => m ()
generateHpcMarkupIndex = do
    outputDir <- hpcReportDir
    let outputFile = outputDir </> $(mkRelFile "index.html")
    createTree outputDir
    (dirs, _) <- listDirectory outputDir
    rows <- liftM (catMaybes . concat) $ forM dirs $ \dir -> do
        (subdirs, _) <- listDirectory dir
        forM subdirs $ \subdir -> do
            let indexPath = subdir </> $(mkRelFile "hpc_index.html")
            exists' <- fileExists indexPath
            if not exists' then return Nothing else do
                relPath <- stripDir outputDir indexPath
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
                , pathToHtml outputDir
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

generateHpcErrorReport :: MonadIO m => Path Abs Dir -> Text -> m ()
generateHpcErrorReport dir err = do
    createTree dir
    liftIO $ T.writeFile (toFilePath (dir </> $(mkRelFile "hpc_index.html"))) $ T.concat $
        [ "<html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></head><body>"
        , "<h1>HPC Report Generation Error</h1>"
        , "<p>"
        , err
        , "</p>"
        , "</body></html>"
        ]

pathToHtml :: Path b t -> Text
pathToHtml = T.dropWhileEnd (=='/') . sanitize . toFilePath

sanitize :: String -> Text
sanitize = LT.toStrict . htmlEscape . LT.pack

findPackageKeyForBuiltPackage :: (MonadIO m, MonadReader env m, MonadThrow m, HasEnvConfig env)
                              => Path Abs Dir -> PackageIdentifier -> m (Maybe Text)
findPackageKeyForBuiltPackage pkgDir pkgId = do
    distDir <- distDirFromDir pkgDir
    path <- liftM (distDir </>) $
        parseRelFile ("package.conf.inplace/" ++ packageIdentifierString pkgId ++ "-inplace.conf")
    contents <- liftIO $ T.readFile (toFilePath path)
    return $ asum (map (T.stripPrefix "key: ") (T.lines contents))
