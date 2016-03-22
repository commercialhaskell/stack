{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
-- | Generate HPC (Haskell Program Coverage) reports
module Stack.Coverage
    ( deleteHpcReports
    , updateTixFile
    , generateHpcReport
    , HpcReportOpts(..)
    , generateHpcReportForTargets
    , generateHpcUnifiedReport
    , generateHpcMarkupIndex
    ) where

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Monad (liftM, when, unless, void)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as S8
import           Data.Foldable (forM_, asum, toList)
import           Data.Function
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Maybe.Extra (mapMaybeM)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import           Data.Traversable (forM)
import           Network.HTTP.Download (HasHttpManager)
import           Path
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.IO
import           Prelude hiding (FilePath, writeFile)
import           Stack.Build.Source (parseTargetsFromBuildOpts)
import           Stack.Build.Target
import           Stack.Constants
import           Stack.Package
import           Stack.Types
import           System.FilePath (isPathSeparator)
import           System.Process.Read
import           Text.Hastache (htmlEscape)
import           Trace.Hpc.Tix

-- | Invoked at the beginning of running with "--coverage"
deleteHpcReports :: (MonadIO m, MonadCatch m, MonadReader env m, HasEnvConfig env)
                 => m ()
deleteHpcReports = do
    hpcDir <- hpcReportDir
    ignoringAbsence (removeDirRecur hpcDir)

-- | Move a tix file into a sub-directory of the hpc report directory. Deletes the old one if one is
-- present.
updateTixFile :: (MonadIO m,MonadReader env m,HasConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,HasEnvConfig env)
            => PackageName -> Path Abs File -> String -> m ()
updateTixFile pkgName tixSrc testName = do
    exists <- doesFileExist tixSrc
    when exists $ do
        tixDest <- tixFilePath pkgName testName
        ignoringAbsence (removeFile tixDest)
        ensureDir (parent tixDest)
        -- Remove exe modules because they are problematic. This could be revisited if there's a GHC
        -- version that fixes https://ghc.haskell.org/trac/ghc/ticket/1853
        mtix <- readTixOrLog tixSrc
        case mtix of
            Nothing -> $logError $ "Failed to read " <> T.pack (toFilePath tixSrc)
            Just tix -> do
                liftIO $ writeTix (toFilePath tixDest) (removeExeModules tix)
                ignoringAbsence (removeFile tixSrc)

-- | Get the directory used for hpc reports for the given pkgId.
hpcPkgPath :: (MonadIO m,MonadReader env m,HasConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,HasEnvConfig env)
            => PackageName -> m (Path Abs Dir)
hpcPkgPath pkgName = do
    outputDir <- hpcReportDir
    pkgNameRel <- parseRelDir (packageNameString pkgName)
    return (outputDir </> pkgNameRel)

-- | Get the tix file location, given the name of the file (without extension), and the package
-- identifier string.
tixFilePath :: (MonadIO m,MonadReader env m,HasConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,HasEnvConfig env)
            => PackageName -> String ->  m (Path Abs File)
tixFilePath pkgName testName = do
    pkgPath <- hpcPkgPath pkgName
    tixRel <- parseRelFile (testName ++ "/" ++ testName ++ ".tix")
    return (pkgPath </> tixRel)

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
        tixSrc <- tixFilePath (packageName package) (T.unpack testName)
        let report = "coverage report for " <> pkgName <> "'s test-suite \"" <> testName <> "\""
            reportDir = parent tixSrc
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
    tixFileExists <- doesFileExist tixSrc
    if not tixFileExists
        then $logError $ T.concat
            [ "Didn't find .tix for "
            , report
            , " - expected to find it at "
            , T.pack (toFilePath tixSrc)
            , "."
            ]
        else (`catch` \err -> do
                 let msg = show (err :: ReadProcessException)
                 $logError (T.pack msg)
                 generateHpcErrorReport reportDir $ sanitize msg) $
             (`onException` $logError ("Error occurred while producing " <> report)) $ do
            -- Directories for .mix files.
            hpcRelDir <- hpcRelativeDir
            -- Compute arguments used for both "hpc markup" and "hpc report".
            pkgDirs <- Map.keys . envConfigPackages <$> asks getEnvConfig
            let args =
                    -- Use index files from all packages (allows cross-package coverage results).
                    concatMap (\x -> ["--srcdir", toFilePathNoTrailingSep x]) pkgDirs ++
                    -- Look for index files in the correct dir (relative to each pkgdir).
                    ["--hpcdir", toFilePathNoTrailingSep hpcRelDir, "--reset-hpcdirs"]
            menv <- getMinimalEnvOverride
            $logInfo $ "Generating " <> report
            outputLines <- liftM (map (S8.filter (/= '\r')) . S8.lines) $
                readProcessStdout Nothing menv "hpc"
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
                    forM_ outputLines ($logInfo . T.decodeUtf8)
                    $logInfo
                        ("The " <> report <> " is available at " <>
                         T.pack (toFilePath (reportDir </> $(mkRelFile "hpc_index.html"))))
                    -- Generate the markup.
                    void $ readProcessStdout Nothing menv "hpc"
                        ( "markup"
                        : toFilePath tixSrc
                        : ("--destdir=" ++ toFilePathNoTrailingSep reportDir)
                        : (args ++ extraMarkupArgs)
                        )

data HpcReportOpts = HpcReportOpts
    { hroptsInputs :: [Text]
    , hroptsAll :: Bool
    , hroptsDestDir :: Maybe String
    } deriving (Show)

generateHpcReportForTargets :: (MonadIO m, HasHttpManager env, MonadReader env m, MonadBaseControl IO m, MonadCatch m, MonadLogger m, HasEnvConfig env)
                            => HpcReportOpts -> m ()
generateHpcReportForTargets opts = do
    let (tixFiles, targetNames) = partition (".tix" `T.isSuffixOf`) (hroptsInputs opts)
    targetTixFiles <-
         -- When there aren't any package component arguments, then
         -- don't default to all package components.
         if not (hroptsAll opts) && null targetNames
         then return []
         else do
             when (hroptsAll opts && not (null targetNames)) $
                 $logWarn $ "Since --all is used, it is redundant to specify these targets: " <> T.pack (show targetNames)
             (_,_,targets) <- parseTargetsFromBuildOpts
                 AllowNoTargets
                 defaultBuildOptsCLI
                    { boptsCLITargets = if hroptsAll opts then [] else targetNames }
             liftM concat $ forM (Map.toList targets) $ \(name, target) ->
                 case target of
                     STUnknown -> fail $
                         packageNameString name ++ " isn't a known local page"
                     STNonLocal -> fail $
                         "Expected a local package, but " ++
                         packageNameString name ++
                         " is either an extra-dep or in the snapshot."
                     STLocalComps comps -> do
                         pkgPath <- hpcPkgPath name
                         forM (toList comps) $ \nc ->
                             case nc of
                                 CTest testName ->
                                     liftM (pkgPath </>) $ parseRelFile (T.unpack testName ++ ".tix")
                                 _ -> fail $
                                     "Can't specify anything except test-suites as hpc report targets (" ++
                                     packageNameString name ++
                                     " is used with a non test-suite target)"
                     STLocalAll -> do
                         pkgPath <- hpcPkgPath name
                         exists <- doesDirExist pkgPath
                         if exists
                             then do
                                 (_, files) <- listDir pkgPath
                                 return (filter ((".tix" `isSuffixOf`) . toFilePath) files)
                             else return []
    tixPaths <- liftM (++ targetTixFiles) $ mapM (resolveFile' . T.unpack) tixFiles
    when (null tixPaths) $
        fail "Not generating combined report, because no targets or tix files are specified."
    reportDir <- case hroptsDestDir opts of
        Nothing -> liftM (</> $(mkRelDir "combined/custom")) hpcReportDir
        Just destDir -> do
            dest <- resolveDir' destDir
            ensureDir dest
            return dest
    generateUnionReport "combined report" reportDir tixPaths

generateHpcUnifiedReport :: (MonadIO m,MonadReader env m,HasConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,HasEnvConfig env)
                         => m ()
generateHpcUnifiedReport = do
    outputDir <- hpcReportDir
    ensureDir outputDir
    (dirs, _) <- listDir outputDir
    tixFiles <- liftM (concat . concat) $ forM (filter (("combined" /=) . dirnameString) dirs) $ \dir -> do
        (dirs', _) <- listDir dir
        forM dirs' $ \dir' -> do
            (_, files) <- listDir dir'
            return (filter ((".tix" `isSuffixOf`) . toFilePath) files)
    let reportDir = outputDir </> $(mkRelDir "combined/all")
    if length tixFiles < 2
        then $logInfo $ T.concat
            [ if null tixFiles then "No tix files" else "Only one tix file"
            , " found in "
            , T.pack (toFilePath outputDir)
            , ", so not generating a unified coverage report."
            ]
        else generateUnionReport "unified report" reportDir tixFiles

generateUnionReport :: (MonadIO m,MonadReader env m,HasConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,HasEnvConfig env)
                    => Text -> Path Abs Dir -> [Path Abs File] -> m ()
generateUnionReport report reportDir tixFiles = do
    (errs, tix) <- fmap (unionTixes . map removeExeModules) (mapMaybeM readTixOrLog tixFiles)
    $logDebug $ "Using the following tix files: " <> T.pack (show tixFiles)
    unless (null errs) $ $logWarn $ T.concat $
        "The following modules are left out of the " : report : " due to version mismatches: " :
        intersperse ", " (map T.pack errs)
    tixDest <- liftM (reportDir </>) $ parseRelFile (dirnameString reportDir ++ ".tix")
    ensureDir (parent tixDest)
    liftIO $ writeTix (toFilePath tixDest) tix
    generateHpcReportInternal tixDest reportDir report [] []

readTixOrLog :: (MonadLogger m, MonadIO m, MonadBaseControl IO m) => Path b File -> m (Maybe Tix)
readTixOrLog path = do
    mtix <- liftIO (readTix (toFilePath path)) `catch` \(ErrorCall err) -> do
        $logError $ "Error while reading tix: " <> T.pack err
        return Nothing
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
    ensureDir outputDir
    (dirs, _) <- listDir outputDir
    rows <- liftM (catMaybes . concat) $ forM dirs $ \dir -> do
        (subdirs, _) <- listDir dir
        forM subdirs $ \subdir -> do
            let indexPath = subdir </> $(mkRelFile "hpc_index.html")
            exists' <- doesFileExist indexPath
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
                , "<tr><th>Package</th><th>TestSuite</th><th>Modification Time</th></tr>"
                ] ++
                rows ++
                ["</tbody></table>"]) ++
        ["</body></html>"]
    unless (null rows) $
        $logInfo $ "\nAn index of the generated HTML coverage reports is available at " <>
            T.pack (toFilePath outputFile)

generateHpcErrorReport :: MonadIO m => Path Abs Dir -> Text -> m ()
generateHpcErrorReport dir err = do
    ensureDir dir
    liftIO $ T.writeFile (toFilePath (dir </> $(mkRelFile "hpc_index.html"))) $ T.concat
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

dirnameString :: Path r Dir -> String
dirnameString = dropWhileEnd isPathSeparator . toFilePath . dirname

findPackageKeyForBuiltPackage :: (MonadIO m, MonadReader env m, MonadThrow m, HasEnvConfig env)
                              => Path Abs Dir -> PackageIdentifier -> m (Maybe Text)
findPackageKeyForBuiltPackage pkgDir pkgId = do
    distDir <- distDirFromDir pkgDir
    path <- liftM (distDir </>) $
        parseRelFile ("package.conf.inplace/" ++ packageIdentifierString pkgId ++ "-inplace.conf")
    exists <- doesFileExist path
    if exists
        then do
            contents <- liftIO $ T.readFile (toFilePath path)
            return $ asum (map (T.stripPrefix "key: ") (T.lines contents))
        else return Nothing
