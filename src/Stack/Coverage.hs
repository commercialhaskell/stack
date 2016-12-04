{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
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

import           Control.Exception.Safe (handleIO)
import           Control.Exception.Lifted
import           Control.Monad (liftM, when, unless, void, (<=<))
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as S8
import           Data.Foldable (forM_, asum, toList)
import           Data.Function
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Maybe.Extra (mapMaybeM)
import           Data.Monoid ((<>))
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import           Data.Traversable (forM)
import           Path
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.IO
import           Prelude hiding (FilePath, writeFile)
import           Stack.Build.Source (parseTargetsFromBuildOpts)
import           Stack.Build.Target
import           Stack.Config (getLocalPackages)
import           Stack.Constants
import           Stack.Package
import           Stack.PrettyPrint
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.Package
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.StackT (StackM)
import           Stack.Types.Version
import           System.FilePath (isPathSeparator)
import           System.Process.Read
import           Text.Hastache (htmlEscape)
import           Trace.Hpc.Tix
import           Web.Browser (openBrowser)

-- | Invoked at the beginning of running with "--coverage"
deleteHpcReports :: (StackM env m, HasEnvConfig env)
                 => m ()
deleteHpcReports = do
    hpcDir <- hpcReportDir
    ignoringAbsence (removeDirRecur hpcDir)

-- | Move a tix file into a sub-directory of the hpc report directory. Deletes the old one if one is
-- present.
updateTixFile :: (StackM env m, HasEnvConfig env)
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
                -- TODO: ideally we'd do a file move, but IIRC this can
                -- have problems. Something about moving between drives
                -- on windows?
                copyFile tixSrc =<< parseAbsFile (toFilePath tixDest ++ ".premunging")
                ignoringAbsence (removeFile tixSrc)

-- | Get the directory used for hpc reports for the given pkgId.
hpcPkgPath :: (StackM env m, HasEnvConfig env)
            => PackageName -> m (Path Abs Dir)
hpcPkgPath pkgName = do
    outputDir <- hpcReportDir
    pkgNameRel <- parseRelDir (packageNameString pkgName)
    return (outputDir </> pkgNameRel)

-- | Get the tix file location, given the name of the file (without extension), and the package
-- identifier string.
tixFilePath :: (StackM env m, HasEnvConfig env)
            => PackageName -> String ->  m (Path Abs File)
tixFilePath pkgName testName = do
    pkgPath <- hpcPkgPath pkgName
    tixRel <- parseRelFile (testName ++ "/" ++ testName ++ ".tix")
    return (pkgPath </> tixRel)

-- | Generates the HTML coverage report and shows a textual coverage summary for a package.
generateHpcReport :: (StackM env m, HasEnvConfig env)
                  => Path Abs Dir -> Package -> [Text] -> m ()
generateHpcReport pkgDir package tests = do
    compilerVersion <- view actualCompilerVersionL
    -- If we're using > GHC 7.10, the hpc 'include' parameter must specify a ghc package key. See
    -- https://github.com/commercialhaskell/stack/issues/785
    let pkgName = packageNameText (packageName package)
        pkgId = packageIdentifierString (packageIdentifier package)
        ghcVersion = getGhcVersion compilerVersion
    eincludeName <-
        -- Pre-7.8 uses plain PKG-version in tix files.
        if ghcVersion < $(mkVersion "7.10") then return $ Right $ Just pkgId
        -- We don't expect to find a package key if there is no library.
        else if not (packageHasLibrary package) then return $ Right Nothing
        -- Look in the inplace DB for the package key.
        -- See https://github.com/commercialhaskell/stack/issues/1181#issuecomment-148968986
        else do
            -- GHC 8.0 uses package id instead of package key.
            -- See https://github.com/commercialhaskell/stack/issues/2424
            let hpcNameField = if ghcVersion >= $(mkVersion "8.0") then "id" else "key"
            eincludeName <- findPackageFieldForBuiltPackage pkgDir (packageIdentifier package) hpcNameField
            case eincludeName of
                Left err -> do
                    $logError err
                    return $ Left err
                Right includeName -> return $ Right $ Just $ T.unpack includeName
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
                mreportPath <- generateHpcReportInternal tixSrc reportDir report extraArgs extraArgs
                forM_ mreportPath (displayReportPath report)

generateHpcReportInternal :: (StackM env m, HasEnvConfig env)
                          => Path Abs File -> Path Abs Dir -> Text -> [String] -> [String] -> m (Maybe (Path Abs File))
generateHpcReportInternal tixSrc reportDir report extraMarkupArgs extraReportArgs = do
    -- If a .tix file exists, move it to the HPC output directory and generate a report for it.
    tixFileExists <- doesFileExist tixSrc
    if not tixFileExists
        then do
            $logError $ T.concat
                 [ "Didn't find .tix for "
                 , report
                 , " - expected to find it at "
                 , T.pack (toFilePath tixSrc)
                 , "."
                 ]
            return Nothing
        else (`catch` \err -> do
                 let msg = show (err :: ReadProcessException)
                 $logError (T.pack msg)
                 generateHpcErrorReport reportDir $ sanitize msg
                 return Nothing) $
             (`onException` $logError ("Error occurred while producing " <> report)) $ do
            -- Directories for .mix files.
            hpcRelDir <- hpcRelativeDir
            -- Compute arguments used for both "hpc markup" and "hpc report".
            pkgDirs <- liftM Map.keys getLocalPackages
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
                    return Nothing
                else do
                    let reportPath = reportDir </> $(mkRelFile "hpc_index.html")
                    -- Print output, stripping @\r@ characters because Windows.
                    forM_ outputLines ($logInfo . T.decodeUtf8)
                    -- Generate the markup.
                    void $ readProcessStdout Nothing menv "hpc"
                        ( "markup"
                        : toFilePath tixSrc
                        : ("--destdir=" ++ toFilePathNoTrailingSep reportDir)
                        : (args ++ extraMarkupArgs)
                        )
                    return (Just reportPath)

data HpcReportOpts = HpcReportOpts
    { hroptsInputs :: [Text]
    , hroptsAll :: Bool
    , hroptsDestDir :: Maybe String
    , hroptsOpenBrowser :: Bool
    } deriving (Show)

generateHpcReportForTargets :: (StackM env m, HasEnvConfig env)
                            => HpcReportOpts -> m ()
generateHpcReportForTargets opts = do
    let (tixFiles, targetNames) = partition (".tix" `T.isSuffixOf`) (hroptsInputs opts)
    targetTixFiles <-
         -- When there aren't any package component arguments, and --all
         -- isn't passed, default to not considering any targets.
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
                                     liftM (pkgPath </>) $ parseRelFile (T.unpack testName ++ "/" ++ T.unpack testName ++ ".tix")
                                 _ -> fail $
                                     "Can't specify anything except test-suites as hpc report targets (" ++
                                     packageNameString name ++
                                     " is used with a non test-suite target)"
                     STLocalAll -> do
                         pkgPath <- hpcPkgPath name
                         exists <- doesDirExist pkgPath
                         if exists
                             then do
                                 (dirs, _) <- listDir pkgPath
                                 liftM concat $ forM dirs $ \dir -> do
                                     (_, files) <- listDir dir
                                     return (filter ((".tix" `isSuffixOf`) . toFilePath) files)
                             else return []
    tixPaths <- liftM (\xs -> xs ++ targetTixFiles) $ mapM (resolveFile' . T.unpack) tixFiles
    when (null tixPaths) $
        fail "Not generating combined report, because no targets or tix files are specified."
    outputDir <- hpcReportDir
    reportDir <- case hroptsDestDir opts of
        Nothing -> return (outputDir </> $(mkRelDir "combined/custom"))
        Just destDir -> do
            dest <- resolveDir' destDir
            ensureDir dest
            return dest
    let report = "combined report"
    mreportPath <- generateUnionReport report reportDir tixPaths
    forM_ mreportPath $ \reportPath ->
        if hroptsOpenBrowser opts
            then do
                $prettyInfo $ "Opening" <+> display reportPath <+> "in the browser."
                void $ liftIO $ openBrowser (toFilePath reportPath)
            else displayReportPath report reportPath

generateHpcUnifiedReport :: (StackM env m, HasEnvConfig env)
                         => m ()
generateHpcUnifiedReport = do
    outputDir <- hpcReportDir
    ensureDir outputDir
    (dirs, _) <- listDir outputDir
    tixFiles0 <- liftM (concat . concat) $ forM (filter (("combined" /=) . dirnameString) dirs) $ \dir -> do
        (dirs', _) <- listDir dir
        forM dirs' $ \dir' -> do
            (_, files) <- listDir dir'
            return (filter ((".tix" `isSuffixOf`) . toFilePath) files)
    extraTixFiles <- findExtraTixFiles
    let tixFiles = tixFiles0  ++ extraTixFiles
        reportDir = outputDir </> $(mkRelDir "combined/all")
    if length tixFiles < 2
        then $logInfo $ T.concat
            [ if null tixFiles then "No tix files" else "Only one tix file"
            , " found in "
            , T.pack (toFilePath outputDir)
            , ", so not generating a unified coverage report."
            ]
        else do
            let report = "unified report"
            mreportPath <- generateUnionReport report reportDir tixFiles
            forM_ mreportPath (displayReportPath report)

generateUnionReport :: (StackM env m, HasEnvConfig env)
                    => Text -> Path Abs Dir -> [Path Abs File] -> m (Maybe (Path Abs File))
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
    mtix <- liftIO (readTix (toFilePath path)) `catch` \errorCall -> do
        $logError $ "Error while reading tix: " <> T.pack (show (errorCall :: ErrorCall))
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

generateHpcMarkupIndex :: (StackM env m, HasEnvConfig env)
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

findPackageFieldForBuiltPackage
    :: (StackM env m, HasEnvConfig env)
    => Path Abs Dir -> PackageIdentifier -> Text -> m (Either Text Text)
findPackageFieldForBuiltPackage pkgDir pkgId field = do
    distDir <- distDirFromDir pkgDir
    let inplaceDir = distDir </> $(mkRelDir "package.conf.inplace")
        pkgIdStr = packageIdentifierString pkgId
        notFoundErr = return $ Left $ "Failed to find package key for " <> T.pack pkgIdStr
        extractField path = do
            contents <- liftIO $ T.readFile (toFilePath path)
            case asum (map (T.stripPrefix (field <> ": ")) (T.lines contents)) of
                Just result -> return $ Right result
                Nothing -> notFoundErr
    cabalVer <- view cabalVersionL
    if cabalVer < $(mkVersion "1.24")
        then do
            path <- liftM (inplaceDir </>) $ parseRelFile (pkgIdStr ++ "-inplace.conf")
            $logDebug $ "Parsing config in Cabal < 1.24 location: " <> T.pack (toFilePath path)
            exists <- doesFileExist path
            if exists then extractField path else notFoundErr
        else do
            -- With Cabal-1.24, it's in a different location.
            $logDebug $ "Scanning " <> T.pack (toFilePath inplaceDir) <> " for files matching " <> T.pack pkgIdStr
            (_, files) <- handleIO (const $ return ([], [])) $ listDir inplaceDir
            $logDebug $ T.pack (show files)
            case mapMaybe (\file -> fmap (const file) . (T.stripSuffix ".conf" <=< T.stripPrefix (T.pack (pkgIdStr ++ "-")))
                          . T.pack . toFilePath . filename $ file) files of
                [] -> notFoundErr
                [path] -> extractField path
                _ -> return $ Left $ "Multiple files matching " <> T.pack (pkgIdStr ++ "-*.conf") <> " found in " <>
                    T.pack (toFilePath inplaceDir) <> ". Maybe try 'stack clean' on this package?"

displayReportPath :: (StackM env m, HasAnsiAnn (Ann a), Display a)
                  => Text -> a -> m ()
displayReportPath report reportPath =
     $prettyInfo $ "The" <+> fromString (T.unpack report) <+> "is available at" <+> display reportPath

findExtraTixFiles :: (StackM env m , HasEnvConfig env) => m [Path Abs File]
findExtraTixFiles = do
    outputDir <- hpcReportDir
    let dir = outputDir </> $(mkRelDir "extra-tix-files")
    dirExists <- doesDirExist dir
    if dirExists
        then do
            (_, files) <- listDir dir
            return $ filter ((".tix" `isSuffixOf`) . toFilePath) files
        else return []
