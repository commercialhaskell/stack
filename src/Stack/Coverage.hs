{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}

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

import           Stack.Prelude hiding (Display (..))
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Distribution.Version (mkVersion)
import           Path
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.IO
import           Stack.Build.Target
import           Stack.Constants
import           Stack.Constants.Config
import           Stack.Package
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.NamedComponent
import           Stack.Types.Package
import           Stack.Types.SourceMap
import           System.FilePath (isPathSeparator)
import qualified RIO
import           RIO.PrettyPrint
import           RIO.Process
import           Trace.Hpc.Tix
import           Web.Browser (openBrowser)

newtype CoverageException = NonTestSuiteTarget PackageName deriving Typeable

instance Exception CoverageException
instance Show CoverageException where
    show (NonTestSuiteTarget name) = 
        "Can't specify anything except test-suites as hpc report targets (" ++
        packageNameString name ++
        " is used with a non test-suite target)"

-- | Invoked at the beginning of running with "--coverage"
deleteHpcReports :: HasEnvConfig env => RIO env ()
deleteHpcReports = do
    hpcDir <- hpcReportDir
    liftIO $ ignoringAbsence (removeDirRecur hpcDir)

-- | Move a tix file into a sub-directory of the hpc report directory. Deletes the old one if one is
-- present.
updateTixFile :: HasEnvConfig env => PackageName -> Path Abs File -> String -> RIO env ()
updateTixFile pkgName' tixSrc testName = do
    exists <- doesFileExist tixSrc
    when exists $ do
        tixDest <- tixFilePath pkgName' testName
        liftIO $ ignoringAbsence (removeFile tixDest)
        ensureDir (parent tixDest)
        -- Remove exe modules because they are problematic. This could be revisited if there's a GHC
        -- version that fixes https://ghc.haskell.org/trac/ghc/ticket/1853
        mtix <- readTixOrLog tixSrc
        case mtix of
            Nothing -> logError $ "Failed to read " <> fromString (toFilePath tixSrc)
            Just tix -> do
                liftIO $ writeTix (toFilePath tixDest) (removeExeModules tix)
                -- TODO: ideally we'd do a file move, but IIRC this can
                -- have problems. Something about moving between drives
                -- on windows?
                copyFile tixSrc =<< parseAbsFile (toFilePath tixDest ++ ".premunging")
                liftIO $ ignoringAbsence (removeFile tixSrc)

-- | Get the directory used for hpc reports for the given pkgId.
hpcPkgPath :: HasEnvConfig env => PackageName -> RIO env (Path Abs Dir)
hpcPkgPath pkgName' = do
    outputDir <- hpcReportDir
    pkgNameRel <- parseRelDir (packageNameString pkgName')
    return (outputDir </> pkgNameRel)

-- | Get the tix file location, given the name of the file (without extension), and the package
-- identifier string.
tixFilePath :: HasEnvConfig env
            => PackageName -> String -> RIO env (Path Abs File)
tixFilePath pkgName' testName = do
    pkgPath <- hpcPkgPath pkgName'
    tixRel <- parseRelFile (testName ++ "/" ++ testName ++ ".tix")
    return (pkgPath </> tixRel)

-- | Generates the HTML coverage report and shows a textual coverage summary for a package.
generateHpcReport :: HasEnvConfig env
                  => Path Abs Dir -> Package -> [Text] -> RIO env ()
generateHpcReport pkgDir package tests = do
    compilerVersion <- view actualCompilerVersionL
    -- If we're using > GHC 7.10, the hpc 'include' parameter must specify a ghc package key. See
    -- https://github.com/commercialhaskell/stack/issues/785
    let pkgName' = T.pack $ packageNameString (packageName package)
        pkgId = packageIdentifierString (packageIdentifier package)
        ghcVersion = getGhcVersion compilerVersion
        hasLibrary =
          case packageLibraries package of
            NoLibraries -> False
            HasLibraries _ -> True
        internalLibs = packageInternalLibraries package
    eincludeName <-
        -- Pre-7.8 uses plain PKG-version in tix files.
        if ghcVersion < mkVersion [7, 10] then return $ Right $ Just [pkgId]
        -- We don't expect to find a package key if there is no library.
        else if not hasLibrary && Set.null internalLibs then return $ Right Nothing
        -- Look in the inplace DB for the package key.
        -- See https://github.com/commercialhaskell/stack/issues/1181#issuecomment-148968986
        else do
            -- GHC 8.0 uses package id instead of package key.
            -- See https://github.com/commercialhaskell/stack/issues/2424
            let hpcNameField = if ghcVersion >= mkVersion [8, 0] then "id" else "key"
            eincludeName <- findPackageFieldForBuiltPackage pkgDir (packageIdentifier package) internalLibs hpcNameField
            case eincludeName of
                Left err -> do
                    logError $ RIO.display err
                    return $ Left err
                Right includeNames -> return $ Right $ Just $ map T.unpack includeNames
    forM_ tests $ \testName -> do
        tixSrc <- tixFilePath (packageName package) (T.unpack testName)
        let report = "coverage report for " <> pkgName' <> "'s test-suite \"" <> testName <> "\""
            reportDir = parent tixSrc
        case eincludeName of
            Left err -> generateHpcErrorReport reportDir (RIO.display (sanitize (T.unpack err)))
            -- Restrict to just the current library code, if there is a library in the package (see
            -- #634 - this will likely be customizable in the future)
            Right mincludeName -> do
                let extraArgs = case mincludeName of
                        Just includeNames -> "--include" : intersperse "--include" (map (\n -> n ++ ":") includeNames)
                        Nothing -> []
                mreportPath <- generateHpcReportInternal tixSrc reportDir report extraArgs extraArgs
                forM_ mreportPath (displayReportPath report . pretty)

generateHpcReportInternal :: HasEnvConfig env
                          => Path Abs File -> Path Abs Dir -> Text -> [String] -> [String]
                          -> RIO env (Maybe (Path Abs File))
generateHpcReportInternal tixSrc reportDir report extraMarkupArgs extraReportArgs = do
    -- If a .tix file exists, move it to the HPC output directory and generate a report for it.
    tixFileExists <- doesFileExist tixSrc
    if not tixFileExists
        then do
            logError $
                 "Didn't find .tix for " <>
                 RIO.display report <>
                 " - expected to find it at " <>
                 fromString (toFilePath tixSrc) <>
                 "."
            return Nothing
        else (`catch` \(err :: ProcessException) -> do
                 logError $ displayShow err
                 generateHpcErrorReport reportDir $ RIO.display $ sanitize $ show err
                 return Nothing) $
             (`onException` logError ("Error occurred while producing " <> RIO.display report)) $ do
            -- Directories for .mix files.
            hpcRelDir <- hpcRelativeDir
            -- Compute arguments used for both "hpc markup" and "hpc report".
            pkgDirs <- view $ buildConfigL.to (map ppRoot . Map.elems . smwProject . bcSMWanted)
            let args =
                    -- Use index files from all packages (allows cross-package coverage results).
                    concatMap (\x -> ["--srcdir", toFilePathNoTrailingSep x]) pkgDirs ++
                    -- Look for index files in the correct dir (relative to each pkgdir).
                    ["--hpcdir", toFilePathNoTrailingSep hpcRelDir, "--reset-hpcdirs"]
            logInfo $ "Generating " <> RIO.display report
            outputLines <- liftM (map (S8.filter (/= '\r')) . S8.lines . BL.toStrict . fst) $
                proc "hpc"
                ( "report"
                : toFilePath tixSrc
                : (args ++ extraReportArgs)
                )
                readProcess_
            if all ("(0/0)" `S8.isSuffixOf`) outputLines
                then do
                    let msg html =
                            "Error: The " <>
                            RIO.display report <>
                            " did not consider any code. One possible cause of this is" <>
                            " if your test-suite builds the library code (see stack " <>
                            (if html then "<a href='https://github.com/commercialhaskell/stack/issues/1008'>" else "") <>
                            "issue #1008" <>
                            (if html then "</a>" else "") <>
                            "). It may also indicate a bug in stack or" <>
                            " the hpc program. Please report this issue if you think" <>
                            " your coverage report should have meaningful results."
                    logError (msg False)
                    generateHpcErrorReport reportDir (msg True)
                    return Nothing
                else do
                    let reportPath = reportDir </> relFileHpcIndexHtml
                    -- Print output, stripping @\r@ characters because Windows.
                    forM_ outputLines (logInfo . displayBytesUtf8)
                    -- Generate the markup.
                    void $ proc "hpc"
                        ( "markup"
                        : toFilePath tixSrc
                        : ("--destdir=" ++ toFilePathNoTrailingSep reportDir)
                        : (args ++ extraMarkupArgs)
                        )
                        readProcess_
                    return (Just reportPath)

data HpcReportOpts = HpcReportOpts
    { hroptsInputs :: [Text]
    , hroptsAll :: Bool
    , hroptsDestDir :: Maybe String
    , hroptsOpenBrowser :: Bool
    } deriving (Show)

generateHpcReportForTargets :: HasEnvConfig env
                            => HpcReportOpts -> [Text] -> [Text] -> RIO env ()
generateHpcReportForTargets opts tixFiles targetNames = do
    targetTixFiles <-
         -- When there aren't any package component arguments, and --all
         -- isn't passed, default to not considering any targets.
         if not (hroptsAll opts) && null targetNames
         then return []
         else do
             when (hroptsAll opts && not (null targetNames)) $
                 logWarn $ "Since --all is used, it is redundant to specify these targets: " <> displayShow targetNames
             targets <- view $ envConfigL.to envConfigSourceMap.to smTargets.to smtTargets
             liftM concat $ forM (Map.toList targets) $ \(name, target) ->
                 case target of
                     TargetAll PTDependency -> throwString $
                         "Error: Expected a local package, but " ++
                         packageNameString name ++
                         " is either an extra-dep or in the snapshot."
                     TargetComps comps -> do
                         pkgPath <- hpcPkgPath name
                         forM (toList comps) $ \nc ->
                             case nc of
                                 CTest testName ->
                                     liftM (pkgPath </>) $ parseRelFile (T.unpack testName ++ "/" ++ T.unpack testName ++ ".tix")
                                 _ -> throwIO $ NonTestSuiteTarget name
                                     
                     TargetAll PTProject -> do
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
        throwString "Not generating combined report, because no targets or tix files are specified."
    outputDir <- hpcReportDir
    reportDir <- case hroptsDestDir opts of
        Nothing -> return (outputDir </> relDirCombined </> relDirCustom)
        Just destDir -> do
            dest <- resolveDir' destDir
            ensureDir dest
            return dest
    let report = "combined report"
    mreportPath <- generateUnionReport report reportDir tixPaths
    forM_ mreportPath $ \reportPath ->
        if hroptsOpenBrowser opts
            then do
                prettyInfo $ "Opening" <+> pretty reportPath <+> "in the browser."
                void $ liftIO $ openBrowser (toFilePath reportPath)
            else displayReportPath report (pretty reportPath)

generateHpcUnifiedReport :: HasEnvConfig env => RIO env ()
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
        reportDir = outputDir </> relDirCombined </> relDirAll
    if length tixFiles < 2
        then logInfo $
            (if null tixFiles then "No tix files" else "Only one tix file") <>
            " found in " <>
            fromString (toFilePath outputDir) <>
            ", so not generating a unified coverage report."
        else do
            let report = "unified report"
            mreportPath <- generateUnionReport report reportDir tixFiles
            forM_ mreportPath (displayReportPath report . pretty)

generateUnionReport :: HasEnvConfig env
                    => Text -> Path Abs Dir -> [Path Abs File]
                    -> RIO env (Maybe (Path Abs File))
generateUnionReport report reportDir tixFiles = do
    (errs, tix) <- fmap (unionTixes . map removeExeModules) (mapMaybeM readTixOrLog tixFiles)
    logDebug $ "Using the following tix files: " <> fromString (show tixFiles)
    unless (null errs) $ logWarn $
        "The following modules are left out of the " <>
        RIO.display report <>
        " due to version mismatches: " <>
        mconcat (intersperse ", " (map fromString errs))
    tixDest <- liftM (reportDir </>) $ parseRelFile (dirnameString reportDir ++ ".tix")
    ensureDir (parent tixDest)
    liftIO $ writeTix (toFilePath tixDest) tix
    generateHpcReportInternal tixDest reportDir report [] []

readTixOrLog :: HasLogFunc env => Path b File -> RIO env (Maybe Tix)
readTixOrLog path = do
    mtix <- liftIO (readTix (toFilePath path)) `catchAny` \errorCall -> do
        logError $ "Error while reading tix: " <> fromString (show errorCall)
        return Nothing
    when (isNothing mtix) $
        logError $ "Failed to read tix file " <> fromString (toFilePath path)
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

generateHpcMarkupIndex :: HasEnvConfig env => RIO env ()
generateHpcMarkupIndex = do
    outputDir <- hpcReportDir
    let outputFile = outputDir </> relFileIndexHtml
    ensureDir outputDir
    (dirs, _) <- listDir outputDir
    rows <- liftM (catMaybes . concat) $ forM dirs $ \dir -> do
        (subdirs, _) <- listDir dir
        forM subdirs $ \subdir -> do
            let indexPath = subdir </> relFileHpcIndexHtml
            exists' <- doesFileExist indexPath
            if not exists' then return Nothing else do
                relPath <- stripProperPrefix outputDir indexPath
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
    writeBinaryFileAtomic outputFile $
        "<html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">" <>
        -- Part of the css from HPC's output HTML
        "<style type=\"text/css\">" <>
        "table.dashboard { border-collapse: collapse; border: solid 1px black }" <>
        ".dashboard td { border: solid 1px black }" <>
        ".dashboard th { border: solid 1px black }" <>
        "</style>" <>
        "</head>" <>
        "<body>" <>
        (if null rows
            then
                "<b>No hpc_index.html files found in \"" <>
                encodeUtf8Builder (pathToHtml outputDir) <>
                "\".</b>"
            else
                "<table class=\"dashboard\" width=\"100%\" border=\"1\"><tbody>" <>
                "<p><b>NOTE: This is merely a listing of the html files found in the coverage reports directory.  Some of these reports may be old.</b></p>" <>
                "<tr><th>Package</th><th>TestSuite</th><th>Modification Time</th></tr>" <>
                foldMap encodeUtf8Builder rows <>
                "</tbody></table>") <>
        "</body></html>"
    unless (null rows) $
        logInfo $ "\nAn index of the generated HTML coverage reports is available at " <>
            fromString (toFilePath outputFile)

generateHpcErrorReport :: MonadIO m => Path Abs Dir -> Utf8Builder -> m ()
generateHpcErrorReport dir err = do
    ensureDir dir
    let fp = toFilePath (dir </> relFileHpcIndexHtml)
    writeFileUtf8Builder fp $
        "<html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></head><body>" <>
        "<h1>HPC Report Generation Error</h1>" <>
        "<p>" <>
        err <>
        "</p>" <>
        "</body></html>"

pathToHtml :: Path b t -> Text
pathToHtml = T.dropWhileEnd (=='/') . sanitize . toFilePath

-- | Escape HTML symbols (copied from Text.Hastache)
htmlEscape :: LT.Text -> LT.Text
htmlEscape = LT.concatMap proc_
  where
    proc_ '&'  = "&amp;"
    proc_ '\\' = "&#92;"
    proc_ '"'  = "&quot;"
    proc_ '\'' = "&#39;"
    proc_ '<'  = "&lt;"
    proc_ '>'  = "&gt;"
    proc_ h    = LT.singleton h

sanitize :: String -> Text
sanitize = LT.toStrict . htmlEscape . LT.pack

dirnameString :: Path r Dir -> String
dirnameString = dropWhileEnd isPathSeparator . toFilePath . dirname

findPackageFieldForBuiltPackage
    :: HasEnvConfig env
    => Path Abs Dir -> PackageIdentifier -> Set.Set Text -> Text
    -> RIO env (Either Text [Text])
findPackageFieldForBuiltPackage pkgDir pkgId internalLibs field = do
    distDir <- distDirFromDir pkgDir
    let inplaceDir = distDir </> relDirPackageConfInplace
        pkgIdStr = packageIdentifierString pkgId
        notFoundErr = return $ Left $ "Failed to find package key for " <> T.pack pkgIdStr
        extractField path = do
            contents <- readFileUtf8 (toFilePath path)
            case asum (map (T.stripPrefix (field <> ": ")) (T.lines contents)) of
                Just result -> return $ Right $ T.strip result
                Nothing -> notFoundErr
    cabalVer <- view cabalVersionL
    if cabalVer < mkVersion [1, 24]
        then do
            -- here we don't need to handle internal libs
            path <- liftM (inplaceDir </>) $ parseRelFile (pkgIdStr ++ "-inplace.conf")
            logDebug $ "Parsing config in Cabal < 1.24 location: " <> fromString (toFilePath path)
            exists <- doesFileExist path
            if exists then fmap (:[]) <$> extractField path else notFoundErr
        else do
            -- With Cabal-1.24, it's in a different location.
            logDebug $ "Scanning " <> fromString (toFilePath inplaceDir) <> " for files matching " <> fromString pkgIdStr
            (_, files) <- handleIO (const $ return ([], [])) $ listDir inplaceDir
            logDebug $ displayShow files
            -- From all the files obtained from the scanning process above, we
            -- need to identify which are .conf files and then ensure that
            -- there is at most one .conf file for each library and internal
            -- library (some might be missing if that component has not been
            -- built yet). We should error if there are more than one .conf
            -- file for a component or if there are no .conf files at all in
            -- the searched location.
            let toFilename = T.pack . toFilePath . filename
                -- strip known prefix and suffix from the found files to determine only the conf files
                stripKnown =  T.stripSuffix ".conf" <=< T.stripPrefix (T.pack (pkgIdStr ++ "-"))
                stripped = mapMaybe (\file -> fmap (,file) . stripKnown . toFilename $ file) files
                -- which component could have generated each of these conf files
                stripHash n = let z = T.dropWhile (/= '-') n in if T.null z then "" else T.tail z
                matchedComponents = map (\(n, f) -> (stripHash n, [f])) stripped
                byComponents = Map.restrictKeys (Map.fromListWith (++) matchedComponents) $ Set.insert "" internalLibs
            logDebug $ displayShow byComponents
            if Map.null $ Map.filter (\fs -> length fs > 1) byComponents
            then case concat $ Map.elems byComponents of
                [] -> notFoundErr
                -- for each of these files, we need to extract the requested field
                paths -> do
                  (errors, keys) <-  partitionEithers <$> traverse extractField paths
                  case errors of
                    (a:_) -> return $ Left a -- the first error only, since they're repeated anyway
                    [] -> return $ Right keys
            else return $ Left $ "Multiple files matching " <> T.pack (pkgIdStr ++ "-*.conf") <> " found in " <>
                    T.pack (toFilePath inplaceDir) <> ". Maybe try 'stack clean' on this package?"

displayReportPath :: (HasTerm env)
                  => Text -> StyleDoc -> RIO env ()
displayReportPath report reportPath =
     prettyInfo $ "The" <+> fromString (T.unpack report) <+> "is available at" <+> reportPath

findExtraTixFiles :: HasEnvConfig env => RIO env [Path Abs File]
findExtraTixFiles = do
    outputDir <- hpcReportDir
    let dir = outputDir </> relDirExtraTixFiles
    dirExists <- doesDirExist dir
    if dirExists
        then do
            (_, files) <- listDir dir
            return $ filter ((".tix" `isSuffixOf`) . toFilePath) files
        else return []
