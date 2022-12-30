{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

-- | Generate HPC (Haskell Program Coverage) reports
module Stack.Coverage
  ( deleteHpcReports
  , updateTixFile
  , generateHpcReport
  , HpcReportOpts (..)
  , generateHpcReportForTargets
  , generateHpcUnifiedReport
  , generateHpcMarkupIndex
  ) where

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Distribution.Version ( mkVersion )
import           Path
                   ( (</>), dirname, filename, parent, parseAbsFile, parseRelDir
                   , parseRelFile, stripProperPrefix
                   )
import           Path.Extra ( toFilePathNoTrailingSep )
import           Path.IO
                   ( copyFile, doesDirExist, doesFileExist, ensureDir
                   , ignoringAbsence, listDir, removeDirRecur, removeFile
                   , resolveDir', resolveFile'
                   )
import           RIO.Process ( ProcessException, proc, readProcess_ )
import           Stack.Build.Target (  )
import           Stack.Constants
                   ( relDirAll, relDirCombined, relDirCustom
                   , relDirExtraTixFiles, relDirPackageConfInplace
                   , relFileHpcIndexHtml, relFileIndexHtml
                   )
import           Stack.Constants.Config ( distDirFromDir, hpcRelativeDir )
import           Stack.Package (  )
import           Stack.Prelude
import           Stack.Types.Compiler ( getGhcVersion )
import           Stack.Types.Config
                   ( BuildConfig (..), EnvConfig (..), HasBuildConfig (..)
                   , HasEnvConfig (..), actualCompilerVersionL, cabalVersionL
                   , hpcReportDir, ppRoot
                   )
import           Stack.Types.NamedComponent ( NamedComponent (..) )
import           Stack.Types.Package
                   ( Package (..), PackageLibraries (..), packageIdentifier )
import           Stack.Types.SourceMap
                   ( PackageType (..), SMTargets (..), SMWanted (..)
                   , SourceMap (..), Target (..)
                   )
import           System.FilePath ( isPathSeparator )
import           Trace.Hpc.Tix ( Tix (..), TixModule (..), readTix, writeTix )
import           Web.Browser ( openBrowser )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Coverage" module.
data CoverageException
  = NonTestSuiteTarget PackageName
  | NoTargetsOrTixSpecified
  | NotLocalPackage PackageName
  deriving (Show, Typeable)

instance Exception CoverageException where
  displayException (NonTestSuiteTarget name) = concat
    [ "Error: [S-6361]\n"
    , "Can't specify anything except test-suites as hpc report targets ("
    , packageNameString name
    , ") is used with a non test-suite target."
    ]
  displayException NoTargetsOrTixSpecified =
    "Error: [S-2321]\n"
    ++ "Not generating combined report, because no targets or tix files \
       \are specified."
  displayException (NotLocalPackage name) = concat
    [ "Error: [S-9975]"
    , "Expected a local package, but "
    , packageNameString name
    , " is either an extra-dep or in the snapshot."
    ]

-- | Invoked at the beginning of running with "--coverage"
deleteHpcReports :: HasEnvConfig env => RIO env ()
deleteHpcReports = do
  hpcDir <- hpcReportDir
  liftIO $ ignoringAbsence (removeDirRecur hpcDir)

-- | Move a tix file into a sub-directory of the hpc report directory. Deletes
-- the old one if one is present.
updateTixFile ::
     HasEnvConfig env
  => PackageName
  -> Path Abs File
  -> String
  -> RIO env ()
updateTixFile pkgName' tixSrc testName = do
  exists <- doesFileExist tixSrc
  when exists $ do
    tixDest <- tixFilePath pkgName' testName
    liftIO $ ignoringAbsence (removeFile tixDest)
    ensureDir (parent tixDest)
    -- Remove exe modules because they are problematic. This could be
    -- revisited if there's a GHC version that fixes
    -- https://ghc.haskell.org/trac/ghc/ticket/1853
    mtix <- readTixOrLog tixSrc
    case mtix of
      Nothing -> logError $
        "Error: [S-2887]\n" <>
        "Failed to read " <>
        fromString (toFilePath tixSrc)
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
  pure (outputDir </> pkgNameRel)

-- | Get the tix file location, given the name of the file (without extension),
-- and the package identifier string.
tixFilePath :: HasEnvConfig env
            => PackageName -> String -> RIO env (Path Abs File)
tixFilePath pkgName' testName = do
  pkgPath <- hpcPkgPath pkgName'
  tixRel <- parseRelFile (testName ++ "/" ++ testName ++ ".tix")
  pure (pkgPath </> tixRel)

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
    if ghcVersion < mkVersion [7, 10] then pure $ Right $ Just [pkgId]
    -- We don't expect to find a package key if there is no library.
    else if not hasLibrary && Set.null internalLibs then pure $ Right Nothing
    -- Look in the inplace DB for the package key.
    -- See https://github.com/commercialhaskell/stack/issues/1181#issuecomment-148968986
    else do
      -- GHC 8.0 uses package id instead of package key.
      -- See https://github.com/commercialhaskell/stack/issues/2424
      let hpcNameField = if ghcVersion >= mkVersion [8, 0] then "id" else "key"
      eincludeName <-
        findPackageFieldForBuiltPackage
          pkgDir
          (packageIdentifier package)
          internalLibs
          hpcNameField
      case eincludeName of
        Left err -> do
          logError $ display err
          pure $ Left err
        Right includeNames -> pure $ Right $ Just $ map T.unpack includeNames
  forM_ tests $ \testName -> do
    tixSrc <- tixFilePath (packageName package) (T.unpack testName)
    let report = "coverage report for " <> pkgName' <> "'s test-suite \"" <> testName <> "\""
        reportDir = parent tixSrc
    case eincludeName of
      Left err -> generateHpcErrorReport reportDir (display (sanitize (T.unpack err)))
      -- Restrict to just the current library code, if there is a library in the package (see
      -- #634 - this will likely be customizable in the future)
      Right mincludeName -> do
        let extraArgs = case mincludeName of
              Nothing -> []
              Just includeNames ->
                  "--include"
                : L.intersperse "--include" (map (++ ":") includeNames)
        mreportPath <-
          generateHpcReportInternal tixSrc reportDir report extraArgs extraArgs
        forM_ mreportPath (displayReportPath "The" report . pretty)

generateHpcReportInternal :: HasEnvConfig env
                          => Path Abs File -> Path Abs Dir -> Text -> [String] -> [String]
                          -> RIO env (Maybe (Path Abs File))
generateHpcReportInternal tixSrc reportDir report extraMarkupArgs extraReportArgs = do
  -- If a .tix file exists, move it to the HPC output directory and generate a
  -- report for it.
  tixFileExists <- doesFileExist tixSrc
  if not tixFileExists
    then do
      logError $
        "Error: [S-4634]\n" <>
        "Didn't find .tix for " <>
        display report <>
        " - expected to find it at " <>
        fromString (toFilePath tixSrc) <>
        "."
      pure Nothing
    else (`catch` \(err :: ProcessException) -> do
           logError $ displayShow err
           generateHpcErrorReport reportDir $ display $ sanitize $
               displayException err
           pure Nothing) $
       (`onException`
           logError
             ("Error: [S-8215]\n" <>
              "Error occurred while producing " <>
              display report)) $ do
      -- Directories for .mix files.
      hpcRelDir <- hpcRelativeDir
      -- Compute arguments used for both "hpc markup" and "hpc report".
      pkgDirs <- view $ buildConfigL.to (map ppRoot . Map.elems . smwProject . bcSMWanted)
      let args =
            -- Use index files from all packages (allows cross-package coverage results).
            concatMap (\x -> ["--srcdir", toFilePathNoTrailingSep x]) pkgDirs ++
            -- Look for index files in the correct dir (relative to each pkgdir).
            ["--hpcdir", toFilePathNoTrailingSep hpcRelDir, "--reset-hpcdirs"]
      logInfo $ "Generating " <> display report
      outputLines <- map (S8.filter (/= '\r')) . S8.lines . BL.toStrict . fst <$>
        proc "hpc"
        ( "report"
        : toFilePath tixSrc
        : (args ++ extraReportArgs)
        )
        readProcess_
      if all ("(0/0)" `S8.isSuffixOf`) outputLines
        then do
          let msg html =
                   "Error: [S-6829]\n"
                <> "The "
                <> display report
                <> " did not consider any code. One possible cause of this is"
                <> " if your test-suite builds the library code (see Stack "
                <> ( if html
                       then "<a href='https://github.com/commercialhaskell/stack/issues/1008'>"
                       else ""
                   )
                <> "issue #1008"
                <> (if html then "</a>" else "")
                <> "). It may also indicate a bug in Stack or"
                <> " the hpc program. Please report this issue if you think"
                <> " your coverage report should have meaningful results."
          logError (msg False)
          generateHpcErrorReport reportDir (msg True)
          pure Nothing
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
          pure (Just reportPath)

data HpcReportOpts = HpcReportOpts
  { hroptsInputs :: [Text]
  , hroptsAll :: Bool
  , hroptsDestDir :: Maybe String
  , hroptsOpenBrowser :: Bool
  }
  deriving Show

generateHpcReportForTargets :: HasEnvConfig env
                            => HpcReportOpts -> [Text] -> [Text] -> RIO env ()
generateHpcReportForTargets opts tixFiles targetNames = do
  targetTixFiles <-
    -- When there aren't any package component arguments, and --all
    -- isn't passed, default to not considering any targets.
    if not (hroptsAll opts) && null targetNames
    then pure []
    else do
      when (hroptsAll opts && not (null targetNames)) $
        logWarn $
             "Since --all is used, it is redundant to specify these targets: "
          <> displayShow targetNames
      targets <-
        view $ envConfigL.to envConfigSourceMap.to smTargets.to smtTargets
      fmap concat $ forM (Map.toList targets) $ \(name, target) ->
        case target of
          TargetAll PTDependency -> throwIO $ NotLocalPackage name
          TargetComps comps -> do
            pkgPath <- hpcPkgPath name
            forM (toList comps) $
              \case
                CTest testName -> (pkgPath </>) <$>
                  parseRelFile
                    (  T.unpack testName
                    ++ "/"
                    ++ T.unpack testName
                    ++ ".tix"
                    )
                _ -> throwIO $ NonTestSuiteTarget name
          TargetAll PTProject -> do
            pkgPath <- hpcPkgPath name
            exists <- doesDirExist pkgPath
            if exists
              then do
                (dirs, _) <- listDir pkgPath
                fmap concat $ forM dirs $ \dir -> do
                  (_, files) <- listDir dir
                  pure (filter ((".tix" `L.isSuffixOf`) . toFilePath) files)
              else pure []
  tixPaths <- (++ targetTixFiles) <$>
    mapM (resolveFile' . T.unpack) tixFiles
  when (null tixPaths) $ throwIO NoTargetsOrTixSpecified
  outputDir <- hpcReportDir
  reportDir <- case hroptsDestDir opts of
    Nothing -> pure (outputDir </> relDirCombined </> relDirCustom)
    Just destDir -> do
      dest <- resolveDir' destDir
      ensureDir dest
      pure dest
  let report = "combined report"
  mreportPath <- generateUnionReport report reportDir tixPaths
  forM_ mreportPath $ \reportPath ->
    if hroptsOpenBrowser opts
      then do
        prettyInfo $ "Opening" <+> pretty reportPath <+> "in the browser."
        void $ liftIO $ openBrowser (toFilePath reportPath)
      else displayReportPath "The" report (pretty reportPath)

generateHpcUnifiedReport :: HasEnvConfig env => RIO env ()
generateHpcUnifiedReport = do
  outputDir <- hpcReportDir
  ensureDir outputDir
  (dirs, _) <- listDir outputDir
  tixFiles0 <- fmap (concat . concat) $ forM (filter (("combined" /=) . dirnameString) dirs) $ \dir -> do
    (dirs', _) <- listDir dir
    forM dirs' $ \dir' -> do
      (_, files) <- listDir dir'
      pure (filter ((".tix" `L.isSuffixOf`) . toFilePath) files)
  extraTixFiles <- findExtraTixFiles
  let tixFiles = tixFiles0  ++ extraTixFiles
      reportDir = outputDir </> relDirCombined </> relDirAll
-- Previously, the test below was:
--
--  if length tixFiles < 2
--      then logInfo $
--          (if null tixFiles then "No tix files" else "Only one tix file") <>
--          " found in " <>
--          fromString (toFilePath outputDir) <>
--          ", so not generating a unified coverage report."
--      else ...
--
-- However, a single *.tix file does not necessarily mean that a unified
-- coverage report is redundant. For example, one package may test the library
-- of another package that does not test its own library. See
-- https://github.com/commercialhaskell/stack/issues/5713
--
-- As an interim solution, a unified coverage report will always be produced
-- even if may be redundant in some circumstances.
  if null tixFiles
    then logInfo $
         "No tix files found in "
      <> fromString (toFilePath outputDir)
      <> ", so not generating a unified coverage report."
    else do
      let report = "unified report"
      mreportPath <- generateUnionReport report reportDir tixFiles
      forM_ mreportPath (displayReportPath "The" report . pretty)

generateUnionReport :: HasEnvConfig env
                    => Text -> Path Abs Dir -> [Path Abs File]
                    -> RIO env (Maybe (Path Abs File))
generateUnionReport report reportDir tixFiles = do
  (errs, tix) <- fmap (unionTixes . map removeExeModules) (mapMaybeM readTixOrLog tixFiles)
  logDebug $ "Using the following tix files: " <> fromString (show tixFiles)
  unless (null errs) $ logWarn $
    "The following modules are left out of the " <>
    display report <>
    " due to version mismatches: " <>
    mconcat (L.intersperse ", " (map fromString errs))
  tixDest <- (reportDir </>) <$> parseRelFile (dirnameString reportDir ++ ".tix")
  ensureDir (parent tixDest)
  liftIO $ writeTix (toFilePath tixDest) tix
  generateHpcReportInternal tixDest reportDir report [] []

readTixOrLog :: HasLogFunc env => Path b File -> RIO env (Maybe Tix)
readTixOrLog path = do
  mtix <- liftIO (readTix (toFilePath path)) `catchAny` \errorCall -> do
    logError $
         "Error: [S-3521]\n"
      <> "Error while reading tix: "
      <> fromString (displayException errorCall)
    pure Nothing
  when (isNothing mtix) $
    logError $
         "Error: [S-7786]\n"
      <> "Failed to read tix file "
      <> fromString (toFilePath path)
  pure mtix

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
  rows <- fmap (catMaybes . concat) $ forM dirs $ \dir -> do
    (subdirs, _) <- listDir dir
    forM subdirs $ \subdir -> do
      let indexPath = subdir </> relFileHpcIndexHtml
      exists' <- doesFileExist indexPath
      if not exists' then pure Nothing else do
        relPath <- stripProperPrefix outputDir indexPath
        let package = dirname dir
            testsuite = dirname subdir
        pure $ Just $ T.concat
          [ "<tr><td>"
          , pathToHtml package
          , "</td><td><a href=\""
          , pathToHtml relPath
          , "\">"
          , pathToHtml testsuite
          , "</a></td></tr>"
          ]
  writeBinaryFileAtomic outputFile $
       "<html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">"
    <>
    -- Part of the css from HPC's output HTML
       "<style type=\"text/css\">"
    <> "table.dashboard { border-collapse: collapse; border: solid 1px black }"
    <> ".dashboard td { border: solid 1px black }"
    <> ".dashboard th { border: solid 1px black }"
    <> "</style>"
    <> "</head>"
    <> "<body>"
    <> ( if null rows
           then
                "<b>No hpc_index.html files found in \""
             <> encodeUtf8Builder (pathToHtml outputDir)
             <> "\".</b>"
           else
                "<table class=\"dashboard\" width=\"100%\" border=\"1\"><tbody>"
             <> "<p><b>NOTE: This is merely a listing of the html files found in the coverage reports directory.  Some of these reports may be old.</b></p>"
             <> "<tr><th>Package</th><th>TestSuite</th><th>Modification Time</th></tr>"
             <> foldMap encodeUtf8Builder rows
             <> "</tbody></table>"
       )
    <> "</body></html>"
  unless (null rows) $
    displayReportPath
      "\nAn" "index of the generated HTML coverage reports"
      (pretty outputFile)

generateHpcErrorReport :: MonadIO m => Path Abs Dir -> Utf8Builder -> m ()
generateHpcErrorReport dir err = do
  ensureDir dir
  let fp = toFilePath (dir </> relFileHpcIndexHtml)
  writeFileUtf8Builder fp $
       "<html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></head><body>"
    <> "<h1>HPC Report Generation Error</h1>"
    <> "<p>"
    <> err
    <> "</p>"
    <> "</body></html>"

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
dirnameString = L.dropWhileEnd isPathSeparator . toFilePath . dirname

findPackageFieldForBuiltPackage ::
     HasEnvConfig env
  => Path Abs Dir -> PackageIdentifier -> Set.Set Text -> Text
  -> RIO env (Either Text [Text])
findPackageFieldForBuiltPackage pkgDir pkgId internalLibs field = do
  distDir <- distDirFromDir pkgDir
  let inplaceDir = distDir </> relDirPackageConfInplace
      pkgIdStr = packageIdentifierString pkgId
      notFoundErr = pure $ Left $ "Failed to find package key for " <> T.pack pkgIdStr
      extractField path = do
        contents <- readFileUtf8 (toFilePath path)
        case asum (map (T.stripPrefix (field <> ": ")) (T.lines contents)) of
          Just result -> pure $ Right $ T.strip result
          Nothing -> notFoundErr
  cabalVer <- view cabalVersionL
  if cabalVer < mkVersion [1, 24]
    then do
      -- here we don't need to handle internal libs
      path <- (inplaceDir </>) <$> parseRelFile (pkgIdStr ++ "-inplace.conf")
      logDebug $
           "Parsing config in Cabal < 1.24 location: "
        <> fromString (toFilePath path)
      exists <- doesFileExist path
      if exists then fmap (:[]) <$> extractField path else notFoundErr
    else do
      -- With Cabal-1.24, it's in a different location.
      logDebug $ "Scanning " <> fromString (toFilePath inplaceDir) <> " for files matching " <> fromString pkgIdStr
      (_, files) <- handleIO (const $ pure ([], [])) $ listDir inplaceDir
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
            (a:_) -> pure $ Left a -- the first error only, since they're repeated anyway
            [] -> pure $ Right keys
      else
        pure
          $ Left
          $    "Multiple files matching "
            <> T.pack (pkgIdStr ++ "-*.conf")
            <> " found in "
            <> T.pack (toFilePath inplaceDir)
            <> ". Maybe try 'stack clean' on this package?"

displayReportPath :: (HasTerm env)
                  => StyleDoc -> Text -> StyleDoc -> RIO env ()
displayReportPath prefix report reportPath =
  prettyInfo $
        prefix
    <+> fromString (T.unpack report)
    <+> "is available at"
    <+> reportPath

findExtraTixFiles :: HasEnvConfig env => RIO env [Path Abs File]
findExtraTixFiles = do
  outputDir <- hpcReportDir
  let dir = outputDir </> relDirExtraTixFiles
  dirExists <- doesDirExist dir
  if dirExists
    then do
      (_, files) <- listDir dir
      pure $ filter ((".tix" `L.isSuffixOf`) . toFilePath) files
    else pure []
