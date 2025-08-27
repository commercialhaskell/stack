{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Stack.Coverage
Description : Generate HPC (Haskell Program Coverage) reports.
License     : BSD-3-Clause

Generate HPC (Haskell Program Coverage) reports.
-}

module Stack.Coverage
  ( hpcReportCmd
  , deleteHpcReports
  , updateTixFile
  , generateHpcReport
  , generateHpcUnifiedReport
  , generateHpcMarkupIndex
  ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Conduit ( await )
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Distribution.Types.MungedPackageId ( computeCompatPackageId )
import           Distribution.Types.UnqualComponentName
                   ( mkUnqualComponentName )
import           Path
                   ( (</>), dirname, parent, parseAbsFile, parseRelDir
                   , parseRelFile, stripProperPrefix
                   )
import           Path.Extra ( toFilePathNoTrailingSep )
import           Path.IO
                   ( copyFile, doesDirExist, doesFileExist, ensureDir
                   , ignoringAbsence, listDir, removeDirRecur, removeFile
                   , resolveDir', resolveFile'
                   )
import           RIO.ByteString.Lazy ( putStrLn )
import           RIO.Process
                   ( ExitCodeException, ProcessException, proc, readProcess_ )
import           Stack.Build.Target ( NeedTargets (..) )
import           Stack.Constants
                   ( relDirAll, relDirCombined, relDirCustom
                   , relDirExtraTixFiles, relDirPackageConfInplace
                   , relFileHpcIndexHtml, relFileIndexHtml
                   )
import           Stack.Constants.Config ( distDirFromDir, hpcRelativeDir )
import           Stack.Package ( hasBuildableMainLibrary )
import           Stack.PackageDump ( ghcPkgField )
import           Stack.Prelude
import           Stack.Runners ( ShouldReexec (..), withConfig, withEnvConfig )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..) )
import           Stack.Types.CompilerPaths ( getGhcPkgExe )
import           Stack.Types.CompCollection ( getBuildableSetText )
import           Stack.Types.ComponentUtils ( unqualCompToString )
import           Stack.Types.BuildOptsCLI
                   ( BuildOptsCLI (..), defaultBuildOptsCLI )
import           Stack.Types.EnvConfig
                   ( EnvConfig (..), HasEnvConfig (..), hpcReportDir )
import           Stack.Types.HpcReportOpts ( HpcReportOpts (..) )
import           Stack.Types.NamedComponent ( NamedComponent (..) )
import           Stack.Types.Package ( Package (..), packageIdentifier )
import           Stack.Types.Runner ( Runner )
import           Stack.Types.SourceMap
                   ( PackageType (..), SMTargets (..), SMWanted (..)
                   , SourceMap (..), Target (..), ppRoot
                   )
import           System.FilePath ( isPathSeparator )
import           Trace.Hpc.Tix ( Tix (..), TixModule (..), readTix, writeTix )
import           Web.Browser ( openBrowser )

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.Coverage" module.
data CoveragePrettyException
  = NonTestSuiteTarget PackageName
  | NoTargetsOrTixSpecified
  | NotLocalPackage PackageName
  deriving Show

instance Pretty CoveragePrettyException where
  pretty (NonTestSuiteTarget name) =
    "[S-6361]"
    <> line
    <> fillSep
         [ flow "Can't specify anything except test-suites as hpc report \
                \targets"
         , parens (style Target . fromPackageName $ name)
         , flow "is used with a non test-suite target."
         ]
  pretty NoTargetsOrTixSpecified =
    "[S-2321]"
    <> line
    <> flow "Not generating combined report, because no targets or tix files \
            \are specified."
  pretty (NotLocalPackage name) =
    "[S-9975]"
    <> line
    <> fillSep
         [ flow "Expected a project package, but"
         , style Target . fromPackageName $ name
         , flow "is either an extra-dep or in the snapshot."
         ]

instance Exception CoveragePrettyException

-- | Function underlying the @stack hpc report@ command.
hpcReportCmd :: HpcReportOpts -> RIO Runner ()
hpcReportCmd hropts = do
  let (tixFiles, targetNames) =
        L.partition (".tix" `T.isSuffixOf`) hropts.inputs
      boptsCLI = defaultBuildOptsCLI
        { targetsCLI = if hropts.all then [] else targetNames }
  withConfig YesReexec $ withEnvConfig AllowNoTargets boptsCLI $
    generateHpcReportForTargets hropts tixFiles targetNames

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
    readTixOrLog tixSrc >>= \case
      Nothing -> prettyError $
        "[S-2887]"
        <> line
        <> fillSep
             [ flow "Failed to read"
             , pretty tixSrc <> "."
             ]
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

-- | Generates the HTML coverage report and shows a textual coverage summary for
-- a package.
generateHpcReport :: HasEnvConfig env
                  => Path Abs Dir -> Package -> [Text] -> RIO env ()
generateHpcReport pkgDir package tests = do
  -- If we're using > GHC 7.10, the hpc 'include' parameter must specify a ghc
  -- package key. See
  -- https://github.com/commercialhaskell/stack/issues/785
  let pkgName' = packageNameString package.name
      hasLibrary = hasBuildableMainLibrary package
      subLibs = package.subLibraries
  eincludeName <-
    if not hasLibrary && null subLibs
      -- We don't expect to find a package key if there is no library.
      then pure $ Right Nothing
      -- Look in the inplace DB for the package key.
      -- See https://github.com/commercialhaskell/stack/issues/1181#issuecomment-148968986
      else do
        eincludeName <-
          findPackageFieldForBuiltPackage
            pkgDir
            (packageIdentifier package)
            (getBuildableSetText subLibs)
            -- GHC 8.0 uses package id instead of package key.
            -- See https://github.com/commercialhaskell/stack/issues/2424
            "id"
        case eincludeName of
          Left err -> do
            logError $ display err
            pure $ Left err
          Right includeNames -> pure $ Right $ Just $ map T.unpack includeNames
  forM_ tests $ \testName -> do
    tixSrc <- tixFilePath package.name (T.unpack testName)
    let report = fillSep
          [ flow "coverage report for"
          , style Current (fromString pkgName') <> "'s"
          , "test-suite"
          , style PkgComponent (fromString $ T.unpack testName)
          ]
        reportHtml =
             "coverage report for"
          <> T.pack pkgName'
          <> "'s test-suite \""
          <> testName
          <> "\""
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
          generateHpcReportInternal tixSrc reportDir report reportHtml extraArgs extraArgs
        forM_ mreportPath (displayReportPath "The" report . pretty)

generateHpcReportInternal ::
     HasEnvConfig env
  => Path Abs File
  -> Path Abs Dir
  -> StyleDoc
     -- ^ The pretty name for the report
  -> Text
     -- ^ The plain name for the report, used in HTML output
  -> [String]
  -> [String]
  -> RIO env (Maybe (Path Abs File))
generateHpcReportInternal
    tixSrc
    reportDir
    report
    reportHtml
    extraMarkupArgs
    extraReportArgs
  = do
      -- If a .tix file exists, move it to the HPC output directory and generate
      -- a report for it.
      tixFileExists <- doesFileExist tixSrc
      if not tixFileExists
        then do
          prettyError $
            "[S-4634]"
            <> line
            <> flow "Didn't find"
            <> style File ".tix"
            <> "for"
            <> report
            <> flow "- expected to find it at"
            <> pretty tixSrc <> "."
          pure Nothing
        else (`catch` \(err :: ProcessException) -> do
               logError $ displayShow err
               generateHpcErrorReport reportDir $ display $ sanitize $
                   displayException err
               pure Nothing) $
           (`onException`
               prettyError
                 ( "[S-8215]"
                   <> line
                   <> flow "Error occurred while producing"
                   <> report <> "."
                 )) $ do
          -- Directories for .mix files.
          hpcRelDir <- hpcRelativeDir
          -- Compute arguments used for both "hpc markup" and "hpc report".
          pkgDirs <- view $ buildConfigL . to
            (map ppRoot . Map.elems . (.smWanted.project))
          let args =
                -- Use index files from all packages (allows cross-package
                -- coverage results).
                concatMap (\x -> ["--srcdir", toFilePathNoTrailingSep x]) pkgDirs ++
                -- Look for index files in the correct dir (relative to each pkgdir).
                ["--hpcdir", toFilePathNoTrailingSep hpcRelDir, "--reset-hpcdirs"]
          prettyInfoL
            [ "Generating"
            , report <> "."
            ]
          -- Strip @\r@ characters because Windows.
          outputLines <- map (L8.filter (/= '\r')) . L8.lines . fst <$>
            proc "hpc"
            ( "report"
            : toFilePath tixSrc
            : (args ++ extraReportArgs)
            )
            readProcess_
          if all ("(0/0)" `L8.isSuffixOf`) outputLines
            then do
              let msgHtml =
                       "Error: [S-6829]\n\
                       \The "
                    <> display reportHtml
                    <> " did not consider any code. One possible cause of this is \
                       \if your test-suite builds the library code (see Stack \
                       \<a href='https://github.com/commercialhaskell/stack/issues/1008'>\
                       \issue #1008\
                       \</a>\
                       \). It may also indicate a bug in Stack or the hpc program. \
                       \Please report this issue if you think your coverage report \
                       \should have meaningful results."
              prettyError $
                "[S-6829]"
                <> line
                <> fillSep
                     [ "The"
                     , report
                     , flow "did not consider any code. One possible cause of this \
                            \is if your test-suite builds the library code (see \
                            \Stack issue #1008). It may also indicate a bug in \
                            \Stack or the hpc program. Please report this issue if \
                            \you think your coverage report should have meaningful \
                            \results."
                     ]
              generateHpcErrorReport reportDir msgHtml
              pure Nothing
            else do
              let reportPath = reportDir </> relFileHpcIndexHtml
              -- Print the summary report to the standard output stream.
              putUtf8Builder =<< displayWithColor
                (  fillSep
                     [ "Summary"
                     , report <> ":"
                     ]
                <> line
                )
              forM_ outputLines putStrLn
              -- Generate the HTML markup.
              void $ proc "hpc"
                ( "markup"
                : toFilePath tixSrc
                : ("--destdir=" ++ toFilePathNoTrailingSep reportDir)
                : (args ++ extraMarkupArgs)
                )
                readProcess_
              pure (Just reportPath)

generateHpcReportForTargets :: HasEnvConfig env
                            => HpcReportOpts -> [Text] -> [Text] -> RIO env ()
generateHpcReportForTargets opts tixFiles targetNames = do
  targetTixFiles <-
    -- When there aren't any package component arguments, and --all
    -- isn't passed, default to not considering any targets.
    if not opts.all && null targetNames
    then pure []
    else do
      when (opts.all && not (null targetNames)) $
        prettyWarnL
          $ "Since"
          : style Shell "--all"
          : flow "is used, it is redundant to specify these targets:"
          : mkNarrativeList (Just Target) False
              (map (fromString . T.unpack) targetNames :: [StyleDoc])
      targets <-
        view $ envConfigL . to (.sourceMap.targets.targets)
      fmap concat $ forM (Map.toList targets) $ \(name, target) ->
        case target of
          TargetAll PTDependency -> prettyThrowIO $ NotLocalPackage name
          TargetComps comps -> do
            pkgPath <- hpcPkgPath name
            forM (toList comps) $
              \case
                CTest testName -> (pkgPath </>) <$>
                  parseRelFile
                    (  testName'
                    ++ "/"
                    ++ testName'
                    ++ ".tix"
                    )
                 where
                  testName' = unqualCompToString testName
                _ -> prettyThrowIO $ NonTestSuiteTarget name
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
  when (null tixPaths) $ prettyThrowIO NoTargetsOrTixSpecified
  outputDir <- hpcReportDir
  reportDir <- case opts.destDir of
    Nothing -> pure (outputDir </> relDirCombined </> relDirCustom)
    Just destDir -> do
      dest <- resolveDir' destDir
      ensureDir dest
      pure dest
  let report = flow "combined coverage report"
      reportHtml = "combined coverage report"
  mreportPath <- generateUnionReport report reportHtml reportDir tixPaths
  forM_ mreportPath $ \reportPath ->
    if opts.openBrowser
      then do
        prettyInfo $ "Opening" <+> pretty reportPath <+> "in the browser."
        void $ liftIO $ openBrowser (toFilePath reportPath)
      else displayReportPath "The" report (pretty reportPath)

-- | Generates the HTML unified coverage report.
generateHpcUnifiedReport :: HasEnvConfig env => RIO env ()
generateHpcUnifiedReport = do
  outputDir <- hpcReportDir
  ensureDir outputDir
  (dirs, _) <- listDir outputDir
  tixFiles0 <-
    fmap (concat . concat) $ forM (filter (("combined" /=) . dirnameString) dirs) $ \dir -> do
      (dirs', _) <- listDir dir
      forM dirs' $ \dir' -> do
        (_, files) <- listDir dir'
        pure (filter ((".tix" `L.isSuffixOf`) . toFilePath) files)
  extraTixFiles <- findExtraTixFiles
  let tixFiles = tixFiles0  ++ extraTixFiles
      reportDir = outputDir </> relDirCombined </> relDirAll
-- A single *.tix file does not necessarily mean that a unified coverage report
-- is redundant. For example, one package may test the library of another
-- package that does not test its own library. See
-- https://github.com/commercialhaskell/stack/issues/5713
--
-- As an interim solution, a unified coverage report will always be produced
-- even if may be redundant in some circumstances.
  if null tixFiles
    then prettyInfoL
      [ flow "No tix files found in"
      , pretty outputDir <> ","
      , flow "so not generating a unified coverage report."
      ]
    else do
      let report = flow "unified coverage report"
          reportHtml = "unified coverage report"
      mreportPath <- generateUnionReport report reportHtml reportDir tixFiles
      forM_ mreportPath (displayReportPath "The" report . pretty)

generateUnionReport ::
     HasEnvConfig env
  => StyleDoc
     -- ^ Pretty description of the report.
  -> Text
     -- ^ Plain description of the report, used in HTML reporting.
  -> Path Abs Dir
  -> [Path Abs File]
  -> RIO env (Maybe (Path Abs File))
generateUnionReport report reportHtml reportDir tixFiles = do
  (errs, tix) <- fmap (unionTixes . map removeExeModules) (mapMaybeM readTixOrLog tixFiles)
  logDebug $ "Using the following tix files: " <> fromString (show tixFiles)
  unless (null errs) $
    prettyWarn $
      fillSep
         [ flow "The following modules are left out of the"
         , report
         , flow "due to version mismatches:"
         ]
    <> line
    <> bulletedList (map fromString errs :: [StyleDoc])
  tixDest <-
    (reportDir </>) <$> parseRelFile (dirnameString reportDir ++ ".tix")
  ensureDir (parent tixDest)
  liftIO $ writeTix (toFilePath tixDest) tix
  generateHpcReportInternal tixDest reportDir report reportHtml [] []

readTixOrLog :: HasTerm env => Path b File -> RIO env (Maybe Tix)
readTixOrLog path = do
  mtix <- liftIO (readTix (toFilePath path)) `catchAny` \errorCall -> do
    prettyError $
      "[S-3521]"
      <> line
      <> flow "Error while reading tix:"
      <> line
      <> string (displayException errorCall)
    pure Nothing
  when (isNothing mtix) $
    prettyError $
      "[S-7786]"
      <> line
      <> fillSep
           [ flow "Failed to read tix file"
           , pretty path <> "."
           ]
  pure mtix

-- | Module names which contain '/' have a package name, and so they weren't
-- built into the executable.
removeExeModules :: Tix -> Tix
removeExeModules (Tix ms) =
  Tix (filter (\(TixModule name _ _ _) -> '/' `elem` name) ms)

unionTixes :: [Tix] -> ([String], Tix)
unionTixes tixes = (Map.keys errs, Tix (Map.elems outputs))
 where
  (errs, outputs) = Map.mapEither id $ Map.unionsWith merge $ map toMap tixes
  toMap (Tix ms) = Map.fromList (map (\x@(TixModule k _ _ _) -> (k, Right x)) ms)
  merge (Right (TixModule k hash1 len1 tix1))
      (Right (TixModule _ hash2 len2 tix2))
    | hash1 == hash2 && len1 == len2 =
        Right (TixModule k hash1 len1 (zipWith (+) tix1 tix2))
  merge _ _ = Left ()

-- | Generates the HTML index report.
generateHpcMarkupIndex :: HasEnvConfig env => RIO env ()
generateHpcMarkupIndex = do
  outputDir <- hpcReportDir
  let outputFile = outputDir </> relFileIndexHtml
  ensureDir outputDir
  (dirs, _) <- listDir outputDir
  rows <- fmap (concatMap catMaybes) $ forM dirs $ \dir -> do
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
findPackageFieldForBuiltPackage pkgDir pkgId subLibs field = do
  let subLibNames =
        Set.map (LSubLibName . mkUnqualComponentName . T.unpack) subLibs
      libraryNames = Set.insert LMainLibName subLibNames
      mungedPackageIds = Set.map (computeCompatPackageId pkgId) libraryNames
  distDir <- distDirFromDir pkgDir
  ghcPkgExe <- getGhcPkgExe
  let inplaceDir = distDir </> relDirPackageConfInplace
      pkgIdStr = packageIdentifierString pkgId
      notFoundErr = pure $
        Left $ "Failed to find package key for " <> T.pack pkgIdStr
      extractField mungedPkgId = do
        mContents <- catch
          (ghcPkgField ghcPkgExe inplaceDir mungedPkgId (T.unpack field) await)
          -- A .conf file may not exist in the package database for a library or
          -- sub-library, if that component has not been built yet.
          (\(_ :: ExitCodeException) -> pure Nothing)
        case mContents of
          Just result -> pure $ Right $ T.strip result
          Nothing -> notFoundErr
  logDebug $
       "Scanning "
    <> fromString (toFilePath inplaceDir)
    <> " for munged packages matching "
    <> fromString pkgIdStr
  (errors, keys) <-
    partitionEithers <$> traverse extractField (Set.toList mungedPackageIds)
  case errors of
    (a:_) -> pure $ Left a -- the first error only, since they're repeated anyway
    [] -> pure $ Right keys

displayReportPath ::
     HasTerm env
  => StyleDoc
  -> StyleDoc
  -> StyleDoc
  -> RIO env ()
displayReportPath prefix report reportPath =
  prettyInfoL
    [ prefix
    , report
    , flow "is available at"
    , reportPath <> "."
    ]

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
