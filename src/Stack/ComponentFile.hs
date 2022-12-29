{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | A module which exports all component-level file-gathering logic. It also
-- includes utility functions for handling paths and directories.

module Stack.ComponentFile
  ( resolveOrWarn
  , libraryFiles
  , executableFiles
  , testFiles
  , benchmarkFiles
  , componentOutputDir
  , componentBuildDir
  , packageAutogenDir
  , buildDir
  , componentAutogenDir
  ) where

import           Control.Exception ( throw )
import           Data.List ( find, isPrefixOf )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Distribution.ModuleName ( ModuleName )
import qualified Distribution.ModuleName as Cabal
import           Distribution.Package
                   hiding
                     ( Module, Package, PackageIdentifier, packageName
                     , packageVersion
                     )
import           Distribution.PackageDescription hiding ( FlagName )
import           Distribution.Text ( display )
import           Distribution.Utils.Path ( getSymbolicPath )
import           Distribution.Version ( mkVersion )
import qualified HiFileParser as Iface
import           Path as FL hiding ( replaceExtension )
import           Path.Extra
import           Path.IO hiding ( findFiles )
import           Stack.Constants
import           Stack.Prelude hiding ( Display (..) )
import           Stack.Types.Config
import           Stack.Types.NamedComponent
import           Stack.Types.Package
import           Stack.Types.PackageFile
                   ( GetPackageFileContext (..), DotCabalDescriptor (..)
                   , DotCabalPath (..), PackageWarning (..)
                   )
import qualified System.Directory as D ( doesFileExist )
import qualified System.FilePath as FilePath

-- | Get all files referenced by the benchmark.
benchmarkFiles ::
     NamedComponent
  -> Benchmark
  -> RIO
       GetPackageFileContext
       (Map ModuleName (Path Abs File), [DotCabalPath], [PackageWarning])
benchmarkFiles component bench = do
  resolveComponentFiles component build names
 where
  names = bnames <> exposed
  exposed =
    case benchmarkInterface bench of
      BenchmarkExeV10 _ fp -> [DotCabalMain fp]
      BenchmarkUnsupported _ -> []
  bnames = map DotCabalModule (otherModules build)
  build = benchmarkBuildInfo bench

-- | Get all files referenced by the test.
testFiles ::
     NamedComponent
  -> TestSuite
  -> RIO
       GetPackageFileContext
       (Map ModuleName (Path Abs File), [DotCabalPath], [PackageWarning])
testFiles component test = do
  resolveComponentFiles component build names
 where
  names = bnames <> exposed
  exposed =
    case testInterface test of
      TestSuiteExeV10 _ fp -> [DotCabalMain fp]
      TestSuiteLibV09 _ mn -> [DotCabalModule mn]
      TestSuiteUnsupported _ -> []
  bnames = map DotCabalModule (otherModules build)
  build = testBuildInfo test

-- | Get all files referenced by the executable.
executableFiles ::
     NamedComponent
  -> Executable
  -> RIO
       GetPackageFileContext
       (Map ModuleName (Path Abs File), [DotCabalPath], [PackageWarning])
executableFiles component exe = do
  resolveComponentFiles component build names
 where
  build = buildInfo exe
  names =
    map DotCabalModule (otherModules build) ++
    [DotCabalMain (modulePath exe)]

-- | Get all files referenced by the library.
libraryFiles ::
     NamedComponent
  -> Library
  -> RIO
       GetPackageFileContext
       (Map ModuleName (Path Abs File), [DotCabalPath], [PackageWarning])
libraryFiles component lib = do
  resolveComponentFiles component build names
 where
  build = libBuildInfo lib
  names = bnames ++ exposed
  exposed = map DotCabalModule (exposedModules lib)
  bnames = map DotCabalModule (otherModules build)

-- | Get all files referenced by the component.
resolveComponentFiles ::
     NamedComponent
  -> BuildInfo
  -> [DotCabalDescriptor]
  -> RIO
       GetPackageFileContext
       (Map ModuleName (Path Abs File), [DotCabalPath], [PackageWarning])
resolveComponentFiles component build names = do
  dirs <- mapMaybeM (resolveDirOrWarn . getSymbolicPath) (hsSourceDirs build)
  dir <- asks (parent . ctxFile)
  agdirs <- autogenDirs
  (modules,files,warnings) <-
    resolveFilesAndDeps
      component
      ((if null dirs then [dir] else dirs) ++ agdirs)
      names
  cfiles <- buildOtherSources build
  pure (modules, files <> cfiles, warnings)
 where
  autogenDirs = do
    cabalVer <- asks ctxCabalVer
    distDir <- asks ctxDistDir
    let compDir = componentAutogenDir cabalVer component distDir
        pkgDir = maybeToList $ packageAutogenDir cabalVer distDir
    filterM doesDirExist $ compDir : pkgDir

-- | Try to resolve the list of base names in the given directory by
-- looking for unique instances of base names applied with the given
-- extensions, plus find any of their module and TemplateHaskell
-- dependencies.
resolveFilesAndDeps ::
     NamedComponent       -- ^ Package component name
  -> [Path Abs Dir]       -- ^ Directories to look in.
  -> [DotCabalDescriptor] -- ^ Base names.
  -> RIO
       GetPackageFileContext
       (Map ModuleName (Path Abs File),[DotCabalPath],[PackageWarning])
resolveFilesAndDeps component dirs names0 = do
  (dotCabalPaths, foundModules, missingModules) <- loop names0 S.empty
  warnings <-
    liftM2 (++) (warnUnlisted foundModules) (warnMissing missingModules)
  pure (foundModules, dotCabalPaths, warnings)
 where
  loop [] _ = pure ([], M.empty, [])
  loop names doneModules0 = do
    resolved <- resolveFiles dirs names
    let foundFiles = mapMaybe snd resolved
        foundModules = mapMaybe toResolvedModule resolved
        missingModules = mapMaybe toMissingModule resolved
    pairs <- mapM (getDependencies component dirs) foundFiles
    let doneModules = S.union
                        doneModules0
                        (S.fromList (mapMaybe dotCabalModule names))
        moduleDeps = S.unions (map fst pairs)
        thDepFiles = concatMap snd pairs
        modulesRemaining = S.difference moduleDeps doneModules
      -- Ignore missing modules discovered as dependencies - they may
      -- have been deleted.
    (resolvedFiles, resolvedModules, _) <-
      loop (map DotCabalModule (S.toList modulesRemaining)) doneModules
    pure
      ( nubOrd $ foundFiles <> map DotCabalFilePath thDepFiles <> resolvedFiles
      , M.union
            (M.fromList foundModules)
            resolvedModules
      , missingModules
      )
  warnUnlisted foundModules = do
    let unlistedModules =
          foundModules `M.difference`
          M.fromList (mapMaybe (fmap (, ()) . dotCabalModule) names0)
    pure $
      if M.null unlistedModules
        then []
        else [ UnlistedModulesWarning
                   component
                   (map fst (M.toList unlistedModules))
             ]
  warnMissing _missingModules = do
    pure []
      -- TODO: bring this back - see
      -- https://github.com/commercialhaskell/stack/issues/2649
      {-
      cabalfp <- asks ctxFile
      pure $
          if null missingModules
             then []
             else [ MissingModulesWarning
                         cabalfp
                         component
                         missingModules]
      -}
  -- TODO: In usages of toResolvedModule / toMissingModule, some sort
  -- of map + partition would probably be better.
  toResolvedModule ::
       (DotCabalDescriptor, Maybe DotCabalPath)
    -> Maybe (ModuleName, Path Abs File)
  toResolvedModule (DotCabalModule mn, Just (DotCabalModulePath fp)) =
    Just (mn, fp)
  toResolvedModule _ =
    Nothing
  toMissingModule ::
       (DotCabalDescriptor, Maybe DotCabalPath)
    -> Maybe ModuleName
  toMissingModule (DotCabalModule mn, Nothing) =
    Just mn
  toMissingModule _ =
    Nothing

-- | Get the dependencies of a Haskell module file.
getDependencies ::
     NamedComponent
  -> [Path Abs Dir]
  -> DotCabalPath
  -> RIO GetPackageFileContext (Set ModuleName, [Path Abs File])
getDependencies component dirs dotCabalPath =
  case dotCabalPath of
    DotCabalModulePath resolvedFile -> readResolvedHi resolvedFile
    DotCabalMainPath resolvedFile -> readResolvedHi resolvedFile
    DotCabalFilePath{} -> pure (S.empty, [])
    DotCabalCFilePath{} -> pure (S.empty, [])
 where
  readResolvedHi resolvedFile = do
    dumpHIDir <- componentOutputDir component <$> asks ctxDistDir
    dir <- asks (parent . ctxFile)
    let sourceDir = fromMaybe dir $ find (`isProperPrefixOf` resolvedFile) dirs
        stripSourceDir d = stripProperPrefix d resolvedFile
    case stripSourceDir sourceDir of
      Nothing -> pure (S.empty, [])
      Just fileRel -> do
        let hiPath = FilePath.replaceExtension
                       (toFilePath (dumpHIDir </> fileRel))
                       ".hi"
        dumpHIExists <- liftIO $ D.doesFileExist hiPath
        if dumpHIExists
          then parseHI hiPath
          else pure (S.empty, [])

-- | Parse a .hi file into a set of modules and files.
parseHI ::
     FilePath
  -> RIO GetPackageFileContext (Set ModuleName, [Path Abs File])
parseHI hiPath = do
  dir <- asks (parent . ctxFile)
  result <-
    liftIO $ catchAnyDeep
      (Iface.fromFile hiPath)
      (\e -> pure (Left (displayException e)))
  case result of
    Left msg -> do
      prettyStackDevL
        [ flow "Failed to decode module interface:"
        , style File $ fromString hiPath
        , flow "Decoding failure:"
        , style Error $ fromString msg
        ]
      pure (S.empty, [])
    Right iface -> do
      let moduleNames = fmap (fromString . T.unpack . decodeUtf8Lenient . fst) .
                        Iface.unList . Iface.dmods . Iface.deps
          resolveFileDependency file = do
            resolved <-
              liftIO (forgivingAbsence (resolveFile dir file)) >>=
                rejectMissingFile
            when (isNothing resolved) $
              prettyWarnL
              [ flow "Dependent file listed in:"
              , style File $ fromString hiPath
              , flow "does not exist:"
              , style File $ fromString file
              ]
            pure resolved
          resolveUsages = traverse
            (resolveFileDependency . Iface.unUsage) . Iface.unList . Iface.usage
      resolvedUsages <- catMaybes <$> resolveUsages iface
      pure (S.fromList $ moduleNames iface, resolvedUsages)

-- | The directory where generated files are put like .o or .hs (from .x files).
componentOutputDir :: NamedComponent -> Path Abs Dir -> Path Abs Dir
componentOutputDir namedComponent distDir =
  case namedComponent of
    CLib -> buildDir distDir
    CInternalLib name -> makeTmp name
    CExe name -> makeTmp name
    CTest name -> makeTmp name
    CBench name -> makeTmp name
 where
  makeTmp name =
    buildDir distDir </> componentNameToDir (name <> "/" <> name <> "-tmp")

-- | Try to resolve the list of base names in the given directory by
-- looking for unique instances of base names applied with the given
-- extensions.
resolveFiles ::
     [Path Abs Dir] -- ^ Directories to look in.
  -> [DotCabalDescriptor] -- ^ Base names.
  -> RIO GetPackageFileContext [(DotCabalDescriptor, Maybe DotCabalPath)]
resolveFiles dirs names =
  forM names (\name -> fmap (name, ) (findCandidate dirs name))

-- | Find a candidate for the given module-or-filename from the list
-- of directories and given extensions.
findCandidate ::
     [Path Abs Dir]
  -> DotCabalDescriptor
  -> RIO GetPackageFileContext (Maybe DotCabalPath)
findCandidate dirs name = do
  pkg <- asks ctxFile >>= parsePackageNameFromFilePath
  customPreprocessorExts <- view $ configL . to configCustomPreprocessorExts
  let haskellPreprocessorExts =
        haskellDefaultPreprocessorExts ++ customPreprocessorExts
  candidates <- liftIO $ makeNameCandidates haskellPreprocessorExts
  case candidates of
    [candidate] -> pure (Just (cons candidate))
    [] -> do
      case name of
        DotCabalModule mn
          | display mn /= paths_pkg pkg -> logPossibilities dirs mn
        _ -> pure ()
      pure Nothing
    (candidate:rest) -> do
      warnMultiple name candidate rest
      pure (Just (cons candidate))
 where
  cons =
    case name of
      DotCabalModule{} -> DotCabalModulePath
      DotCabalMain{} -> DotCabalMainPath
      DotCabalFile{} -> DotCabalFilePath
      DotCabalCFile{} -> DotCabalCFilePath
  paths_pkg pkg = "Paths_" ++ packageNameString pkg
  makeNameCandidates haskellPreprocessorExts =
    fmap
      (nubOrd . concat)
      (mapM (makeDirCandidates haskellPreprocessorExts) dirs)
  makeDirCandidates :: [Text]
                    -> Path Abs Dir
                    -> IO [Path Abs File]
  makeDirCandidates haskellPreprocessorExts dir =
    case name of
        DotCabalMain fp -> resolveCandidate dir fp
        DotCabalFile fp -> resolveCandidate dir fp
        DotCabalCFile fp -> resolveCandidate dir fp
        DotCabalModule mn -> do
          let perExt ext =
                resolveCandidate
                  dir (Cabal.toFilePath mn ++ "." ++ T.unpack ext)
          withHaskellExts <- mapM perExt haskellFileExts
          withPPExts <- mapM perExt haskellPreprocessorExts
          pure $
            case (concat withHaskellExts, concat withPPExts) of
              -- If we have exactly 1 Haskell extension and exactly
              -- 1 preprocessor extension, assume the former file is
              -- generated from the latter
              --
              -- See https://github.com/commercialhaskell/stack/issues/4076
              ([_], [y]) -> [y]

              -- Otherwise, return everything
              (xs, ys) -> xs ++ ys
  resolveCandidate dir = fmap maybeToList . resolveDirFile dir

-- | Log that we couldn't find a candidate, but there are
-- possibilities for custom preprocessor extensions.
--
-- For example: .erb for a Ruby file might exist in one of the
-- directories.
logPossibilities ::
     HasTerm env
  => [Path Abs Dir]
  -> ModuleName
  -> RIO env ()
logPossibilities dirs mn = do
  possibilities <- fmap concat (makePossibilities mn)
  unless (null possibilities) $ prettyWarnL
    [ flow "Unable to find a known candidate for the Cabal entry"
    , (style Module . fromString $ display mn) <> ","
    , flow "but did find:"
    , line <> bulletedList (map pretty possibilities)
    , flow "If you are using a custom preprocessor for this module"
    , flow "with its own file extension, consider adding the extension"
    , flow "to the 'custom-preprocessor-extensions' field in stack.yaml."
    ]
 where
  makePossibilities name =
    mapM
      ( \dir -> do
           (_,files) <- listDir dir
           pure
             ( map
                 filename
                 ( filter
                     (isPrefixOf (display name) . toFilePath . filename)
                     files
                 )
             )
      )
      dirs

-- | Get all C sources and extra source files in a build.
buildOtherSources :: BuildInfo -> RIO GetPackageFileContext [DotCabalPath]
buildOtherSources build = do
  cwd <- liftIO getCurrentDir
  dir <- asks (parent . ctxFile)
  file <- asks ctxFile
  let resolveDirFiles files toCabalPath =
        forMaybeM files $ \fp -> do
          result <- resolveDirFile dir fp
          case result of
            Nothing -> do
              warnMissingFile "File" cwd fp file
              pure Nothing
            Just p -> pure $ Just (toCabalPath p)
  csources <- resolveDirFiles (cSources build) DotCabalCFilePath
  jsources <- resolveDirFiles (targetJsSources build) DotCabalFilePath
  pure (csources <> jsources)

-- | Get the target's JS sources.
targetJsSources :: BuildInfo -> [FilePath]
targetJsSources = jsSources

-- | Resolve file as a child of a specified directory, symlinks
-- don't get followed.
resolveDirFile ::
     (MonadIO m, MonadThrow m)
  => Path Abs Dir -> FilePath.FilePath -> m (Maybe (Path Abs File))
resolveDirFile x y = do
  -- The standard canonicalizePath does not work for this case
  p <- parseCollapsedAbsFile (toFilePath x FilePath.</> y)
  exists <- doesFileExist p
  pure $ if exists then Just p else Nothing

-- | Warn the user that multiple candidates are available for an
-- entry, but that we picked one anyway and continued.
warnMultiple ::
     DotCabalDescriptor
  -> Path b t
  -> [Path b t]
  -> RIO GetPackageFileContext ()
warnMultiple name candidate rest =
  -- TODO: figure out how to style 'name' and the dispOne stuff
  prettyWarnL
    [ flow "There were multiple candidates for the Cabal entry"
    , fromString . showName $ name
    , line <> bulletedList (map dispOne (candidate:rest))
    , line <> flow "picking:"
    , dispOne candidate
    ]
 where
  showName (DotCabalModule name') = display name'
  showName (DotCabalMain fp) = fp
  showName (DotCabalFile fp) = fp
  showName (DotCabalCFile fp) = fp
  dispOne = fromString . toFilePath
    -- TODO: figure out why dispOne can't be just `display`
    --       (remove the .hlint.yaml exception if it can be)

-- | Parse a package name from a file path.
parsePackageNameFromFilePath :: MonadThrow m => Path a File -> m PackageName
parsePackageNameFromFilePath fp = do
  base <- clean $ toFilePath $ filename fp
  case parsePackageName base of
    Nothing -> throwM $ CabalFileNameInvalidPackageName $ toFilePath fp
    Just x -> pure x
 where
  clean = fmap reverse . strip . reverse
  strip ('l':'a':'b':'a':'c':'.':xs) = pure xs
  strip _ = throwM (CabalFileNameParseFail (toFilePath fp))

-- | Resolve the directory, if it can't be resolved, warn for the user
-- (purely to be helpful).
resolveDirOrWarn :: FilePath.FilePath
                 -> RIO GetPackageFileContext (Maybe (Path Abs Dir))
resolveDirOrWarn = resolveOrWarn "Directory" f
 where
  f p x = liftIO (forgivingAbsence (resolveDir p x)) >>= rejectMissingDir

-- | Make the global autogen dir if Cabal version is new enough.
packageAutogenDir :: Version -> Path Abs Dir -> Maybe (Path Abs Dir)
packageAutogenDir cabalVer distDir
  | cabalVer < mkVersion [2, 0] = Nothing
  | otherwise = Just $ buildDir distDir </> relDirGlobalAutogen

-- | Make the autogen dir.
componentAutogenDir :: Version -> NamedComponent -> Path Abs Dir -> Path Abs Dir
componentAutogenDir cabalVer component distDir =
  componentBuildDir cabalVer component distDir </> relDirAutogen

-- | Make the build dir. Note that Cabal >= 2.0 uses the
-- 'componentBuildDir' above for some things.
buildDir :: Path Abs Dir -> Path Abs Dir
buildDir distDir = distDir </> relDirBuild

-- NOTE: don't export this, only use it for valid paths based on
-- component names.
componentNameToDir :: Text -> Path Rel Dir
componentNameToDir name =
  fromMaybe (throw ComponentNotParsedBug) (parseRelDir (T.unpack name))

-- | See 'Distribution.Simple.LocalBuildInfo.componentBuildDir'
componentBuildDir :: Version -> NamedComponent -> Path Abs Dir -> Path Abs Dir
componentBuildDir cabalVer component distDir
  | cabalVer < mkVersion [2, 0] = buildDir distDir
  | otherwise =
      case component of
        CLib -> buildDir distDir
        CInternalLib name -> buildDir distDir </> componentNameToDir name
        CExe name -> buildDir distDir </> componentNameToDir name
        CTest name -> buildDir distDir </> componentNameToDir name
        CBench name -> buildDir distDir </> componentNameToDir name

-- Internal helper to define resolveFileOrWarn and resolveDirOrWarn
resolveOrWarn :: Text
              -> (Path Abs Dir -> String -> RIO GetPackageFileContext (Maybe a))
              -> FilePath.FilePath
              -> RIO GetPackageFileContext (Maybe a)
resolveOrWarn subject resolver path = do
  cwd <- liftIO getCurrentDir
  file <- asks ctxFile
  dir <- asks (parent . ctxFile)
  result <- resolver dir path
  when (isNothing result) $ warnMissingFile subject cwd path file
  pure result

warnMissingFile ::
     Text
  -> Path Abs Dir
  -> FilePath
  -> Path Abs File
  -> RIO GetPackageFileContext ()
warnMissingFile subject cwd path fromFile =
  prettyWarnL
    [ fromString . T.unpack $ subject -- TODO: needs style?
    , flow "listed in"
    , maybe (pretty fromFile) pretty (stripProperPrefix cwd fromFile)
    , flow "file does not exist:"
    , style Dir . fromString $ path
    ]
