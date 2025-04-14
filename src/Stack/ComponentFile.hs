{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | A module which exports all component-level file-gathering logic. It also
-- includes utility functions for handling paths and directories.

module Stack.ComponentFile
  ( resolveOrWarn
  , componentOutputDir
  , componentBuildDir
  , packageAutogenDir
  , buildDir
  , componentAutogenDir
  , ComponentFile (..)
  , stackLibraryFiles
  , stackExecutableFiles
  , stackTestSuiteFiles
  , stackBenchmarkFiles
  ) where

import           Control.Exception ( throw )
import           Data.Foldable ( foldrM )
import           Data.List ( find, isPrefixOf )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Distribution.ModuleName ( ModuleName )
import qualified Distribution.ModuleName as Cabal
import           Distribution.PackageDescription
                   ( BenchmarkInterface (..), TestSuiteInterface (..) )
import           Distribution.Text ( display )
import           Distribution.Utils.Path
                   ( Pkg, Source, SymbolicPath, getSymbolicPath )
import qualified Distribution.Utils.Path as Cabal
import           GHC.Records ( HasField )
import qualified HiFileParser as Iface
import           Path
                   ( (</>), filename, isProperPrefixOf, parent, parseRelDir
                   , stripProperPrefix
                   )
import           Path.Extra
                   ( forgivingResolveDir, forgivingResolveFile
                   , parseCollapsedAbsFile, rejectMissingDir, rejectMissingFile
                   )
import           Path.IO
                   ( doesDirExist, doesFileExist, getCurrentDir, listDir )
import           Stack.Constants
                   ( haskellDefaultPreprocessorExts, haskellFileExts
                   , relDirAutogen, relDirBuild, relDirGlobalAutogen
                   )
import           Stack.Prelude hiding ( Display (..) )
import           Stack.Types.Component
                   ( StackBenchmark (..), StackBuildInfo (..)
                   , StackExecutable (..), StackLibrary (..)
                   , StackTestSuite (..), StackUnqualCompName (..)
                   )
import           Stack.Types.ComponentUtils
                   ( emptyCompName, unqualCompToString )
import           Stack.Types.Config
                   ( Config (..), HasConfig (..), prettyStackDevL )
import           Stack.Types.NamedComponent ( NamedComponent (..) )
import           Stack.Types.Package ( PackageException (..), dotCabalModule )
import           Stack.Types.PackageFile
                   ( GetPackageFileContext (..), DotCabalDescriptor (..)
                   , DotCabalPath (..), PackageWarning (..)
                   )
import qualified System.Directory as D ( doesFileExist )
import qualified System.FilePath as FilePath

data ComponentFile = ComponentFile
  { moduleFileMap :: !(Map ModuleName (Path Abs File))
  , otherFile :: ![DotCabalPath]
  , packageWarning :: ![PackageWarning]
  }

-- | Get all files referenced by the benchmark.
stackBenchmarkFiles ::
     StackBenchmark
  -> RIO GetPackageFileContext (NamedComponent, ComponentFile)
stackBenchmarkFiles bench =
  resolveComponentFiles (CBench bench.name) build names
 where
  names :: [DotCabalDescriptor]
  names = bnames <> exposed

  exposed :: [DotCabalDescriptor]
  exposed =
    case bench.interface of
      BenchmarkExeV10 _ fp -> [DotCabalMain $ getSymbolicPath fp]
      BenchmarkUnsupported _ -> []

  bnames :: [DotCabalDescriptor]
  bnames = map DotCabalModule build.otherModules

  build :: StackBuildInfo
  build = bench.buildInfo

-- | Get all files referenced by the test.
stackTestSuiteFiles ::
     StackTestSuite
  -> RIO GetPackageFileContext (NamedComponent, ComponentFile)
stackTestSuiteFiles test =
  resolveComponentFiles (CTest test.name) build names
 where
  names :: [DotCabalDescriptor]
  names = bnames <> exposed

  exposed :: [DotCabalDescriptor]
  exposed =
    case test.interface of
      TestSuiteExeV10 _ fp -> [DotCabalMain $ getSymbolicPath fp]
      TestSuiteLibV09 _ mn -> [DotCabalModule mn]
      TestSuiteUnsupported _ -> []

  bnames :: [DotCabalDescriptor]
  bnames = map DotCabalModule build.otherModules

  build :: StackBuildInfo
  build = test.buildInfo

-- | Get all files referenced by the executable.
stackExecutableFiles ::
     StackExecutable
  -> RIO GetPackageFileContext (NamedComponent, ComponentFile)
stackExecutableFiles exe =
  resolveComponentFiles (CExe exe.name) build names
 where
  build :: StackBuildInfo
  build = exe.buildInfo

  names :: [DotCabalDescriptor]
  names =
    map DotCabalModule build.otherModules ++ [DotCabalMain exe.modulePath]

-- | Get all files referenced by the library. Handle all libraries (CLib and
-- SubLib), based on empty name or not.
stackLibraryFiles ::
     StackLibrary
  -> RIO GetPackageFileContext (NamedComponent, ComponentFile)
stackLibraryFiles lib =
  resolveComponentFiles componentName build names
 where
  componentRawName :: StackUnqualCompName
  componentRawName = lib.name

  componentName :: NamedComponent
  componentName
    | componentRawName == emptyCompName = CLib
    | otherwise = CSubLib componentRawName

  build :: StackBuildInfo
  build = lib.buildInfo

  names :: [DotCabalDescriptor]
  names = bnames ++ exposed

  exposed :: [DotCabalDescriptor]
  exposed = map DotCabalModule lib.exposedModules

  bnames :: [DotCabalDescriptor]
  bnames = map DotCabalModule build.otherModules

-- | Get all files referenced by the component.
resolveComponentFiles ::
     ( CAndJsSources rec
     , HasField "hsSourceDirs" rec [SymbolicPath Pkg (Cabal.Dir Source)]
     )
  => NamedComponent
  -> rec
  -> [DotCabalDescriptor]
  -> RIO GetPackageFileContext (NamedComponent, ComponentFile)
resolveComponentFiles component build names = do
  dirs <- mapMaybeM (resolveDirOrWarn . getSymbolicPath) build.hsSourceDirs
  dir <- asks (parent . (.file))
  agdirs <- autogenDirs
  (modules,files,warnings) <-
    resolveFilesAndDeps
      component
      ((if null dirs then [dir] else dirs) ++ agdirs)
      names
  cfiles <- buildOtherSources build
  pure (component, ComponentFile modules (files <> cfiles) warnings)
 where
  autogenDirs :: RIO GetPackageFileContext [Path Abs Dir]
  autogenDirs = do
    distDir <- asks (.distDir)
    let compDir = componentAutogenDir component distDir
        pkgDir = [packageAutogenDir distDir]
    filterM doesDirExist $ compDir : pkgDir

-- | Try to resolve the list of base names in the given directory by looking for
-- unique instances of base names applied with the given extensions, plus find
-- any of their module and TemplateHaskell dependencies.
resolveFilesAndDeps ::
     NamedComponent       -- ^ Package component name
  -> [Path Abs Dir]       -- ^ Directories to look in.
  -> [DotCabalDescriptor] -- ^ Base names.
  -> RIO
       GetPackageFileContext
       (Map ModuleName (Path Abs File), [DotCabalPath], [PackageWarning])
resolveFilesAndDeps component dirs names0 = do
  (dotCabalPaths, foundModules, missingModules, _) <- loop names0 S.empty M.empty
  warnings <-
    liftM2 (++) (warnUnlisted foundModules) (warnMissing missingModules)
  pure (foundModules, dotCabalPaths, warnings)
 where
  loop ::
       [DotCabalDescriptor]
    -> Set ModuleName
    -> Map FilePath (Path Abs File)
       -- ^ Known file usages, where the file path has already been resolved.
    -> RIO
         GetPackageFileContext
         ( [DotCabalPath]
         , Map ModuleName (Path Abs File)
         , [ModuleName]
         , Map k a
         )
  loop [] _ _ = pure ([], M.empty, [], M.empty)
  loop names doneModules0 knownUsages = do
    resolved <- resolveFiles dirs names
    let foundFiles = mapMaybe snd resolved
        foundModules = mapMaybe toResolvedModule resolved
        missingModules = mapMaybe toMissingModule resolved
        getDependenciesFold c (ps, ku) = do
          p <- getDependencies ku component dirs c
          pure (p : ps, ku <> snd p)
    (pairs, foundUsages) <- foldrM getDependenciesFold ([], knownUsages) foundFiles
    let doneModules = S.union
                        doneModules0
                        (S.fromList (mapMaybe dotCabalModule names))
        moduleDeps = S.unions (map fst pairs)
        thDepFiles = concatMap (M.elems . snd) pairs
        modulesRemaining = S.difference moduleDeps doneModules
      -- Ignore missing modules discovered as dependencies - they may
      -- have been deleted.
    (resolvedFiles, resolvedModules, _, foundUsages') <-
      loop (map DotCabalModule (S.toList modulesRemaining)) doneModules foundUsages
    pure
      ( nubOrd $ foundFiles <> map DotCabalFilePath thDepFiles <> resolvedFiles
      , M.union (M.fromList foundModules) resolvedModules
      , missingModules
      , foundUsages'
      )
  warnUnlisted foundModules = do
    let unlistedModules =
          foundModules `M.difference`
          M.fromList (mapMaybe (fmap (, ()) . dotCabalModule) names0)
    pure $
      [ UnlistedModulesWarning
          component
          (map fst (M.toList unlistedModules))
      | not (M.null unlistedModules)
      ]
  warnMissing _missingModules =
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
     Map FilePath (Path Abs File)
     -- ^ Known file usages, where the file path has already been resolved.
  -> NamedComponent
  -> [Path Abs Dir]
  -> DotCabalPath
  -> RIO GetPackageFileContext (Set ModuleName, Map FilePath (Path Abs File))
getDependencies knownUsages component dirs dotCabalPath =
  case dotCabalPath of
    DotCabalModulePath resolvedFile -> readResolvedHi resolvedFile
    DotCabalMainPath resolvedFile -> readResolvedHi resolvedFile
    DotCabalFilePath{} -> pure (S.empty, M.empty)
    DotCabalCFilePath{} -> pure (S.empty, M.empty)
 where
  readResolvedHi resolvedFile = do
    dumpHIDir <- componentOutputDir component <$> asks (.distDir)
    dir <- asks (parent . (.file))
    let sourceDir = fromMaybe dir $ find (`isProperPrefixOf` resolvedFile) dirs
        stripSourceDir d = stripProperPrefix d resolvedFile
    case stripSourceDir sourceDir of
      Nothing -> pure (S.empty, M.empty)
      Just fileRel -> do
        let hiPath = FilePath.replaceExtension
                       (toFilePath (dumpHIDir </> fileRel))
                       ".hi"
        dumpHIExists <- liftIO $ D.doesFileExist hiPath
        if dumpHIExists
          then parseHI knownUsages hiPath
          else pure (S.empty, M.empty)

-- | Parse a .hi file into a set of modules and files (a map from a given path
-- to a file to the resolved absolute path to the file).
parseHI ::
     Map FilePath (Path Abs File)
     -- ^ Known file usages, where the file path has already been resolved.
  -> FilePath
     -- ^ The path to the *.hi file to be parsed
  -> RIO GetPackageFileContext (Set ModuleName, Map FilePath (Path Abs File))
parseHI knownUsages hiPath = do
  dir <- asks (parent . (.file))
  result <-
    liftIO $ catchAnyDeep
      (Iface.fromFile hiPath)
      (pure . Left . displayException)
  case result of
    Left msg -> do
      prettyStackDevL
        [ flow "Failed to decode module interface:"
        , style File $ fromString hiPath
        , flow "Decoding failure:"
        , style Error $ fromString msg
        ]
      pure (S.empty, M.empty)
    Right iface -> do
      let moduleNames = fmap (fromString . T.unpack . decodeUtf8Lenient . fst) .
                        Iface.unList . Iface.dmods . Iface.deps
          resolveFileDependency file =
            case M.lookup file knownUsages of
              Just p ->
                pure $ Just (file, p)
              Nothing -> do
                resolved <- forgivingResolveFile dir file >>= rejectMissingFile
                when (isNothing resolved) $
                  prettyWarnL
                    [ flow "Dependent file listed in:"
                    , style File $ fromString hiPath
                    , flow "does not exist:"
                    , style File $ fromString file
                    ]
                pure $ (file,) <$> resolved
          resolveUsages = traverse
            (resolveFileDependency . Iface.unUsage) . Iface.unList . Iface.usage
      resolvedUsages <- catMaybes <$> resolveUsages iface
      pure (S.fromList $ moduleNames iface, M.fromList resolvedUsages)

-- | The directory where generated files are put like .o or .hs (from .x files).
componentOutputDir :: NamedComponent -> Path Abs Dir -> Path Abs Dir
componentOutputDir namedComponent distDir =
  case namedComponent of
    CLib -> buildDir distDir
    CSubLib name -> makeTmp name
    CFlib name -> makeTmp name
    CExe name -> makeTmp name
    CTest name -> makeTmp name
    CBench name -> makeTmp name
 where
  makeTmp name =
    buildDir distDir </> componentNameToDirNormOrTmp True name

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
  pkg <- asks (.file) >>= parsePackageNameFromFilePath
  customPreprocessorExts <- view $ configL . to (.customPreprocessorExts)
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
  makeDirCandidates ::
       [Text]
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
logPossibilities :: HasTerm env => [Path Abs Dir] -> ModuleName -> RIO env ()
logPossibilities dirs mn = do
  possibilities <- fmap concat (makePossibilities mn)
  unless (null possibilities) $ prettyWarnL
    [ flow "Unable to find a known candidate for the Cabal entry"
    , (style Module . fromString $ display mn) <> ","
    , flow "but did find:"
    , line <> bulletedList (map pretty possibilities)
    , flow "If you are using a custom preprocessor for this module"
    , flow "with its own file extension, consider adding the extension"
    , flow "to the value of the"
    , style Shell "custom-preprocessor-extensions"
    , flow "key in Stack's project-level configuration file"
    , "(" <> style File "stack.yaml" <> ")."
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

type CAndJsSources rec =
  (HasField "cSources" rec [FilePath], HasField "jsSources" rec [FilePath])

-- | Get all C sources and extra source files in a build.
buildOtherSources ::
     CAndJsSources rec
  => rec
  -> RIO GetPackageFileContext [DotCabalPath]
buildOtherSources build = do
  cwd <- liftIO getCurrentDir
  dir <- asks (parent . (.file))
  file <- asks (.file)
  let resolveDirFiles files toCabalPath =
        forMaybeM files $ \fp -> do
          result <- resolveDirFile dir fp
          case result of
            Nothing -> do
              warnMissingFile "File" cwd fp file
              pure Nothing
            Just p -> pure $ Just (toCabalPath p)
  csources <- resolveDirFiles build.cSources DotCabalCFilePath
  jsources <- resolveDirFiles build.jsSources DotCabalFilePath
  pure (csources <> jsources)

-- | Resolve file as a child of a specified directory, symlinks
-- don't get followed.
resolveDirFile ::
     (MonadIO m, MonadThrow m)
  => Path Abs Dir
  -> FilePath.FilePath
  -> m (Maybe (Path Abs File))
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
resolveDirOrWarn ::
     FilePath.FilePath
  -> RIO GetPackageFileContext (Maybe (Path Abs Dir))
resolveDirOrWarn = resolveOrWarn "Directory" f
 where
  f p x = forgivingResolveDir p x >>= rejectMissingDir

-- | Make the global autogen dir if Cabal version is new enough.
packageAutogenDir :: Path Abs Dir -> Path Abs Dir
packageAutogenDir distDir = buildDir distDir </> relDirGlobalAutogen

-- | Make the autogen dir.
componentAutogenDir :: NamedComponent -> Path Abs Dir -> Path Abs Dir
componentAutogenDir component distDir =
  componentBuildDir component distDir </> relDirAutogen

-- | Make the build dir. Note that Cabal >= 2.0 uses the
-- 'componentBuildDir' above for some things.
buildDir :: Path Abs Dir -> Path Abs Dir
buildDir distDir = distDir </> relDirBuild

-- NOTE: don't export this, only use it for valid paths based on
-- component names.
componentNameToDir :: StackUnqualCompName -> Path Rel Dir
componentNameToDir = componentNameToDirNormOrTmp False

componentNameToDirNormOrTmp :: Bool -> StackUnqualCompName -> Path Rel Dir
componentNameToDirNormOrTmp isTemp name =
  fromMaybe (throw $ ComponentNotParsedBug sName) (parseRelDir fullName)
 where
  fullName = if isTemp then sName <> "/" <> sName <> "-tmp" else sName
  sName = unqualCompToString name

-- | See 'Distribution.Simple.LocalBuildInfo.componentBuildDir'
componentBuildDir :: NamedComponent -> Path Abs Dir -> Path Abs Dir
componentBuildDir component distDir = case component of
  CLib -> buildDir distDir
  CSubLib name -> buildDir distDir </> componentNameToDir name
  CFlib name -> buildDir distDir </> componentNameToDir name
  CExe name -> buildDir distDir </> componentNameToDir name
  CTest name -> buildDir distDir </> componentNameToDir name
  CBench name -> buildDir distDir </> componentNameToDir name

-- Internal helper to define resolveFileOrWarn and resolveDirOrWarn
resolveOrWarn ::
     Text
  -> (Path Abs Dir -> String -> RIO GetPackageFileContext (Maybe a))
  -> FilePath.FilePath
  -> RIO GetPackageFileContext (Maybe a)
resolveOrWarn subject resolver path = do
  cwd <- liftIO getCurrentDir
  file <- asks (.file)
  dir <- asks (parent . (.file))
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
