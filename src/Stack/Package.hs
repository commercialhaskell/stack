{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Dealing with Cabal.

module Stack.Package
  (readPackage
  ,readPackageBS
  ,readPackageDir
  ,readPackageUnresolved
  ,readPackageUnresolvedBS
  ,resolvePackage
  ,getCabalFileName
  ,Package(..)
  ,GetPackageFiles(..)
  ,GetPackageOpts(..)
  ,PackageConfig(..)
  ,buildLogPath
  ,PackageException (..)
  ,resolvePackageDescription
  ,packageToolDependencies
  ,packageDependencies
  ,packageIdentifier
  ,autogenDir)
  where

import           Control.Exception hiding (try,catch)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger (MonadLogger,logWarn)
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Either
import           Data.Function
import           Data.List
import           Data.List.Extra (nubOrd)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Maybe.Extra
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Distribution.Compiler
import           Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as Cabal
import           Distribution.Package hiding (Package,PackageName,packageName,packageVersion,PackageIdentifier)
import           Distribution.PackageDescription hiding (FlagName)
import           Distribution.PackageDescription.Parse
import           Distribution.Simple.Utils
import           Distribution.System (OS (..), Arch, Platform (..))
import           Distribution.Text (display, simpleParse)
import           Path as FL
import           Path.Find
import           Path.IO
import           Prelude
import           Safe (headDef, tailSafe)
import           Stack.Constants
import           Stack.Types
import qualified Stack.Types.PackageIdentifier
import           System.Directory (doesFileExist, getDirectoryContents)
import           System.FilePath (splitExtensions)
import qualified System.FilePath as FilePath
import           System.IO.Error

packageIdentifier :: Package -> Stack.Types.PackageIdentifier.PackageIdentifier
packageIdentifier pkg =
    Stack.Types.PackageIdentifier.PackageIdentifier
        (packageName pkg)
        (packageVersion pkg)

-- | Read the raw, unresolved package information.
readPackageUnresolved :: (MonadIO m, MonadThrow m)
                      => Path Abs File
                      -> m GenericPackageDescription
readPackageUnresolved cabalfp =
  liftIO (BS.readFile (FL.toFilePath cabalfp))
  >>= readPackageUnresolvedBS (Just cabalfp)

-- | Read the raw, unresolved package information from a ByteString.
readPackageUnresolvedBS :: (MonadThrow m)
                        => Maybe (Path Abs File)
                        -> BS.ByteString
                        -> m GenericPackageDescription
readPackageUnresolvedBS mcabalfp bs =
    case parsePackageDescription chars of
       ParseFailed per ->
         throwM (PackageInvalidCabalFile mcabalfp per)
       ParseOk _ gpkg -> return gpkg
  where
    chars = T.unpack (dropBOM (decodeUtf8With lenientDecode bs))

    -- https://github.com/haskell/hackage-server/issues/351
    dropBOM t = fromMaybe t $ T.stripPrefix "\xFEFF" t

-- | Reads and exposes the package information
readPackage :: (MonadLogger m, MonadIO m, MonadThrow m, MonadCatch m)
            => PackageConfig
            -> Path Abs File
            -> m Package
readPackage packageConfig cabalfp =
  resolvePackage packageConfig `liftM` readPackageUnresolved cabalfp

-- | Reads and exposes the package information, from a ByteString
readPackageBS :: (MonadThrow m)
              => PackageConfig
              -> BS.ByteString
              -> m Package
readPackageBS packageConfig bs =
  resolvePackage packageConfig `liftM` readPackageUnresolvedBS Nothing bs

-- | Convenience wrapper around @readPackage@ that first finds the cabal file
-- in the given directory.
readPackageDir :: (MonadLogger m, MonadIO m, MonadThrow m, MonadCatch m)
               => PackageConfig
               -> Path Abs Dir
               -> m (Path Abs File, Package)
readPackageDir packageConfig dir = do
    cabalfp <- getCabalFileName dir
    pkg <- readPackage packageConfig cabalfp
    name <- parsePackageNameFromFilePath cabalfp
    when (packageName pkg /= name)
        $ throwM $ MismatchedCabalName cabalfp name
    return (cabalfp, pkg)

-- | Resolve a parsed cabal file into a 'Package'.
resolvePackage :: PackageConfig
               -> GenericPackageDescription
               -> Package
resolvePackage packageConfig gpkg =
    Package
    { packageName = name
    , packageVersion = fromCabalVersion (pkgVersion pkgId)
    , packageDeps = deps
    , packageFiles = GetPackageFiles $
      \cabalfp ->
           do distDir <- distDirFromDir (parent cabalfp)
              (_,files,mains,extra) <-
                  runReaderT
                      (packageDescModulesAndFiles pkg)
                      (cabalfp, buildDir distDir)
              return (files, mains, S.singleton cabalfp <> extra)
    , packageModules = GetPackageModules $
      \cabalfp ->
           do distDir <- distDirFromDir (parent cabalfp)
              (modules,_,_,_) <-
                  runReaderT
                      (packageDescModulesAndFiles pkg)
                      (cabalfp, buildDir distDir)
              return modules
    , packageTools = packageDescTools pkg
    , packageFlags = packageConfigFlags packageConfig
    , packageAllDeps = S.fromList (M.keys deps)
    , packageHasLibrary = maybe False (buildable . libBuildInfo) (library pkg)
    , packageTests = S.fromList $
      [T.pack (testName t) | t <- testSuites pkg
                           , buildable (testBuildInfo t)]
    , packageBenchmarks = S.fromList $
      [T.pack (benchmarkName b) | b <- benchmarks pkg
                                , buildable (benchmarkBuildInfo b)]
    , packageExes = S.fromList $
      [T.pack (exeName b) | b <- executables pkg
                          , buildable (buildInfo b)]
    , packageOpts = GetPackageOpts $
      \sourceMap locals cabalfp ->
           generatePkgDescOpts sourceMap locals cabalfp pkg
    , packageHasExposedModules = maybe
          False
          (not . null . exposedModules)
          (library pkg)
    , packageSimpleType = buildType (packageDescription gpkg) == Just Simple
    , packageDefinedFlags = S.fromList $
      map (fromCabalFlagName . flagName) $ genPackageFlags gpkg
    }
  where
    pkgId = package (packageDescription gpkg)
    name = fromCabalPackageName (pkgName pkgId)
    pkg = resolvePackageDescription packageConfig gpkg
    deps = M.filterWithKey (const . (/= name)) (packageDependencies pkg)

-- | Generate GHC options for the package's components, and a list of
-- options which apply generally to the package, not one specific
-- component.
generatePkgDescOpts
    :: (HasEnvConfig env, HasPlatform env, MonadThrow m, MonadReader env m, MonadIO m)
    => SourceMap
    -> [PackageName]
    -> Path Abs File
    -> PackageDescription
    -> m (Map NamedComponent [String],[String])
generatePkgDescOpts sourceMap locals cabalfp pkg = do
    distDir <- distDirFromDir cabalDir
    let cabalmacros = autogenDir distDir </> $(mkRelFile "cabal_macros.h")
    exists <- fileExists cabalmacros
    let mcabalmacros =
            if exists
                then Just cabalmacros
                else Nothing
    let generate =
            generateBuildInfoOpts
                sourceMap
                mcabalmacros
                cabalDir
                distDir
                locals
    return
        ( M.fromList
              (concat
                   [ maybe
                         []
                         (return . (CLib, ) . generate . libBuildInfo)
                         (library pkg)
                   , map
                         (\exe ->
                               ( CExe (T.pack (exeName exe))
                               , generate (buildInfo exe)))
                         (executables pkg)
                   , map
                         (\bench ->
                               ( CBench (T.pack (benchmarkName bench))
                               , generate (benchmarkBuildInfo bench)))
                         (benchmarks pkg)
                   , map
                         (\test ->
                               ( CBench (T.pack (testName test))
                               , generate (testBuildInfo test)))
                         (testSuites pkg)])
        , ["-hide-all-packages"])
  where
    cabalDir = parent cabalfp

-- | Generate GHC options for the target.
generateBuildInfoOpts
    :: SourceMap
    -> Maybe (Path Abs File)
    -> Path Abs Dir
    -> Path Abs Dir
    -> [PackageName]
    -> BuildInfo
    -> [String]
generateBuildInfoOpts sourceMap mcabalmacros cabalDir distDir locals b =
    nubOrd (concat [ghcOpts b, extOpts b, srcOpts, includeOpts, macros, deps, extra b, extraDirs, fworks b])
  where
    deps =
        concat
            [ ["-package=" <> display name <>
               maybe "" -- This empty case applies to e.g. base.
                     ((("-" <>) . versionString) . sourceVersion)
                     (M.lookup (fromCabalPackageName name) sourceMap)]
            | Dependency name _ <- targetBuildDepends b
            , not (elem name (map toCabalPackageName locals))]
        -- Generates: -package=base -package=base16-bytestring-0.1.1.6 ...
    sourceVersion (PSUpstream ver _ _) = ver
    sourceVersion (PSLocal localPkg) = packageVersion (lpPackage localPkg)
    macros =
        case mcabalmacros of
            Nothing -> []
            Just cabalmacros ->
                ["-optP-include", "-optP" <> toFilePath cabalmacros]
    ghcOpts = concatMap snd . filter (isGhc . fst) . options
      where
        isGhc GHC = True
        isGhc _ = False
    extOpts = map (("-X" ++) . display) . usedExtensions
    srcOpts =
        map
            (("-i" <>) . toFilePath)
            (cabalDir :
             map (cabalDir </>) (mapMaybe parseRelDir (hsSourceDirs b)) <>
             [autogenDir distDir,buildDir distDir]) ++
        ["-stubdir=" ++ toFilePath (buildDir distDir)]
    includeOpts =
        [ "-I" <> toFilePath absDir
        | dir <- includeDirs b
        , absDir <- case (parseAbsDir dir, parseRelDir dir) of
          (Just ab, _       ) -> [ab]
          (_      , Just rel) -> [cabalDir </> rel]
          (Nothing, Nothing ) -> []
        ]
    extra
        = map ("-l" <>)
        . extraLibs
    extraDirs =
        [ "-L" <> toFilePath absDir
        | dir <- extraLibDirs b
        , absDir <- case (parseAbsDir dir, parseRelDir dir) of
          (Just ab, _       ) -> [ab]
          (_      , Just rel) -> [cabalDir </> rel]
          (Nothing, Nothing ) -> []
        ]
    fworks = map (\fwk -> "-framework=" <> fwk) . frameworks

-- | Make the autogen dir.
autogenDir :: Path Abs Dir -> Path Abs Dir
autogenDir distDir = buildDir distDir </> $(mkRelDir "autogen")

-- | Make the build dir.
buildDir :: Path Abs Dir -> Path Abs Dir
buildDir distDir = distDir </> $(mkRelDir "build")

-- | Make the component-specific subdirectory of the build directory.
getBuildComponentDir :: Maybe String -> Maybe (Path Rel Dir)
getBuildComponentDir Nothing = Nothing
getBuildComponentDir (Just name) = parseRelDir (name FilePath.</> (name ++ "-tmp"))

-- | Get all dependencies of the package (buildable targets only).
packageDependencies :: PackageDescription -> Map PackageName VersionRange
packageDependencies =
  M.fromListWith intersectVersionRanges .
  concatMap (map (\dep -> ((depName dep),depRange dep)) .
             targetBuildDepends) .
  allBuildInfo'

-- | Get all build tool dependencies of the package (buildable targets only).
packageToolDependencies :: PackageDescription -> Map BS.ByteString VersionRange
packageToolDependencies =
  M.fromList .
  concatMap (map (\dep -> ((packageNameByteString $ depName dep),depRange dep)) .
             buildTools) .
  allBuildInfo'

-- | Get all dependencies of the package (buildable targets only).
packageDescTools :: PackageDescription -> [Dependency]
packageDescTools = concatMap buildTools . allBuildInfo'

-- | This is a copy-paste from Cabal's @allBuildInfo@ function, but with the
-- @buildable@ test removed. The reason is that (surprise) Cabal is broken,
-- see: https://github.com/haskell/cabal/issues/1725
allBuildInfo' :: PackageDescription -> [BuildInfo]
allBuildInfo' pkg_descr = [ bi | Just lib <- [library pkg_descr]
                              , let bi = libBuildInfo lib
                              , True || buildable bi ]
                      ++ [ bi | exe <- executables pkg_descr
                              , let bi = buildInfo exe
                              , True || buildable bi ]
                      ++ [ bi | tst <- testSuites pkg_descr
                              , let bi = testBuildInfo tst
                              , True || buildable bi
                              , testEnabled tst ]
                      ++ [ bi | tst <- benchmarks pkg_descr
                              , let bi = benchmarkBuildInfo tst
                              , True || buildable bi
                              , benchmarkEnabled tst ]

-- | Get all files referenced by the package.
packageDescModulesAndFiles
    :: (MonadLogger m, MonadIO m, MonadThrow m, MonadReader (Path Abs File, Path Abs Dir) m, MonadCatch m)
    => PackageDescription
    -> m (Map NamedComponent (Set ModuleName), Map NamedComponent (Set (Path Abs File)), Map NamedComponent (Set MainIs), Set (Path Abs File))
packageDescModulesAndFiles pkg = do
    (libraryMods,libFiles,_) <-
        maybe
            (return (M.empty, M.empty, M.empty))
            (asModuleAndFileMap libComponent libraryFiles)
            (library pkg)
    (executableMods,exeFiles,exeMainIs) <-
        liftM
            foldTriples
            (mapM
                 (asModuleAndFileMap exeComponent executableFiles)
                 (executables pkg))
    (testMods,testFps,testMainIs) <-
        liftM
            foldTriples
            (mapM (asModuleAndFileMap testComponent testFiles) (testSuites pkg))
    (benchModules,benchFiles,benchMainIs) <-
        liftM
            foldTriples
            (mapM
                 (asModuleAndFileMap benchComponent benchmarkFiles)
                 (benchmarks pkg))
    dfiles <- resolveGlobFiles (map (dataDir pkg FilePath.</>) (dataFiles pkg))
    let modules = libraryMods <> executableMods <> testMods <> benchModules
        files = libFiles <> exeFiles <> testFps <> benchFiles
        mains = exeMainIs <> benchMainIs <> testMainIs
    return (modules, files, mains, dfiles)
  where
    libComponent = const CLib
    exeComponent = CExe . T.pack . exeName
    testComponent = CTest . T.pack . testName
    benchComponent = CBench . T.pack . benchmarkName
    asModuleAndFileMap label f lib = do
        (a,b,c) <- f lib
        return
            ( M.singleton (label lib) a
            , M.singleton (label lib) b
            , M.singleton (label lib) c)
    foldTriples =
        foldl'
            (\(a,b,c) (x,y,z) ->
                  (a <> x, b <> y, c <> z))
            (M.empty, M.empty, M.empty)

-- | Resolve globbing of files (e.g. data files) to absolute paths.
resolveGlobFiles :: (MonadLogger m,MonadIO m,MonadThrow m,MonadReader (Path Abs File, Path Abs Dir) m,MonadCatch m)
                 => [String] -> m (Set (Path Abs File))
resolveGlobFiles =
    liftM (S.fromList . catMaybes . concat) .
    mapM resolve
  where
    resolve name =
        if any (== '*') name
            then explode name
            else liftM return (resolveFileOrWarn name)
    explode name = do
        dir <- asks (parent . fst)
        names <-
            matchDirFileGlob'
                (FL.toFilePath dir)
                name
        mapM resolveFileOrWarn names
    matchDirFileGlob' dir glob =
        catch
            (liftIO (matchDirFileGlob_ dir glob))
            (\(e :: IOException) ->
                  if isUserError e
                      then do
                          $logWarn
                              ("Wildcard does not match any files: " <> T.pack glob <> "\n" <>
                               "in directory: " <> T.pack dir)
                          return []
                      else throwM e)

-- | This is a copy/paste of the Cabal library function, but with
--
-- @ext == ext'@
--
-- Changed to
--
-- @isSuffixOf ext ext'@
--
-- So that this will work:
--
-- @
-- λ> matchDirFileGlob_ "." "test/package-dump/*.txt"
-- ["test/package-dump/ghc-7.8.txt","test/package-dump/ghc-7.10.txt"]
-- @
--
matchDirFileGlob_ :: String -> String -> IO [String]
matchDirFileGlob_ dir filepath = case parseFileGlob filepath of
  Nothing -> die $ "invalid file glob '" ++ filepath
                ++ "'. Wildcards '*' are only allowed in place of the file"
                ++ " name, not in the directory name or file extension."
                ++ " If a wildcard is used it must be with an file extension."
  Just (NoGlob filepath') -> return [filepath']
  Just (FileGlob dir' ext) -> do
    files <- getDirectoryContents (dir FilePath.</> dir')
    case   [ dir' FilePath.</> file
           | file <- files
           , let (name, ext') = splitExtensions file
           , not (null name) && isSuffixOf ext ext' ] of
      []      -> die $ "filepath wildcard '" ++ filepath
                    ++ "' does not match any files."
      matches -> return matches

-- | Get all files referenced by the benchmark.
benchmarkFiles :: (MonadLogger m, MonadIO m, MonadThrow m, MonadReader (Path Abs File, Path Abs Dir) m)
               => Benchmark -> m (Set ModuleName,Set (Path Abs File),Set MainIs)
benchmarkFiles bench = do
    dirs <- mapMaybeM resolveDirOrWarn (hsSourceDirs build)
    dir <- asks (parent . fst)
    (rmodules,rfiles) <-
        resolveFilesAndDeps
            (Just $ benchmarkName bench)
            (dirs ++ [dir])
            bnames
            haskellModuleExts
    mainFiles <- resolveFiles (dirs ++ [dir]) exposed haskellModuleExts
    cfiles <- buildCSources build
    return (rmodules, rfiles <> cfiles, S.map MainIs (S.fromList mainFiles))
  where
    exposed =
        case benchmarkInterface bench of
            BenchmarkExeV10 _ fp -> [Right fp]
            BenchmarkUnsupported _ -> []
    bnames = map Left (otherModules build)
    build = benchmarkBuildInfo bench

-- | Get all files referenced by the test.
testFiles :: (MonadLogger m, MonadIO m, MonadThrow m, MonadReader (Path Abs File, Path Abs Dir) m)
          => TestSuite -> m (Set ModuleName,Set (Path Abs File),Set MainIs)
testFiles test = do
    dirs <- mapMaybeM resolveDirOrWarn (hsSourceDirs build)
    dir <- asks (parent . fst)
    (modules,rfiles) <-
        resolveFilesAndDeps
            (Just $ testName test)
            (dirs ++ [dir])
            bnames
            haskellModuleExts
    mainFiles <- resolveFiles (dirs ++ [dir]) exposed haskellModuleExts
    cfiles <- buildCSources build
    return (modules, rfiles <> cfiles, S.map MainIs (S.fromList mainFiles))
  where
    exposed =
        case testInterface test of
            TestSuiteExeV10 _ fp -> [Right fp]
            TestSuiteLibV09 _ mn -> [Left mn]
            TestSuiteUnsupported _ -> []
    bnames = map Left (otherModules build)
    build = testBuildInfo test

-- | Get all files referenced by the executable.
executableFiles :: (MonadLogger m,MonadIO m,MonadThrow m,MonadReader (Path Abs File, Path Abs Dir) m)
                => Executable -> m (Set ModuleName,Set (Path Abs File),Set MainIs)
executableFiles exe = do
    dirs <- mapMaybeM resolveDirOrWarn (hsSourceDirs build)
    dir <- asks (parent . fst)
    (modules,rfiles) <-
        resolveFilesAndDeps
            (Just $ exeName exe)
            (dirs ++ [dir])
            (map Left (otherModules build))
            haskellModuleExts
    mainFiles <-
        resolveFiles (dirs ++ [dir]) [Right (modulePath exe)] haskellModuleExts
    cfiles <- buildCSources build
    return (modules, rfiles <> cfiles, S.map MainIs (S.fromList mainFiles))
  where
    build = buildInfo exe

-- | Get all files referenced by the library.
libraryFiles :: (MonadLogger m,MonadIO m,MonadThrow m,MonadReader (Path Abs File, Path Abs Dir) m)
             => Library -> m (Set ModuleName,Set (Path Abs File), Set MainIs)
libraryFiles lib =
  do dirs <- mapMaybeM resolveDirOrWarn (hsSourceDirs build)
     dir <- asks (parent . fst)
     (modules,rfiles) <- resolveFilesAndDeps
         Nothing
         (dirs ++ [dir])
         names
         haskellModuleExts
     cfiles <- buildCSources build
     return (modules,rfiles <> cfiles, mempty)
  where
    names =
        concat [bnames,exposed]
    exposed = map Left (exposedModules lib)
    bnames = map Left (otherModules build)
    build = libBuildInfo lib

-- | Get all C sources in a build.
buildCSources :: (MonadLogger m,MonadIO m,MonadThrow m,MonadReader (Path Abs File, Path Abs Dir) m)
           => BuildInfo -> m (Set (Path Abs File))
buildCSources build =
    liftM S.fromList (mapMaybeM resolveFileOrWarn (cSources build))

-- | Get all dependencies of a package, including library,
-- executables, tests, benchmarks.
resolvePackageDescription :: PackageConfig
                          -> GenericPackageDescription
                          -> PackageDescription
resolvePackageDescription packageConfig (GenericPackageDescription desc defaultFlags mlib exes tests benches) =
  desc {library =
          fmap (resolveConditions rc updateLibDeps) mlib
       ,executables =
          map (\(n, v) -> (resolveConditions rc updateExeDeps v){exeName=n})
              exes
       ,testSuites =
          map (\(n,v) -> (resolveConditions rc updateTestDeps v){testName=n})
              tests
       ,benchmarks =
          map (\(n,v) -> (resolveConditions rc updateBenchmarkDeps v){benchmarkName=n})
              benches}
  where flags =
          M.union (packageConfigFlags packageConfig)
                  (flagMap defaultFlags)

        rc = mkResolveConditions
                (packageConfigGhcVersion packageConfig)
                (packageConfigPlatform packageConfig)
                flags

        updateLibDeps lib deps =
          lib {libBuildInfo =
                 ((libBuildInfo lib) {targetBuildDepends =
                                        deps})}
        updateExeDeps exe deps =
          exe {buildInfo =
                 (buildInfo exe) {targetBuildDepends = deps}}
        updateTestDeps test deps =
          test {testBuildInfo =
                  (testBuildInfo test) {targetBuildDepends = deps}
               ,testEnabled = packageConfigEnableTests packageConfig}
        updateBenchmarkDeps benchmark deps =
          benchmark {benchmarkBuildInfo =
                       (benchmarkBuildInfo benchmark) {targetBuildDepends = deps}
                    ,benchmarkEnabled = packageConfigEnableBenchmarks packageConfig}

-- | Make a map from a list of flag specifications.
--
-- What is @flagManual@ for?
flagMap :: [Flag] -> Map FlagName Bool
flagMap = M.fromList . map pair
  where pair :: Flag -> (FlagName, Bool)
        pair (MkFlag (fromCabalFlagName -> name) _desc def _manual) = (name,def)

data ResolveConditions = ResolveConditions
    { rcFlags :: Map FlagName Bool
    , rcGhcVersion :: Version
    , rcOS :: OS
    , rcArch :: Arch
    }

-- | Generic a @ResolveConditions@ using sensible defaults.
mkResolveConditions :: Version -- ^ GHC version
                    -> Platform -- ^ installation target platform
                    -> Map FlagName Bool -- ^ enabled flags
                    -> ResolveConditions
mkResolveConditions ghcVersion (Platform arch os) flags = ResolveConditions
    { rcFlags = flags
    , rcGhcVersion = ghcVersion
    , rcOS = if isWindows os then Windows else os
    , rcArch = arch
    }

-- | Resolve the condition tree for the library.
resolveConditions :: (Monoid target,Show target)
                  => ResolveConditions
                  -> (target -> cs -> target)
                  -> CondTree ConfVar cs target
                  -> target
resolveConditions rc addDeps (CondNode lib deps cs) = basic <> children
  where basic = addDeps lib deps
        children = mconcat (map apply cs)
          where apply (cond,node,mcs) =
                  if (condSatisfied cond)
                     then resolveConditions rc addDeps node
                     else maybe mempty (resolveConditions rc addDeps) mcs
                condSatisfied c =
                  case c of
                    Var v -> varSatisifed v
                    Lit b -> b
                    CNot c' ->
                      not (condSatisfied c')
                    COr cx cy ->
                      or [condSatisfied cx,condSatisfied cy]
                    CAnd cx cy ->
                      and [condSatisfied cx,condSatisfied cy]
                varSatisifed v =
                  case v of
                    OS os -> os == rcOS rc
                    Arch arch -> arch == rcArch rc
                    Flag flag ->
                        case M.lookup (fromCabalFlagName flag) (rcFlags rc) of
                            Just x -> x
                            Nothing ->
                                -- NOTE: This should never happen, as all flags
                                -- which are used must be declared. Defaulting
                                -- to False
                                False
                    Impl flavor range ->
                        flavor == GHC &&
                        withinRange (rcGhcVersion rc) range

-- | Get the name of a dependency.
depName :: Dependency -> PackageName
depName = \(Dependency n _) -> fromCabalPackageName n

-- | Get the version range of a dependency.
depRange :: Dependency -> VersionRange
depRange = \(Dependency _ r) -> r

-- | Try to resolve the list of base names in the given directory by
-- looking for unique instances of base names applied with the given
-- extensions, plus find any of their module and TemplateHaskell
-- dependencies.
resolveFilesAndDeps
    :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader (Path Abs File, Path Abs Dir) m)
    => Maybe (String) -- ^ Package component name
    -> [Path Abs Dir] -- ^ Directories to look in.
    -> [Either ModuleName String] -- ^ Base names.
    -> [Text] -- ^ Extentions.
    -> m (Set ModuleName,Set (Path Abs File))
resolveFilesAndDeps component dirs names0 exts = do
    (moduleFiles,thFiles,foundModules) <- loop names0 S.empty
    warnUnlisted component (lefts names0) foundModules
    return (foundModules, moduleFiles <> S.fromList thFiles)
  where
    loop [] doneModules = return (S.empty, [], doneModules)
    loop names doneModules0 = do
        resolvedFiles <- resolveFiles dirs names exts
        pairs <- mapM (getDependencies component) resolvedFiles
        let doneModules' = S.union doneModules0 (S.fromList (lefts names))
            moduleDeps = S.unions (map fst pairs)
            thDepFiles = concatMap snd pairs
            modulesRemaining = S.difference moduleDeps doneModules'
        (moduleDepFiles',thDepFiles',doneModules'') <-
            loop (map Left (S.toList modulesRemaining)) doneModules'
        return
            ( S.union (S.fromList resolvedFiles) moduleDepFiles'
            , thDepFiles ++ thDepFiles'
            , doneModules'')

-- | Warn about modules which are used but not listed in the cabal
-- file.
warnUnlisted
    :: (MonadLogger m, MonadReader (Path Abs File, void) m)
    => Maybe String -> [ModuleName] -> Set ModuleName -> m ()
warnUnlisted component names0 foundModules = do
    cabalfp <- asks fst
    unless (S.null unlistedModules) $
        $(logWarn) $
        T.pack $
        "Warning: " ++
        (if S.size unlistedModules == 1
             then "module"
             else "modules") ++
        " not listed in " ++
        toFilePath (filename cabalfp) ++
        (case component of
             Nothing -> " for library"
             Just c -> " for '" ++ c ++ "'") ++
        " component (add to other-modules):\n    " ++
        intercalate "\n    " (map display (S.toList unlistedModules))
  where
    unlistedModules =
        foundModules `S.difference` (S.fromList names0)

-- | Get the dependencies of a Haskell module file.
getDependencies
    :: (MonadReader (Path Abs File, Path Abs Dir) m, MonadIO m)
    => Maybe String -> Path Abs File -> m (Set ModuleName, [Path Abs File])
getDependencies component resolvedFile = do
    dir <- asks (parent . fst)
    dumpHIDir <- getDumpHIDir
    case stripDir dir resolvedFile of
        Nothing -> return (S.empty, [])
        Just fileRel -> do
            let dumpHIPath =
                    FilePath.replaceExtension
                        (toFilePath (dumpHIDir </> fileRel))
                        ".dump-hi"
            dumpHIExists <- liftIO $ doesFileExist dumpHIPath
            if dumpHIExists
                then parseDumpHI dumpHIPath
                else return (S.empty, [])
  where getDumpHIDir = do
            bld <- asks snd
            return $ maybe bld (bld </>) (getBuildComponentDir component)

-- | Parse a .dump-hi file into a set of modules and files.
parseDumpHI
    :: (MonadReader (Path Abs File, void) m, MonadIO m)
    => FilePath -> m (Set ModuleName, [Path Abs File])
parseDumpHI dumpHIPath = do
    dir <- asks (parent . fst)
    dumpHI <- liftIO $ fmap C8.lines (C8.readFile dumpHIPath)
    let startModuleDeps =
            dropWhile (not . ("module dependencies:" `C8.isPrefixOf`)) dumpHI
        moduleDeps =
            S.fromList $
            mapMaybe (simpleParse . T.unpack . decodeUtf8) $
            C8.words $
            C8.concat $
            C8.dropWhile (/= ' ') (headDef "" startModuleDeps) :
            takeWhile (" " `C8.isPrefixOf`) (tailSafe startModuleDeps)
        thDeps =
            -- The dependent file path is surrounded by quotes but is not escaped.
            -- It can be an absolute or relative path.
            mapMaybe
                (parseAbsOrRelFile dir <=<
                 (fmap T.unpack .
                  (T.stripSuffix "\"" <=< T.stripPrefix "\"") .
                  T.dropWhileEnd (== '\r') . decodeUtf8 . C8.dropWhile (/= '"'))) $
            filter ("addDependentFile \"" `C8.isPrefixOf`) dumpHI
    return (moduleDeps, thDeps)
  where
    parseAbsOrRelFile dir fp =
        case parseRelFile fp of
            Just rel -> Just (dir </> rel)
            Nothing -> parseAbsFile fp

-- | Try to resolve the list of base names in the given directory by
-- looking for unique instances of base names applied with the given
-- extensions.
resolveFiles
    :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader (Path Abs File, Path Abs Dir) m)
    => [Path Abs Dir] -- ^ Directories to look in.
    -> [Either ModuleName String] -- ^ Base names.
    -> [Text] -- ^ Extentions.
    -> m [Path Abs File]
resolveFiles dirs names exts = do
    liftM catMaybes (forM names (findCandidate dirs exts))

-- | Find a candidate for the given module-or-filename from the list
-- of directories and given extensions.
findCandidate
    :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader (Path Abs File, Path Abs Dir) m)
    => [Path Abs Dir]
    -> [Text]
    -> Either ModuleName String
    -> m (Maybe (Path Abs File))
findCandidate dirs exts name = do
    pkg <- asks fst >>= parsePackageNameFromFilePath
    candidates <- liftIO makeNameCandidates
    case candidates of
        [candidate] -> return (Just candidate)
        [] -> do
            case name of
                Left mn
                  | not (display mn == paths_pkg pkg) -> do
                      logPossibilities dirs mn
                _ -> return ()
            return Nothing
        (candidate:rest) -> do
            warnMultiple name candidate rest
            return (Just candidate)
  where
    paths_pkg pkg = "Paths_" ++ packageNameString pkg
    makeNameCandidates =
        liftM (nubOrd . rights . concat) (mapM makeDirCandidates dirs)
    makeDirCandidates
        :: Path Abs Dir
        -> IO [Either ResolveException (Path Abs File)]
    makeDirCandidates dir =
        case name of
            Right fp -> liftM return (try (resolveFile dir fp))
            Left mn ->
                mapM
                    (\ext ->
                          try
                              (resolveFile
                                   dir
                                   (Cabal.toFilePath mn ++ "." ++ ext)))
                    (map T.unpack exts)

-- | Warn the user that multiple candidates are available for an
-- entry, but that we picked one anyway and continued.
warnMultiple
    :: MonadLogger m
    => Either ModuleName String -> Path b t -> [Path b t] -> m ()
warnMultiple name candidate rest =
    $logWarn
        ("There were multiple candidates for the Cabal entry \"" <>
         showName name <>
         "(" <>
         T.intercalate "," (map (T.pack . toFilePath) rest) <>
         "), picking " <>
         T.pack (toFilePath candidate))
  where showName (Left name') = T.pack (display name')
        showName (Right fp) = T.pack fp

-- | Log that we couldn't find a candidate, but there are
-- possibilities for custom preprocessor extensions.
--
-- For example: .erb for a Ruby file might exist in one of the
-- directories.
logPossibilities
    :: (MonadIO m, MonadThrow m, MonadLogger m)
    => [Path Abs Dir] -> ModuleName -> m ()
logPossibilities dirs mn = do
    possibilities <- liftM concat (makePossibilities mn)
    case possibilities of
        [] -> return ()
        _ ->
            $logWarn
                ("Unable to find a known candidate for the Cabal entry \"" <>
                 T.pack (display mn) <>
                 "\", but did find: " <>
                 T.intercalate ", " (map (T.pack . toFilePath) possibilities) <>
                 ". If you are using a custom preprocessor for this module \
                 \with its own file extension, consider adding the file(s) \
                 \to your .cabal under extra-source-files.")
  where
    makePossibilities name =
        mapM
            (\dir ->
                  do (_,files) <- listDirectory dir
                     return
                         (map
                              filename
                              (filter
                                   (isPrefixOf (display name) .
                                    toFilePath . filename)
                                   files)))
            dirs

-- | Get the filename for the cabal file in the given directory.
--
-- If no .cabal file is present, or more than one is present, an exception is
-- thrown via 'throwM'.
getCabalFileName
    :: (MonadThrow m, MonadIO m)
    => Path Abs Dir -- ^ package directory
    -> m (Path Abs File)
getCabalFileName pkgDir = do
    files <- liftIO $ findFiles
        pkgDir
        (flip hasExtension "cabal" . FL.toFilePath)
        (const False)
    case files of
        [] -> throwM $ PackageNoCabalFileFound pkgDir
        [x] -> return x
        _:_ -> throwM $ PackageMultipleCabalFilesFound pkgDir files
  where hasExtension fp x = FilePath.takeExtensions fp == "." ++ x

-- | Path for the package's build log.
buildLogPath :: (MonadReader env m, HasBuildConfig env, MonadThrow m)
             => Package -> Maybe String -> m (Path Abs File)
buildLogPath package' msuffix = do
  env <- ask
  let stack = configProjectWorkDir env
  fp <- parseRelFile $ concat $
    (packageIdentifierString (packageIdentifier package')) :
    (maybe id (\suffix -> ("-" :) . (suffix :)) msuffix) [".log"]
  return $ stack </> $(mkRelDir "logs") </> fp

-- Internal helper to define resolveFileOrWarn and resolveDirOrWarn
resolveOrWarn :: (MonadLogger m, MonadIO m, MonadReader (Path Abs File, Path Abs Dir) m)
              => Text
              -> (Path Abs Dir -> String -> m (Maybe a))
              -> FilePath.FilePath
              -> m (Maybe a)
resolveOrWarn subject resolver path =
  do cwd <- getWorkingDir
     file <- asks fst
     dir <- asks (parent . fst)
     result <- resolver dir path
     when (isNothing result) $
       $logWarn ("Warning: " <> subject <> " listed in " <>
         T.pack (maybe (FL.toFilePath file) FL.toFilePath (stripDir cwd file)) <>
         " file does not exist: " <>
         T.pack path)
     return result

-- | Resolve the file, if it can't be resolved, warn for the user
-- (purely to be helpful).
resolveFileOrWarn :: (MonadThrow m,MonadIO m,MonadLogger m,MonadReader (Path Abs File, Path Abs Dir) m)
                  => FilePath.FilePath
                  -> m (Maybe (Path Abs File))
resolveFileOrWarn = resolveOrWarn "File" resolveFileMaybe

-- | Resolve the directory, if it can't be resolved, warn for the user
-- (purely to be helpful).
resolveDirOrWarn :: (MonadThrow m,MonadIO m,MonadLogger m,MonadReader (Path Abs File, Path Abs Dir) m)
                 => FilePath.FilePath
                 -> m (Maybe (Path Abs Dir))
resolveDirOrWarn = resolveOrWarn "Directory" resolveDirMaybe
