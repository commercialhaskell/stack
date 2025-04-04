{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Dealing with Cabal.

module Stack.Package
  ( readDotBuildinfo
  , resolvePackage
  , packageFromPackageDescription
  , Package (..)
  , PackageConfig (..)
  , buildLogPath
  , PackageException (..)
  , resolvePackageDescription
  , packageDependencies
  , applyForceCustomBuild
  , hasBuildableMainLibrary
  , mainLibraryHasExposedModules
  , packageUnknownTools
  , buildableForeignLibs
  , buildableSubLibs
  , buildableExes
  , buildableTestSuites
  , buildableBenchmarks
  , getPackageOpts
  , processPackageDepsEither
  , listOfPackageDeps
  , setOfPackageDeps
  , topSortPackageComponent
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Distribution.CabalSpecVersion ( cabalSpecToVersionDigits )
import           Distribution.Compiler
                   ( CompilerFlavor (..), PerCompilerFlavor (..) )
import           Distribution.ModuleName ( ModuleName )
import           Distribution.Package ( mkPackageName )
import           Distribution.PackageDescription
                   ( Benchmark (..), BuildInfo (..), BuildType (..)
                   , CondTree (..), Condition (..), ConfVar (..)
                   , Dependency (..), Executable (..), ForeignLib (..)
                   , GenericPackageDescription (..), HookedBuildInfo
                   , Library (..), PackageDescription (..), PackageFlag (..)
                   , SetupBuildInfo (..), TestSuite (..), allLibraries
                   , buildType, depPkgName, depVerRange
                   , unqualComponentNameToPackageName
                   )
import qualified Distribution.PackageDescription as Executable
                   ( Executable (..) )
import           Distribution.Simple.PackageDescription ( readHookedBuildInfo )
import           Distribution.System ( OS (..), Arch, Platform (..) )
import           Distribution.Text ( display )
import qualified Distribution.Types.CondTree as Cabal
import           Distribution.Utils.Path ( makeSymbolicPath, getSymbolicPath )
import           Distribution.Verbosity ( silent )
import           Distribution.Version
                   ( anyVersion, mkVersion, orLaterVersion )
import           Path
                   ( (</>), parent, parseAbsDir, parseRelDir, parseRelFile
                   , stripProperPrefix
                   )
import           Path.Extra ( concatAndCollapseAbsDir, toFilePathNoTrailingSep )
import           Stack.Component
                   ( componentDependencyMap, foldOnNameAndBuildInfo
                   , isComponentBuildable, stackBenchmarkFromCabal
                   , stackExecutableFromCabal, stackForeignLibraryFromCabal
                   , stackLibraryFromCabal, stackTestFromCabal
                   )
import           Stack.ComponentFile
                   ( buildDir, componentAutogenDir, componentBuildDir
                   , componentOutputDir, packageAutogenDir
                   )
import           Stack.Constants ( relFileCabalMacrosH, relDirLogs )
import           Stack.Constants.Config ( distDirFromDir )
import           Stack.PackageFile ( getPackageFile, stackPackageFileFromCabal )
import           Stack.Prelude hiding ( Display (..) )
import           Stack.Types.BuildConfig ( HasBuildConfig (..), getWorkDir )
import           Stack.Types.CompCollection
                   ( CompCollection, collectionLookup, foldAndMakeCollection
                   , foldComponentToAnotherCollection, getBuildableSet
                   , getBuildableSetText
                   )
import           Stack.Types.Compiler ( ActualCompiler (..) )
import           Stack.Types.CompilerPaths ( cabalVersionL )
import           Stack.Types.Component
                   ( HasBuildInfo, HasComponentInfo, StackUnqualCompName (..) )
import           Stack.Types.ComponentUtils ( emptyCompName, toCabalName )
import qualified Stack.Types.Component as Component
import           Stack.Types.Config ( Config (..), HasConfig (..) )
import           Stack.Types.Dependency
                   ( DepLibrary (..), DepType (..), DepValue (..)
                   , cabalSetupDepsToStackDep, libraryDepFromVersionRange
                   )
import           Stack.Types.EnvConfig ( HasEnvConfig )
import           Stack.Types.Installed
                   ( InstallMap, Installed (..), InstalledMap
                   , installedToPackageIdOpt
                   )
import           Stack.Types.NamedComponent
                   ( NamedComponent (..), isPotentialDependency
                   , subLibComponents
                   )
import           Stack.Types.Package
                   ( BioInput(..), BuildInfoOpts (..), Package (..)
                   , PackageConfig (..), PackageException (..)
                   , dotCabalCFilePath, packageIdentifier
                   )
import           Stack.Types.PackageFile
                   ( DotCabalPath, PackageComponentFile (..) )
import           Stack.Types.SourceMap ( Target(..), PackageType (..) )
import           Stack.Types.Version
                   ( VersionRange, intersectVersionRanges, withinRange )
import           System.FilePath ( replaceExtension )
import           RIO.Seq ((|>))

-- | Read @<package>.buildinfo@ ancillary files produced by some Setup.hs hooks.
-- The file includes Cabal file syntax to be merged into the package description
-- derived from the package's Cabal file.
--
-- NOTE: not to be confused with BuildInfo, an Stack-internal datatype.
readDotBuildinfo :: MonadIO m => Path Abs File -> m HookedBuildInfo
readDotBuildinfo =
  liftIO . readHookedBuildInfo silent Nothing . makeSymbolicPath . toFilePath

-- | Resolve a parsed Cabal file into a 'Package', which contains all of the
-- info needed for Stack to build the 'Package' given the current configuration.
resolvePackage :: PackageConfig -> GenericPackageDescription -> Package
resolvePackage packageConfig gpkg =
  packageFromPackageDescription
    packageConfig
    (genPackageFlags gpkg)
    (resolvePackageDescription packageConfig gpkg)

packageFromPackageDescription ::
     PackageConfig
  -> [PackageFlag]
  -> PackageDescription
  -> Package
packageFromPackageDescription
    packageConfig
    pkgFlags
    pkg
  = Package
      { name = name
      , version = pkgVersion pkgId
      , license = licenseRaw pkg
      , ghcOptions =  packageConfig.ghcOptions
      , cabalConfigOpts =  packageConfig.cabalConfigOpts
      , flags = packageConfig.flags
      , defaultFlags = M.fromList
          [(flagName flag, flagDefault flag) | flag <- pkgFlags]
      , library = stackLibraryFromCabal <$> library pkg
      , subLibraries =
          foldAndMakeCollection stackLibraryFromCabal $ subLibraries pkg
      , foreignLibraries =
          foldAndMakeCollection stackForeignLibraryFromCabal $ foreignLibs pkg
      , testSuites =
          foldAndMakeCollection stackTestFromCabal $ testSuites pkg
      , benchmarks =
          foldAndMakeCollection stackBenchmarkFromCabal $ benchmarks pkg
      , executables =
          foldAndMakeCollection stackExecutableFromCabal $ executables pkg
      , buildType = buildType pkg
      , setupDeps = fmap cabalSetupDepsToStackDep (setupBuildInfo pkg)
      , cabalSpec = specVersion pkg
      , file = stackPackageFileFromCabal pkg
      , testEnabled =  packageConfig.enableTests
      , benchmarkEnabled = packageConfig.enableBenchmarks
      }
 where
  -- Gets all of the modules, files, build files, and data files that constitute
  -- the package. This is primarily used for dirtiness checking during build, as
  -- well as use by "stack ghci"
  pkgId = package pkg
  name = pkgName pkgId

-- | This is an action used to collect info needed for "stack ghci". This info
-- isn't usually needed, so computation of it is deferred.
getPackageOpts ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m, MonadUnliftIO m )
  => Package
  -> InstallMap
  -> InstalledMap
  -> [PackageName]
  -> [PackageName]
  -> Path Abs File
  -> m ( Map NamedComponent (Map ModuleName (Path Abs File))
       , Map NamedComponent [DotCabalPath]
       , Map NamedComponent BuildInfoOpts
       )
getPackageOpts
    stackPackage
    installMap
    installedMap
    omitPkgs
    addPkgs
    cabalFP
  = do
      PackageComponentFile !componentsModules componentFiles _ _ <-
        getPackageFile stackPackage cabalFP
      let subLibs =
            S.toList $ subLibComponents $ M.keysSet componentsModules
          excludedSubLibs =
            map (unqualComponentNameToPackageName . toCabalName) subLibs
      componentsOpts <- generatePkgDescOpts
        installMap
        installedMap
        (excludedSubLibs ++ omitPkgs)
        addPkgs
        cabalFP
        stackPackage
        componentFiles
      pure (componentsModules, componentFiles, componentsOpts)

-- | Generate GHC options for the package's components, and a list of options
-- which apply generally to the package, not one specific component.
generatePkgDescOpts ::
     (HasEnvConfig env, MonadThrow m, MonadReader env m, MonadIO m)
  => InstallMap
  -> InstalledMap
  -> [PackageName]
     -- ^ Packages to omit from the "-package" / "-package-id" flags
  -> [PackageName]
     -- ^ Packages to add to the "-package" flags
  -> Path Abs File
  -> Package
  -> Map NamedComponent [DotCabalPath]
  -> m (Map NamedComponent BuildInfoOpts)
generatePkgDescOpts
    installMap
    installedMap
    omitPackages
    addPackages
    cabalFP
    pkg
    componentPaths
  = do
      config <- view configL
      cabalVersion <- view cabalVersionL
      distDir <- distDirFromDir cabalDir
      let generate componentName buildInfo = generateBuildInfoOpts BioInput
            { installMap
            , installedMap
            , cabalDir
            , distDir
            , omitPackages
            , addPackages
            , buildInfo
            , dotCabalPaths =
                fromMaybe [] (M.lookup componentName componentPaths)
            , configLibDirs = config.extraLibDirs
            , configIncludeDirs = config.extraIncludeDirs
            , componentName
            , cabalVersion
            }
      let insertInMap name compVal = M.insert name (generate name compVal)
      let translatedInsertInMap constructor name =
            insertInMap (constructor name)
      let makeBuildInfoOpts selector constructor =
            foldOnNameAndBuildInfo
              (selector pkg)
              (translatedInsertInMap constructor)
      let aggregateAllBuildInfoOpts =
              makeBuildInfoOpts (.library) (const CLib)
            . makeBuildInfoOpts (.subLibraries) CSubLib
            . makeBuildInfoOpts (.executables) CExe
            . makeBuildInfoOpts (.benchmarks) CBench
            . makeBuildInfoOpts (.testSuites) CTest
      pure $ aggregateAllBuildInfoOpts mempty
 where
  cabalDir = parent cabalFP

-- | Generate GHC options for the target. Since Cabal also figures out these
-- options, currently this is only used for invoking GHCI (via stack ghci).
generateBuildInfoOpts :: BioInput -> BuildInfoOpts
generateBuildInfoOpts bi =
  BuildInfoOpts
    { opts =
           ghcOpts
        ++ fmap ("-optP" <>) bi.buildInfo.cppOptions
    -- NOTE for future changes: Due to this use of nubOrd (and other uses
    -- downstream), these generated options must not rely on multiple
    -- argument sequences.  For example, ["--main-is", "Foo.hs", "--main-
    -- is", "Bar.hs"] would potentially break due to the duplicate
    -- "--main-is" being removed.
    --
    -- See https://github.com/commercialhaskell/stack/issues/1255
    , oneWordOpts = nubOrd $ concat
        [extOpts, srcOpts, includeOpts, libOpts, fworks, cObjectFiles]
    , packageFlags = deps
    , cabalMacros = componentAutogen </> relFileCabalMacrosH
    }
 where
  cObjectFiles = mapMaybe
    ( fmap toFilePath
    . makeObjectFilePathFromC bi.cabalDir bi.componentName bi.distDir
    )
    cfiles
  cfiles = mapMaybe dotCabalCFilePath bi.dotCabalPaths
  installVersion = snd
  -- Generates: -package=base -package=base16-bytestring-0.1.1.6 ...
  deps =
    concat
      [ case M.lookup name bi.installedMap of
          Just (_, Stack.Types.Installed.Library _ident installedInfo) ->
            installedToPackageIdOpt installedInfo
          _ -> ["-package=" <> packageNameString name <>
            maybe "" -- This empty case applies to e.g. base.
              ((("-" <>) . versionString) . installVersion)
              (M.lookup name bi.installMap)]
      | name <- pkgs
      ]
  pkgs =
    bi.addPackages ++
    [ name
    | Dependency name _ _ <- bi.buildInfo.targetBuildDepends
      -- TODO: Cabal 3.0 introduced multiple public libraries in a single
      -- dependency
    , name `notElem` bi.omitPackages
    ]
  PerCompilerFlavor ghcOpts _ = bi.buildInfo.options
  extOpts =
       map (("-X" ++) . display) bi.buildInfo.allLanguages
    <> map (("-X" ++) . display) bi.buildInfo.usedExtensions
  srcOpts =
    map (("-i" <>) . toFilePathNoTrailingSep)
      (concat
        [ [ componentBuildDir bi.componentName bi.distDir ]
        , [ bi.cabalDir
          | null bi.buildInfo.hsSourceDirs
          ]
        , mapMaybe
            (toIncludeDir . getSymbolicPath)
            bi.buildInfo.hsSourceDirs
        , [ componentAutogen ]
        , [ packageAutogenDir bi.distDir ]
        , [ componentOutputDir bi.componentName bi.distDir ]
        ]) ++
    [ "-stubdir=" ++ toFilePathNoTrailingSep (buildDir bi.distDir) ]
  componentAutogen = componentAutogenDir bi.componentName bi.distDir
  toIncludeDir "." = Just bi.cabalDir
  toIncludeDir relDir = concatAndCollapseAbsDir bi.cabalDir relDir
  includeOpts =
    map ("-I" <>) (bi.configIncludeDirs <> pkgIncludeOpts)
  pkgIncludeOpts =
    [ toFilePathNoTrailingSep absDir
    | dir <- bi.buildInfo.includeDirs
    , absDir <- handleDir dir
    ]
  libOpts =
    map ("-l" <>) bi.buildInfo.extraLibs <>
    map ("-L" <>) (bi.configLibDirs <> pkgLibDirs)
  pkgLibDirs =
    [ toFilePathNoTrailingSep absDir
    | dir <- bi.buildInfo.extraLibDirs
    , absDir <- handleDir dir
    ]
  handleDir dir = case (parseAbsDir dir, parseRelDir dir) of
    (Just ab, _       ) -> [ab]
    (_      , Just rel) -> [bi.cabalDir </> rel]
    (Nothing, Nothing ) -> []
  fworks = map ("-framework=" <>) bi.buildInfo.frameworks

-- | Make the .o path from the .c file path for a component. Example:
--
-- @
-- executable FOO
--   c-sources:        cbits/text_search.c
-- @
--
-- Produces
--
-- <dist-dir>/build/FOO/FOO-tmp/cbits/text_search.o
--
-- Example:
--
-- λ> makeObjectFilePathFromC
--     $(mkAbsDir "/Users/chris/Repos/hoogle")
--     CLib
--     $(mkAbsDir "/Users/chris/Repos/hoogle/.stack-work/Cabal-x.x.x/dist")
--     $(mkAbsFile "/Users/chris/Repos/hoogle/cbits/text_search.c")
-- Just "/Users/chris/Repos/hoogle/.stack-work/Cabal-x.x.x/dist/build/cbits/text_search.o"
-- λ> makeObjectFilePathFromC
--     $(mkAbsDir "/Users/chris/Repos/hoogle")
--     (CExe "hoogle")
--     $(mkAbsDir "/Users/chris/Repos/hoogle/.stack-work/Cabal-x.x.x/dist")
--     $(mkAbsFile "/Users/chris/Repos/hoogle/cbits/text_search.c")
-- Just "/Users/chris/Repos/hoogle/.stack-work/Cabal-x.x.x/dist/build/hoogle/hoogle-tmp/cbits/text_search.o"
-- λ>
makeObjectFilePathFromC ::
     MonadThrow m
  => Path Abs Dir      -- ^ The cabal directory.
  -> NamedComponent    -- ^ The name of the component.
  -> Path Abs Dir      -- ^ Dist directory.
  -> Path Abs File     -- ^ The path to the .c file.
  -> m (Path Abs File) -- ^ The path to the .o file for the component.
makeObjectFilePathFromC cabalDir namedComponent distDir cFilePath = do
  relCFilePath <- stripProperPrefix cabalDir cFilePath
  relOFilePath <-
    parseRelFile (replaceExtension (toFilePath relCFilePath) "o")
  pure (componentOutputDir namedComponent distDir </> relOFilePath)

-- | Get all dependencies of the package (buildable targets only).
packageDependencies ::
     PackageDescription
  -> Map PackageName VersionRange
packageDependencies pkg =
  M.fromListWith intersectVersionRanges $
    map (depPkgName &&& depVerRange) $
         concatMap targetBuildDepends (allBuildInfo' pkg)
      <> maybe [] setupDepends (setupBuildInfo pkg)

-- | Variant of 'allBuildInfo' from Cabal that, like versions before Cabal 2.2
-- only includes buildable components.
allBuildInfo' :: PackageDescription -> [BuildInfo]
allBuildInfo' pkg_descr = [ bi | lib <- allLibraries pkg_descr
                               , let bi = libBuildInfo lib
                               , buildable bi ]
                       ++ [ bi | flib <- foreignLibs pkg_descr
                               , let bi = foreignLibBuildInfo flib
                               , buildable bi ]
                       ++ [ bi | exe <- executables pkg_descr
                               , let bi = buildInfo exe
                               , buildable bi ]
                       ++ [ bi | tst <- testSuites pkg_descr
                               , let bi = testBuildInfo tst
                               , buildable bi ]
                       ++ [ bi | tst <- benchmarks pkg_descr
                               , let bi = benchmarkBuildInfo tst
                               , buildable bi ]

-- | Evaluates the conditions of a 'GenericPackageDescription', yielding
-- a resolved 'PackageDescription'.
resolvePackageDescription ::
     PackageConfig
  -> GenericPackageDescription
  -> PackageDescription
resolvePackageDescription
    packageConfig
    ( GenericPackageDescription
        desc _ defaultFlags mlib subLibs foreignLibs' exes tests benches
    )
  = desc
      { library = fmap (resolveConditions rc updateLibDeps) mlib
      , subLibraries = map
          ( \(n, v) ->
              (resolveConditions rc updateLibDeps v){libName = LSubLibName n}
          )
          subLibs
      , foreignLibs = map
          ( \(n, v) ->
              (resolveConditions rc updateForeignLibDeps v){foreignLibName = n}
          )
          foreignLibs'
      , executables = map
          ( \(n, v) -> (resolveConditions rc updateExeDeps v){exeName = n} )
          exes
      , testSuites = map
          ( \(n, v) ->
              (resolveConditions rc updateTestDeps v){testName = n}
          )
          tests
      , benchmarks = map
          ( \(n, v) ->
              (resolveConditions rc updateBenchmarkDeps v){benchmarkName = n}
          )
          benches
      }
 where
  flags = M.union packageConfig.flags (flagMap defaultFlags)
  rc = mkResolveConditions
         packageConfig.compilerVersion
         packageConfig.platform
         flags
  updateLibDeps lib deps = lib
    { libBuildInfo = (libBuildInfo lib) {targetBuildDepends = deps} }
  updateForeignLibDeps lib deps = lib
    { foreignLibBuildInfo =
        (foreignLibBuildInfo lib) {targetBuildDepends = deps}
    }
  updateExeDeps exe deps = exe
    { Executable.buildInfo = (buildInfo exe) {targetBuildDepends = deps} }
  updateTestDeps test deps = test
    { testBuildInfo = (testBuildInfo test) {targetBuildDepends = deps} }
  updateBenchmarkDeps bench deps = bench
    { benchmarkBuildInfo =
        (benchmarkBuildInfo bench) {targetBuildDepends = deps}
    }

-- | Make a map from a list of flag specifications.
--
-- What is @flagManual@ for?
flagMap :: [PackageFlag] -> Map FlagName Bool
flagMap = M.fromList . map pair
 where
  pair :: PackageFlag -> (FlagName, Bool)
  pair = flagName &&& flagDefault

data ResolveConditions = ResolveConditions
  { flags :: Map FlagName Bool
  , compilerVersion :: ActualCompiler
  , os :: OS
  , arch :: Arch
  }

-- | Generic a @ResolveConditions@ using sensible defaults.
mkResolveConditions ::
     ActualCompiler -- ^ Compiler version
  -> Platform -- ^ installation target platform
  -> Map FlagName Bool -- ^ enabled flags
  -> ResolveConditions
mkResolveConditions compilerVersion (Platform arch os) flags = ResolveConditions
  { flags
  , compilerVersion
  , os
  , arch
  }

-- | Resolve the condition tree for the library.
resolveConditions ::
     (Semigroup target, Monoid target, Show target)
  => ResolveConditions
  -> (target -> cs -> target)
  -> CondTree ConfVar cs target
  -> target
resolveConditions rc addDeps (CondNode lib deps cs) = basic <> children
 where
  basic = addDeps lib deps
  children = mconcat (map apply cs)
   where
    apply (Cabal.CondBranch cond node mcs) =
      if condSatisfied cond
         then resolveConditions rc addDeps node
         else maybe mempty (resolveConditions rc addDeps) mcs
    condSatisfied c =
      case c of
        Var v -> varSatisfied v
        Lit b -> b
        CNot c' -> not (condSatisfied c')
        COr cx cy -> condSatisfied cx || condSatisfied cy
        CAnd cx cy -> condSatisfied cx && condSatisfied cy
    varSatisfied v =
      case v of
        OS os -> os == rc.os
        Arch arch -> arch == rc.arch
        PackageFlag flag -> fromMaybe False $ M.lookup flag rc.flags
        -- NOTE:  ^^^^^ This should never happen, as all flags which are used
        -- must be declared. Defaulting to False.
        Impl flavor range ->
          case (flavor, rc.compilerVersion) of
            (GHC, ACGhc vghc) -> vghc `withinRange` range
            _ -> False

-- | Path for the package's build log.
buildLogPath ::
     (MonadReader env m, HasBuildConfig env, MonadThrow m)
  => Package
  -> Maybe String
  -> m (Path Abs File)
buildLogPath package' msuffix = do
  env <- ask
  let workDir = getWorkDir env
  fp <- parseRelFile $ concat $
    packageIdentifierString (packageIdentifier package') :
    maybe id (\suffix -> ("-" :) . (suffix :)) msuffix [".log"]
  pure $ workDir </> relDirLogs </> fp

    {- FIXME
-- | Create a 'ProjectPackage' from a directory containing a package.
mkProjectPackage
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PrintWarnings
  -> ResolvedPath Dir
  -> RIO env ProjectPackage
mkProjectPackage printWarnings dir = do
  (gpd, name, cabalfp) <- loadCabalFilePath (resolvedAbsolute dir)
  pure ProjectPackage
    { ppCabalFP = cabalfp
    , ppGPD' = gpd printWarnings
    , ppResolvedDir = dir
    , ppName = name
    }

-- | Create a 'DepPackage' from a 'PackageLocation'
mkDepPackage
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocation
  -> RIO env DepPackage
mkDepPackage pl = do
  (name, gpdio) <-
    case pl of
      PLMutable dir -> do
        (gpdio, name, _cabalfp) <- loadCabalFilePath (resolvedAbsolute dir)
        pure (name, gpdio NoPrintWarnings)
      PLImmutable pli -> do
        PackageIdentifier name _ <- getPackageLocationIdent pli
        run <- askRunInIO
        pure (name, run $ loadCabalFileImmutable pli)
  pure DepPackage
    { dpGPD' = gpdio
    , dpLocation = pl
    , dpName = name
    }

    -}

-- | Force a package to be treated as a custom build type, see
-- <https://github.com/commercialhaskell/stack/issues/4488>
applyForceCustomBuild ::
     Version -- ^ global Cabal version
  -> Package
  -> Package
applyForceCustomBuild cabalVersion package
  | forceCustomBuild =
      package
        { buildType = Custom
        , setupDeps = Just $ M.fromList
            [ ("Cabal", libraryDepFromVersionRange cabalVersionRange)
            , ("base", libraryDepFromVersionRange anyVersion)
            ]
        }
  | otherwise = package
 where
  cabalVersionRange =
    orLaterVersion $ mkVersion $ cabalSpecToVersionDigits
      package.cabalSpec
  forceCustomBuild = package.buildType == Simple
    && not (cabalVersion `withinRange` cabalVersionRange)

-- | Check if the package has a main library that is buildable.
hasBuildableMainLibrary :: Package -> Bool
hasBuildableMainLibrary package =
  maybe False isComponentBuildable package.library

-- | Check if the main library has any exposed modules.
--
-- This should become irrelevant at some point since there's nothing inherently
-- wrong or different with packages exposing only modules in internal libraries
-- (for instance).
mainLibraryHasExposedModules :: Package -> Bool
mainLibraryHasExposedModules package =
  maybe False (not . null . (.exposedModules)) package.library

-- | Aggregate all unknown tools from all components. Mostly meant for
-- build tools specified in the legacy manner (build-tools:) that failed the
-- hard-coded lookup. See 'Stack.Types.Component.unknownTools' for more
-- information.
packageUnknownTools :: Package -> Set Text
packageUnknownTools pkg = lib (bench <> tests <> flib <> sublib <> exe)
 where
  lib setT = case pkg.library of
    Just libV -> addUnknownTools libV setT
    Nothing -> setT
  bench = gatherUnknownTools pkg.benchmarks
  tests = gatherUnknownTools pkg.testSuites
  flib = gatherUnknownTools pkg.foreignLibraries
  sublib = gatherUnknownTools pkg.subLibraries
  exe = gatherUnknownTools pkg.executables
  addUnknownTools :: HasBuildInfo x => x -> Set Text -> Set Text
  addUnknownTools = (<>) . (.buildInfo.unknownTools)
  gatherUnknownTools :: HasBuildInfo x => CompCollection x -> Set Text
  gatherUnknownTools = foldr' addUnknownTools mempty

buildableForeignLibs :: Package -> Set StackUnqualCompName
buildableForeignLibs pkg = getBuildableSet pkg.foreignLibraries

buildableSubLibs :: Package -> Set StackUnqualCompName
buildableSubLibs pkg = getBuildableSet pkg.subLibraries

buildableExes :: Package -> Set StackUnqualCompName
buildableExes pkg = getBuildableSet pkg.executables

buildableTestSuites :: Package -> Set StackUnqualCompName
buildableTestSuites pkg = getBuildableSet pkg.testSuites

buildableBenchmarks :: Package -> Set StackUnqualCompName
buildableBenchmarks pkg = getBuildableSet pkg.benchmarks

-- | Apply a generic processing function in a Monad over all of the Package's
-- components.
processPackageComponent ::
     forall m a. (Monad m)
  => Package
  -> (forall component. HasComponentInfo component => component -> m a -> m a)
     -- ^ Processing function with all the component's info.
  -> m a
     -- ^ Initial value.
  -> m a
processPackageComponent pkg componentFn = do
  let componentKindProcessor ::
           forall component. HasComponentInfo component
        => (Package -> CompCollection component)
        -> m a
        -> m a
      componentKindProcessor target =
        foldComponentToAnotherCollection
          (target pkg)
          componentFn
      processMainLib = maybe id componentFn pkg.library
      processAllComp =
        ( if pkg.benchmarkEnabled
            then componentKindProcessor (.benchmarks)
            else id
        )
        . ( if pkg.testEnabled
              then componentKindProcessor (.testSuites)
              else id
          )
        . componentKindProcessor (.foreignLibraries)
        . componentKindProcessor (.executables)
        . componentKindProcessor (.subLibraries)
        . processMainLib
  processAllComp

-- | This is a function to iterate in a monad over all of a package's
-- dependencies, and yield a collection of results (used with list and set).
processPackageMapDeps ::
     (Monad m)
  => Package
  -> (Map PackageName DepValue -> m a -> m a)
  -> m a
  -> m a
processPackageMapDeps pkg fn = do
  let packageSetupDepsProcessor resAction = case pkg.setupDeps of
        Nothing -> resAction
        Just v -> fn v resAction
      processAllComp = processPackageComponent pkg (fn . componentDependencyMap)
        . packageSetupDepsProcessor
  processAllComp

-- | This is a function to iterate in a monad over all of a package component's
-- dependencies, and yield a collection of results.
processPackageDeps ::
     (Monad m)
  => Package
  -> (smallResT -> resT -> resT)
  -> (PackageName -> DepValue -> m smallResT)
  -> m resT
  -> m resT
processPackageDeps pkg combineResults fn = do
  let asPackageNameSet accessor =
        S.map (mkPackageName . T.unpack) $ getBuildableSetText $ accessor pkg
      (!subLibNames, !foreignLibNames) =
        ( asPackageNameSet (.subLibraries)
        , asPackageNameSet (.foreignLibraries)
        )
      shouldIgnoreDep (packageNameV :: PackageName)
        | packageNameV == pkg.name = True
        | packageNameV `S.member` subLibNames = True
        | packageNameV `S.member` foreignLibNames = True
        | otherwise = False
      innerIterator packageName depValue resListInMonad
        | shouldIgnoreDep packageName = resListInMonad
        | otherwise = do
            resList <- resListInMonad
            newResElement <- fn packageName depValue
            pure $ combineResults newResElement resList
  processPackageMapDeps pkg (flip (M.foldrWithKey' innerIterator))

-- | Iterate/fold on all the package dependencies, components, setup deps and
-- all.
processPackageDepsToList ::
     Monad m
  => Package
  -> (PackageName -> DepValue -> m resT)
  -> m [resT]
processPackageDepsToList pkg fn = processPackageDeps pkg (:) fn (pure [])

-- | Iterate/fold on all the package dependencies, components, setup deps and
-- all.
processPackageDepsEither ::
     (Monad m, Monoid a, Monoid b)
  => Package
  -> (PackageName -> DepValue -> m (Either a b))
  -> m (Either a b)
processPackageDepsEither pkg fn =
  processPackageDeps pkg combineRes fn (pure (Right mempty))
 where
  combineRes (Left err) (Left errs) = Left (errs <> err)
  combineRes _ (Left b) = Left b
  combineRes (Left err) _ = Left err
  combineRes (Right a) (Right b) = Right $ a <> b

-- | List all package's dependencies in a "free" context through the identity
-- monad.
listOfPackageDeps :: Package -> [PackageName]
listOfPackageDeps pkg =
  runIdentity $ processPackageDepsToList pkg (\pn _ -> pure pn)

-- | The set of package's dependencies.
setOfPackageDeps :: Package -> Set PackageName
setOfPackageDeps pkg =
  runIdentity $ processPackageDeps pkg S.insert (\pn _ -> pure pn) (pure mempty)

-- | This implements a topological sort on all targeted components for the build
-- and their dependencies. It's only targeting internal dependencies, so it's
-- doing a topological sort on a subset of a package's components.
--
-- Note that in Cabal they use the Data.Graph struct to pursue the same goal.
-- But dong this here would require a large number intermediate data structure.
-- This is needed because we need to get the right GhcPkgId of the relevant
-- internal dependencies of a component before building it as a component.
topSortPackageComponent ::
     Package
  -> Target
  -> Bool
     -- ^ Include directTarget or not. False here means we won't include the
     -- actual targets in the result, only their deps. Using it with False here
     -- only in GHCi
  -> Seq NamedComponent
topSortPackageComponent package target includeDirectTarget =
  topProcessPackageComponent package target processor mempty
 where
  processor packageType component
    | not includeDirectTarget && packageType == PTProject = id
    | otherwise = \v -> v |> component.qualifiedName

-- | Process a package's internal components in the order of their topological sort.
-- The first iteration will effect the component depending on no other component etc,
-- iterating by increasing amount of required dependencies.
-- 'PackageType' with value 'PTProject' here means the component is a direct target
-- and 'PTDependency' means it's a dependency of a direct target.
topProcessPackageComponent ::
     forall b.
     Package
  -> Target
  -> (    forall component. (HasComponentInfo component)
       => PackageType
       -> component
       -> b
       -> b
     )
  -> b
  -> b
topProcessPackageComponent package target fn res = do
  let initialState = (mempty, res)
      processInitialComponents c = case target of
        TargetAll{} -> processComponent PTProject c
        TargetComps targetSet -> if S.member c.qualifiedName targetSet
          then processComponent PTProject c
          else id
  snd $ processPackageComponent package processInitialComponents initialState
 where
  processComponent :: HasComponentInfo component
    => PackageType
       -- ^ Finally add this component in the seq
    -> component
    -> (Set NamedComponent, b)
    -> (Set NamedComponent, b)
  processComponent packageType component currentRes@(_a, !_b) = do
    let depMap = componentDependencyMap component
        internalDep = M.lookup package.name depMap
        qualName = component.qualifiedName
        alreadyProcessed = fst currentRes
        !appendToResult = fn packageType component
        -- This is an optimization, the only components we are likely to process
        -- multiple times are the ones we can find in dependencies, otherwise we
        -- only fold on a single version of each component by design.
        processedDeps = processOneDep internalDep currentRes
    if isPotentialDependency qualName
      then
        if S.member qualName alreadyProcessed
          then currentRes
          else bimap (S.insert qualName) appendToResult processedDeps
      else second appendToResult processedDeps
  lookupLibName isMain name = if isMain
    then package.library
    else collectionLookup name package.subLibraries
  processOneDep mDependency res' =
    case (.depType) <$> mDependency of
      Just (AsLibrary (DepLibrary mainLibDep subLibDeps)) -> do
        let processMainLibDep =
              case (mainLibDep, lookupLibName True emptyCompName) of
                (True, Just mainLib) ->
                  processComponent PTDependency mainLib
                _ -> id
            processSingleSubLib name =
              case lookupLibName False name of
                Just lib -> processComponent PTDependency lib
                Nothing -> id
            processSubLibDep r = foldr' processSingleSubLib r subLibDeps
        processSubLibDep (processMainLibDep res')
      _ -> res'
