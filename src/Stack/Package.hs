{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
  , processPackageDepsToList
  , listOfPackageDeps
  , setOfPackageDeps
  , topSortPackageComponent
  ) where

import           Data.Foldable ( Foldable (..) )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.STRef ( STRef, modifySTRef', readSTRef, newSTRef )
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
                   )
import           Distribution.Simple.PackageDescription ( readHookedBuildInfo )
import           Distribution.System ( OS (..), Arch, Platform (..) )
import           Distribution.Text ( display )
import qualified Distribution.Types.CondTree as Cabal
import           Distribution.Utils.Path ( getSymbolicPath )
import           Distribution.Verbosity ( silent )
import           Distribution.Version
                   ( anyVersion, mkVersion, orLaterVersion )
import           GHC.Records ( getField )
import           Path
                   ( (</>), parent, parseAbsDir, parseRelDir, parseRelFile
                   , stripProperPrefix
                   )
import           Path.Extra ( concatAndCollapseAbsDir, toFilePathNoTrailingSep )
import           Stack.Component
                   ( foldOnNameAndBuildInfo, isComponentBuildable
                   , stackBenchmarkFromCabal
                   , stackExecutableFromCabal, stackForeignLibraryFromCabal
                   , stackLibraryFromCabal, stackTestFromCabal
                   , stackUnqualToQual, componentDependencyMap
                   )
import           Stack.ComponentFile
                   ( buildDir, componentAutogenDir, componentBuildDir
                   , componentOutputDir, packageAutogenDir
                   )
import           Stack.Constants (relFileCabalMacrosH, relDirLogs)
import           Stack.Constants.Config ( distDirFromDir )
import           Stack.PackageFile ( getPackageFile, stackPackageFileFromCabal )
import           Stack.Prelude hiding ( Display (..) )
import           Stack.Types.BuildConfig
                   ( HasBuildConfig (..), getProjectWorkDir )
import           Stack.Types.CompCollection
                   ( CompCollection, foldAndMakeCollection
                   , foldComponentToAnotherCollection, getBuildableSetText, collectionLookup
                   )
import           Stack.Types.Compiler ( ActualCompiler (..) )
import           Stack.Types.CompilerPaths ( cabalVersionL )
import           Stack.Types.Component ( HasBuildInfo, HasComponentInfo, StackUnqualCompName (unqualCompToText) )
import qualified Stack.Types.Component as Component
import           Stack.Types.Config ( Config (..), HasConfig (..) )
import           Stack.Types.Dependency
                   ( DepValue (..), cabalSetupDepsToStackDep
                   , libraryDepFromVersionRange, DepType (..), DepLibrary (..)
                   )
import           Stack.Types.EnvConfig ( HasEnvConfig )
import           Stack.Types.Installed
                   ( InstallMap, Installed (..), InstalledMap
                   , installedToPackageIdOpt
                   )
import           Stack.Types.NamedComponent
                   ( NamedComponent (..), subLibComponents, isPotentialDependency )
import           Stack.Types.Package
                   ( BioInput(..), BuildInfoOpts (..), Package (..)
                   , PackageConfig (..), PackageException (..)
                   , dotCabalCFilePath, packageIdentifier
                   )
import           Stack.Types.PackageFile
                   ( DotCabalPath, PackageComponentFile (..) )
import           Stack.Types.SourceMap (Target(..))
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
readDotBuildinfo buildinfofp =
  liftIO $ readHookedBuildInfo silent (toFilePath buildinfofp)

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
      { packageName = name
      , packageVersion = pkgVersion pkgId
      , packageLicense = licenseRaw pkg
      , packageGhcOptions = packageConfigGhcOptions packageConfig
      , packageCabalConfigOpts = packageConfigCabalConfigOpts packageConfig
      , packageFlags = packageConfigFlags packageConfig
      , packageDefaultFlags = M.fromList
          [(flagName flag, flagDefault flag) | flag <- pkgFlags]
      , packageLibrary = stackLibraryFromCabal <$> library pkg
      , packageSubLibraries =
          foldAndMakeCollection stackLibraryFromCabal $ subLibraries pkg
      , packageForeignLibraries =
          foldAndMakeCollection stackForeignLibraryFromCabal $ foreignLibs pkg
      , packageTestSuites =
          foldAndMakeCollection stackTestFromCabal $ testSuites pkg
      , packageBenchmarks =
          foldAndMakeCollection stackBenchmarkFromCabal $ benchmarks pkg
      , packageExecutables =
          foldAndMakeCollection stackExecutableFromCabal $ executables pkg
      , packageBuildType = buildType pkg
      , packageSetupDeps = fmap cabalSetupDepsToStackDep (setupBuildInfo pkg)
      , packageCabalSpec = specVersion pkg
      , packageFile = stackPackageFileFromCabal pkg
      , packageTestEnabled = packageConfigEnableTests packageConfig
      , packageBenchmarkEnabled = packageConfigEnableBenchmarks packageConfig
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
    cabalfp
  = do
      PackageComponentFile !componentsModules componentFiles _ _ <-
        getPackageFile stackPackage cabalfp
      let subLibs =
            S.toList $ subLibComponents $ M.keysSet componentsModules
      excludedSubLibs <- mapM (parsePackageNameThrowing . T.unpack) subLibs
      componentsOpts <- generatePkgDescOpts
        installMap
        installedMap
        (excludedSubLibs ++ omitPkgs)
        addPkgs
        cabalfp
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
    omitPkgs
    addPkgs
    cabalfp
    pkg
    componentPaths
  = do
      config <- view configL
      cabalVer <- view cabalVersionL
      distDir <- distDirFromDir cabalDir
      let generate namedComponent binfo = generateBuildInfoOpts BioInput
            { biInstallMap = installMap
            , biInstalledMap = installedMap
            , biCabalDir = cabalDir
            , biDistDir = distDir
            , biOmitPackages = omitPkgs
            , biAddPackages = addPkgs
            , biBuildInfo = binfo
            , biDotCabalPaths =
                fromMaybe [] (M.lookup namedComponent componentPaths)
            , biConfigLibDirs = configExtraLibDirs config
            , biConfigIncludeDirs = configExtraIncludeDirs config
            , biComponentName = namedComponent
            , biCabalVersion = cabalVer
            }
      let insertInMap name compVal = M.insert name (generate name compVal)
      let translatedInsertInMap constructor name =
            insertInMap (stackUnqualToQual constructor name)
      let makeBuildInfoOpts selector constructor =
            foldOnNameAndBuildInfo
              (selector pkg)
              (translatedInsertInMap constructor)
      let aggregateAllBuildInfoOpts =
              makeBuildInfoOpts packageLibrary (const CLib)
            . makeBuildInfoOpts packageSubLibraries CSubLib
            . makeBuildInfoOpts packageExecutables CExe
            . makeBuildInfoOpts packageBenchmarks CBench
            . makeBuildInfoOpts packageTestSuites CTest
      pure $ aggregateAllBuildInfoOpts mempty
 where
  cabalDir = parent cabalfp

-- | Generate GHC options for the target. Since Cabal also figures out these
-- options, currently this is only used for invoking GHCI (via stack ghci).
generateBuildInfoOpts :: BioInput -> BuildInfoOpts
generateBuildInfoOpts BioInput {..} =
  BuildInfoOpts
    { bioOpts = ghcOpts ++ fmap ("-optP" <>) (Component.cppOptions biBuildInfo)
    -- NOTE for future changes: Due to this use of nubOrd (and other uses
    -- downstream), these generated options must not rely on multiple
    -- argument sequences.  For example, ["--main-is", "Foo.hs", "--main-
    -- is", "Bar.hs"] would potentially break due to the duplicate
    -- "--main-is" being removed.
    --
    -- See https://github.com/commercialhaskell/stack/issues/1255
    , bioOneWordOpts = nubOrd $ concat
        [extOpts, srcOpts, includeOpts, libOpts, fworks, cObjectFiles]
    , bioPackageFlags = deps
    , bioCabalMacros = componentAutogen </> relFileCabalMacrosH
    }
 where
  cObjectFiles =
    mapMaybe (fmap toFilePath .
              makeObjectFilePathFromC biCabalDir biComponentName biDistDir)
             cfiles
  cfiles = mapMaybe dotCabalCFilePath biDotCabalPaths
  installVersion = snd
  -- Generates: -package=base -package=base16-bytestring-0.1.1.6 ...
  deps =
    concat
      [ case M.lookup name biInstalledMap of
          Just (_, Stack.Types.Installed.Library _ident installedInfo) ->
            installedToPackageIdOpt installedInfo
          _ -> ["-package=" <> packageNameString name <>
            maybe "" -- This empty case applies to e.g. base.
              ((("-" <>) . versionString) . installVersion)
              (M.lookup name biInstallMap)]
      | name <- pkgs
      ]
  pkgs =
    biAddPackages ++
    [ name
    | Dependency name _ _ <- Component.targetBuildDepends biBuildInfo
      -- TODO: Cabal 3.0 introduced multiple public libraries in a single
      -- dependency
    , name `notElem` biOmitPackages
    ]
  PerCompilerFlavor ghcOpts _ = Component.options biBuildInfo
  extOpts =
       map (("-X" ++) . display) (Component.allLanguages biBuildInfo)
    <> map (("-X" ++) . display) (Component.usedExtensions biBuildInfo)
  srcOpts =
    map (("-i" <>) . toFilePathNoTrailingSep)
      (concat
        [ [ componentBuildDir biCabalVersion biComponentName biDistDir ]
        , [ biCabalDir
          | null (Component.hsSourceDirs biBuildInfo)
          ]
        , mapMaybe
            (toIncludeDir . getSymbolicPath)
            (Component.hsSourceDirs biBuildInfo)
        , [ componentAutogen ]
        , maybeToList (packageAutogenDir biCabalVersion biDistDir)
        , [ componentOutputDir biComponentName biDistDir ]
        ]) ++
    [ "-stubdir=" ++ toFilePathNoTrailingSep (buildDir biDistDir) ]
  componentAutogen =
    componentAutogenDir biCabalVersion biComponentName biDistDir
  toIncludeDir "." = Just biCabalDir
  toIncludeDir relDir = concatAndCollapseAbsDir biCabalDir relDir
  includeOpts =
    map ("-I" <>) (biConfigIncludeDirs <> pkgIncludeOpts)
  pkgIncludeOpts =
    [ toFilePathNoTrailingSep absDir
    | dir <- Component.includeDirs biBuildInfo
    , absDir <- handleDir dir
    ]
  libOpts =
    map ("-l" <>) (Component.extraLibs biBuildInfo) <>
    map ("-L" <>) (biConfigLibDirs <> pkgLibDirs)
  pkgLibDirs =
    [ toFilePathNoTrailingSep absDir
    | dir <- Component.extraLibDirs biBuildInfo
    , absDir <- handleDir dir
    ]
  handleDir dir = case (parseAbsDir dir, parseRelDir dir) of
    (Just ab, _       ) -> [ab]
    (_      , Just rel) -> [biCabalDir </> rel]
    (Nothing, Nothing ) -> []
  fworks = map ("-framework=" <>) (Component.frameworks biBuildInfo)

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
  flags = M.union (packageConfigFlags packageConfig) (flagMap defaultFlags)
  rc = mkResolveConditions
         (packageConfigCompilerVersion packageConfig)
         (packageConfigPlatform packageConfig)
         flags
  updateLibDeps lib deps = lib
    { libBuildInfo = (libBuildInfo lib) {targetBuildDepends = deps} }
  updateForeignLibDeps lib deps = lib
    { foreignLibBuildInfo =
        (foreignLibBuildInfo lib) {targetBuildDepends = deps}
    }
  updateExeDeps exe deps = exe
    { buildInfo = (buildInfo exe) {targetBuildDepends = deps} }
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
  { rcFlags :: Map FlagName Bool
  , rcCompilerVersion :: ActualCompiler
  , rcOS :: OS
  , rcArch :: Arch
  }

-- | Generic a @ResolveConditions@ using sensible defaults.
mkResolveConditions ::
     ActualCompiler -- ^ Compiler version
  -> Platform -- ^ installation target platform
  -> Map FlagName Bool -- ^ enabled flags
  -> ResolveConditions
mkResolveConditions compilerVersion (Platform arch os) flags = ResolveConditions
  { rcFlags = flags
  , rcCompilerVersion = compilerVersion
  , rcOS = os
  , rcArch = arch
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
        OS os -> os == rcOS rc
        Arch arch -> arch == rcArch rc
        PackageFlag flag -> fromMaybe False $ M.lookup flag (rcFlags rc)
        -- NOTE:  ^^^^^ This should never happen, as all flags which are used
        -- must be declared. Defaulting to False.
        Impl flavor range ->
          case (flavor, rcCompilerVersion rc) of
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
  let stack = getProjectWorkDir env
  fp <- parseRelFile $ concat $
    packageIdentifierString (packageIdentifier package') :
    maybe id (\suffix -> ("-" :) . (suffix :)) msuffix [".log"]
  pure $ stack </> relDirLogs </> fp

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
        { packageBuildType = Custom
        , packageSetupDeps = Just $ M.fromList
            [ ("Cabal", libraryDepFromVersionRange cabalVersionRange)
            , ("base", libraryDepFromVersionRange anyVersion)
            ]
        }
  | otherwise = package
 where
  cabalVersionRange =
    orLaterVersion $ mkVersion $ cabalSpecToVersionDigits $
      packageCabalSpec package
  forceCustomBuild =
       packageBuildType package == Simple
    && not (cabalVersion `withinRange` cabalVersionRange)

-- | Check if the package has a main library that is buildable.
hasBuildableMainLibrary :: Package -> Bool
hasBuildableMainLibrary package =
  maybe False isComponentBuildable $ packageLibrary package

-- | Check if the main library has any exposed modules.
--
-- This should become irrelevant at some point since there's nothing inherently
-- wrong or different with packages exposing only modules in internal libraries
-- (for instance).
mainLibraryHasExposedModules :: Package -> Bool
mainLibraryHasExposedModules package =
  maybe False (not . null . Component.exposedModules) $ packageLibrary package

-- | Aggregate all unknown tools from all components. Mostly meant for
-- build tools specified in the legacy manner (build-tools:) that failed the
-- hard-coded lookup. See 'Stack.Types.Component.sbiUnknownTools' for more
-- information.
packageUnknownTools :: Package -> Set Text
packageUnknownTools pkg = lib (bench <> tests <> flib <> sublib <> exe)
 where
  lib setT = case packageLibrary pkg of
    Just libV -> addUnknownTools libV setT
    Nothing -> setT
  bench = gatherUnknownTools $ packageBenchmarks pkg
  tests = gatherUnknownTools $ packageTestSuites pkg
  flib = gatherUnknownTools $ packageForeignLibraries pkg
  sublib = gatherUnknownTools $ packageSubLibraries pkg
  exe = gatherUnknownTools $ packageExecutables pkg
  addUnknownTools :: HasBuildInfo x => x -> Set Text -> Set Text
  addUnknownTools = (<>) . Component.sbiUnknownTools . getField @"buildInfo"
  gatherUnknownTools :: HasBuildInfo x => CompCollection x -> Set Text
  gatherUnknownTools = foldr' addUnknownTools mempty

buildableForeignLibs :: Package -> Set Text
buildableForeignLibs pkg = getBuildableSetText (packageForeignLibraries pkg)

buildableSubLibs :: Package -> Set Text
buildableSubLibs pkg = getBuildableSetText (packageSubLibraries pkg)

buildableExes :: Package -> Set Text
buildableExes pkg = getBuildableSetText (packageExecutables pkg)

buildableTestSuites :: Package -> Set Text
buildableTestSuites pkg = getBuildableSetText (packageTestSuites pkg)

buildableBenchmarks :: Package -> Set Text
buildableBenchmarks pkg = getBuildableSetText (packageBenchmarks pkg)

-- | Apply a generic processing function in a Monad over all of the Package's components.
processPackageComponent ::
     forall m a.
     (Monad m)
  => Package
  -> (forall component. HasComponentInfo component => component -> m a -> m a)
   -- ^ Processing function with all the component's info.
  -> m a
   -- ^ Initial value.
  -> m a
processPackageComponent pkg componentFn = do
  let componentKindProcessor :: forall component. HasComponentInfo component => (Package -> CompCollection component) -> m a -> m a
      componentKindProcessor target =
        foldComponentToAnotherCollection
          (target pkg)
          componentFn
  let processMainLib = maybe id componentFn (packageLibrary pkg)
  let processAllComp =
        ( if packageBenchmarkEnabled pkg
              then componentKindProcessor packageBenchmarks
              else id
          )
        . ( if packageTestEnabled pkg
              then componentKindProcessor packageTestSuites
              else id
          )
        . componentKindProcessor packageForeignLibraries
        . componentKindProcessor packageExecutables
        . componentKindProcessor packageSubLibraries
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
  let packageSetupDepsProcessor resAction = case packageSetupDeps pkg of
        Nothing -> resAction
        Just v -> fn v resAction
  let processAllComp = processPackageComponent pkg (fn . componentDependencyMap)
        . packageSetupDepsProcessor
  processAllComp

-- | This is a function to iterate in a monad over all of a package component's
-- dependencies, and yield a collection of results.
processPackageDeps ::
     (Monad m, Monoid (targetedCollection resT))
  => Package
  -> (resT -> targetedCollection resT -> targetedCollection resT)
  -> (PackageName -> DepValue -> m resT)
  -> m (targetedCollection resT)
  -> m (targetedCollection resT)
processPackageDeps pkg combineResults fn = do
  let asPackageNameSet accessor =
        S.map (mkPackageName . T.unpack) $ getBuildableSetText $ accessor pkg
  let (!subLibNames, !foreignLibNames) =
        ( asPackageNameSet packageSubLibraries
        , asPackageNameSet packageForeignLibraries
        )
  let shouldIgnoreDep (packageNameV :: PackageName)
          | packageNameV == packageName pkg = True
          | packageNameV `S.member` subLibNames = True
          | packageNameV `S.member` foreignLibNames = True
          | otherwise = False
  let innerIterator packageName depValue resListInMonad
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
-- and their dependencies. It's only targeting internal dependencies, so it's doing
-- a topological sort on a subset of a package's components.
-- 
-- Note that in Cabal they use the Data.Graph struct to pursue the same goal. But dong this here
-- would require a large number intermediate data structure.
-- This is needed because we need to get the right GhcPkgId of the relevant internal dependencies
-- of a component before building it as a component.
topSortPackageComponent ::
     Package
  -> Target
  -> Bool
   -- ^ Include directTarget or not. False here means we won't 
   -- include the actual targets in the result, only their deps.
   -- Using it with False here only in GHCi
  -> Seq NamedComponent
topSortPackageComponent package target includeDirectTarget = runST $ do
  alreadyProcessedRef <- newSTRef (mempty :: Set NamedComponent)
  let processInitialComponents c = case target of
        TargetAll{} -> processComponent includeDirectTarget alreadyProcessedRef c
        TargetComps targetSet -> if S.member c.qualifiedName targetSet
          then processComponent includeDirectTarget alreadyProcessedRef c
          else id
  processPackageComponent package processInitialComponents (pure mempty)
  where
    processComponent :: forall s component. HasComponentInfo component
      => Bool
       -- ^ Finally add this component in the seq
      -> STRef s (Set NamedComponent)
      -> component
      -> ST s (Seq NamedComponent)
      -> ST s (Seq NamedComponent)
    processComponent finallyAddComponent alreadyProcessedRef component res = do
      let depMap = componentDependencyMap component
      let internalDep = M.lookup (packageName package) depMap
      let processSubDep = processOneDep alreadyProcessedRef internalDep res
      let qualName = component.qualifiedName
      let processSubDepSaveName
            | finallyAddComponent = (|> qualName) <$> processSubDep
            | otherwise = processSubDep
      -- This is an optimization, the only components we are likely to process
      -- multiple times are the ones we can find in dependencies, 
      -- otherwise we only fold on a single version of each component
      -- by design.
      if isPotentialDependency qualName then do
        alreadyProcessed <- readSTRef alreadyProcessedRef
        if S.member qualName alreadyProcessed then res
        else modifySTRef' alreadyProcessedRef (S.insert qualName) >> processSubDepSaveName
      else processSubDepSaveName
    lookupLibName isMain name = if isMain
          then packageLibrary package
          else collectionLookup name $ packageSubLibraries package
    processOneDep alreadyProcessed mDependency res = case dvType <$> mDependency of
          Just (AsLibrary (DepLibrary mainLibDep subLibDeps)) -> do
            let processMainLibDep = case (mainLibDep, lookupLibName True mempty) of
                  (True, Just mainLib) ->
                    processComponent True alreadyProcessed mainLib
                  _ -> id
            let processSingleSubLib name = case lookupLibName False (unqualCompToText name) of
                  Just lib -> processComponent True alreadyProcessed lib
                  Nothing -> id
            let processSubLibDep r = foldr' processSingleSubLib r subLibDeps
            processSubLibDep (processMainLibDep res)
          _ -> res
