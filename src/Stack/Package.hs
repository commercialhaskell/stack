{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

-- | Dealing with Cabal.

module Stack.Package
  ( readDotBuildinfo
  , resolvePackage
  , packageFromPackageDescription
  , Package (..)
  , PackageDescriptionPair (..)
  , GetPackageOpts (..)
  , PackageConfig (..)
  , buildLogPath
  , PackageException (..)
  , resolvePackageDescription
  , packageDependencies
  , applyForceCustomBuild
  ) where

import           Data.List ( unzip )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Distribution.CabalSpecVersion ( cabalSpecToVersionDigits )
import qualified Distribution.Compat.NonEmptySet as NES
import           Distribution.Compiler
                   ( CompilerFlavor (..), PerCompilerFlavor (..) )
import           Distribution.Package ( mkPackageName )
import           Distribution.PackageDescription
                   ( Benchmark (..), BuildInfo (..), BuildType (..)
                   , CondTree (..), Condition (..), ConfVar (..)
                   , Dependency (..), Executable (..), ForeignLib (..)
                   , GenericPackageDescription (..), HookedBuildInfo
                   , Library (..), PackageDescription (..), PackageFlag (..)
                   , SetupBuildInfo (..), TestSuite (..), allLanguages
                   , allLibraries, buildType, depPkgName, depVerRange
                   , libraryNameString, maybeToLibraryName, usedExtensions
                   )
import           Distribution.Pretty ( prettyShow )
import           Distribution.Simple.PackageDescription ( readHookedBuildInfo )
import           Distribution.System ( OS (..), Arch, Platform (..) )
import           Distribution.Text ( display )
import qualified Distribution.Types.CondTree as Cabal
import qualified Distribution.Types.ExeDependency as Cabal
import qualified Distribution.Types.LegacyExeDependency as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import           Distribution.Utils.Path ( getSymbolicPath )
import           Distribution.Verbosity ( silent )
import           Distribution.Version
                   ( anyVersion, mkVersion, orLaterVersion )
import           Path as FL hiding ( replaceExtension )
import           Path.Extra ( concatAndCollapseAbsDir, toFilePathNoTrailingSep )
import           Stack.Constants (relFileCabalMacrosH, relDirLogs)
import           Stack.Constants.Config ( distDirFromDir )
import           Stack.Prelude hiding ( Display (..) )
import           Stack.ComponentFile
                   ( buildDir, componentAutogenDir, componentBuildDir
                   , componentOutputDir, packageAutogenDir
                   )
import           Stack.Types.BuildConfig
                   ( HasBuildConfig (..), getProjectWorkDir )
import           Stack.Types.Compiler ( ActualCompiler (..), getGhcVersion )
import           Stack.Types.CompilerPaths ( cabalVersionL )
import           Stack.Types.Config ( Config (..), HasConfig (..) )
import           Stack.Types.EnvConfig ( HasEnvConfig )
import           Stack.Types.GhcPkgId ( ghcPkgIdString )
import           Stack.Types.NamedComponent
                   ( NamedComponent (..), internalLibComponents )
import           Stack.Types.Package
                   ( BuildInfoOpts (..), ExeName (..), GetPackageOpts (..)
                   , InstallMap, Installed (..), InstalledMap, Package (..)
                   , PackageConfig (..), PackageException (..)
                   , PackageLibraries (..), dotCabalCFilePath, packageIdentifier
                   )
import           Stack.Types.Version
                   ( VersionRange, intersectVersionRanges, withinRange )
import           System.FilePath ( replaceExtension )
import           Stack.Types.Dependency ( DepValue (..), DepType (..) )
import           Stack.Types.PackageFile ( DotCabalPath , GetPackageFiles (..) )
import           Stack.PackageFile ( getPackageFile )

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
  -> PackageDescriptionPair
  -> Package
packageFromPackageDescription packageConfig pkgFlags (PackageDescriptionPair pkgNoMod pkg) =
  Package
  { packageName = name
  , packageVersion = pkgVersion pkgId
  , packageLicense = licenseRaw pkg
  , packageDeps = deps
  , packageFiles = pkgFiles
  , packageUnknownTools = unknownTools
  , packageGhcOptions = packageConfigGhcOptions packageConfig
  , packageCabalConfigOpts = packageConfigCabalConfigOpts packageConfig
  , packageFlags = packageConfigFlags packageConfig
  , packageDefaultFlags = M.fromList
      [(flagName flag, flagDefault flag) | flag <- pkgFlags]
  , packageAllDeps = M.keysSet deps
  , packageSubLibDeps = subLibDeps
  , packageLibraries =
      let mlib = do
            lib <- library pkg
            guard $ buildable $ libBuildInfo lib
            Just lib
       in
        case mlib of
          Nothing -> NoLibraries
          Just _ -> HasLibraries foreignLibNames
  , packageInternalLibraries = subLibNames
  , packageTests = M.fromList
      [ (T.pack (Cabal.unUnqualComponentName $ testName t), testInterface t)
      | t <- testSuites pkgNoMod
      , buildable (testBuildInfo t)
      ]
  , packageBenchmarks = S.fromList
      [ T.pack (Cabal.unUnqualComponentName $ benchmarkName b)
      | b <- benchmarks pkgNoMod
      , buildable (benchmarkBuildInfo b)
      ]
      -- Same comment about buildable applies here too.
  , packageExes = S.fromList
      [ T.pack (Cabal.unUnqualComponentName $ exeName biBuildInfo)
      | biBuildInfo <- executables pkg
      , buildable (buildInfo biBuildInfo)
      ]
  -- This is an action used to collect info needed for "stack ghci".
  -- This info isn't usually needed, so computation of it is deferred.
  , packageOpts = GetPackageOpts $
      \installMap installedMap omitPkgs addPkgs cabalfp -> do
        (componentsModules,componentFiles, _, _) <- getPackageFiles pkgFiles cabalfp
        let internals =
              S.toList $ internalLibComponents $ M.keysSet componentsModules
        excludedInternals <- mapM (parsePackageNameThrowing . T.unpack) internals
        mungedInternals <- mapM
          (parsePackageNameThrowing . T.unpack . toInternalPackageMungedName)
          internals
        componentsOpts <- generatePkgDescOpts
          installMap
          installedMap
          (excludedInternals ++ omitPkgs)
          (mungedInternals ++ addPkgs)
          cabalfp
          pkg
          componentFiles
        pure (componentsModules, componentFiles, componentsOpts)
  , packageHasExposedModules = maybe
      False
      (not . null . exposedModules)
      (library pkg)
  , packageBuildType = buildType pkg
  , packageSetupDeps = msetupDeps
  , packageCabalSpec = specVersion pkg
  }
 where
  extraLibNames = S.union subLibNames foreignLibNames

  subLibNames
    = S.fromList
    $ map (T.pack . Cabal.unUnqualComponentName)
    $ mapMaybe (libraryNameString . libName) -- this is a design bug in the
                                             -- Cabal API: this should
                                             -- statically be known to exist
    $ filter (buildable . libBuildInfo)
    $ subLibraries pkg

  foreignLibNames
    = S.fromList
    $ map (T.pack . Cabal.unUnqualComponentName . foreignLibName)
    $ filter (buildable . foreignLibBuildInfo)
    $ foreignLibs pkg

  toInternalPackageMungedName
    = T.pack . prettyShow . MungedPackageName (pkgName pkgId)
    . maybeToLibraryName . Just . Cabal.mkUnqualComponentName . T.unpack

  -- Gets all of the modules, files, build files, and data files that constitute
  -- the package. This is primarily used for dirtiness checking during build, as
  -- well as use by "stack ghci"
  pkgFiles = GetPackageFiles $ getPackageFile pkg
  pkgId = package pkg
  name = pkgName pkgId

  (unknownTools, knownTools) = packageDescTools pkg

  deps = M.filterWithKey (const . not . isMe) (M.unionsWith (<>)
    [ asLibrary <$> packageDependencies packageConfig pkg
    -- We include all custom-setup deps - if present - in the package deps
    -- themselves. Stack always works with the invariant that there will be a
    -- single installed package relating to a package name, and this applies at
    -- the setup dependency level as well.
    , asLibrary <$> fromMaybe M.empty msetupDeps
    , knownTools
    ])

  msetupDeps = fmap
    (M.fromList . map (depPkgName &&& depVerRange) . setupDepends)
    (setupBuildInfo pkg)

  subLibDeps = M.fromList $ concatMap
    (\(Dependency n vr libs) -> mapMaybe (getSubLibName n vr) (NES.toList libs))
    (concatMap targetBuildDepends (allBuildInfo' pkg))

  getSubLibName pn vr lib@(LSubLibName _) =
    Just (MungedPackageName pn lib, asLibrary vr)
  getSubLibName _ _ _ = Nothing

  asLibrary range = DepValue
    { dvVersionRange = range
    , dvType = AsLibrary
    }

  -- Is the package dependency mentioned here me: either the package name
  -- itself, or the name of one of the sub libraries
  isMe name' =  name' == name
             || fromString (packageNameString name') `S.member` extraLibNames

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
  -> PackageDescription
  -> Map NamedComponent [DotCabalPath]
  -> m (Map NamedComponent BuildInfoOpts)
generatePkgDescOpts installMap installedMap omitPkgs addPkgs cabalfp pkg componentPaths = do
  config <- view configL
  cabalVer <- view cabalVersionL
  distDir <- distDirFromDir cabalDir
  let generate namedComponent binfo =
        ( namedComponent
        , generateBuildInfoOpts BioInput
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
        )
  pure
    ( M.fromList
        ( concat
            [ maybe
                []
                (pure . generate CLib . libBuildInfo)
                (library pkg)
            , mapMaybe
                (\sublib -> do
                  let maybeLib =
                        CInternalLib . T.pack . Cabal.unUnqualComponentName <$>
                          (libraryNameString . libName) sublib
                  flip generate  (libBuildInfo sublib) <$> maybeLib
                 )
                (subLibraries pkg)
            , fmap
                (\exe ->
                  generate
                    (CExe (T.pack (Cabal.unUnqualComponentName (exeName exe))))
                    (buildInfo exe)
                )
                (executables pkg)
            , fmap
                (\bench ->
                  generate
                    (CBench
                      (T.pack (Cabal.unUnqualComponentName (benchmarkName bench)))
                    )
                    (benchmarkBuildInfo bench)
                )
                (benchmarks pkg)
            , fmap
                (\test ->
                  generate
                    (CTest (T.pack (Cabal.unUnqualComponentName (testName test))))
                    (testBuildInfo test)
                )
                (testSuites pkg)
            ]
        )
    )
 where
  cabalDir = parent cabalfp

-- | Input to 'generateBuildInfoOpts'
data BioInput = BioInput
  { biInstallMap :: !InstallMap
  , biInstalledMap :: !InstalledMap
  , biCabalDir :: !(Path Abs Dir)
  , biDistDir :: !(Path Abs Dir)
  , biOmitPackages :: ![PackageName]
  , biAddPackages :: ![PackageName]
  , biBuildInfo :: !BuildInfo
  , biDotCabalPaths :: ![DotCabalPath]
  , biConfigLibDirs :: ![FilePath]
  , biConfigIncludeDirs :: ![FilePath]
  , biComponentName :: !NamedComponent
  , biCabalVersion :: !Version
  }

-- | Generate GHC options for the target. Since Cabal also figures out these
-- options, currently this is only used for invoking GHCI (via stack ghci).
generateBuildInfoOpts :: BioInput -> BuildInfoOpts
generateBuildInfoOpts BioInput {..} =
  BuildInfoOpts
    { bioOpts = ghcOpts ++ fmap ("-optP" <>) (cppOptions biBuildInfo)
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
          Just (_, Stack.Types.Package.Library _ident ipid _) ->
            ["-package-id=" <> ghcPkgIdString ipid]
          _ -> ["-package=" <> packageNameString name <>
            maybe "" -- This empty case applies to e.g. base.
              ((("-" <>) . versionString) . installVersion)
              (M.lookup name biInstallMap)]
      | name <- pkgs
      ]
  pkgs =
    biAddPackages ++
    [ name
    | Dependency name _ _ <- targetBuildDepends biBuildInfo
      -- TODO: cabal 3 introduced multiple public libraries in a single dependency
    , name `notElem` biOmitPackages
    ]
  PerCompilerFlavor ghcOpts _ = options biBuildInfo
  extOpts =
       map (("-X" ++) . display) (allLanguages biBuildInfo)
    <> map (("-X" ++) . display) (usedExtensions biBuildInfo)
  srcOpts =
    map (("-i" <>) . toFilePathNoTrailingSep)
      (concat
        [ [ componentBuildDir biCabalVersion biComponentName biDistDir ]
        , [ biCabalDir
          | null (hsSourceDirs biBuildInfo)
          ]
        , mapMaybe (toIncludeDir . getSymbolicPath) (hsSourceDirs biBuildInfo)
        , [ componentAutogen ]
        , maybeToList (packageAutogenDir biCabalVersion biDistDir)
        , [ componentOutputDir biComponentName biDistDir ]
        ]) ++
    [ "-stubdir=" ++ toFilePathNoTrailingSep (buildDir biDistDir) ]
  componentAutogen = componentAutogenDir biCabalVersion biComponentName biDistDir
  toIncludeDir "." = Just biCabalDir
  toIncludeDir relDir = concatAndCollapseAbsDir biCabalDir relDir
  includeOpts =
    map ("-I" <>) (biConfigIncludeDirs <> pkgIncludeOpts)
  pkgIncludeOpts =
    [ toFilePathNoTrailingSep absDir
    | dir <- includeDirs biBuildInfo
    , absDir <- handleDir dir
    ]
  libOpts =
    map ("-l" <>) (extraLibs biBuildInfo) <>
    map ("-L" <>) (biConfigLibDirs <> pkgLibDirs)
  pkgLibDirs =
    [ toFilePathNoTrailingSep absDir
    | dir <- extraLibDirs biBuildInfo
    , absDir <- handleDir dir
    ]
  handleDir dir = case (parseAbsDir dir, parseRelDir dir) of
    (Just ab, _       ) -> [ab]
    (_      , Just rel) -> [biCabalDir </> rel]
    (Nothing, Nothing ) -> []
  fworks = map ("-framework=" <>) (frameworks biBuildInfo)

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
--
-- Note that for Cabal versions 1.22 and earlier, there is a bug where Cabal
-- requires dependencies for non-buildable components to be present. We're going
-- to use GHC version as a proxy for Cabal library version in this case for
-- simplicity, so we'll check for GHC being 7.10 or earlier. This obviously
-- makes our function a lot more fun to write...
packageDependencies ::
     PackageConfig
  -> PackageDescription
  -> Map PackageName VersionRange
packageDependencies pkgConfig pkg' =
  M.fromListWith intersectVersionRanges $
  map (depPkgName &&& depVerRange) $
  concatMap targetBuildDepends (allBuildInfo' pkg) ++
  maybe [] setupDepends (setupBuildInfo pkg)
 where
  pkg
    | getGhcVersion (packageConfigCompilerVersion pkgConfig) >= mkVersion [8, 0] = pkg'
    -- Set all components to buildable. Only need to worry  library, exe, test,
    -- and bench, since others didn't exist in older Cabal versions
    | otherwise = pkg'
      { library =
          (\c -> c { libBuildInfo = go (libBuildInfo c) }) <$> library pkg'
      , executables =
          (\c -> c { buildInfo = go (buildInfo c) }) <$> executables pkg'
      , testSuites =
          if packageConfigEnableTests pkgConfig
            then (\c -> c { testBuildInfo = go (testBuildInfo c) }) <$>
                   testSuites pkg'
            else testSuites pkg'
      , benchmarks =
          if packageConfigEnableBenchmarks pkgConfig
            then (\c -> c { benchmarkBuildInfo = go (benchmarkBuildInfo c) }) <$>
                   benchmarks pkg'
            else benchmarks pkg'
      }

  go bi = bi { buildable = True }

-- | Get all dependencies of the package (buildable targets only).
--
-- This uses both the new 'buildToolDepends' and old 'buildTools' information.
packageDescTools ::
     PackageDescription
  -> (Set ExeName, Map PackageName DepValue)
packageDescTools pd =
  (S.fromList $ concat unknowns, M.fromListWith (<>) $ concat knowns)
 where
  (unknowns, knowns) = unzip $ map perBI $ allBuildInfo' pd

  perBI :: BuildInfo -> ([ExeName], [(PackageName, DepValue)])
  perBI bi =
    (unknownTools, tools)
   where
    (unknownTools, knownTools) = partitionEithers $ map go1 (buildTools bi)

    tools = mapMaybe go2 (knownTools ++ buildToolDepends bi)

    -- This is similar to desugarBuildTool from Cabal, however it
    -- uses our own hard-coded map which drops tools shipped with
    -- GHC (like hsc2hs), and includes some tools from Stackage.
    go1 :: Cabal.LegacyExeDependency -> Either ExeName Cabal.ExeDependency
    go1 (Cabal.LegacyExeDependency name range) =
      case M.lookup name hardCodedMap of
        Just pkgName ->
          Right $
            Cabal.ExeDependency pkgName (Cabal.mkUnqualComponentName name) range
        Nothing -> Left $ ExeName $ T.pack name

    go2 :: Cabal.ExeDependency -> Maybe (PackageName, DepValue)
    go2 (Cabal.ExeDependency pkg _name range)
      | pkg `S.member` preInstalledPackages = Nothing
      | otherwise = Just
          ( pkg
          , DepValue
              { dvVersionRange = range
              , dvType = AsBuildTool
              }
          )

-- | A hard-coded map for tool dependencies
hardCodedMap :: Map String PackageName
hardCodedMap = M.fromList
  [ ("alex", Distribution.Package.mkPackageName "alex")
  , ("happy", Distribution.Package.mkPackageName "happy")
  , ("cpphs", Distribution.Package.mkPackageName "cpphs")
  , ("greencard", Distribution.Package.mkPackageName "greencard")
  , ("c2hs", Distribution.Package.mkPackageName "c2hs")
  , ("hscolour", Distribution.Package.mkPackageName "hscolour")
  , ("hspec-discover", Distribution.Package.mkPackageName "hspec-discover")
  , ("hsx2hs", Distribution.Package.mkPackageName "hsx2hs")
  , ("gtk2hsC2hs", Distribution.Package.mkPackageName "gtk2hs-buildtools")
  , ("gtk2hsHookGenerator", Distribution.Package.mkPackageName "gtk2hs-buildtools")
  , ("gtk2hsTypeGen", Distribution.Package.mkPackageName "gtk2hs-buildtools")
  ]

-- | Executable-only packages which come pre-installed with GHC and do not need
-- to be built. Without this exception, we would either end up unnecessarily
-- rebuilding these packages, or failing because the packages do not appear in
-- the Stackage snapshot.
preInstalledPackages :: Set PackageName
preInstalledPackages = S.fromList
  [ mkPackageName "hsc2hs"
  , mkPackageName "haddock"
  ]

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

-- | A pair of package descriptions: one which modified the buildable values of
-- test suites and benchmarks depending on whether they are enabled, and one
-- which does not.
--
-- Fields are intentionally lazy, we may only need one or the other value.
--
-- Michael S Snoyman 2017-08-29: The very presence of this data type is terribly
-- ugly, it represents the fact that the Cabal 2.0 upgrade did _not_ go well.
-- Specifically, we used to have a field to indicate whether a component was
-- enabled in addition to buildable, but that's gone now, and this is an ugly
-- proxy. We should at some point clean up the mess of Package, LocalPackage,
-- etc, and probably pull in the definition of PackageDescription from Cabal
-- with our additionally needed metadata. But this is a good enough hack for the
-- moment. Odds are, you're reading this in the year 2024 and thinking "wtf?"
data PackageDescriptionPair = PackageDescriptionPair
  { pdpOrigBuildable :: PackageDescription
  , pdpModifiedBuildable :: PackageDescription
  }

-- | Evaluates the conditions of a 'GenericPackageDescription', yielding
-- a resolved 'PackageDescription'.
resolvePackageDescription ::
     PackageConfig
  -> GenericPackageDescription
  -> PackageDescriptionPair
resolvePackageDescription
  packageConfig
  ( GenericPackageDescription
      desc _ defaultFlags mlib subLibs foreignLibs' exes tests benches
  )
  =
  PackageDescriptionPair
    { pdpOrigBuildable = go False
    , pdpModifiedBuildable = go True
    }
 where
  go modBuildable = desc
    { library = fmap (resolveConditions rc updateLibDeps) mlib
    , subLibraries = map
        (\(n, v) -> (resolveConditions rc updateLibDeps v){libName=LSubLibName n})
        subLibs
    , foreignLibs = map
        (\(n, v) -> (resolveConditions rc updateForeignLibDeps v){foreignLibName=n})
        foreignLibs'
    , executables = map
        (\(n, v) -> (resolveConditions rc updateExeDeps v){exeName=n})
        exes
    , testSuites = map
        (\(n, v) -> (resolveConditions rc (updateTestDeps modBuildable) v){testName=n})
        tests
    , benchmarks = map
        (\(n, v) -> (resolveConditions rc (updateBenchmarkDeps modBuildable) v){benchmarkName=n})
        benches
    }

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

  -- Note that, prior to moving to Cabal 2.0, we would set testEnabled or
  -- benchmarkEnabled here. These fields no longer exist, so we modify buildable
  -- instead here. The only wrinkle in the Cabal 2.0 story is
  -- https://github.com/haskell/cabal/issues/1725, where older versions of Cabal
  -- (which may be used for actually building code) don't properly exclude
  -- build-depends for non-buildable components. Testing indicates that
  -- everything is working fine, and that this comment can be completely
  -- ignored. I'm leaving the comment anyway in case something breaks and you,
  -- poor reader, are investigating.
  updateTestDeps modBuildable test deps =
    let bi = testBuildInfo test
        bi' = bi
          { targetBuildDepends = deps
          , buildable =
                 buildable bi
              && (  not modBuildable
                 || packageConfigEnableTests packageConfig
                 )
          }
    in  test { testBuildInfo = bi' }
  updateBenchmarkDeps modBuildable benchmark deps =
    let bi = benchmarkBuildInfo benchmark
        bi' = bi
          { targetBuildDepends = deps
          , buildable =
                 buildable bi
              && (  not modBuildable
                 || packageConfigEnableBenchmarks packageConfig
                 )
          }
    in  benchmark { benchmarkBuildInfo = bi' }

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
        , packageDeps =
            M.insertWith (<>) "Cabal" (DepValue cabalVersionRange AsLibrary) $
              packageDeps package
        , packageSetupDeps = Just $ M.fromList
            [ ("Cabal", cabalVersionRange)
            , ("base", anyVersion)
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
