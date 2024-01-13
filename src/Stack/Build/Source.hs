{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- Load information on package sources
module Stack.Build.Source
  ( projectLocalPackages
  , localDependencies
  , loadCommonPackage
  , loadLocalPackage
  , loadSourceMap
  , getLocalFlags
  , addUnlistedToBuildCache
  , hashSourceMapData
  ) where

import           Data.ByteString.Builder ( toLazyByteString )
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Distribution.PackageDescription as C
import qualified Pantry.SHA256 as SHA256
import           Stack.Build.Cache ( tryGetBuildCache )
import           Stack.Build.Haddock ( shouldHaddockDeps )
import           Stack.Package
                   ( buildableBenchmarks, buildableExes, buildableTestSuites
                   , hasBuildableMainLibrary, resolvePackage
                   )
import           Stack.PackageFile ( getPackageFile )
import           Stack.Prelude
import           Stack.SourceMap
                   ( DumpedGlobalPackage, checkFlagsUsedThrowing
                   , getCompilerInfo, immutableLocSha, mkProjectPackage
                   , pruneGlobals
                   )
import           Stack.Types.ApplyGhcOptions ( ApplyGhcOptions (..) )
import           Stack.Types.ApplyProgOptions ( ApplyProgOptions (..) )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..) )
import           Stack.Types.BuildOpts
                   ( ApplyCLIFlag (..), BuildOpts (..), BuildOptsCLI (..)
                   , TestOpts (..), boptsCLIAllProgOptions
                   )
import           Stack.Types.CabalConfigKey ( CabalConfigKey (..) )
import           Stack.Types.CompilerPaths ( HasCompiler, getCompilerPath )
import           Stack.Types.Config ( Config (..), HasConfig (..), buildOptsL )
import           Stack.Types.Curator ( Curator (..) )
import           Stack.Types.EnvConfig
                   ( EnvConfig (..), HasEnvConfig (..), HasSourceMap (..)
                   , actualCompilerVersionL
                   )
import           Stack.Types.FileDigestCache ( readFileDigest )
import           Stack.Types.NamedComponent
                   ( NamedComponent (..), isCSubLib, splitComponents )
import           Stack.Types.Package
                   ( FileCacheInfo (..), LocalPackage (..), Package (..)
                   , PackageConfig (..), dotCabalGetPath, memoizeRefWith
                   , runMemoizedWith
                   )
import           Stack.Types.PackageFile
                   ( PackageComponentFile (..), PackageWarning )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.SourceMap
                   ( CommonPackage (..), DepPackage (..), ProjectPackage (..)
                   , SMActual (..), SMTargets (..), SourceMap (..)
                   , SourceMapHash (..), Target (..), ppGPD, ppRoot
                   )
import           Stack.Types.UnusedFlags ( FlagSource (..) )
import           System.FilePath ( takeFileName )
import           System.IO.Error ( isDoesNotExistError )

-- | loads and returns project packages
projectLocalPackages :: HasEnvConfig env => RIO env [LocalPackage]
projectLocalPackages = do
  sm <- view $ envConfigL . to (.envConfigSourceMap)
  for (toList sm.smProject) loadLocalPackage

-- | loads all local dependencies - project packages and local extra-deps
localDependencies :: HasEnvConfig env => RIO env [LocalPackage]
localDependencies = do
  bopts <- view $ configL . to (.build)
  sourceMap <- view $ envConfigL . to (.envConfigSourceMap)
  forMaybeM (Map.elems sourceMap.smDeps) $ \dp ->
    case dp.dpLocation of
      PLMutable dir -> do
        pp <- mkProjectPackage YesPrintWarnings dir (shouldHaddockDeps bopts)
        Just <$> loadLocalPackage pp
      _ -> pure Nothing

-- | Given the parsed targets and build command line options constructs a source
-- map
loadSourceMap :: HasBuildConfig env
              => SMTargets
              -> BuildOptsCLI
              -> SMActual DumpedGlobalPackage
              -> RIO env SourceMap
loadSourceMap smt boptsCli sma = do
  bconfig <- view buildConfigL
  let compiler = sma.smaCompiler
      project = M.map applyOptsFlagsPP sma.smaProject
      bopts = bconfig.config.build
      applyOptsFlagsPP p@ProjectPackage{ppCommon = c} =
        p{ppCommon = applyOptsFlags (M.member c.cpName smt.smtTargets) True c}
      deps0 = smt.smtDeps <> sma.smaDeps
      deps = M.map applyOptsFlagsDep deps0
      applyOptsFlagsDep d@DepPackage{dpCommon = c} =
        d{dpCommon = applyOptsFlags (M.member c.cpName smt.smtDeps) False c}
      applyOptsFlags isTarget isProjectPackage common =
        let name = common.cpName
            flags = getLocalFlags boptsCli name
            ghcOptions =
              generalGhcOptions bconfig boptsCli isTarget isProjectPackage
            cabalConfigOpts =
              generalCabalConfigOpts bconfig boptsCli common.cpName isTarget isProjectPackage
        in  common
              { cpFlags =
                  if M.null flags
                    then common.cpFlags
                    else flags
              , cpGhcOptions =
                  ghcOptions ++ common.cpGhcOptions
              , cpCabalConfigOpts =
                  cabalConfigOpts ++ common.cpCabalConfigOpts
              , cpHaddocks =
                  if isTarget
                    then bopts.haddock
                    else shouldHaddockDeps bopts
              }
      packageCliFlags = Map.fromList $
        mapMaybe maybeProjectFlags $
        Map.toList boptsCli.boptsCLIFlags
      maybeProjectFlags (ACFByName name, fs) = Just (name, fs)
      maybeProjectFlags _ = Nothing
      globals = pruneGlobals sma.smaGlobal (Map.keysSet deps)
  logDebug "Checking flags"
  checkFlagsUsedThrowing packageCliFlags FSCommandLine project deps
  logDebug "SourceMap constructed"
  pure
    SourceMap
      { smTargets = smt
      , smCompiler = compiler
      , smProject = project
      , smDeps = deps
      , smGlobal = globals
      }

-- | Get a 'SourceMapHash' for a given 'SourceMap'
--
-- Basic rules:
--
-- * If someone modifies a GHC installation in any way after Stack looks at it,
--   they voided the warranty. This includes installing a brand new build to the
--   same directory, or registering new packages to the global database.
--
-- * We should include everything in the hash that would relate to immutable
--   packages and identifying the compiler itself. Mutable packages (both
--   project packages and dependencies) will never make it into the snapshot
--   database, and can be ignored.
--
-- * Target information is only relevant insofar as it effects the dependency
--   map. The actual current targets for this build are irrelevant to the cache
--   mechanism, and can be ignored.
--
-- * Make sure things like profiling and haddocks are included in the hash
--
hashSourceMapData ::
     (HasBuildConfig env, HasCompiler env)
  => BuildOptsCLI
  -> SourceMap
  -> RIO env SourceMapHash
hashSourceMapData boptsCli sm = do
  compilerPath <- getUtf8Builder . fromString . toFilePath <$> getCompilerPath
  compilerInfo <- getCompilerInfo
  immDeps <- forM (Map.elems sm.smDeps) depPackageHashableContent
  bc <- view buildConfigL
  let -- extra bytestring specifying GHC options supposed to be applied to GHC
      -- boot packages so we'll have different hashes when bare resolver
      -- 'ghc-X.Y.Z' is used, no extra-deps and e.g. user wants builds with
      -- profiling or without
      bootGhcOpts = map display (generalGhcOptions bc boptsCli False False)
      hashedContent =
           toLazyByteString $ compilerPath
        <> compilerInfo
        <> getUtf8Builder (mconcat bootGhcOpts)
        <> mconcat immDeps
  pure $ SourceMapHash (SHA256.hashLazyBytes hashedContent)

depPackageHashableContent :: (HasConfig env) => DepPackage -> RIO env Builder
depPackageHashableContent dp =
  case dp.dpLocation of
    PLMutable _ -> pure ""
    PLImmutable pli -> do
      let flagToBs (f, enabled) =
            if enabled
              then ""
              else "-" <> fromString (C.unFlagName f)
          flags = map flagToBs $ Map.toList dp.dpCommon.cpFlags
          ghcOptions = map display dp.dpCommon.cpGhcOptions
          cabalConfigOpts = map display dp.dpCommon.cpCabalConfigOpts
          haddocks = if dp.dpCommon.cpHaddocks then "haddocks" else ""
          hash = immutableLocSha pli
      pure
        $  hash
        <> haddocks
        <> getUtf8Builder (mconcat flags)
        <> getUtf8Builder (mconcat ghcOptions)
        <> getUtf8Builder (mconcat cabalConfigOpts)

-- | All flags for a local package.
getLocalFlags ::
     BuildOptsCLI
  -> PackageName
  -> Map FlagName Bool
getLocalFlags boptsCli name = Map.unions
  [ Map.findWithDefault Map.empty (ACFByName name) cliFlags
  , Map.findWithDefault Map.empty ACFAllProjectPackages cliFlags
  ]
 where
  cliFlags = boptsCli.boptsCLIFlags

-- | Get the options to pass to @./Setup.hs configure@
generalCabalConfigOpts ::
     BuildConfig
  -> BuildOptsCLI
  -> PackageName
  -> Bool
  -> Bool
  -> [Text]
generalCabalConfigOpts bconfig boptsCli name isTarget isLocal = concat
  [ Map.findWithDefault [] CCKEverything config.cabalConfigOpts
  , if isLocal
      then Map.findWithDefault [] CCKLocals config.cabalConfigOpts
      else []
  , if isTarget
      then Map.findWithDefault [] CCKTargets config.cabalConfigOpts
      else []
  , Map.findWithDefault [] (CCKPackage name) config.cabalConfigOpts
  , if includeExtraOptions
      then boptsCLIAllProgOptions boptsCli
      else []
  ]
 where
  config = view configL bconfig
  includeExtraOptions =
    case config.applyProgOptions of
      APOTargets -> isTarget
      APOLocals -> isLocal
      APOEverything -> True

-- | Get the configured options to pass from GHC, based on the build
-- configuration and commandline.
generalGhcOptions :: BuildConfig -> BuildOptsCLI -> Bool -> Bool -> [Text]
generalGhcOptions bconfig boptsCli isTarget isLocal = concat
  [ Map.findWithDefault [] AGOEverything config.ghcOptionsByCat
  , if isLocal
      then Map.findWithDefault [] AGOLocals config.ghcOptionsByCat
      else []
  , if isTarget
      then Map.findWithDefault [] AGOTargets config.ghcOptionsByCat
      else []
  , concat [["-fhpc"] | isLocal && bopts.testOpts.toCoverage]
  , if bopts.libProfile || bopts.exeProfile
      then ["-fprof-auto", "-fprof-cafs"]
      else []
  , [ "-g" | not $ bopts.libStrip || bopts.exeStrip ]
  , if includeExtraOptions
      then boptsCli.boptsCLIGhcOptions
      else []
  ]
 where
  bopts =  config.build
  config = view configL bconfig
  includeExtraOptions =
    case config.applyGhcOptions of
      AGOTargets -> isTarget
      AGOLocals -> isLocal
      AGOEverything -> True

loadCommonPackage ::
     forall env. (HasBuildConfig env, HasSourceMap env)
  => CommonPackage
  -> RIO env Package
loadCommonPackage common = do
  config <-
    getPackageConfig
      common.cpFlags
      common.cpGhcOptions
      common.cpCabalConfigOpts
  gpkg <- liftIO common.cpGPD
  pure $ resolvePackage config gpkg

-- | Upgrade the initial project package info to a full-blown @LocalPackage@
-- based on the selected components
loadLocalPackage ::
     forall env. (HasBuildConfig env, HasSourceMap env)
  => ProjectPackage
  -> RIO env LocalPackage
loadLocalPackage pp = do
  sm <- view sourceMapL
  let common = pp.ppCommon
  bopts <- view buildOptsL
  mcurator <- view $ buildConfigL . to (.curator)
  config <- getPackageConfig
              common.cpFlags
              common.cpGhcOptions
              common.cpCabalConfigOpts
  gpkg <- ppGPD pp
  let name = common.cpName
      mtarget = M.lookup name sm.smTargets.smtTargets
      (exeCandidates, testCandidates, benchCandidates) =
        case mtarget of
          Just (TargetComps comps) ->
            -- Currently, a named library component (a sub-library) cannot be
            -- specified as a build target.
            let (_s, e, t, b) = splitComponents $ Set.toList comps
            in  (e, t, b)
          Just (TargetAll _packageType) ->
            ( buildableExes pkg
            , if    bopts.tests
                 && maybe True (Set.notMember name . (.curatorSkipTest)) mcurator
                then buildableTestSuites pkg
                else Set.empty
            , if    bopts.benchmarks
                 && maybe
                      True
                      (Set.notMember name . (.curatorSkipBenchmark))
                      mcurator
                then buildableBenchmarks pkg
                else Set.empty
            )
          Nothing -> mempty

      -- See https://github.com/commercialhaskell/stack/issues/2862
      isWanted = case mtarget of
        Nothing -> False
        -- FIXME: When issue #1406 ("stack 0.1.8 lost ability to build
        -- individual executables or library") is resolved, 'hasLibrary' is only
        -- relevant if the library is part of the target spec.
        Just _ ->
             hasBuildableMainLibrary pkg
          || not (Set.null nonLibComponents)
          || not (null pkg.subLibraries)

      filterSkippedComponents =
        Set.filter (not . (`elem` bopts.skipComponents))

      (exes, tests, benches) = ( filterSkippedComponents exeCandidates
                               , filterSkippedComponents testCandidates
                               , filterSkippedComponents benchCandidates
                               )

      nonLibComponents = toComponents exes tests benches

      toComponents e t b = Set.unions
        [ Set.map CExe e
        , Set.map CTest t
        , Set.map CBench b
        ]

      btconfig = config
        { enableTests = not $ Set.null tests
        , enableBenchmarks = not $ Set.null benches
        }

      -- We resolve the package in 2 different configurations:
      --
      -- - pkg doesn't have tests or benchmarks enabled.
      --
      -- - btpkg has them enabled if they are present.
      --
      -- The latter two configurations are used to compute the deps when
      -- --enable-benchmarks or --enable-tests are configured. This allows us to
      -- do an optimization where these are passed if the deps are present. This
      -- can avoid doing later unnecessary reconfigures.
      pkg = resolvePackage config gpkg
      btpkg
        | Set.null tests && Set.null benches = Nothing
        | otherwise = Just (resolvePackage btconfig gpkg)

  componentFiles <- memoizeRefWith $
    fst <$> getPackageFilesForTargets pkg pp.ppCabalFP nonLibComponents

  checkCacheResults <- memoizeRefWith $ do
    componentFiles' <- runMemoizedWith componentFiles
    forM (Map.toList componentFiles') $ \(component, files) -> do
      mbuildCache <- tryGetBuildCache (ppRoot pp) component
      checkCacheResult <- checkBuildCache
        (fromMaybe Map.empty mbuildCache)
        (Set.toList files)
      pure (component, checkCacheResult)

  let dirtyFiles = do
        checkCacheResults' <- checkCacheResults
        let allDirtyFiles =
              Set.unions $ map (\(_, (x, _)) -> x) checkCacheResults'
        pure $
          if not (Set.null allDirtyFiles)
            then let tryStripPrefix y =
                      fromMaybe y (L.stripPrefix (toFilePath $ ppRoot pp) y)
                 in  Just $ Set.map tryStripPrefix allDirtyFiles
            else Nothing
      newBuildCaches =
        M.fromList . map (\(c, (_, cache)) -> (c, cache)) <$> checkCacheResults

  pure LocalPackage
    { package = pkg
    , testBench = btpkg
    , componentFiles = componentFiles
    , buildHaddocks = pp.ppCommon.cpHaddocks
    , forceDirty = bopts.forceDirty
    , dirtyFiles = dirtyFiles
    , newBuildCaches = newBuildCaches
    , cabalFile = pp.ppCabalFP
    , wanted = isWanted
    , components = nonLibComponents
      -- TODO: refactor this so that it's easier to be sure that these
      -- components are indeed unbuildable.
      --
      -- The reasoning here is that if the STLocalComps specification made it
      -- through component parsing, but the components aren't present, then they
      -- must not be buildable.
    , unbuildable = toComponents
        (exes `Set.difference` buildableExes pkg)
        (tests `Set.difference` buildableTestSuites pkg)
        (benches `Set.difference` buildableBenchmarks pkg)
    }

-- | Compare the current filesystem state to the cached information, and
-- determine (1) if the files are dirty, and (2) the new cache values.
checkBuildCache ::
     HasEnvConfig env
  => Map FilePath FileCacheInfo -- ^ old cache
  -> [Path Abs File] -- ^ files in package
  -> RIO env (Set FilePath, Map FilePath FileCacheInfo)
checkBuildCache oldCache files = do
  fileDigests <- fmap Map.fromList $ forM files $ \fp -> do
    mdigest <- getFileDigestMaybe (toFilePath fp)
    pure (toFilePath fp, mdigest)
  fmap (mconcat . Map.elems) $ sequence $
    Map.merge
      (Map.mapMissing (\fp mdigest -> go fp mdigest Nothing))
      (Map.mapMissing (\fp fci -> go fp Nothing (Just fci)))
      (Map.zipWithMatched (\fp mdigest fci -> go fp mdigest (Just fci)))
      fileDigests
      oldCache
 where
  go :: FilePath
     -> Maybe SHA256
     -> Maybe FileCacheInfo
     -> RIO env (Set FilePath, Map FilePath FileCacheInfo)
  -- Filter out the cabal_macros file to avoid spurious recompilations
  go fp _ _ | takeFileName fp == "cabal_macros.h" = pure (Set.empty, Map.empty)
  -- Common case where it's in the cache and on the filesystem.
  go fp (Just digest') (Just fci)
      | fci.fciHash == digest' = pure (Set.empty, Map.singleton fp fci)
      | otherwise =
          pure (Set.singleton fp, Map.singleton fp $ FileCacheInfo digest')
  -- Missing file. Add it to dirty files, but no FileCacheInfo.
  go fp Nothing _ = pure (Set.singleton fp, Map.empty)
  -- Missing cache. Add it to dirty files and compute FileCacheInfo.
  go fp (Just digest') Nothing =
    pure (Set.singleton fp, Map.singleton fp $ FileCacheInfo digest')

-- | Returns entries to add to the build cache for any newly found unlisted
-- modules
addUnlistedToBuildCache ::
     HasEnvConfig env
  => Package
  -> Path Abs File
  -> Set NamedComponent
  -> Map NamedComponent (Map FilePath a)
  -> RIO env (Map NamedComponent [Map FilePath FileCacheInfo], [PackageWarning])
addUnlistedToBuildCache pkg cabalFP nonLibComponents buildCaches = do
  (componentFiles, warnings) <-
    getPackageFilesForTargets pkg cabalFP nonLibComponents
  results <- forM (M.toList componentFiles) $ \(component, files) -> do
    let buildCache = M.findWithDefault M.empty component buildCaches
        newFiles =
            Set.toList $
            Set.map toFilePath files `Set.difference` Map.keysSet buildCache
    addBuildCache <- mapM addFileToCache newFiles
    pure ((component, addBuildCache), warnings)
  pure (M.fromList (map fst results), concatMap snd results)
 where
  addFileToCache fp = do
    mdigest <- getFileDigestMaybe fp
    case mdigest of
      Nothing -> pure Map.empty
      Just digest' -> pure $ Map.singleton fp $ FileCacheInfo digest'

-- | Gets list of Paths for files relevant to a set of components in a package.
-- Note that the library component, if any, is always automatically added to the
-- set of components.
getPackageFilesForTargets ::
     HasEnvConfig env
  => Package
  -> Path Abs File
  -> Set NamedComponent
  -> RIO env (Map NamedComponent (Set (Path Abs File)), [PackageWarning])
getPackageFilesForTargets pkg cabalFP nonLibComponents = do
  PackageComponentFile components' compFiles otherFiles warnings <-
    getPackageFile pkg cabalFP
  let necessaryComponents =
        Set.insert CLib $ Set.filter isCSubLib (M.keysSet components')
      components = necessaryComponents `Set.union` nonLibComponents
      componentsFiles = M.map
        (\files ->
           Set.union otherFiles (Set.map dotCabalGetPath $ Set.fromList files)
        )
        $ M.filterWithKey (\component _ -> component `elem` components) compFiles
  pure (componentsFiles, warnings)

-- | Get file digest, if it exists
getFileDigestMaybe :: HasEnvConfig env => FilePath -> RIO env (Maybe SHA256)
getFileDigestMaybe fp = do
  cache <- view $ envConfigL . to (.envConfigFileDigestCache)
  catch
    (Just <$> readFileDigest cache fp)
    (\e -> if isDoesNotExistError e then pure Nothing else throwM e)

-- | Get 'PackageConfig' for package given its name.
getPackageConfig ::
     (HasBuildConfig env, HasSourceMap env)
  => Map FlagName Bool
  -> [Text] -- ^ GHC options
  -> [Text] -- ^ cabal config opts
  -> RIO env PackageConfig
getPackageConfig flags ghcOptions cabalConfigOpts = do
  platform <- view platformL
  compilerVersion <- view actualCompilerVersionL
  pure PackageConfig
    { enableTests = False
    , enableBenchmarks = False
    , flags = flags
    , ghcOptions = ghcOptions
    , cabalConfigOpts = cabalConfigOpts
    , compilerVersion = compilerVersion
    , platform = platform
    }
