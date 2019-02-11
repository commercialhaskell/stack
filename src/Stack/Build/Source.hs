{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ConstraintKinds #-}
-- Load information on package sources
module Stack.Build.Source
    ( projectLocalPackages
    , localDependencies
    , loadCommonPackage
    , loadLocalPackage
    , loadSourceMap
    , getLocalFlags
    , addUnlistedToBuildCache
    ) where

import              Stack.Prelude
import qualified    Pantry.SHA256 as SHA256
import qualified    Data.ByteString as S
import              Conduit (ZipSink (..), withSourceFile)
import qualified    Data.Conduit.List as CL
import qualified    Distribution.PackageDescription as C
import              Data.List
import qualified    Data.Map as Map
import qualified    Data.Map.Strict as M
import qualified    Data.Set as Set
import qualified    Data.Text as T
import              Foreign.C.Types (CTime)
import              Stack.Build.Cache
import              Stack.Build.Haddock (shouldHaddockDeps)
import              Stack.Build.Target
import              Stack.Package
import              Stack.SourceMap
import              Stack.Types.Build
import              Stack.Types.Compiler (whichCompiler, WhichCompiler(..))
import              Stack.Types.Config
import              Stack.Types.NamedComponent
import              Stack.Types.Package
import              Stack.Types.SourceMap
import              System.FilePath (takeFileName)
import              System.IO.Error (isDoesNotExistError)
import              System.PosixCompat.Files (modificationTime, getFileStatus)
import qualified    RIO.ByteString as B
import qualified    RIO.ByteString.Lazy as BL
import              RIO.Process (proc, readProcess_)

-- | loads and returns project packages
projectLocalPackages :: HasEnvConfig env
              => RIO env [LocalPackage]
projectLocalPackages = do
    sm <- view $ envConfigL.to envConfigSourceMap
    for (toList $ smProject sm) $ loadLocalPackage sm

-- | loads all local dependencies - project packages and local extra-deps
localDependencies :: HasEnvConfig env => RIO env [LocalPackage]
localDependencies = do
    bopts <- view $ configL.to configBuild
    sourceMap <- view $ envConfigL . to envConfigSourceMap
    forMaybeM (Map.elems $ smDeps sourceMap) $ \dp ->
        case dpLocation dp of
            PLMutable dir -> do
                pp <- mkProjectPackage YesPrintWarnings dir (shouldHaddockDeps bopts)
                Just <$> loadLocalPackage sourceMap pp
            _ -> return Nothing

-- | Given the parsed targets and buld command line options constructs
--   a source map
loadSourceMap :: HasBuildConfig env
              => SMTargets
              -> BuildOptsCLI
              -> SMActual DumpedGlobalPackage
              -> RIO env SourceMap
loadSourceMap smt boptsCli sma = do
    bconfig <- view buildConfigL
    let compiler = smaCompiler sma
        project = M.map applyOptsFlagsPP $ smaProject sma
        bopts = configBuild (bcConfig bconfig)
        applyOptsFlagsPP p@ProjectPackage{ppCommon = c} =
          p{ppCommon = applyOptsFlags (M.member (cpName c) (smtTargets smt)) True c}
        deps0 = smtDeps smt <> smaDeps sma
        deps = M.map applyOptsFlagsDep deps0
        applyOptsFlagsDep d@DepPackage{dpCommon = c} =
          d{dpCommon = applyOptsFlags (M.member (cpName c) (smtDeps smt)) False c}
        applyOptsFlags isTarget isProjectPackage common =
            let name = cpName common
                flags = getLocalFlags boptsCli name
                ghcOptions =
                  generalGhcOptions bconfig boptsCli isTarget isProjectPackage
            in common
               { cpFlags =
                     if M.null flags
                         then cpFlags common
                         else flags
               , cpGhcOptions =
                     ghcOptions ++ cpGhcOptions common
               , cpHaddocks =
                     if isTarget
                         then boptsHaddock bopts
                         else shouldHaddockDeps bopts
               }
        packageCliFlags = Map.fromList $
          mapMaybe maybeProjectFlags $
          Map.toList (boptsCLIFlags boptsCli)
        maybeProjectFlags (ACFByName name, fs) = Just (name, fs)
        maybeProjectFlags _ = Nothing
        globals = pruneGlobals (smaGlobal sma) (Map.keysSet deps)
    checkFlagsUsedThrowing packageCliFlags FSCommandLine project deps
    smh <- hashSourceMapData (whichCompiler compiler) deps
    return
        SourceMap
        { smTargets = smt
        , smCompiler = compiler
        , smProject = project
        , smDeps = deps
        , smGlobal = globals
        , smHash = smh
        }

-- | Get a 'SourceMapHash' for a given 'SourceMap'
--
-- Basic rules:
--
-- * If someone modifies a GHC installation in any way after Stack
--   looks at it, they voided the warranty. This includes installing a
--   brand new build to the same directory, or registering new
--   packages to the global database.
--
-- * We should include everything in the hash that would relate to
--   immutable packages and identifying the compiler itself. Mutable
--   packages (both project packages and dependencies) will never make
--   it into the snapshot database, and can be ignored.
--
-- * Target information is only relevant insofar as it effects the
--   dependency map. The actual current targets for this build are
--   irrelevant to the cache mechanism, and can be ignored.
--
-- * Make sure things like profiling and haddocks are included in the hash
--
hashSourceMapData
    :: (HasConfig env)
    => WhichCompiler
    -> Map PackageName DepPackage
    -> RIO env SourceMapHash
hashSourceMapData wc smDeps = do
    path <- encodeUtf8 . T.pack . toFilePath <$> getCompilerPath wc
    let compilerExe =
            case wc of
                Ghc -> "ghc"
                Ghcjs -> "ghcjs"
    info <- BL.toStrict . fst <$> proc compilerExe ["--info"] readProcess_
    immDeps <- forM (Map.elems smDeps) depPackageHashableContent
    return $ SourceMapHash (SHA256.hashLazyBytes $ BL.fromChunks (path:info:immDeps))

depPackageHashableContent :: (HasConfig env) => DepPackage -> RIO env ByteString
depPackageHashableContent DepPackage {..} = do
    case dpLocation of
        PLMutable _ -> return ""
        PLImmutable pli -> do
            let flagToBs (f, enabled) =
                    if enabled
                        then ""
                        else "-" <> encodeUtf8 (T.pack $ C.unFlagName f)
                flags = map flagToBs $ Map.toList (cpFlags dpCommon)
                locationTreeKey (PLIHackage _ _ tk) = tk
                locationTreeKey (PLIArchive _ pm) = pmTreeKey pm
                locationTreeKey (PLIRepo _ pm) = pmTreeKey pm
                treeKeyToBs (TreeKey (BlobKey sha _)) = SHA256.toHexBytes sha
                ghcOptions = map encodeUtf8 (cpGhcOptions dpCommon)
                haddocks = if cpHaddocks dpCommon then "haddocks" else ""
                hash = treeKeyToBs $ locationTreeKey pli
            return $ B.concat ([hash, haddocks] ++ flags ++ ghcOptions)

-- | All flags for a local package.
getLocalFlags
    :: BuildOptsCLI
    -> PackageName
    -> Map FlagName Bool
getLocalFlags boptsCli name = Map.unions
    [ Map.findWithDefault Map.empty (ACFByName name) cliFlags
    , Map.findWithDefault Map.empty ACFAllProjectPackages cliFlags
    ]
  where
    cliFlags = boptsCLIFlags boptsCli

-- | Get the configured options to pass from GHC, based on the build
-- configuration and commandline.
generalGhcOptions :: BuildConfig -> BuildOptsCLI -> Bool -> Bool -> [Text]
generalGhcOptions bconfig boptsCli isTarget isLocal = concat
    [ Map.findWithDefault [] AGOEverything (configGhcOptionsByCat config)
    , if isLocal
        then Map.findWithDefault [] AGOLocals (configGhcOptionsByCat config)
        else []
    , if isTarget
        then Map.findWithDefault [] AGOTargets (configGhcOptionsByCat config)
        else []
    , concat [["-fhpc"] | isLocal && toCoverage (boptsTestOpts bopts)]
    , if boptsLibProfile bopts || boptsExeProfile bopts
         then ["-fprof-auto","-fprof-cafs"]
         else []
    , if not $ boptsLibStrip bopts || boptsExeStrip bopts
         then ["-g"]
         else []
    , if includeExtraOptions
         then boptsCLIGhcOptions boptsCli
         else []
    ]
  where
    bopts = configBuild config
    config = view configL bconfig
    includeExtraOptions =
        case configApplyGhcOptions config of
            AGOTargets -> isTarget
            AGOLocals -> isLocal
            AGOEverything -> True

splitComponents :: [NamedComponent]
                -> (Set Text, Set Text, Set Text)
splitComponents =
    go id id id
  where
    go a b c [] = (Set.fromList $ a [], Set.fromList $ b [], Set.fromList $ c [])
    go a b c (CLib:xs) = go a b c xs
    go a b c (CInternalLib x:xs) = go (a . (x:)) b c xs
    go a b c (CExe x:xs) = go (a . (x:)) b c xs
    go a b c (CTest x:xs) = go a (b . (x:)) c xs
    go a b c (CBench x:xs) = go a b (c . (x:)) xs

loadCommonPackage ::
       forall env. HasEnvConfig env
    => CommonPackage
    -> RIO env Package
loadCommonPackage common = do
    config <- getPackageConfig (cpFlags common) (cpGhcOptions common)
    gpkg <- liftIO $ cpGPD common
    return $ resolvePackage config gpkg

-- | Upgrade the initial project package info to a full-blown @LocalPackage@
-- based on the selected components
loadLocalPackage ::
       forall env. HasEnvConfig env
    => SourceMap
    -> ProjectPackage
    -> RIO env LocalPackage
loadLocalPackage sm pp = do
    let common = ppCommon pp
    bopts <- view buildOptsL
    mcurator <- view $ buildConfigL.to bcCurator
    config <- getPackageConfig (cpFlags common) (cpGhcOptions common)
    gpkg <- ppGPD pp
    let name = cpName common
        mtarget = M.lookup name (smtTargets $ smTargets sm)
        (exeCandidates, testCandidates, benchCandidates) =
            case mtarget of
                Just (TargetComps comps) -> splitComponents $ Set.toList comps
                Just (TargetAll _packageType) ->
                    ( packageExes pkg
                    , if boptsTests bopts && maybe True (Set.notMember name . curatorSkipTest) mcurator
                        then Map.keysSet (packageTests pkg)
                        else Set.empty
                    , if boptsBenchmarks bopts && maybe True (Set.notMember name . curatorSkipBenchmark) mcurator
                        then packageBenchmarks pkg
                        else Set.empty
                    )
                Nothing -> mempty

        -- See https://github.com/commercialhaskell/stack/issues/2862
        isWanted = case mtarget of
            Nothing -> False
            -- FIXME: When issue #1406 ("stack 0.1.8 lost ability to
            -- build individual executables or library") is resolved,
            -- 'hasLibrary' is only relevant if the library is
            -- part of the target spec.
            Just _ ->
              let hasLibrary =
                    case packageLibraries pkg of
                      NoLibraries -> False
                      HasLibraries _ -> True
               in hasLibrary
               || not (Set.null nonLibComponents)
               || not (Set.null $ packageInternalLibraries pkg)

        filterSkippedComponents = Set.filter (not . (`elem` boptsSkipComponents bopts))

        (exes, tests, benches) = (filterSkippedComponents exeCandidates,
                                  filterSkippedComponents testCandidates,
                                  filterSkippedComponents benchCandidates)

        nonLibComponents = toComponents exes tests benches

        toComponents e t b = Set.unions
            [ Set.map CExe e
            , Set.map CTest t
            , Set.map CBench b
            ]

        btconfig = config
            { packageConfigEnableTests = not $ Set.null tests
            , packageConfigEnableBenchmarks = not $ Set.null benches
            }
        testconfig = config
            { packageConfigEnableTests = True
            , packageConfigEnableBenchmarks = False
            }
        benchconfig = config
            { packageConfigEnableTests = False
            , packageConfigEnableBenchmarks = True
            }

        -- We resolve the package in 4 different configurations:
        --
        -- - pkg doesn't have tests or benchmarks enabled.
        --
        -- - btpkg has them enabled if they are present.
        --
        -- - testpkg has tests enabled, but not benchmarks.
        --
        -- - benchpkg has benchmarks enablde, but not tests.
        --
        -- The latter two configurations are used to compute the deps
        -- when --enable-benchmarks or --enable-tests are configured.
        -- This allows us to do an optimization where these are passed
        -- if the deps are present. This can avoid doing later
        -- unnecessary reconfigures.
        pkg = resolvePackage config gpkg
        btpkg
            | Set.null tests && Set.null benches = Nothing
            | otherwise = Just (resolvePackage btconfig gpkg)
        testpkg = resolvePackage testconfig gpkg
        benchpkg = resolvePackage benchconfig gpkg

    componentFiles <- memoizeRef $ fst <$> getPackageFilesForTargets pkg (ppCabalFP pp) nonLibComponents

    checkCacheResults <- memoizeRef $ do
      componentFiles' <- runMemoized componentFiles
      forM (Map.toList componentFiles') $ \(component, files) -> do
        mbuildCache <- tryGetBuildCache (ppRoot pp) component
        checkCacheResult <- checkBuildCache
            (fromMaybe Map.empty mbuildCache)
            (Set.toList files)
        return (component, checkCacheResult)

    let dirtyFiles = do
          checkCacheResults' <- checkCacheResults
          let allDirtyFiles = Set.unions $ map (\(_, (x, _)) -> x) checkCacheResults'
          pure $
            if not (Set.null allDirtyFiles)
                then let tryStripPrefix y =
                          fromMaybe y (stripPrefix (toFilePath $ ppRoot pp) y)
                      in Just $ Set.map tryStripPrefix allDirtyFiles
                else Nothing
        newBuildCaches =
            M.fromList . map (\(c, (_, cache)) -> (c, cache))
            <$> checkCacheResults

    return LocalPackage
        { lpPackage = pkg
        , lpTestDeps = dvVersionRange <$> packageDeps testpkg
        , lpBenchDeps = dvVersionRange <$> packageDeps benchpkg
        , lpTestBench = btpkg
        , lpComponentFiles = componentFiles
        , lpBuildHaddocks = cpHaddocks (ppCommon pp)
        , lpForceDirty = boptsForceDirty bopts
        , lpDirtyFiles = dirtyFiles
        , lpNewBuildCaches = newBuildCaches
        , lpCabalFile = ppCabalFP pp
        , lpWanted = isWanted
        , lpComponents = nonLibComponents
        -- TODO: refactor this so that it's easier to be sure that these
        -- components are indeed unbuildable.
        --
        -- The reasoning here is that if the STLocalComps specification
        -- made it through component parsing, but the components aren't
        -- present, then they must not be buildable.
        , lpUnbuildable = toComponents
            (exes `Set.difference` packageExes pkg)
            (tests `Set.difference` Map.keysSet (packageTests pkg))
            (benches `Set.difference` packageBenchmarks pkg)
        }

-- | Compare the current filesystem state to the cached information, and
-- determine (1) if the files are dirty, and (2) the new cache values.
checkBuildCache :: forall m. (MonadIO m)
                => Map FilePath FileCacheInfo -- ^ old cache
                -> [Path Abs File] -- ^ files in package
                -> m (Set FilePath, Map FilePath FileCacheInfo)
checkBuildCache oldCache files = do
    fileTimes <- liftM Map.fromList $ forM files $ \fp -> do
        mmodTime <- liftIO (getModTimeMaybe (toFilePath fp))
        return (toFilePath fp, mmodTime)
    liftM (mconcat . Map.elems) $ sequence $
        Map.mergeWithKey
            (\fp mmodTime fci -> Just (go fp mmodTime (Just fci)))
            (Map.mapWithKey (\fp mmodTime -> go fp mmodTime Nothing))
            (Map.mapWithKey (\fp fci -> go fp Nothing (Just fci)))
            fileTimes
            oldCache
  where
    go :: FilePath -> Maybe CTime -> Maybe FileCacheInfo -> m (Set FilePath, Map FilePath FileCacheInfo)
    -- Filter out the cabal_macros file to avoid spurious recompilations
    go fp _ _ | takeFileName fp == "cabal_macros.h" = return (Set.empty, Map.empty)
    -- Common case where it's in the cache and on the filesystem.
    go fp (Just modTime') (Just fci)
        | fciModTime fci == modTime' = return (Set.empty, Map.singleton fp fci)
        | otherwise = do
            newFci <- calcFci modTime' fp
            let isDirty =
                    fciSize fci /= fciSize newFci ||
                    fciHash fci /= fciHash newFci
                newDirty = if isDirty then Set.singleton fp else Set.empty
            return (newDirty, Map.singleton fp newFci)
    -- Missing file. Add it to dirty files, but no FileCacheInfo.
    go fp Nothing _ = return (Set.singleton fp, Map.empty)
    -- Missing cache. Add it to dirty files and compute FileCacheInfo.
    go fp (Just modTime') Nothing = do
        newFci <- calcFci modTime' fp
        return (Set.singleton fp, Map.singleton fp newFci)

-- | Returns entries to add to the build cache for any newly found unlisted modules
addUnlistedToBuildCache
    :: HasEnvConfig env
    => CTime
    -> Package
    -> Path Abs File
    -> Set NamedComponent
    -> Map NamedComponent (Map FilePath a)
    -> RIO env (Map NamedComponent [Map FilePath FileCacheInfo], [PackageWarning])
addUnlistedToBuildCache preBuildTime pkg cabalFP nonLibComponents buildCaches = do
    (componentFiles, warnings) <- getPackageFilesForTargets pkg cabalFP nonLibComponents
    results <- forM (M.toList componentFiles) $ \(component, files) -> do
        let buildCache = M.findWithDefault M.empty component buildCaches
            newFiles =
                Set.toList $
                Set.map toFilePath files `Set.difference` Map.keysSet buildCache
        addBuildCache <- mapM addFileToCache newFiles
        return ((component, addBuildCache), warnings)
    return (M.fromList (map fst results), concatMap snd results)
  where
    addFileToCache fp = do
        mmodTime <- getModTimeMaybe fp
        case mmodTime of
            Nothing -> return Map.empty
            Just modTime' ->
                if modTime' < preBuildTime
                    then Map.singleton fp <$> calcFci modTime' fp
                    else return Map.empty

-- | Gets list of Paths for files relevant to a set of components in a package.
--   Note that the library component, if any, is always automatically added to the
--   set of components.
getPackageFilesForTargets
    :: HasEnvConfig env
    => Package
    -> Path Abs File
    -> Set NamedComponent
    -> RIO env (Map NamedComponent (Set (Path Abs File)), [PackageWarning])
getPackageFilesForTargets pkg cabalFP nonLibComponents = do
    (components',compFiles,otherFiles,warnings) <-
        getPackageFiles (packageFiles pkg) cabalFP
    let necessaryComponents = Set.insert CLib $ Set.filter isCInternalLib (M.keysSet components')
        components = necessaryComponents `Set.union` nonLibComponents
        componentsFiles =
            M.map (\files -> Set.union otherFiles (Set.map dotCabalGetPath $ Set.fromList files)) $
                M.filterWithKey (\component _ -> component `elem` components) compFiles
    return (componentsFiles, warnings)

-- | Get file modification time, if it exists.
getModTimeMaybe :: MonadIO m => FilePath -> m (Maybe CTime)
getModTimeMaybe fp =
    liftIO
        (catch
             (liftM
                  (Just . modificationTime)
                  (getFileStatus fp))
             (\e ->
                   if isDoesNotExistError e
                       then return Nothing
                       else throwM e))

-- | Create FileCacheInfo for a file.
calcFci :: MonadIO m => CTime -> FilePath -> m FileCacheInfo
calcFci modTime' fp = liftIO $
    withSourceFile fp $ \src -> do
        (size, digest) <- runConduit $ src .| getZipSink
            ((,)
                <$> ZipSink (CL.fold
                    (\x y -> x + fromIntegral (S.length y))
                    0)
                <*> ZipSink SHA256.sinkHash)
        return FileCacheInfo
            { fciModTime = modTime'
            , fciSize = FileSize size
            , fciHash = digest
            }

-- | Get 'PackageConfig' for package given its name.
getPackageConfig :: (MonadReader env m, HasEnvConfig env)
  => Map FlagName Bool
  -> [Text]
  -> m PackageConfig
getPackageConfig flags ghcOptions = do
  platform <- view platformL
  compilerVersion <- view actualCompilerVersionL
  return PackageConfig
    { packageConfigEnableTests = False
    , packageConfigEnableBenchmarks = False
    , packageConfigFlags = flags
    , packageConfigGhcOptions = ghcOptions
    , packageConfigCompilerVersion = compilerVersion
    , packageConfigPlatform = platform
    }
