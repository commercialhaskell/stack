{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ConstraintKinds #-}
-- Load information on package sources
module Stack.Build.Source
    ( localPackages
    , loadSourceMap'
    , loadSourceMap
    , loadSourceMapFull
    , getLocalFlags
    , getGhcOptions
    , addUnlistedToBuildCache
    ) where

import              Stack.Prelude
import qualified    Pantry.SHA256 as SHA256
import qualified    Data.ByteString as S
import              Conduit (ZipSink (..), withSourceFile)
import qualified    Data.Conduit.List as CL
import              Data.List
import qualified    Data.Map as Map
import qualified    Data.Map.Strict as M
import qualified    Data.Set as Set
import              Foreign.C.Types (CTime)
import              Stack.Build.Cache
import              Stack.Build.Target
import              Stack.Constants (wiredInPackages)
import              Stack.Package
import              Stack.SourceMap
import              Stack.Types.Build
import              Stack.Types.BuildPlan
import              Stack.Types.Config
import              Stack.Types.NamedComponent
import              Stack.Types.Package
import              Stack.Types.SourceMap
import              System.FilePath (takeFileName)
import              System.IO.Error (isDoesNotExistError)
import              System.PosixCompat.Files (modificationTime, getFileStatus)

-- FIXME:qrilka move to a better place?
localPackages :: HasEnvConfig env
              => Map PackageName PackageSource
              -> RIO env [LocalPackage]
localPackages pkgSources = do
    prjPackages <- view $ envConfigL . to (smaProject . envConfigSMActual)
    let maybeToLocal (PSFilePath lp _) = Just lp
        maybeToLocal _ = Nothing
    return . mapMaybe maybeToLocal $
        Map.elems (M.restrictKeys pkgSources (M.keysSet prjPackages))

loadSourceMap' :: HasEnvConfig env
               => SMTargets
               -> BuildOptsCLI
               -> RIO env (Map PackageName PackageSource)
loadSourceMap' smt boptsCli = do
  bconfig <- view buildConfigL
  sma <- view $ envConfigL.to envConfigSMActual
  let targets' = smtTargets smt
  locals <- mapM (loadLocalPackage True boptsCli targets') $ Map.toList (smaProject sma)
  let goDepPackage nm dp =
          let common = dpCommon dp
          in case dpLocation dp of
            PLImmutable pkgloc -> do
              ident <- getPackageLocationIdent pkgloc
              let configOpts = getGhcOptions bconfig boptsCli nm False False
              return $ PSRemote Snap (cpFlags common) configOpts pkgloc ident
            PLMutable dir -> do
              pp <- mkProjectPackage YesPrintWarnings dir
              lp' <- loadLocalPackage False boptsCli targets' (nm, pp)
              return $ PSFilePath lp' Local
  packageSources' <- Map.unions <$> sequence
      [ return $ Map.fromList $ map (\lp' -> (packageName $ lpPackage lp', PSFilePath lp' Local)) locals
      , sequence $ Map.mapWithKey goDepPackage (smtDeps smt)
      ]
  let packageSources = packageSources'
            `Map.difference` Map.fromList (map (, ()) (toList wiredInPackages))
  return packageSources

-- | Like 'loadSourceMapFull', but doesn't return values that aren't as
-- commonly needed.
loadSourceMap :: HasEnvConfig env
              => NeedTargets
              -> BuildOptsCLI
              -> RIO env ([LocalPackage], Map PackageName PackageSource) -- FIXME:qrilka SourceMap)
loadSourceMap needTargets boptsCli = do
    (_, _, locals, _, sourceMap) <- loadSourceMapFull needTargets boptsCli
    return (locals, sourceMap)

-- | Given the build commandline options, does the following:
--
-- * Parses the build targets.
--
-- * Loads the 'LoadedSnapshot' from the resolver, with extra-deps
--   shadowing any packages that should be built locally.
--
-- * Loads up the 'LocalPackage' info.
--
-- * Builds a 'SourceMap', which contains info for all the packages that
--   will be involved in the build.
loadSourceMapFull :: HasEnvConfig env
                  => NeedTargets
                  -> BuildOptsCLI
                  -> RIO env
                       ( Map PackageName Target
                       , LoadedSnapshot
                       , [LocalPackage] -- FIXME do we really want this? it's in the SourceMap
                       , Set PackageName -- non-project targets
                       , Map PackageName PackageSource -- FIXME:qrilka SourceMap
                       )
loadSourceMapFull needTargets boptsCli = do
    bconfig <- view buildConfigL
    (ls, localDeps, targets) <- parseTargets needTargets boptsCli
    packages <- view $ buildConfigL.to (error "could be smwProject but this code should be removed" . bcSMWanted)
    locals <- mapM (loadLocalPackage True boptsCli targets) $ Map.toList packages
    checkFlagsUsed boptsCli locals localDeps (lsPackages ls)
    checkComponentsBuildable locals

    -- TODO for extra sanity, confirm that the targets we threw away are all TargetAll
    let nonProjectTargets = Map.keysSet targets `Set.difference` Map.keysSet packages

    -- Combine the local packages, extra-deps, and LoadedSnapshot into
    -- one unified source map.
    let goLPI loc n lpi = do
          let configOpts = getGhcOptions bconfig boptsCli n False False
          case lpiLocation lpi of
            -- NOTE: configOpts includes lpiGhcOptions for now, this may get refactored soon
            PLImmutable pkgloc -> do
              ident <- getPackageLocationIdent pkgloc
              return $ PSRemote loc (lpiFlags lpi) configOpts pkgloc ident
            PLMutable dir -> do -- FIXME this is not correct, we don't want to treat all Mutable as local
              pp <- error "mkProjectPackage YesPrintWarnings dir"
              lp' <- loadLocalPackage False boptsCli targets (n, pp)
              return $ PSFilePath lp' loc
    sourceMap' <- Map.unions <$> sequence
      [ return $ Map.fromList $ map (\lp' -> (packageName $ lpPackage lp', PSFilePath lp' Local)) locals
      , sequence $ Map.mapWithKey (goLPI Local) localDeps
      , sequence $ Map.mapWithKey (goLPI Snap) (lsPackages ls)
      ]
    let sourceMap = sourceMap'
            `Map.difference` Map.fromList (map (, ()) (toList wiredInPackages))

    return
      ( targets
      , ls
      , locals
      , nonProjectTargets
      , sourceMap
      )

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
getGhcOptions :: BuildConfig -> BuildOptsCLI -> PackageName -> Bool -> Bool -> [Text]
getGhcOptions bconfig boptsCli name isTarget isLocal = concat
    [ Map.findWithDefault [] AGOEverything (configGhcOptionsByCat config)
    , if isLocal
        then Map.findWithDefault [] AGOLocals (configGhcOptionsByCat config)
        else []
    , if isTarget
        then Map.findWithDefault [] AGOTargets (configGhcOptionsByCat config)
        else []
    , Map.findWithDefault [] name (configGhcOptionsByName config)
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

-- | Upgrade the initial local package info to a full-blown @LocalPackage@
-- based on the selected components
loadLocalPackage
    :: forall env. HasEnvConfig env
    => Bool
    -- ^ Should this be treated as part of $locals? False for extra-deps.
    --
    -- See: https://github.com/commercialhaskell/stack/issues/3574#issuecomment-346512821
    -> BuildOptsCLI
    -> Map PackageName Target
    -> (PackageName, ProjectPackage)
    -> RIO env LocalPackage
loadLocalPackage isLocal boptsCli targets (name, pp) = do
    let mtarget = Map.lookup name targets
    config  <- getPackageConfig boptsCli name (isJust mtarget) isLocal
    bopts <- view buildOptsL
    mcurator <- view $ buildConfigL.to bcCurator
    gpkg <- ppGPD pp
    let (exeCandidates, testCandidates, benchCandidates) =
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

-- | Ensure that the flags specified in the stack.yaml file and on the command
-- line are used.
checkFlagsUsed :: (MonadThrow m, MonadReader env m, HasBuildConfig env)
               => BuildOptsCLI
               -> [LocalPackage]
               -> Map PackageName (LoadedPackageInfo PackageLocation) -- ^ local deps
               -> Map PackageName snapshot -- ^ snapshot, for error messages
               -> m ()
checkFlagsUsed boptsCli lps extraDeps snapshot = do
    bconfig <- view buildConfigL

        -- Check if flags specified in stack.yaml and the command line are
        -- used, see https://github.com/commercialhaskell/stack/issues/617
    let flags = map (, FSCommandLine) [(k, v) | (ACFByName k, v) <- Map.toList $ boptsCLIFlags boptsCli]
             ++ map (, FSStackYaml) (Map.toList $ error "bcFlags" bconfig)

        localNameMap = Map.fromList $ map (packageName . lpPackage &&& lpPackage) lps
        checkFlagUsed ((name, userFlags), source) =
            case Map.lookup name localNameMap of
                -- Package is not available locally
                Nothing ->
                    if Map.member name extraDeps
                        -- We don't check for flag presence for extra deps
                        then Nothing
                        -- Also not in extra-deps, it's an error
                        else
                            case Map.lookup name snapshot of
                                Nothing -> Just $ UFNoPackage source name
                                Just _ -> Just $ UFSnapshot name
                -- Package exists locally, let's check if the flags are defined
                Just pkg ->
                    let unused = Set.difference (Map.keysSet userFlags) (packageDefinedFlags pkg)
                     in if Set.null unused
                            -- All flags are defined, nothing to do
                            then Nothing
                            -- Error about the undefined flags
                            else Just $ UFFlagsNotDefined source pkg unused

        unusedFlags = mapMaybe checkFlagUsed flags

    unless (null unusedFlags)
        $ throwM
        $ InvalidFlagSpecification
        $ Set.fromList unusedFlags

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
            M.map (\files -> Set.union otherFiles (Set.map dotCabalGetPath files)) $
                M.filterWithKey (\component _ -> component `Set.member` components) compFiles
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

checkComponentsBuildable :: MonadThrow m => [LocalPackage] -> m ()
checkComponentsBuildable lps =
    unless (null unbuildable) $ throwM $ SomeTargetsNotBuildable unbuildable
  where
    unbuildable =
        [ (packageName (lpPackage lp), c)
        | lp <- lps
        , c <- Set.toList (lpUnbuildable lp)
        ]

-- | Get 'PackageConfig' for package given its name.
getPackageConfig :: (MonadIO m, MonadReader env m, HasEnvConfig env)
  => BuildOptsCLI
  -> PackageName
  -> Bool
  -> Bool
  -> m PackageConfig
getPackageConfig boptsCli name isTarget isLocal = do
  bconfig <- view buildConfigL
  platform <- view platformL
  compilerVersion <- view actualCompilerVersionL
  return PackageConfig
    { packageConfigEnableTests = False
    , packageConfigEnableBenchmarks = False
    , packageConfigFlags = getLocalFlags boptsCli name
    , packageConfigGhcOptions = getGhcOptions bconfig boptsCli name isTarget isLocal
    , packageConfigCompilerVersion = compilerVersion
    , packageConfigPlatform = platform
    }
