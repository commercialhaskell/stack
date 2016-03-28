{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- Load information on package sources
module Stack.Build.Source
    ( loadSourceMap
    , SourceMap
    , PackageSource (..)
    , localFlags
    , getLocalPackageViews
    , loadLocalPackage
    , parseTargetsFromBuildOpts
    , addUnlistedToBuildCache
    , getPackageConfig
    ) where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Exception (assert, catch)
import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Resource
import "cryptohash" Crypto.Hash (Digest, SHA256)
import           Crypto.Hash.Conduit (sinkHash)
import qualified Data.ByteString as S
import           Data.Byteable (toBytes)
import           Data.Conduit (($$), ZipSink (..))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Either
import           Data.Function
import qualified Data.HashSet as HashSet
import           Data.List
import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Distribution.Package (pkgName, pkgVersion)
import           Distribution.PackageDescription (GenericPackageDescription, package, packageDescription)
import qualified Distribution.PackageDescription as C
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Path.IO
import           Prelude
import           Stack.Build.Cache
import           Stack.Build.Target
import           Stack.BuildPlan (loadMiniBuildPlan, shadowMiniBuildPlan,
                                  parseCustomMiniBuildPlan)
import           Stack.Constants (wiredInPackages)
import           Stack.Package
import           Stack.PackageIndex (getPackageVersions)
import           Stack.Types

import qualified System.Directory as D
import           System.IO (withBinaryFile, IOMode (ReadMode))
import           System.IO.Error (isDoesNotExistError)

loadSourceMap :: (MonadIO m, MonadCatch m, MonadReader env m, HasBuildConfig env, MonadBaseControl IO m, HasHttpManager env, MonadLogger m, HasEnvConfig env)
              => NeedTargets
              -> BuildOptsCLI
              -> m ( Map PackageName SimpleTarget
                   , MiniBuildPlan
                   , [LocalPackage]
                   , Set PackageName -- non-local targets
                   , SourceMap
                   )
loadSourceMap needTargets boptsCli = do
    bconfig <- asks getBuildConfig
    rawLocals <- getLocalPackageViews
    (mbp0, cliExtraDeps, targets) <- parseTargetsFromBuildOpts needTargets boptsCli
    -- Extend extra-deps to encompass targets requested on the command line
    -- that are not in the snapshot.
    extraDeps0 <- extendExtraDeps
        (bcExtraDeps bconfig)
        cliExtraDeps
        (Map.keysSet $ Map.filter (== STUnknown) targets)

    locals <- mapM (loadLocalPackage boptsCli targets) $ Map.toList rawLocals
    checkFlagsUsed boptsCli locals extraDeps0 (mbpPackages mbp0)
    checkComponentsBuildable locals

    let
        -- loadLocals returns PackageName (foo) and PackageIdentifier (bar-1.2.3) targets separately;
        -- here we combine them into nonLocalTargets. This is one of the
        -- return values of this function.
        nonLocalTargets :: Set PackageName
        nonLocalTargets =
            Map.keysSet $ Map.filter (not . isLocal) targets
          where
            isLocal (STLocalComps _) = True
            isLocal STLocalAll = True
            isLocal STUnknown = False
            isLocal STNonLocal = False

        shadowed = Map.keysSet rawLocals <> Map.keysSet extraDeps0
        (mbp, extraDeps1) = shadowMiniBuildPlan mbp0 shadowed

        -- Add the extra deps from the stack.yaml file to the deps grabbed from
        -- the snapshot
        extraDeps2 = Map.union
            (Map.map (\v -> (v, Map.empty)) extraDeps0)
            (Map.map (mpiVersion &&& mpiFlags) extraDeps1)

        -- Overwrite any flag settings with those from the config file
        extraDeps3 = Map.mapWithKey
            (\n (v, f) -> PSUpstream v Local $
                case ( Map.lookup (Just n) $ boptsCLIFlags boptsCli
                     , Map.lookup Nothing $ boptsCLIFlags boptsCli
                     , Map.lookup n $ bcFlags bconfig
                     ) of
                    -- Didn't have any flag overrides, fall back to the flags
                    -- defined in the snapshot.
                    (Nothing, Nothing, Nothing) -> f
                    -- Either command line flag for this package, general
                    -- command line flag, or flag in stack.yaml is defined.
                    -- Take all of those and ignore the snapshot flags.
                    (x, y, z) -> Map.unions
                        [ fromMaybe Map.empty x
                        , fromMaybe Map.empty y
                        , fromMaybe Map.empty z
                        ])
            extraDeps2

    let sourceMap = Map.unions
            [ Map.fromList $ flip map locals $ \lp ->
                let p = lpPackage lp
                 in (packageName p, PSLocal lp)
            , extraDeps3
            , flip fmap (mbpPackages mbp) $ \mpi ->
                PSUpstream (mpiVersion mpi) Snap (mpiFlags mpi)
            ] `Map.difference` Map.fromList (map (, ()) (HashSet.toList wiredInPackages))

    return (targets, mbp, locals, nonLocalTargets, sourceMap)

-- | Use the build options and environment to parse targets.
parseTargetsFromBuildOpts
    :: (MonadIO m, MonadCatch m, MonadReader env m, HasBuildConfig env, MonadBaseControl IO m, HasHttpManager env, MonadLogger m, HasEnvConfig env)
    => NeedTargets
    -> BuildOptsCLI
    -> m (MiniBuildPlan, M.Map PackageName Version, M.Map PackageName SimpleTarget)
parseTargetsFromBuildOpts needTargets boptscli = do
    bconfig <- asks getBuildConfig
    mbp0 <-
        case bcResolver bconfig of
            ResolverSnapshot snapName -> do
                $logDebug $ "Checking resolver: " <> renderSnapName snapName
                loadMiniBuildPlan snapName
            ResolverCompiler _ -> do
                -- We ignore the resolver version, as it might be
                -- GhcMajorVersion, and we want the exact version
                -- we're using.
                version <- asks (envConfigCompilerVersion . getEnvConfig)
                return MiniBuildPlan
                    { mbpCompilerVersion = version
                    , mbpPackages = Map.empty
                    }
            ResolverCustom _ url -> do
                stackYamlFP <- asks $ bcStackYaml . getBuildConfig
                parseCustomMiniBuildPlan stackYamlFP url
    rawLocals <- getLocalPackageViews
    workingDir <- getCurrentDir

    let snapshot = mpiVersion <$> mbpPackages mbp0
    flagExtraDeps <- convertSnapshotToExtra
        snapshot
        (bcExtraDeps bconfig)
        rawLocals
        (catMaybes $ Map.keys $ boptsCLIFlags boptscli)

    (cliExtraDeps, targets) <-
        parseTargets
            needTargets
            (bcImplicitGlobal bconfig)
            snapshot
            (flagExtraDeps <> bcExtraDeps bconfig)
            (fst <$> rawLocals)
            workingDir
            (boptsCLITargets boptscli)
    return (mbp0, cliExtraDeps <> flagExtraDeps, targets)

-- | For every package in the snapshot which is referenced by a flag, give the
-- user a warning and then add it to extra-deps.
convertSnapshotToExtra
    :: MonadLogger m
    => Map PackageName Version -- ^ snapshot
    -> Map PackageName Version -- ^ extra-deps
    -> Map PackageName a -- ^ locals
    -> [PackageName] -- ^ packages referenced by a flag
    -> m (Map PackageName Version)
convertSnapshotToExtra snapshot extra0 locals = go Map.empty
  where
    go !extra [] = return extra
    go extra (flag:flags)
        | Just _ <- Map.lookup flag extra0 = go extra flags
        | flag `Map.member` locals = go extra flags
        | otherwise = case Map.lookup flag snapshot of
            Nothing -> go extra flags
            Just version -> do
                $logWarn $ T.concat
                    [ "- Implicitly adding "
                    , T.pack $ packageNameString flag
                    , " to extra-deps based on command line flag"
                    ]
                go (Map.insert flag version extra) flags

-- | Parse out the local package views for the current project
getLocalPackageViews :: (MonadThrow m, MonadIO m, MonadReader env m, HasEnvConfig env, MonadLogger m)
                     => m (Map PackageName (LocalPackageView, GenericPackageDescription))
getLocalPackageViews = do
    econfig <- asks getEnvConfig
    locals <- forM (Map.toList $ envConfigPackages econfig) $ \(dir, treatLikeExtraDep) -> do
        cabalfp <- findOrGenerateCabalFile dir
        (warnings,gpkg) <- readPackageUnresolved cabalfp
        mapM_ (printCabalFileWarning cabalfp) warnings
        let cabalID = package $ packageDescription gpkg
            name = fromCabalPackageName $ pkgName cabalID
        checkCabalFileName name cabalfp
        let lpv = LocalPackageView
                { lpvVersion = fromCabalVersion $ pkgVersion cabalID
                , lpvRoot = dir
                , lpvCabalFP = cabalfp
                , lpvExtraDep = treatLikeExtraDep
                , lpvComponents = getNamedComponents gpkg
                }
        return (name, (lpv, gpkg))
    checkDuplicateNames locals
    return $ Map.fromList locals
  where
    getNamedComponents gpkg = Set.fromList $ concat
        [ maybe [] (const [CLib]) (C.condLibrary gpkg)
        , go CExe  C.condExecutables
        , go CTest C.condTestSuites
        , go CBench C.condBenchmarks
        ]
      where
        go wrapper f = map (wrapper . T.pack . fst) $ f gpkg

-- | Check if there are any duplicate package names and, if so, throw an
-- exception.
checkDuplicateNames :: MonadThrow m => [(PackageName, (LocalPackageView, gpd))] -> m ()
checkDuplicateNames locals =
    case filter hasMultiples $ Map.toList $ Map.fromListWith (++) $ map toPair locals of
        [] -> return ()
        x -> throwM $ DuplicateLocalPackageNames x
  where
    toPair (pn, (lpv, _)) = (pn, [lpvRoot lpv])
    hasMultiples (_, _:_:_) = True
    hasMultiples _ = False

splitComponents :: [NamedComponent]
                -> (Set Text, Set Text, Set Text)
splitComponents =
    go id id id
  where
    go a b c [] = (Set.fromList $ a [], Set.fromList $ b [], Set.fromList $ c [])
    go a b c (CLib:xs) = go a b c xs
    go a b c (CExe x:xs) = go (a . (x:)) b c xs
    go a b c (CTest x:xs) = go a (b . (x:)) c xs
    go a b c (CBench x:xs) = go a b (c . (x:)) xs

-- | Upgrade the initial local package info to a full-blown @LocalPackage@
-- based on the selected components
loadLocalPackage
    :: forall m env.
       (MonadReader env m, HasEnvConfig env, MonadCatch m, MonadLogger m, MonadIO m)
    => BuildOptsCLI
    -> Map PackageName SimpleTarget
    -> (PackageName, (LocalPackageView, GenericPackageDescription))
    -> m LocalPackage
loadLocalPackage boptsCli targets (name, (lpv, gpkg)) = do
    config  <- getPackageConfig boptsCli name
    bopts <- asks (configBuild . getConfig)
    let pkg = resolvePackage config gpkg

        mtarget = Map.lookup name targets
        (exes, tests, benches) =
            case mtarget of
                Just (STLocalComps comps) -> splitComponents $ Set.toList comps
                Just STLocalAll ->
                    ( packageExes pkg
                    , if boptsTests bopts
                        then Map.keysSet (packageTests pkg)
                        else Set.empty
                    , if boptsBenchmarks bopts
                        then packageBenchmarks pkg
                        else Set.empty
                    )
                Just STNonLocal -> assert False mempty
                Just STUnknown -> assert False mempty
                Nothing -> mempty

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

        btpkg
            | Set.null tests && Set.null benches = Nothing
            | otherwise = Just (resolvePackage btconfig gpkg)
        testpkg = resolvePackage testconfig gpkg
        benchpkg = resolvePackage benchconfig gpkg
    mbuildCache <- tryGetBuildCache $ lpvRoot lpv
    (files,_) <- getPackageFilesSimple pkg (lpvCabalFP lpv)

    -- Filter out the cabal_macros file to avoid spurious recompilations
    let filteredFiles = Set.filter ((/= $(mkRelFile "cabal_macros.h")) . filename) files

    (dirtyFiles, newBuildCache) <- checkBuildCache
        (fromMaybe Map.empty mbuildCache)
        (map toFilePath $ Set.toList filteredFiles)

    return LocalPackage
        { lpPackage = pkg
        , lpTestDeps = packageDeps testpkg
        , lpBenchDeps = packageDeps benchpkg
        , lpTestBench = btpkg
        , lpFiles = files
        , lpForceDirty = boptsForceDirty bopts
        , lpDirtyFiles =
            if not (Set.null dirtyFiles)
                then let tryStripPrefix y =
                          fromMaybe y (stripPrefix (toFilePath $ lpvRoot lpv) y)
                      in Just $ Set.map tryStripPrefix dirtyFiles
                else Nothing
        , lpNewBuildCache = newBuildCache
        , lpCabalFile = lpvCabalFP lpv
        , lpDir = lpvRoot lpv
        , lpWanted = isJust mtarget
        , lpComponents = toComponents exes tests benches
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
               -> Map PackageName extraDeps -- ^ extra deps
               -> Map PackageName snapshot -- ^ snapshot, for error messages
               -> m ()
checkFlagsUsed boptsCli lps extraDeps snapshot = do
    bconfig <- asks getBuildConfig

        -- Check if flags specified in stack.yaml and the command line are
        -- used, see https://github.com/commercialhaskell/stack/issues/617
    let flags = map (, FSCommandLine) [(k, v) | (Just k, v) <- Map.toList $ boptsCLIFlags boptsCli]
             ++ map (, FSStackYaml) (Map.toList $ bcFlags bconfig)

        localNameMap = Map.fromList $ map (packageName . lpPackage &&& lpPackage) lps
        checkFlagUsed ((name, userFlags), source) =
            case Map.lookup name localNameMap of
                -- Package is not available locally
                Nothing ->
                    case Map.lookup name extraDeps of
                        -- Also not in extra-deps, it's an error
                        Nothing ->
                            case Map.lookup name snapshot of
                                Nothing -> Just $ UFNoPackage source name
                                Just _ -> Just $ UFSnapshot name
                        -- We don't check for flag presence for extra deps
                        Just _ -> Nothing
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

-- | All flags for a local package
localFlags :: Map (Maybe PackageName) (Map FlagName Bool)
           -> BuildConfig
           -> PackageName
           -> Map FlagName Bool
localFlags boptsflags bconfig name = Map.unions
    [ Map.findWithDefault Map.empty (Just name) boptsflags
    , Map.findWithDefault Map.empty Nothing boptsflags
    , Map.findWithDefault Map.empty name (bcFlags bconfig)
    ]

-- | Add in necessary packages to extra dependencies
--
-- Originally part of https://github.com/commercialhaskell/stack/issues/272,
-- this was then superseded by
-- https://github.com/commercialhaskell/stack/issues/651
extendExtraDeps
    :: (HasBuildConfig env, MonadIO m, MonadLogger m, MonadReader env m, HasHttpManager env, MonadBaseControl IO m, MonadCatch m)
    => Map PackageName Version -- ^ original extra deps
    -> Map PackageName Version -- ^ package identifiers from the command line
    -> Set PackageName -- ^ all packages added on the command line
    -> m (Map PackageName Version) -- ^ new extradeps
extendExtraDeps extraDeps0 cliExtraDeps unknowns = do
    (errs, unknowns') <- fmap partitionEithers $ mapM addUnknown $ Set.toList unknowns
    case errs of
        [] -> return $ Map.unions $ extraDeps1 : unknowns'
        _ -> do
            bconfig <- asks getBuildConfig
            throwM $ UnknownTargets
                (Set.fromList errs)
                Map.empty -- TODO check the cliExtraDeps for presence in index
                (bcStackYaml bconfig)
  where
    extraDeps1 = Map.union extraDeps0 cliExtraDeps
    addUnknown pn = do
        case Map.lookup pn extraDeps1 of
            Just _ -> return (Right Map.empty)
            Nothing -> do
                mlatestVersion <- getLatestVersion pn
                case mlatestVersion of
                    Just v -> return (Right $ Map.singleton pn v)
                    Nothing -> return (Left pn)
    getLatestVersion pn = do
        vs <- getPackageVersions pn
        return (fmap fst (Set.maxView vs))

-- | Compare the current filesystem state to the cached information, and
-- determine (1) if the files are dirty, and (2) the new cache values.
checkBuildCache :: MonadIO m
                => Map FilePath FileCacheInfo -- ^ old cache
                -> [FilePath] -- ^ files in package
                -> m (Set FilePath, Map FilePath FileCacheInfo)
checkBuildCache oldCache files = liftIO $ do
    (dirtyFiles, m) <- mconcat <$> mapM go files
    return (dirtyFiles, m)
  where
    go fp = do
        mmodTime <- getModTimeMaybe fp
        case mmodTime of
            Nothing -> return (Set.empty, Map.empty)
            Just modTime' -> do
                (isDirty, newFci) <-
                    case Map.lookup fp oldCache of
                        Just fci
                            | fciModTime fci == modTime' -> return (False, fci)
                            | otherwise -> do
                                newFci <- calcFci modTime' fp
                                let isDirty =
                                        fciSize fci /= fciSize newFci ||
                                        fciHash fci /= fciHash newFci
                                return (isDirty, newFci)
                        Nothing -> do
                            newFci <- calcFci modTime' fp
                            return (True, newFci)
                return (if isDirty then Set.singleton fp else Set.empty, Map.singleton fp newFci)

-- | Returns entries to add to the build cache for any newly found unlisted modules
addUnlistedToBuildCache
    :: (MonadIO m, MonadReader env m, MonadCatch m, MonadLogger m, HasEnvConfig env)
    => ModTime
    -> Package
    -> Path Abs File
    -> Map FilePath a
    -> m ([Map FilePath FileCacheInfo], [PackageWarning])
addUnlistedToBuildCache preBuildTime pkg cabalFP buildCache = do
    (files,warnings) <- getPackageFilesSimple pkg cabalFP
    let newFiles =
            Set.toList $
            Set.map toFilePath files `Set.difference` Map.keysSet buildCache
    addBuildCache <- mapM addFileToCache newFiles
    return (addBuildCache, warnings)
  where
    addFileToCache fp = do
        mmodTime <- getModTimeMaybe fp
        case mmodTime of
            Nothing -> return Map.empty
            Just modTime' ->
                if modTime' < preBuildTime
                    then do
                        newFci <- calcFci modTime' fp
                        return (Map.singleton fp newFci)
                    else return Map.empty

-- | Gets list of Paths for files in a package
getPackageFilesSimple
    :: (MonadIO m, MonadReader env m, MonadCatch m, MonadLogger m, HasEnvConfig env)
    => Package -> Path Abs File -> m (Set (Path Abs File), [PackageWarning])
getPackageFilesSimple pkg cabalFP = do
    (_,compFiles,cabalFiles,warnings) <-
        getPackageFiles (packageFiles pkg) cabalFP
    return
        ( Set.map dotCabalGetPath (mconcat (M.elems compFiles)) <> cabalFiles
        , warnings)

-- | Get file modification time, if it exists.
getModTimeMaybe :: MonadIO m => FilePath -> m (Maybe ModTime)
getModTimeMaybe fp =
    liftIO
        (catch
             (liftM
                  (Just . modTime)
                  (D.getModificationTime fp))
             (\e ->
                   if isDoesNotExistError e
                       then return Nothing
                       else throwM e))

-- | Create FileCacheInfo for a file.
calcFci :: MonadIO m => ModTime -> FilePath -> m FileCacheInfo
calcFci modTime' fp = liftIO $
    withBinaryFile fp ReadMode $ \h -> do
        (size, digest) <- CB.sourceHandle h $$ getZipSink
            ((,)
                <$> ZipSink (CL.fold
                    (\x y -> x + fromIntegral (S.length y))
                    0)
                <*> ZipSink sinkHash)
        return FileCacheInfo
            { fciModTime = modTime'
            , fciSize = size
            , fciHash = toBytes (digest :: Digest SHA256)
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
getPackageConfig :: (MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, MonadReader env m, HasEnvConfig env)
  => BuildOptsCLI
  -> PackageName
  -> m PackageConfig
getPackageConfig boptsCli name = do
  econfig <- asks getEnvConfig
  bconfig <- asks getBuildConfig
  return PackageConfig
    { packageConfigEnableTests = False
    , packageConfigEnableBenchmarks = False
    , packageConfigFlags = localFlags (boptsCLIFlags boptsCli) bconfig name
    , packageConfigCompilerVersion = envConfigCompilerVersion econfig
    , packageConfigPlatform = configPlatform $ getConfig bconfig
    }
