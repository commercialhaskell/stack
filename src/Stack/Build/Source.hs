{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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
    ) where

import           Control.Applicative ((<|>), (<$>), (<*>))
import           Control.Arrow ((&&&))
import           Control.Exception (assert, catch)
import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Resource
import           Crypto.Hash (Digest, SHA256)
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
import           Data.Maybe
import           Data.Monoid ((<>), Any (..), mconcat)
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
import           Stack.Types.Build
import           Stack.BuildPlan (loadMiniBuildPlan, shadowMiniBuildPlan,
                                  parseCustomMiniBuildPlan)
import           Stack.Constants (wiredInPackages)
import           Stack.Package
import           Stack.PackageIndex
import           Stack.Types
import           System.Directory
import           System.IO (withBinaryFile, IOMode (ReadMode))
import           System.IO.Error (isDoesNotExistError)

loadSourceMap :: (MonadIO m, MonadCatch m, MonadReader env m, HasBuildConfig env, MonadBaseControl IO m, HasHttpManager env, MonadLogger m, HasEnvConfig env)
              => BuildOpts
              -> m ( MiniBuildPlan
                   , [LocalPackage]
                   , Set PackageName -- non-local targets
                   , SourceMap
                   )
loadSourceMap bopts = do
    bconfig <- asks getBuildConfig
    mbp0 <- case bcResolver bconfig of
        ResolverSnapshot snapName -> do
            $logDebug $ "Checking resolver: " <> renderSnapName snapName
            loadMiniBuildPlan snapName
        ResolverGhc ghc -> return MiniBuildPlan
            { mbpGhcVersion = fromMajorVersion ghc
            , mbpPackages = Map.empty
            }
        ResolverCustom _ url -> do
            stackYamlFP <- asks $ bcStackYaml . getBuildConfig
            parseCustomMiniBuildPlan stackYamlFP url

    rawLocals <- getLocalPackageViews
    workingDir <- getWorkingDir
    (cliExtraDeps, targets) <-
        parseTargets
            (boptsTests bopts)
            (boptsBenchmarks bopts)
            (mpiVersion <$> mbpPackages mbp0)
            (bcExtraDeps bconfig)
            (fst <$> rawLocals)
            workingDir
            (boptsTargets bopts)

    menv <- getMinimalEnvOverride
    caches <- getPackageCaches menv
    let latestVersion = Map.fromListWith max $ map toTuple $ Map.keys caches

    -- Extend extra-deps to encompass targets requested on the command line
    -- that are not in the snapshot.
    extraDeps0 <- extendExtraDeps
        (bcExtraDeps bconfig)
        cliExtraDeps
        (Map.keysSet $ Map.filter (== STUnknown) targets)
        latestVersion

    locals <- mapM (loadLocalPackage bopts targets) $ Map.toList rawLocals
    checkFlagsUsed bopts locals

    let
        -- loadLocals returns PackageName (foo) and PackageIdentifier (bar-1.2.3) targets separately;
        -- here we combine them into nonLocalTargets. This is one of the
        -- return values of this function.
        nonLocalTargets :: Set PackageName
        nonLocalTargets =
            Map.keysSet $ Map.filter (not . isLocal) targets
          where
            isLocal (STLocal _) = True
            isLocal STUnknown = False
            isLocal STNonLocal = False

        shadowed = Map.keysSet rawLocals <> Map.keysSet extraDeps0
        (mbp, extraDeps1) = shadowMiniBuildPlan mbp0 shadowed

        -- Add the extra deps from the stack.yaml file to the deps grabbed from
        -- the snapshot
        extraDeps2 = Map.union
            (Map.map (\v -> (v, Map.empty)) extraDeps0)
            (Map.map (\mpi -> (mpiVersion mpi, mpiFlags mpi)) extraDeps1)

        -- Overwrite any flag settings with those from the config file
        extraDeps3 = Map.mapWithKey
            (\n (v, f) -> PSUpstream v Local $ fromMaybe f $ Map.lookup n $ bcFlags bconfig)
            extraDeps2

    let sourceMap = Map.unions
            [ Map.fromList $ flip map locals $ \lp ->
                let p = lpPackage lp
                 in (packageName p, PSLocal lp)
            , extraDeps3
            , flip fmap (mbpPackages mbp) $ \mpi ->
                (PSUpstream (mpiVersion mpi) Snap (mpiFlags mpi))
            ] `Map.difference` Map.fromList (map (, ()) (HashSet.toList wiredInPackages))

    return (mbp, locals, nonLocalTargets, sourceMap)

-- | Parse out the local package views for the current project
getLocalPackageViews :: (MonadThrow m, MonadIO m, MonadReader env m, HasEnvConfig env)
                     => m (Map PackageName (LocalPackageView, GenericPackageDescription))
getLocalPackageViews = do
    econfig <- asks getEnvConfig
    -- TODO ensure that there are no overlapping package names
    liftM Map.fromList $ forM (Map.toList $ envConfigPackages econfig) $ \(dir, validWanted) -> do
        cabalfp <- getCabalFileName dir
        gpkg <- readPackageUnresolved cabalfp
        let cabalID = package $ packageDescription gpkg
        name <- parsePackageNameFromFilePath cabalfp
        when (fromCabalPackageName (pkgName $ cabalID) /= name)
            $ throwM $ MismatchedCabalName cabalfp name
        let lpv = LocalPackageView
                { lpvVersion = fromCabalVersion $ pkgVersion cabalID
                , lpvRoot = dir
                , lpvCabalFP = cabalfp
                , lpvExtraDep = not validWanted
                , lpvComponents = getNamedComponents gpkg
                }
        return (name, (lpv, gpkg))
  where
    getNamedComponents gpkg = Set.fromList $ concat
        [ maybe [] (const [CLib]) (C.condLibrary gpkg)
        , go CExe  C.condExecutables
        , go CTest C.condTestSuites
        , go CBench C.condBenchmarks
        ]
      where
        go wrapper f = map (wrapper . T.pack . fst) $ f gpkg

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
    => BuildOpts
    -> Map PackageName SimpleTarget
    -> (PackageName, (LocalPackageView, GenericPackageDescription))
    -> m LocalPackage
loadLocalPackage bopts targets (name, (lpv, gpkg)) = do
    bconfig <- asks getBuildConfig
    econfig <- asks getEnvConfig

    let mtarget = Map.lookup name targets
        components =
            case mtarget of
                Just (STLocal comps) -> comps
                Just STNonLocal -> assert False Set.empty
                Just STUnknown -> assert False Set.empty
                Nothing -> Set.empty
        (exes, tests, benches) = splitComponents $ Set.toList components
        config = PackageConfig
            { packageConfigEnableTests = False
            , packageConfigEnableBenchmarks = False
            , packageConfigFlags = localFlags (boptsFlags bopts) bconfig name
            , packageConfigGhcVersion = envConfigGhcVersion econfig
            , packageConfigPlatform = configPlatform $ getConfig bconfig
            }
        btconfig = config
            { packageConfigEnableTests = not $ Set.null tests
            , packageConfigEnableBenchmarks = not $ Set.null benches
            }
        pkg = resolvePackage config gpkg
        btpkg
            | Set.null tests && Set.null benches = Nothing
            | otherwise = Just $ LocalPackageTB
                { lptbPackage = resolvePackage btconfig gpkg
                , lptbTests = tests
                , lptbBenches = benches
                }
    mbuildCache <- tryGetBuildCache $ lpvRoot lpv
    files <- getPackageFiles (packageFiles pkg) (lpvCabalFP lpv)
    (isDirty, newBuildCache) <- checkBuildCache
        (fromMaybe Map.empty mbuildCache)
        (map toFilePath $ Set.toList files)

    return LocalPackage
        { lpPackage = pkg
        , lpExeComponents =
            case mtarget of
                Nothing -> Nothing
                Just _ -> Just exes
        , lpTestBench = btpkg
        , lpFiles = files
        , lpDirtyFiles = isDirty || boptsForceDirty bopts
        , lpNewBuildCache = newBuildCache
        , lpCabalFile = lpvCabalFP lpv
        , lpDir = lpvRoot lpv
        , lpComponents = components
        }

-- | Ensure that the flags specified in the stack.yaml file and on the command
-- line are used.
checkFlagsUsed :: (MonadThrow m, MonadReader env m, HasBuildConfig env)
               => BuildOpts
               -> [LocalPackage]
               -> m ()
checkFlagsUsed bopts lps = do
    bconfig <- asks getBuildConfig

        -- Check if flags specified in stack.yaml and the command line are
        -- used, see https://github.com/commercialhaskell/stack/issues/617
    let flags = map (, FSCommandLine) [(k, v) | (Just k, v) <- Map.toList $ boptsFlags bopts]
             ++ map (, FSStackYaml) (Map.toList $ bcFlags bconfig)

        localNameMap = Map.fromList $ map (packageName . lpPackage &&& lpPackage) lps
        checkFlagUsed ((name, userFlags), source) =
            case Map.lookup name localNameMap of
                -- Package is not available locally
                Nothing ->
                    case Map.lookup name $ bcExtraDeps bconfig of
                        -- Also not in extra-deps, it's an error
                        Nothing -> Just $ UFNoPackage source name
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
localFlags :: (Map (Maybe PackageName) (Map FlagName Bool))
           -> BuildConfig
           -> PackageName
           -> Map FlagName Bool
localFlags boptsflags bconfig name = Map.unions
    [ fromMaybe Map.empty $ Map.lookup (Just name) $ boptsflags
    , fromMaybe Map.empty $ Map.lookup Nothing $ boptsflags
    , fromMaybe Map.empty $ Map.lookup name $ bcFlags bconfig
    ]

-- | Add in necessary packages to extra dependencies
--
-- Originally part of https://github.com/commercialhaskell/stack/issues/272,
-- this was then superseded by
-- https://github.com/commercialhaskell/stack/issues/651
extendExtraDeps :: (MonadThrow m, MonadReader env m, HasBuildConfig env)
                => Map PackageName Version -- ^ original extra deps
                -> Map PackageName Version -- ^ package identifiers from the command line
                -> Set PackageName -- ^ all packages added on the command line
                -> Map PackageName Version -- ^ latest versions in indices
                -> m (Map PackageName Version) -- ^ new extradeps
extendExtraDeps extraDeps0 cliExtraDeps unknowns latestVersion
    | null errs = return $ Map.unions $ extraDeps1 : unknowns'
    | otherwise = do
        bconfig <- asks getBuildConfig
        throwM $ UnknownTargets
            (Set.fromList errs)
            Map.empty -- TODO check the cliExtraDeps for presence in index
            (bcStackYaml bconfig)
  where
    extraDeps1 = Map.union extraDeps0 cliExtraDeps

    (errs, unknowns') = partitionEithers $ map addUnknown $ Set.toList unknowns
    addUnknown pn =
        case Map.lookup pn extraDeps1 of
            Just _ -> Right Map.empty
            Nothing ->
                case Map.lookup pn latestVersion of
                    Just v -> Right $ Map.singleton pn v
                    Nothing -> Left pn

-- | Compare the current filesystem state to the cached information, and
-- determine (1) if the files are dirty, and (2) the new cache values.
checkBuildCache :: MonadIO m
                => Map FilePath FileCacheInfo -- ^ old cache
                -> [FilePath] -- ^ files in package
                -> m (Bool, Map FilePath FileCacheInfo)
checkBuildCache oldCache files = liftIO $ do
    (Any isDirty, m) <- fmap mconcat $ mapM go files
    return (isDirty, m)
  where
    go fp = do
        mmodTime <- getModTimeMaybe fp
        case mmodTime of
            Nothing -> return (Any False, Map.empty)
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
                return (Any isDirty, Map.singleton fp newFci)

    getModTimeMaybe fp =
        liftIO
            (catch
                 (liftM
                      (Just . modTime)
                      (getModificationTime fp))
                 (\e ->
                       if isDoesNotExistError e
                           then return Nothing
                           else throwM e))

    calcFci modTime' fp =
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
