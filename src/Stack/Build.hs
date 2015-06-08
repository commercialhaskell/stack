{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Build project(s).

module Stack.Build
  (build
  ,clean)
  where

import           Control.Applicative
import           Control.Concurrent (getNumCapabilities, forkIO)
import           Control.Concurrent.Execute
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.STM
import           Control.Exception.Enclosed (handleIO, tryIO)
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Catch (MonadCatch, MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, asks, ask, runReaderT)
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Control (liftBaseWith)
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Char (isSpace)
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set as Set
import qualified Data.Streaming.Process as Process
import           Data.Streaming.Process hiding (env,callProcess)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Typeable (Typeable)
import           Distribution.Package (Dependency (..))
import           Distribution.System (Platform (Platform), OS (Windows))
import           Distribution.Text (display)
import           Distribution.Version (intersectVersionRanges, anyVersion)
import           GHC.Generics (Generic)
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Path.IO
import           Prelude hiding (FilePath, writeFile)
import           Stack.Build.Cache
import           Stack.Build.Types
import           Stack.BuildPlan
import           Stack.Constants
import           Stack.Fetch as Fetch
import           Stack.GhcPkg
import           Stack.Package
import           Stack.PackageDump
import           Stack.Types
import           Stack.Types.Internal
import           System.Directory hiding (findFiles, findExecutable)
import           System.Exit (ExitCode (ExitSuccess))
import           System.IO
import           System.IO.Error
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process.Internals (createProcess_)
import           System.Process.Read

{- EKB TODO: doc generation for stack-doc-server
#ifndef mingw32_HOST_OS
import           System.Posix.Files (createSymbolicLink,removeLink)
#endif
--}
data Installed = Library GhcPkgId | Executable
    deriving (Show, Eq, Ord)

type M env m = (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m,HasLogLevel env)

type SourceMap = Map PackageName PackageSource
data PackageSource
    = PSLocal LocalPackage
    | PSUpstream Version Location (Map FlagName Bool)

psVersion :: PackageSource -> Version
psVersion (PSLocal lp) = packageVersion $ lpPackage lp
psVersion (PSUpstream v _ _) = v

type InstalledMap = Map PackageName (Version, Location, Installed)

-- | Returns the new SourceMap and all of the locally registered packages.
getInstalled :: M env m
             => EnvOverride
             -> Bool -- ^ profiling?
             -> SourceMap -- ^ does not contain any installed information
             -> m (InstalledMap, Set GhcPkgId)
getInstalled menv profiling sourceMap = do
    snapDBPath <- packageDatabaseDeps
    localDBPath <- packageDatabaseLocal

    bconfig <- asks getBuildConfig

    mpcache <-
        if profiling
            then liftM Just $ loadProfilingCache $ configProfilingCache bconfig
            else return Nothing

    let loadDatabase' = loadDatabase menv mpcache sourceMap
    (installedLibs', localInstalled) <-
        loadDatabase' Nothing [] >>=
        loadDatabase' (Just (Snap, snapDBPath)) . fst >>=
        loadDatabase' (Just (Local, localDBPath)) . fst
    let installedLibs = M.fromList $ map lhPair installedLibs'

    case mpcache of
        Nothing -> return ()
        Just pcache -> saveProfilingCache (configProfilingCache bconfig) pcache

    -- Add in the executables that are installed, making sure to only trust a
    -- listed installation under the right circumstances (see below)
    let exesToSM loc = Map.unions . map (exeToSM loc)
        exeToSM loc (PackageIdentifier name version) =
            case Map.lookup name sourceMap of
                -- Doesn't conflict with anything, so that's OK
                Nothing -> m
                Just ps
                    -- Not the version we want, ignore it
                    | version /= psVersion ps -> Map.empty
                    | otherwise -> case ps of
                        {- FIXME revisit this logic
                        -- Never mark locals as installed, instead do dirty
                        -- checking
                        PSLocal _ -> Map.empty

                        -- FIXME start recording build flags for installed
                        -- executables, and only count as installed if it
                        -- matches

                        PSUpstream loc' _flags | loc == loc' -> Map.empty
                        -}

                        -- Passed all the tests, mark this as installed!
                        _ -> m
          where
            m = Map.singleton name (version, loc, Executable)
    exesSnap <- getInstalledExes Snap
    exesLocal <- getInstalledExes Local
    let installedMap = Map.unions
            [ exesToSM Local exesLocal
            , exesToSM Snap exesSnap
            , installedLibs
            ]

    return (installedMap, localInstalled)

data LocalPackage = LocalPackage
    { lpPackage :: Package
    , lpWanted :: Bool
    , lpDir :: !(Path Abs Dir)                  -- ^ Directory of the package.
    , lpCabalFile :: !(Path Abs File)           -- ^ The .cabal file
    , lpLastConfigOpts :: !(Maybe [Text])       -- ^ configure options used during last Setup.hs configure, if available
    , lpDirtyFiles :: !Bool                     -- ^ are there files that have changed since the last build?
    }
    deriving Show

loadLocals :: M env m
           => BuildOpts
           -> m [LocalPackage]
loadLocals bopts = do
    targets <- mapM parseTarget $
        case boptsTargets bopts of
            Left [] -> ["."]
            Left x -> x
            Right _ -> []
    (dirs, names0) <- case partitionEithers targets of
        ([], targets') -> return $ partitionEithers targets'
        (bad, _) -> throwM $ Couldn'tParseTargets bad
    let names = Set.fromList names0

    bconfig <- asks getBuildConfig
    lps <- forM (Set.toList $ bcPackages bconfig) $ \dir -> do
        cabalfp <- getCabalFileName dir
        name <- parsePackageNameFromFilePath cabalfp
        let wanted = isWanted dirs names dir name
        pkg <- readPackage
            PackageConfig
                { packageConfigEnableTests = wanted && boptsFinalAction bopts == DoTests
                , packageConfigEnableBenchmarks = wanted && boptsFinalAction bopts == DoBenchmarks
                , packageConfigFlags = localFlags bopts bconfig name
                , packageConfigGhcVersion = bcGhcVersion bconfig
                , packageConfigPlatform = configPlatform $ getConfig bconfig
                }
            cabalfp
        when (packageName pkg /= name) $ throwM
            $ MismatchedCabalName cabalfp (packageName pkg)
        mbuildCache <- tryGetBuildCache dir
        mconfigCache <- tryGetConfigCache dir
        fileModTimes <- getPackageFileModTimes pkg cabalfp
        return LocalPackage
            { lpPackage = pkg
            , lpWanted = wanted
            , lpLastConfigOpts =
                  fmap (map T.decodeUtf8 . configCacheOpts) mconfigCache
            , lpDirtyFiles =
                  maybe True
                        ((/= fileModTimes) . buildCacheTimes)
                        mbuildCache
            , lpCabalFile = cabalfp
            , lpDir = dir
            }

    let known = Set.fromList $ map (packageName . lpPackage) lps
        unknown = Set.difference names known
    unless (Set.null unknown) $ throwM $ UnknownTargets $ Set.toList unknown

    return lps
  where
    parseTarget t = do
        let s = T.unpack t
        isDir <- liftIO $ doesDirectoryExist s
        if isDir
            then liftM (Right . Left) $ liftIO (canonicalizePath s) >>= parseAbsDir
            else return $ case parsePackageNameFromString s of
                     Left _ -> Left t
                     Right pname -> Right $ Right pname
    isWanted dirs names dir name =
        name `Set.member` names ||
        any (`isParentOf` dir) dirs ||
        any (== dir) dirs

data LoadHelper = LoadHelper
    { lhId :: !GhcPkgId
    , lhDeps :: ![GhcPkgId]
    , lhPair :: !(PackageName, (Version, Location, Installed))
    }
    deriving Show

-- | Outputs both the modified InstalledMap and the Set of all installed packages in this database
--
-- The goal is to ascertain that the dependencies for a package are present,
-- that it has profiling if necessary, and that it matches the version and
-- location needed by the SourceMap
loadDatabase :: M env m
             => EnvOverride
             -> Maybe ProfilingCache -- ^ if Just, profiling is required
             -> SourceMap -- ^ to determine which installed things we should include
             -> Maybe (Location, Path Abs Dir) -- ^ package database, Nothing for global
             -> [LoadHelper] -- ^ from parent databases
             -> m ([LoadHelper], Set GhcPkgId)
loadDatabase menv mpcache sourceMap mdb lhs0 = do
    (lhs1, gids) <- ghcPkgDump menv (fmap snd mdb)
                  $ conduitDumpPackage =$ sink
    let lhs = pruneDeps
            (packageIdentifierName . ghcPkgIdPackageIdentifier)
            lhId
            lhDeps
            const
            (lhs0 ++ lhs1)
    return (map (\lh -> lh { lhDeps = [] }) $ Map.elems lhs, Set.fromList gids)
  where
    conduitCache =
        case mpcache of
            Just pcache -> addProfiling pcache
            -- Just an optimization to avoid calculating the profiling
            -- values when they aren't necessary
            Nothing -> CL.map (\dp -> dp { dpProfiling = False })
    sinkDP = conduitCache
          =$ CL.mapMaybe (isAllowed mpcache sourceMap (fmap fst mdb))
          =$ CL.consume
    sinkGIDs = CL.map dpGhcPkgId =$ CL.consume
    sink = getZipSink $ (,)
        <$> ZipSink sinkDP
        <*> ZipSink sinkGIDs

-- | Check if a can be included in the set of installed packages or not, based
-- on the package selections made by the user. This does not perform any
-- dirtiness or flag change checks.
isAllowed mpcache sourceMap mloc dp
    -- Check that it can do profiling if necessary
    | isJust mpcache && not (dpProfiling dp) = Nothing
    | toInclude = Just LoadHelper
        { lhId = gid
        , lhDeps = dpDepends dp
        , lhPair = (name, (version, fromMaybe Snap mloc, Library gid))
        }
    | otherwise = Nothing
  where
    toInclude =
        case Map.lookup name sourceMap of
            -- The sourceMap has nothing to say about this package, so we can use it
            Nothing -> True

            Just ps ->
                version == psVersion ps -- only accept the desired version
                && checkLocation (targetLocation ps)

    targetLocation (PSLocal _) = Local
    targetLocation (PSUpstream _ loc _) = loc

    -- Ensure that the installed location matches where the sourceMap says it
    -- should be installed
    checkLocation Snap = mloc /= Just Local -- we can allow either global or snap
    checkLocation Local = mloc == Just Local

    gid = dpGhcPkgId dp
    PackageIdentifier name version = ghcPkgIdPackageIdentifier gid

data BaseConfigOpts = BaseConfigOpts
    { bcoSnapDB :: !(Path Abs Dir)
    , bcoLocalDB :: !(Path Abs Dir)
    , bcoSnapInstallRoot :: !(Path Abs Dir)
    , bcoLocalInstallRoot :: !(Path Abs Dir)
    , bcoLibProfiling :: !Bool
    , bcoExeProfiling :: !Bool
    , bcoFinalAction :: !FinalAction
    , bcoGhcOptions :: ![Text]
    }

configureOpts :: BaseConfigOpts
              -> Set GhcPkgId -- ^ dependencies
              -> Bool -- ^ wanted?
              -> Location
              -> Map FlagName Bool
              -> [Text]
configureOpts bco deps wanted loc flags = map T.pack $ concat
    [ ["--user", "--package-db=clear", "--package-db=global"]
    , map (("--package-db=" ++) . toFilePath) $ case loc of
        Snap -> [bcoSnapDB bco]
        Local -> [bcoSnapDB bco, bcoLocalDB bco]
    , depOptions
    , [ "--libdir=" ++ toFilePath (installRoot </> $(mkRelDir "lib"))
      , "--bindir=" ++ toFilePath (installRoot </> bindirSuffix)
      , "--datadir=" ++ toFilePath (installRoot </> $(mkRelDir "share"))
      , "--docdir=" ++ toFilePath (installRoot </> $(mkRelDir "doc"))
      ]
    , ["--enable-library-profiling" | bcoLibProfiling bco || bcoExeProfiling bco]
    , ["--enable-executable-profiling" | bcoLibProfiling bco]
    , ["--enable-tests" | wanted && bcoFinalAction bco == DoTests]
    , ["--enable-benchmarks" | wanted && bcoFinalAction bco == DoBenchmarks]
    , map (\(name,enabled) ->
                       "-f" <>
                       (if enabled
                           then ""
                           else "-") <>
                       flagNameString name)
                    (Map.toList flags)
    -- FIXME Chris: where does this come from now? , ["--ghc-options=-O2" | gconfigOptimize gconfig]
    , if wanted
        then concatMap (\x -> ["--ghc-options", T.unpack x]) (bcoGhcOptions bco)
        else []
    ]
  where
    installRoot =
        case loc of
            Snap -> bcoSnapInstallRoot bco
            Local -> bcoLocalInstallRoot bco

    depOptions = map toDepOption $ Set.toList deps

    {- TODO does this work with some versions of Cabal?
    toDepOption gid = T.pack $ concat
        [ "--dependency="
        , packageNameString $ packageIdentifierName $ ghcPkgIdPackageIdentifier gid
        , "="
        , ghcPkgIdString gid
        ]
    -}
    toDepOption gid = concat
        [ "--constraint="
        , packageNameString name
        , "=="
        , versionString version
        ]
      where
        PackageIdentifier name version = ghcPkgIdPackageIdentifier gid

data Task = Task
    { taskProvides :: !PackageIdentifier
    , taskRequiresMissing :: !(Set PackageIdentifier)
    , taskRequiresPresent :: !(Set GhcPkgId)
    , taskType :: !TaskType
    }
    deriving Show

taskLocation :: Task -> Location
taskLocation task =
    case taskType task of
        TTLocal _ _ -> Local
        TTUpstream _ loc -> loc

data TaskType = TTLocal LocalPackage NeededSteps
              | TTUpstream Package Location
    deriving Show

data AddDepRes
    = ADRToInstall Task
    | ADRFound Version Installed
    deriving Show

type S = Map PackageName (Either ConstructPlanException AddDepRes)

adrVersion :: AddDepRes -> Version
adrVersion (ADRToInstall task) = packageIdentifierVersion $ taskProvides task
adrVersion (ADRFound v _) = v

data NeededSteps = AllSteps | SkipConfig | JustFinal
    deriving (Show, Eq)
data DirtyResult = Dirty NeededSteps | Clean

data Plan = Plan
    { planTasks :: !(Map PackageName Task)
    , planUnregisterLocal :: !(Set GhcPkgId)
    }
constructPlan :: forall env m.
                 M env m
              => MiniBuildPlan
              -> BaseConfigOpts
              -> [LocalPackage]
              -> [PackageName] -- ^ additional packages that must be built
              -> Set GhcPkgId -- ^ locally registered
              -> (PackageName -> Version -> Map FlagName Bool -> m Package) -- ^ load upstream package
              -> SourceMap
              -> InstalledMap
              -> m Plan
constructPlan mbp baseConfigOpts locals extraToBuild locallyRegistered loadPackage sourceMap installedMap = do
    m <- flip execStateT M.empty $ do
        let allTargets = Set.fromList
                       $ map (packageName . lpPackage) locals ++ extraToBuild
        mapM_ (addDep []) $ Set.toList allTargets
    let toEither (_, Left e)  = Left e
        toEither (k, Right v) = Right (k, v)
    case partitionEithers $ map toEither $ M.toList m of
        ([], adrs) -> do
            let toTask (_, ADRFound _ _) = Nothing
                toTask (name, ADRToInstall task) = Just (name, task)
                tasks = M.fromList $ mapMaybe toTask adrs
            return Plan
                { planTasks = tasks
                , planUnregisterLocal = mkUnregisterLocal tasks locallyRegistered
                }
        (errs, _) -> throwM $ ConstructPlanExceptions errs
  where
    addDep :: [PackageName] -- ^ call stack
           -> PackageName
           -> StateT S m (Either ConstructPlanException AddDepRes)
    addDep callStack name = do
        m <- get
        case M.lookup name m of
            Just res -> return res
            Nothing -> do
                res <- addDep' callStack name
                modify $ Map.insert name res
                return res

    addDep' callStack name | name `elem` callStack =
        return $ Left $ DependencyCycleDetected $ name : callStack
    addDep' callStack0 name = do
        case M.lookup name installedMap of
            Nothing ->
                case M.lookup name sourceMap of
                    Nothing -> return $ Left $ UnknownPackage name
                    Just (PSLocal lp) -> installLocalPackage callStack lp
                    Just (PSUpstream version loc flags) -> installUpstream callStack name version loc flags
            Just (version, Snap, installed) -> return $ Right $ ADRFound version installed
            Just (version, Local, installed) -> checkDirty callStack name version installed
      where
        callStack = name : callStack0

    installLocalPackage callStack lp = do
        eres <- checkPackage callStack (lpPackage lp)
        case eres of
            Left e -> return $ Left e
            Right (present, missing) -> return $ Right $ ADRToInstall Task
                { taskProvides = PackageIdentifier
                    (packageName $ lpPackage lp)
                    (packageVersion $ lpPackage lp)
                , taskRequiresMissing = missing
                , taskRequiresPresent = present
                , taskType = TTLocal lp AllSteps
                }

    installUpstream callStack name version loc flags = do
        package <- lift $ loadPackage name version flags
        eres <- checkPackage callStack package
        case eres of
            Left e -> return $ Left e
            Right (present, missing) -> return $ Right $ ADRToInstall Task
                { taskProvides = PackageIdentifier name version
                , taskRequiresMissing = missing
                , taskRequiresPresent = present
                , taskType = TTUpstream package loc
                }

    -- Check if a locally installed package is dirty and must be reinstalled
    checkDirty callStack name version installed =
        case M.lookup name sourceMap of
            Nothing -> return $ Right $ ADRFound version installed
            Just (PSLocal lp) -> assert (version == packageVersion (lpPackage lp)) $ do
                cpr <- checkPackage callStack $ lpPackage lp
                case cpr of
                    Left e -> return $ Left e
                    Right (present, missing) -> do
                        let configOpts = configureOpts baseConfigOpts present (lpWanted lp) Local (packageFlags $ lpPackage lp)
                        let mneededSteps
                                | not $ Set.null missing = Just AllSteps
                                | Just configOpts /= lpLastConfigOpts lp
                                    = Just AllSteps
                                | lpDirtyFiles lp = Just SkipConfig
                                | lpWanted lp = Just JustFinal -- FIXME this currently causes too much recompilation
                                | otherwise = Nothing
                        return $ Right $
                            case mneededSteps of
                                Nothing -> ADRFound version installed
                                Just neededSteps -> ADRToInstall Task
                                    { taskProvides = PackageIdentifier name version
                                    , taskRequiresMissing = missing
                                    , taskRequiresPresent = present
                                    , taskType = TTLocal lp neededSteps
                                    }
            Just (PSUpstream version' loc flags) -> assert (version == version') $
                case loc of
                    Snap -> return $ Right $ ADRFound version installed
                    Local -> do
                        package <- lift $ loadPackage name version flags
                        eres <- checkPackage callStack package
                        let toInstall present missing = Right $ ADRToInstall Task
                                    { taskProvides = PackageIdentifier name version
                                    , taskRequiresMissing = missing
                                    , taskRequiresPresent = present
                                    , taskType = TTUpstream package loc
                                    }
                        case eres of
                            Left e -> return $ Left e
                            Right (present, missing)
                                | Set.null missing ->
                                    case installed of
                                        Library gid -> do
                                            oldFlags <- tryGetFlagCache gid
                                            if oldFlags == Just flags
                                                then return $ Right $ ADRFound version installed
                                                else return $ toInstall present missing
                                        Executable -> return $ Right $ ADRFound version installed -- TODO track flags for executables too
                                | otherwise -> return $ toInstall present missing

    -- Check all of the dependencies for the given package
    checkPackage :: [PackageName] -- ^ call stack
                 -> Package
                 -> StateT S m (Either ConstructPlanException (Set GhcPkgId, Set PackageIdentifier))
    checkPackage callStack package = do
        eress <- forM (M.toList $ packageDepsWithTools package) $ \(name, range) -> do
            eres <- addDep callStack name
            case eres of
                Left e -> return $ Left name
                Right adr
                    | adrVersion adr `withinRange` range -> return $ Right adr
                    | otherwise -> do
                        -- TODO change exception setup so we can give a meaningful error message about ranges here
                        return $ Left name
        case partitionEithers eress of
            ([], adrs) ->
                let loop present missing [] = (present, missing)
                    loop present missing (x:xs) =
                        case x of
                            ADRToInstall t -> loop present (Set.insert (taskProvides t) missing) xs
                            ADRFound _ Executable -> loop present missing xs
                            ADRFound _ (Library gid) -> loop (Set.insert gid present) missing xs
                 in return $ Right $ loop Set.empty Set.empty adrs
            (errs, _) -> return $ Left $ DependencyPlanFailures (packageName package) (Set.fromList errs)

    toolMap = getToolMap mbp
    toolToPackages (Dependency name _) =
        Map.fromList
      $ map (, anyVersion)
      $ maybe [] Set.toList
      $ Map.lookup (S8.pack . packageNameString . fromCabalPackageName $ name) toolMap
    packageDepsWithTools p = Map.unionsWith intersectVersionRanges
        $ packageDeps p
        : map toolToPackages (packageTools p)

mkUnregisterLocal :: Map PackageName Task -> Set GhcPkgId -> Set GhcPkgId
mkUnregisterLocal tasks locallyRegistered =
    Set.filter toUnregister locallyRegistered
  where
    toUnregister gid =
        case M.lookup name tasks of
            Nothing -> False
            Just task ->
                case taskType task of
                    TTLocal _ JustFinal -> False
                    _ -> True
      where
        ident = ghcPkgIdPackageIdentifier gid
        name = packageIdentifierName ident

-- | Build using Shake.
build :: M env m => BuildOpts -> m ()
build bopts = do
    menv <- getMinimalEnvOverride
    cabalPkgVer <- getCabalPkgVer menv

    bconfig <- asks getBuildConfig
    mbp0 <- case bcResolver bconfig of
        ResolverSnapshot snapName -> do
            $logDebug $ "Checking resolver: " <> renderSnapName snapName
            mbp <- loadMiniBuildPlan snapName
            return mbp
        ResolverGhc ghc -> return MiniBuildPlan
            { mbpGhcVersion = fromMajorVersion ghc
            , mbpPackages = M.empty
            }

    locals <- loadLocals bopts

    let shadowed = Set.fromList (map (packageName . lpPackage) locals)
                <> Map.keysSet (bcExtraDeps bconfig)
        (mbp, extraDeps0) = shadowMiniBuildPlan mbp0 shadowed

        -- Add the extra deps from the stack.yaml file to the deps grabbed from
        -- the snapshot
        extraDeps1 = Map.union
            (Map.map (\v -> (v, M.empty)) (bcExtraDeps bconfig))
            (Map.map (\mpi -> (mpiVersion mpi, mpiFlags mpi)) extraDeps0)

        -- Overwrite any flag settings with those from the config file
        extraDeps2 = Map.mapWithKey
            (\n (v, f) -> PSUpstream v Local $ fromMaybe f $ Map.lookup n $ bcFlags bconfig)
            extraDeps1

    let sourceMap = Map.unions
            [ Map.fromList $ flip map locals $ \lp ->
                let p = lpPackage lp
                 in (packageName p, PSLocal lp)
            , extraDeps2
            , flip fmap (mbpPackages mbp) $ \mpi ->
                (PSUpstream (mpiVersion mpi) Snap (mpiFlags mpi))
            ]

    (installedMap, locallyRegistered) <- getInstalled menv profiling sourceMap

    snapDBPath <- packageDatabaseDeps
    localDBPath <- packageDatabaseLocal
    snapInstallRoot <- installationRootDeps
    localInstallRoot <- installationRootLocal
    let baseConfigOpts = BaseConfigOpts
            { bcoSnapDB = snapDBPath
            , bcoLocalDB = localDBPath
            , bcoSnapInstallRoot = snapInstallRoot
            , bcoLocalInstallRoot = localInstallRoot
            , bcoLibProfiling = boptsLibProfile bopts
            , bcoExeProfiling = boptsExeProfile bopts
            , bcoFinalAction = boptsFinalAction bopts
            , bcoGhcOptions = boptsGhcOptions bopts
            }
        extraToBuild = either (const []) id $ boptsTargets bopts
    plan <- withCabalLoader menv $ \cabalLoader -> do
        let loadPackage name version flags = do
                bs <- cabalLoader $ PackageIdentifier name version -- TODO automatically update index the first time this fails
                readPackageBS (depPackageConfig bconfig flags) bs
        constructPlan mbp baseConfigOpts locals extraToBuild locallyRegistered loadPackage sourceMap installedMap

    if boptsDryrun bopts
        then printPlan plan
        else withSystemTempDirectory stackProgName $ \tmpdir -> do
            tmpdir' <- parseAbsDir tmpdir
            configLock <- newMVar ()
            installLock <- newMVar ()
            idMap <- liftIO $ newTVarIO M.empty
            let setupHs = tmpdir' </> $(mkRelFile "Setup.hs")
            liftIO $ writeFile (toFilePath setupHs) "import Distribution.Simple\nmain = defaultMain"
            executePlan plan ExecuteEnv
                { eeEnvOverride = menv
                , eeBuildOpts = bopts
                 -- Uncertain as to why we cannot run configures in parallel. This appears
                 -- to be a Cabal library bug. Original issue:
                 -- https://github.com/fpco/stack/issues/84. Ideally we'd be able to remove
                 -- this.
                , eeConfigureLock = configLock
                , eeInstallLock = installLock
                , eeBaseConfigOpts = baseConfigOpts
                , eeGhcPkgIds = idMap
                , eeTempDir = tmpdir'
                , eeSetupHs = setupHs
                , eeCabalPkgVer = cabalPkgVer
                , eeTotalWanted = length $ filter lpWanted locals
                }
  where
    profiling = boptsLibProfile bopts || boptsExeProfile bopts

-- | All flags for a local package
localFlags :: BuildOpts -> BuildConfig -> PackageName -> Map FlagName Bool
localFlags bopts bconfig name = M.union
    (fromMaybe M.empty $ M.lookup name $ boptsFlags bopts)
    (fromMaybe M.empty $ M.lookup name $ bcFlags bconfig)

-- | Package config to be used for dependencies
depPackageConfig :: BuildConfig -> Map FlagName Bool -> PackageConfig
depPackageConfig bconfig flags = PackageConfig
    { packageConfigEnableTests = False
    , packageConfigEnableBenchmarks = False
    , packageConfigFlags = flags
    , packageConfigGhcVersion = bcGhcVersion bconfig
    , packageConfigPlatform = configPlatform (getConfig bconfig)
    }

printPlan :: M env m => Plan -> m ()
printPlan plan = do
    case Set.toList $ planUnregisterLocal plan of
        [] -> $logInfo "Nothing to unregister"
        xs -> do
            $logInfo "Would unregister locally:"
            mapM_ ($logInfo . T.pack . ghcPkgIdString) xs

    $logInfo ""

    case Map.elems $ planTasks plan of
        [] -> $logInfo "Nothing to build"
        xs -> do
            $logInfo "Would build:"
            mapM_ ($logInfo . displayTask) xs

-- | For a dry run
displayTask :: Task -> Text
displayTask task = T.pack $ concat
    [ packageIdentifierString $ taskProvides task
    , ": database="
    , case taskLocation task of
        Snap -> "snapshot"
        Local -> "local"
    , ", source="
    , case taskType task of
        TTLocal lp steps -> concat
            [ toFilePath $ lpDir lp
            , case steps of
                AllSteps -> " (configure)"
                SkipConfig -> " (build)"
                JustFinal -> " (already built)"
            ]
        TTUpstream _ _ -> "package index"
    , if Set.null $ taskRequiresMissing task
        then ""
        else ", after: " ++ intercalate "," (map packageIdentifierString $ Set.toList $ taskRequiresMissing task)
    ]

data ExecuteEnv = ExecuteEnv
    { eeEnvOverride :: !EnvOverride
    , eeConfigureLock :: !(MVar ())
    , eeInstallLock :: !(MVar ())
    , eeBuildOpts :: !BuildOpts
    , eeBaseConfigOpts :: !BaseConfigOpts
    , eeGhcPkgIds :: !(TVar (Map PackageIdentifier Installed))
    , eeTempDir :: !(Path Abs Dir)
    , eeSetupHs :: !(Path Abs File)
    , eeCabalPkgVer :: !PackageIdentifier
    , eeTotalWanted :: !Int
    }

-- | Perform the actual plan
executePlan :: M env m
            => Plan
            -> ExecuteEnv
            -> m ()
executePlan plan ee = do
    case Set.toList $ planUnregisterLocal plan of
        [] -> return ()
        ids -> do
            localDB <- packageDatabaseLocal
            forM_ ids $ \id' -> do
                $logInfo $ T.concat
                    [ T.pack $ ghcPkgIdString id'
                    , ": unregistering"
                    ]
                unregisterGhcPkgId (eeEnvOverride ee) localDB id'

    -- Yes, we're explicitly discarding result values, which in general would
    -- be bad. monad-unlift does this all properly at the type system level,
    -- but I don't want to pull it in for this one use case, when we know that
    -- stack always using transformer stacks that are safe for this use case.
    runInBase <- liftBaseWith $ \run -> return (void . run)

    let actions = concatMap (toActions runInBase ee) $ Map.elems $ planTasks plan
    threads <- liftIO getNumCapabilities -- TODO make a build opt to override this
    liftIO $ runActions threads actions

toActions :: M env m
          => (m () -> IO ())
          -> ExecuteEnv
          -> Task
          -> [Action]
toActions runInBase ee task@Task {..} =
    -- TODO in the future, we need to have proper support for cyclic
    -- dependencies from test suites, in which case we'll need more than one
    -- Action here

    [ Action
        { actionId = ActionId taskProvides ATBuild
        , actionDeps =
            (Set.map (\ident -> ActionId ident ATBuild) taskRequiresMissing)
        , actionDo = \ac -> runInBase $ singleBuild ac ee task
        }
    ]

singleBuild :: M env m
            => ActionContext
            -> ExecuteEnv
            -> Task
            -> m ()
singleBuild ActionContext {..} ExecuteEnv {..} task@Task {..} =
  withPackage $ \package cabalfp pkgDir ->
  withLogFile package $ \mlogFile ->
  withCabal pkgDir mlogFile $ \cabal -> do
    when needsConfig $ withMVar eeConfigureLock $ \_ -> do
        deleteCaches pkgDir
        idMap <- liftIO $ readTVarIO eeGhcPkgIds
        let getMissing ident =
                case Map.lookup ident idMap of
                    Nothing -> error "singleBuild: invariant violated, missing package ID missing"
                    Just (Library x) -> Just x
                    Just Executable -> Nothing
            allDeps = Set.union
                taskRequiresPresent
                (Set.fromList $ mapMaybe getMissing $ Set.toList taskRequiresMissing)
        let configOpts = configureOpts
                eeBaseConfigOpts
                allDeps
                wanted
                (taskLocation task)
                (packageFlags package)
        announce "configure"
        cabal False $ "configure" : map T.unpack configOpts
        $logDebug $ T.pack $ show configOpts
        writeConfigCache pkgDir configOpts

    fileModTimes <- getPackageFileModTimes package cabalfp
    writeBuildCache pkgDir fileModTimes

    unless justFinal $ do
        announce "build"
        config <- asks getConfig
        cabal (console && configHideTHLoading config) ["build"]

    case boptsFinalAction eeBuildOpts of
        DoTests -> when wanted $ do
            announce "test"
            runTests package pkgDir mlogFile
        DoBenchmarks -> when wanted $ do
            announce "benchmarks"
            cabal False ["bench"]
        DoHaddock -> do
            announce "haddock"
            hscolourExists <- doesExecutableExist eeEnvOverride "hscolour"
              {- EKB TODO: doc generation for stack-doc-server
 #ifndef mingw32_HOST_OS
              liftIO (removeDocLinks docLoc package)
 #endif
              ifcOpts <- liftIO (haddockInterfaceOpts docLoc package packages)
              -}
            cabal False (concat [["haddock", "--html"]
                                ,["--hyperlink-source" | hscolourExists]])
              {- EKB TODO: doc generation for stack-doc-server
                         ,"--hoogle"
                         ,"--html-location=../$pkg-$version/"
                         ,"--haddock-options=" ++ intercalate " " ifcOpts ]
              haddockLocs <-
                liftIO (findFiles (packageDocDir package)
                                  (\loc -> FilePath.takeExtensions (toFilePath loc) ==
                                           "." ++ haddockExtension)
                                  (not . isHiddenDir))
              forM_ haddockLocs $ \haddockLoc ->
                do let hoogleTxtPath = FilePath.replaceExtension (toFilePath haddockLoc) "txt"
                       hoogleDbPath = FilePath.replaceExtension hoogleTxtPath hoogleDbExtension
                   hoogleExists <- liftIO (doesFileExist hoogleTxtPath)
                   when hoogleExists
                        (callProcess
                             "hoogle"
                             ["convert"
                             ,"--haddock"
                             ,hoogleTxtPath
                             ,hoogleDbPath])
                        -}
                 {- EKB TODO: doc generation for stack-doc-server
             #ifndef mingw32_HOST_OS
                 case setupAction of
                   DoHaddock -> liftIO (createDocLinks docLoc package)
                   _ -> return ()
             #endif

-- | Package's documentation directory.
packageDocDir :: (MonadThrow m, MonadReader env m, HasPlatform env)
              => PackageIdentifier -- ^ Cabal version
              -> Package
              -> m (Path Abs Dir)
packageDocDir cabalPkgVer package' = do
  dist <- distDirFromDir cabalPkgVer (packageDir package')
  return (dist </> $(mkRelDir "doc/"))
                 --}
        DoNothing -> return ()

    unless justFinal $ withMVar eeInstallLock $ \_ -> do
        announce "install"
        cabal False ["install"]

    -- It seems correct to leave this outside of the "justFinal" check above,
    -- in case another package depends on a justFinal target
    let pkgDbs =
            case taskLocation task of
                Snap -> [bcoSnapDB eeBaseConfigOpts]
                Local ->
                    [ bcoSnapDB eeBaseConfigOpts
                    , bcoLocalDB eeBaseConfigOpts
                    ]
    mpkgid <- findGhcPkgId eeEnvOverride pkgDbs (packageName package)
    mpkgid' <- case (packageHasLibrary package, mpkgid) of
        (False, _) -> assert (isNothing mpkgid) $ do
            markExeInstalled (taskLocation task) taskProvides
            return Executable
        (True, Nothing) -> throwM $ Couldn'tFindPkgId $ packageName package
        (True, Just pkgid) -> do
            writeFlagCache pkgid $ packageFlags package
            return $ Library pkgid
    liftIO $ atomically $ modifyTVar eeGhcPkgIds $ Map.insert taskProvides mpkgid'
  where
    announce x = $logInfo $ T.concat
        [ T.pack $ packageIdentifierString taskProvides
        , ": "
        , x
        ]

    needsConfig =
        case taskType of
            TTLocal _ y -> y == AllSteps
            TTUpstream _ _ -> True
    justFinal =
        case taskType of
            TTLocal _ JustFinal -> True
            _ -> False

    wanted =
        case taskType of
            TTLocal lp _ -> lpWanted lp
            TTUpstream _ _ -> False

    console = wanted && acRemaining == 0 && eeTotalWanted == 1

    withPackage inner =
        case taskType of
            TTLocal lp _ -> inner (lpPackage lp) (lpCabalFile lp) (lpDir lp)
            TTUpstream package _ -> do
                mdist <- liftM Just $ distRelativeDir eeCabalPkgVer
                m <- unpackPackageIdents eeEnvOverride eeTempDir mdist $ Set.singleton taskProvides
                case M.toList m of
                    [(ident, dir)]
                        | ident == taskProvides -> do
                            let name = packageIdentifierName taskProvides
                            cabalfpRel <- parseRelFile $ packageNameString name ++ ".cabal"
                            let cabalfp = dir </> cabalfpRel
                            inner package cabalfp dir
                    _ -> error $ "withPackage: invariant violated: " ++ show m

    withLogFile package inner
        | console = inner Nothing
        | otherwise = do
            logPath <- buildLogPath package
            liftIO $ createDirectoryIfMissing True $ toFilePath $ parent logPath
            let fp = toFilePath logPath
            bracket
                (liftIO $ openBinaryFile fp WriteMode)
                (liftIO . hClose)
                $ \h -> inner (Just (logPath, h))

    withCabal pkgDir mlogFile inner = do
        config <- asks getConfig
        menv <- liftIO $ configEnvOverride config EnvSettings
            { esIncludeLocals = taskLocation task == Local
            , esIncludeGhcPackagePath = False
            }
        exeName <- liftIO $ join $ findExecutable menv "runhaskell"
        distRelativeDir' <- distRelativeDir eeCabalPkgVer
        msetuphs <- liftIO $ getSetupHs pkgDir
        let setuphs = fromMaybe eeSetupHs msetuphs
        inner $ \stripTHLoading args -> do
            let fullArgs =
                      ("-package=" ++ packageIdentifierString eeCabalPkgVer)
                    : "-clear-package-db"
                    : "-global-package-db"
                    -- TODO: Perhaps we want to include the snapshot package database here
                    -- as well
                    : toFilePath setuphs
                    : ("--builddir=" ++ toFilePath distRelativeDir')
                    : args
                cp0 = proc (toFilePath exeName) fullArgs
                cp = cp0
                    { cwd = Just $ toFilePath pkgDir
                    , Process.env = envHelper menv
                    , std_in = CreatePipe
                    , std_out =
                        if stripTHLoading
                            then CreatePipe
                            else case mlogFile of
                                Nothing -> Inherit
                                Just (_, h) -> UseHandle h
                    , std_err =
                        case mlogFile of
                            Nothing -> Inherit
                            Just (_, h) -> UseHandle h
                    }
            $logDebug $ "Running: " <> T.pack (show $ toFilePath exeName : fullArgs)

            -- Use createProcess_ to avoid the log file being closed afterwards
            (Just inH, moutH, Nothing, ph) <- liftIO $ createProcess_ "singleBuild" cp
            liftIO $ hClose inH
            case moutH of
                Just outH -> assert stripTHLoading $ printWithoutTHLoading outH
                Nothing -> return ()
            ec <- liftIO $ waitForProcess ph
            case ec of
                ExitSuccess -> return ()
                _ -> do
                    bs <- liftIO $
                        case mlogFile of
                            Nothing -> return ""
                            Just (logFile, h) -> do
                                hClose h
                                S.readFile $ toFilePath logFile
                    throwM $ CabalExitedUnsuccessfully
                        ec
                        taskProvides
                        exeName
                        fullArgs
                        (fmap fst mlogFile)
                        bs

    runTests package pkgDir mlogFile = do
        bconfig <- asks getBuildConfig
        distRelativeDir' <- distRelativeDir eeCabalPkgVer
        let buildDir = pkgDir </> distRelativeDir'
        let exeExtension =
                case configPlatform $ getConfig bconfig of
                    Platform _ Windows -> ".exe"
                    _ -> ""

        errs <- liftM Map.unions $ forM (Set.toList $ packageTests package) $ \testName -> do
            nameDir <- liftIO $ parseRelDir $ T.unpack testName
            nameExe <- liftIO $ parseRelFile $ T.unpack testName ++ exeExtension
            let exeName = buildDir </> $(mkRelDir "build") </> nameDir </> nameExe
            exists <- liftIO $ doesFileExist $ toFilePath exeName
            config <- asks getConfig
            menv <- liftIO $ configEnvOverride config EnvSettings
                { esIncludeLocals = taskLocation task == Local
                , esIncludeGhcPackagePath = True
                }
            if exists
                then do
                    announce $ "test " <> testName
                    let cp = (proc (toFilePath exeName) [])
                            { cwd = Just $ toFilePath pkgDir
                            , Process.env = envHelper menv
                            , std_in = CreatePipe
                            , std_out =
                                case mlogFile of
                                    Nothing -> Inherit
                                    Just (_, h) -> UseHandle h
                            , std_err =
                                case mlogFile of
                                    Nothing -> Inherit
                                    Just (_, h) -> UseHandle h
                            }

                    -- Use createProcess_ to avoid the log file being closed afterwards
                    (Just inH, Nothing, Nothing, ph) <- liftIO $ createProcess_ "singleBuild.runTests" cp
                    liftIO $ hClose inH
                    ec <- liftIO $ waitForProcess ph
                    return $ case ec of
                        ExitSuccess -> M.empty
                        _ -> M.singleton testName $ Just ec
                else do
                    $logError $ T.concat
                        [ "Test suite "
                        , testName
                        , " executable not found for "
                        , T.pack $ packageNameString $ packageName package
                        ]
                    return $ Map.singleton testName Nothing
        unless (Map.null errs) $ throwM $ TestSuiteFailure2 taskProvides errs (fmap fst mlogFile)

-- | Grab all output from the given @Handle@ and print it to stdout, stripping
-- Template Haskell "Loading package" lines. Does work in a separate thread.
printWithoutTHLoading :: MonadIO m => Handle -> m ()
printWithoutTHLoading outH = liftIO $ void $ forkIO $
       CB.sourceHandle outH
    $$ CB.lines
    =$ CL.filter (not . isTHLoading)
    =$ CL.mapM_ S8.putStrLn
  where
    -- | Is this line a Template Haskell "Loading package" line
    -- ByteString
    isTHLoading :: S8.ByteString -> Bool
    isTHLoading bs =
        "Loading package " `S8.isPrefixOf` bs &&
        ("done." `S8.isSuffixOf` bs || "done.\r" `S8.isSuffixOf` bs)


-- | Reset the build (remove Shake database and .gen files).
clean :: (M env m) => m ()
clean = do
    bconfig <- asks getBuildConfig
    menv <- getMinimalEnvOverride
    cabalPkgVer <- getCabalPkgVer menv
    forM_
        (S.toList (bcPackages bconfig))
        (distDirFromDir cabalPkgVer >=> removeTreeIfExists)

{- EKB TODO: doc generation for stack-doc-server
            (boptsFinalAction bopts == DoHaddock)
            (buildDocIndex
                 (wanted pwd)
                 docLoc
                 packages
                 mgr
                 logLevel)
                                  -}

-- | Ensure Setup.hs exists in the given directory. Returns an action
-- to remove it later.
getSetupHs :: Path Abs Dir -- ^ project directory
           -> IO (Maybe (Path Abs File))
getSetupHs dir = do
    exists1 <- doesFileExist (toFilePath fp1)
    if exists1
        then return $ Just fp1
        else do
            exists2 <- doesFileExist (toFilePath fp2)
            if exists2
                then return $ Just fp2
                else return Nothing
  where
    fp1 = dir </> $(mkRelFile "Setup.hs")
    fp2 = dir </> $(mkRelFile "Setup.lhs")

{- EKB TODO: doc generation for stack-doc-server
-- | Build the haddock documentation index and contents.
buildDocIndex :: (Package -> Wanted)
              -> Path Abs Dir
              -> Set Package
              -> Manager
              -> LogLevel
              -> Rules ()
buildDocIndex wanted docLoc packages mgr logLevel =
  do runHaddock "--gen-contents" $(mkRelFile "index.html")
     runHaddock "--gen-index" $(mkRelFile "doc-index.html")
     combineHoogle
  where
    runWithLogging = runStackLoggingT mgr logLevel
    runHaddock genOpt destFilename =
      do let destPath = toFilePath (docLoc </> destFilename)
         want [destPath]
         destPath %> \_ ->
           runWithLogging
               (do needDeps
                   ifcOpts <- liftIO (fmap concat (mapM toInterfaceOpt (S.toList packages)))
                   runIn docLoc
                         "haddock"
                         mempty
                         (genOpt:ifcOpts)
                         Nothing)
    toInterfaceOpt package =
      do let pv = joinPkgVer (packageName package,packageVersion package)
             srcPath = (toFilePath docLoc) ++ "/" ++
                       pv ++ "/" ++
                       packageNameString (packageName package) ++ "." ++
                       haddockExtension
         exists <- doesFileExist srcPath
         return (if exists
                    then ["-i"
                         ,"../" ++
                          pv ++
                          "," ++
                          srcPath]
                     else [])
    combineHoogle =
      do let destHoogleDbLoc = hoogleDatabaseFile docLoc
             destPath = toFilePath destHoogleDbLoc
         want [destPath]
         destPath %> \_ ->
           runWithLogging
               (do needDeps
                   srcHoogleDbs <- liftIO (fmap concat (mapM toSrcHoogleDb (S.toList packages)))
                   callProcess
                        "hoogle"
                        ("combine" :
                         "-o" :
                         toFilePath destHoogleDbLoc :
                         srcHoogleDbs))
    toSrcHoogleDb package =
      do let srcPath = toFilePath docLoc ++ "/" ++
                       joinPkgVer (packageName package,packageVersion package) ++ "/" ++
                       packageNameString (packageName package) ++ "." ++
                       hoogleDbExtension
         exists <- doesFileExist srcPath
         return (if exists
                    then [srcPath]
                    else [])
    needDeps =
      need (concatMap (\package -> if wanted package == Wanted
                                    then let dir = packageDir package
                                         in [toFilePath (builtFileFromDir dir)]
                                    else [])
                      (S.toList packages))

#ifndef mingw32_HOST_OS
-- | Remove existing links docs for package from @~/.shake/doc@.
removeDocLinks :: Path Abs Dir -> Package -> IO ()
removeDocLinks docLoc package =
  do createDirectoryIfMissing True
                              (toFilePath docLoc)
     userDocLs <-
       fmap (map (toFilePath docLoc ++))
            (getDirectoryContents (toFilePath docLoc))
     forM_ userDocLs $
       \docPath ->
         do isDir <- doesDirectoryExist docPath
            when isDir
                 (case breakPkgVer (FilePath.takeFileName docPath) of
                    Just (p,_) ->
                      when (p == packageName package)
                           (removeLink docPath)
                    Nothing -> return ())

-- | Add link for package to @~/.shake/doc@.
createDocLinks :: Path Abs Dir -> Package -> IO ()
createDocLinks docLoc package =
  do let pkgVer =
           joinPkgVer (packageName package,(packageVersion package))
     pkgVerLoc <- liftIO (parseRelDir pkgVer)
     let pkgDestDocLoc = docLoc </> pkgVerLoc
         pkgDestDocPath =
           FilePath.dropTrailingPathSeparator (toFilePath pkgDestDocLoc)
         cabalDocLoc = parent docLoc </>
                       $(mkRelDir "share/doc/")
     haddockLocs <-
       do cabalDocExists <- doesDirectoryExist (toFilePath cabalDocLoc)
          if cabalDocExists
             then findFiles cabalDocLoc
                            (\fileLoc ->
                               FilePath.takeExtensions (toFilePath fileLoc) ==
                               "." ++ haddockExtension &&
                               dirname (parent fileLoc) ==
                               $(mkRelDir "html/") &&
                               toFilePath (dirname (parent (parent fileLoc))) ==
                               (pkgVer ++ "/"))
                            (\dirLoc ->
                               not (isHiddenDir dirLoc) &&
                               dirname (parent (parent dirLoc)) /=
                               $(mkRelDir "html/"))
             else return []
     case haddockLocs of
       [haddockLoc] ->
         case stripDir (parent docLoc)
                          haddockLoc of
           Just relHaddockPath ->
             do let srcRelPathCollapsed =
                      FilePath.takeDirectory (FilePath.dropTrailingPathSeparator (toFilePath relHaddockPath))
                    {-srcRelPath = "../" ++ srcRelPathCollapsed-}
                createSymbolicLink (FilePath.dropTrailingPathSeparator srcRelPathCollapsed)
                                   pkgDestDocPath
           Nothing -> return ()
       _ -> return ()
#endif /* not defined(mingw32_HOST_OS) */

-- | Get @-i@ arguments for haddock for dependencies.
haddockInterfaceOpts :: Path Abs Dir -> Package -> Set Package -> IO [String]
haddockInterfaceOpts userDocLoc package packages =
  do mglobalDocLoc <- getGlobalDocPath
     globalPkgVers <-
       case mglobalDocLoc of
         Nothing -> return M.empty
         Just globalDocLoc -> getDocPackages globalDocLoc
     let toInterfaceOpt pn =
           case find (\dpi -> packageName dpi == pn) (S.toList packages) of
             Nothing ->
               return (case (M.lookup pn globalPkgVers,mglobalDocLoc) of
                         (Just (v:_),Just globalDocLoc) ->
                           ["-i"
                           ,"../" ++ joinPkgVer (pn,v) ++
                            "," ++
                            toFilePath globalDocLoc ++ "/" ++
                            joinPkgVer (pn,v) ++ "/" ++
                            packageNameString pn ++ "." ++
                            haddockExtension]
                         _ -> [])
             Just dpi ->
               do let destPath = (toFilePath userDocLoc ++ "/" ++
                                 joinPkgVer (pn,packageVersion dpi) ++ "/" ++
                                 packageNameString pn ++ "." ++
                                 haddockExtension)
                  exists <- doesFileExist destPath
                  return (if exists
                             then ["-i"
                                  ,"../" ++
                                   joinPkgVer (pn,packageVersion dpi) ++
                                   "," ++
                                   destPath]
                             else [])
     --TODO: use not only direct dependencies, but dependencies of dependencies etc.
     --(e.g. redis-fp doesn't include @text@ in its dependencies which means the 'Text'
     --datatype isn't linked in its haddocks)
     fmap concat (mapM toInterfaceOpt (S.toList (packageAllDeps package)))

--------------------------------------------------------------------------------
-- Paths

{- EKB TODO: doc generation for stack-doc-server
-- | Returns true for paths whose last directory component begins with ".".
isHiddenDir :: Path b Dir -> Bool
isHiddenDir = isPrefixOf "." . toFilePath . dirname
        -}
--}
