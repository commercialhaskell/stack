{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Build project(s).

module Stack.Build
  (build
  ,clean)
  where

import qualified Control.Applicative as A
import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow ((&&&))
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Exception.Enclosed (handleIO)
import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (asks, runReaderT)
import           Control.Monad.Trans.Resource
import           Control.Monad.State.Strict
import           Control.Monad.Writer
import           Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Conduit
import           Data.Conduit.Binary (sinkHandle)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Either
import           Data.Function
import           Data.IORef
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
import           Data.Typeable (Typeable)
import           Development.Shake (addOracle,Action)
import           Distribution.Package (Dependency (..))
import           Distribution.System (Platform (Platform), OS (Windows))
import           Distribution.Version (intersectVersionRanges, anyVersion)
import           Network.HTTP.Conduit (Manager)
import           Network.HTTP.Download
import           Path as FL
import           Prelude hiding (FilePath,writeFile)
import           Shake
import           Stack.Build.Types
import           Stack.BuildPlan
import           Stack.Constants
import           Stack.Fetch as Fetch
import           Stack.GhcPkg
import           Stack.Package
import           Stack.PackageDump
import           Stack.Types
import           Stack.Types.Internal
import           Stack.Types.StackT
import           System.Directory hiding (findFiles, findExecutable)
import           System.Exit (ExitCode (ExitSuccess))
import           System.IO
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process.Read

{- EKB FIXME: doc generation for stack-doc-server
#ifndef mingw32_HOST_OS
import           System.Posix.Files (createSymbolicLink,removeLink)
#endif
--}
data Installed = Library GhcPkgId | Executable
    deriving Show

data Location = Global | Snap | Local
    deriving (Show, Eq)

type M env m = (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m,HasLogLevel env)

type SourceMap = Map PackageName (Version, PackageSource)
data PackageSource
    = PSLocal LocalPackage
    | PSExtraDeps Package
    | PSSnapshot MiniPackageInfo
    | PSInstalledLib Location GhcPkgId
    | PSInstalledExe Location

getInstalled :: M env m
             => EnvOverride
             -> Bool -- ^ profiling?
             -> SourceMap -- ^ does not contain any installed information
             -> m SourceMap
getInstalled menv profiling sourceMap1 = do
    snapDBPath <- packageDatabaseDeps
    localDBPath <- packageDatabaseLocal

    bconfig <- asks getBuildConfig

    pcache <- loadProfilingCache $ configProfilingCache bconfig

    let loadDatabase' = loadDatabase menv pcache profiling
    (sourceMap2, localInstalled) <-
        loadDatabase' Global Nothing sourceMap1 >>=
        loadDatabase' Snap (Just snapDBPath) . fst >>=
        loadDatabase' Local (Just localDBPath) . fst

    saveProfilingCache (configProfilingCache bconfig) pcache

    {- FIXME executables
    snapExePath <- error "snap exes"
    localExePath <- error "localExePath"

    let libraries = Map.fromList
                  $ map (\(gid, loc) ->
                    let PackageIdentifier name ver = ghcPkgIdPackageIdentifier gid
                     in (name, (ver, loc, Library gid)))
                  $ Map.toList libraryIds
        executables = M.empty -- FIXME
    return $ M.union libraries executables
    -}

    return sourceMap2

data LocalPackage = LocalPackage
    { lpPackage :: Package
    , lpWanted :: Bool
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
                , packageConfigFlags =
                    fromMaybe M.empty $ M.lookup name $ bcFlags bconfig
                , packageConfigGhcVersion = bcGhcVersion bconfig
                , packageConfigPlatform = configPlatform $ getConfig bconfig
                }
            cabalfp
            PTUser
        -- FIXME check if name == what's inside pkg
        return LocalPackage
            { lpPackage = pkg
            , lpWanted = wanted
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
        any (`FL.isParentOf` dir) dirs ||
        any (== dir) dirs

data LoadHelper = LoadHelper
    { lhId :: !GhcPkgId
    , lhDeps :: ![GhcPkgId]
    , lhNew :: !Bool
    }

-- | Outputs both the modified SourceMap and the Set of all installed packages in this database
loadDatabase :: M env m
             => EnvOverride
             -> ProfilingCache
             -> Bool -- ^ require profiling libraries?
             -> Location
             -> Maybe (Path Abs Dir) -- ^ package database
             -> SourceMap
             -> m (SourceMap, Set GhcPkgId)
loadDatabase menv pcache profiling loc mdb sourceMap0 = do
    let sinkDP = addProfiling pcache
              =$ CL.filter isAllowed
              =$ CL.map dpToLH
              =$ CL.consume
        sinkGIDs = CL.map dpGhcPkgId =$ CL.consume
        sink = getZipSink $ (,)
            <$> ZipSink sinkDP
            <*> ZipSink sinkGIDs
    (lhs1, gids) <- ghcPkgDump menv mdb $ conduitDumpPackage =$= sink
    let lhs2 = lhs1 ++ installed0
        lhs3 = pruneDeps
            (packageIdentifierName . ghcPkgIdPackageIdentifier)
            lhId
            lhDeps
            const
            lhs2
        sourceMap1 = Map.fromList
            $ map (\lh ->
                let gid = lhId lh
                    PackageIdentifier name version = ghcPkgIdPackageIdentifier gid
                 in (name, (version, PSInstalledLib loc gid)))
            $ filter lhNew
            $ Map.elems lhs3
        sourceMap2 = Map.union sourceMap1 sourceMap0
    return (sourceMap2, Set.fromList gids)
  where
    -- Get a list of all installed GhcPkgIds with their "dependencies". The
    -- dependencies are always an empty list, since we don't need anything to
    -- use an installed dependency
    installed0 = flip mapMaybe (Map.toList sourceMap0) $ \x ->
        case x of
            (_, (_, PSInstalledLib _ gid)) -> Just LoadHelper
                { lhId = gid
                , lhDeps = []
                , lhNew = False
                }
            _ -> Nothing

    dpToLH dp = LoadHelper
        { lhId = dpGhcPkgId dp
        , lhDeps = dpDepends dp
        , lhNew = True
        }

    isAllowed dp
        | profiling && not (dpProfiling dp) = False
        | otherwise =
            case Map.lookup name sourceMap0 of
                Nothing -> True
                Just (version', ps)
                  | version /= version' -> False
                  | otherwise -> case ps of
                    -- Never trust an installed local, instead we do dirty
                    -- checking later when constructing the plan
                    PSLocal _ -> False

                    -- Shadow any installations in the global and snapshot
                    -- databases
                    PSExtraDeps _ -> loc == Local

                    -- Alows cool to have the right version of a
                    -- snapshot-listed package
                    PSSnapshot _ -> True

                    -- And then above we just resolve the conflict
                    PSInstalledLib _ _ -> True

                    -- Something's wrong if we think a package is
                    -- executable-only and it appears in a package datbase
                    PSInstalledExe _ -> assert False False
      where
        PackageIdentifier name version = ghcPkgIdPackageIdentifier $ dpGhcPkgId dp

data Task = Task
    { taskProvides :: !PackageIdentifier
    , taskRequiresMissing :: !(Set PackageIdentifier)
    , taskRequiresPresent :: !(Set GhcPkgId)
    , taskLocation :: !Location
    , taskType :: !TaskType
    }
    deriving Show

data TaskType = TTPackage LocalPackage | TTMPI MiniPackageInfo
    deriving Show

data S = S
    { callStack :: ![PackageName]
    , tasks :: !(Map PackageName Task)
    , failures :: ![ConstructPlanException]
    }

data ConstructPlanException
    = SnapshotPackageDependsOnLocal PackageName PackageIdentifier
    -- ^ Recommend adding to extra-deps
    | DependencyCycleDetected [PackageName]
    | DependencyPlanFailures PackageName (Set PackageName)
    | UnknownPackage PackageName
    -- ^ Recommend adding to extra-deps, give a helpful version number?
    | VersionOutsideRange PackageName PackageIdentifier VersionRange
    | Couldn'tMakePlanForWanted (Set PackageName)
    deriving (Show, Typeable, Eq)

newtype ConstructPlanExceptions = ConstructPlanExceptions [ConstructPlanException]
    deriving (Show, Typeable)
instance Exception ConstructPlanExceptions

data AddDepRes
    = ADRToInstall PackageIdentifier Location
    | ADRFound GhcPkgId Location
    | ADRFoundExe Location
    deriving Show

constructPlan :: MonadThrow m
              => [LocalPackage]
              -> SourceMap
              -> m (Map PackageName Task)
constructPlan locals sourceMap = do
    let s0 = S
            { callStack = []
            , tasks = M.empty
            , failures = []
            }
    ((), s) <- flip runStateT s0 $ do
        eres <- mapM addLocal $ filter lpWanted locals
        case partitionEithers eres of
            ([], _) -> return ()
            (errs, _) -> addFailure $ Couldn'tMakePlanForWanted $ Set.fromList errs
    if null $ failures s
        then return $ tasks s
        else throwM $ ConstructPlanExceptions $ failures s
  where
    addTask task = do
        modify $ \s -> s
            { tasks = Map.insert
                (packageIdentifierName $ taskProvides task)
                task
                (tasks s)
            }
        return $ Just $ ADRToInstall (taskProvides task) (taskLocation task)

    addFailure e = modify $ \s -> s { failures = e : failures s }
    checkCallStack name inner = do
        s <- get
        if name `elem` callStack s
            then do
                addFailure $ DependencyCycleDetected $ callStack s
                return $ Left name
            else do
                put s { callStack = name : callStack s }
                res <- inner
                s' <- get
                case callStack s' of
                    name':rest | name == name' -> do
                        put s' { callStack = rest }
                        return $ maybe (Left name) Right res
                    _ -> error $ "constructPlan invariant violated: call stack is corrupted: " ++ show (name, callStack s, callStack s')

    withDeps name loc deps inner = do
        eadrs <- mapM (uncurry (addDep name loc)) deps
        let (errs, adrs) = partitionEithers eadrs
        if null errs
            then inner adrs
            else do
                addFailure $ DependencyPlanFailures name $ Set.fromList errs
                return Nothing

    addLocal lp = checkCallStack name $ do
        withDeps name Local (M.toList $ packageDeps p) $ \adrs -> do
            -- FIXME do dirtiness checking here and, if not dirty, don't build
            -- FIXME probably need to cache results of calls to addLocal in S, possibly all of addDep
            addTask Task
                { taskProvides = ident
                , taskRequiresMissing = Set.fromList $ mapMaybe toMissing adrs
                , taskRequiresPresent = Set.fromList $ mapMaybe toPresent adrs
                , taskLocation = Local
                , taskType = TTPackage lp
                }
      where
        p = lpPackage lp
        name = packageName p
        version = packageVersion p
        ident = PackageIdentifier name version

    addExtraDep p =
        -- FIXME won't work once we add dirtiness tracking
        addLocal LocalPackage
            { lpPackage = p
            , lpWanted = False
            }

    addMPI name mpi = checkCallStack name $ do
        withDeps name Snap (map (, anyVersion) $ Set.toList $ mpiPackageDeps mpi) $ \adrs -> do
            addTask Task
                { taskProvides = ident
                , taskRequiresMissing = Set.fromList $ mapMaybe toMissing adrs
                , taskRequiresPresent = Set.fromList $ mapMaybe toPresent adrs
                , taskLocation = Snap
                , taskType = TTMPI mpi
                }
      where
        ident = PackageIdentifier name version
        version = mpiVersion mpi

    addDep user userloc name range =
        case Map.lookup name sourceMap of
            Nothing -> do
                addFailure $ UnknownPackage name
                return $ Left name
            Just (version, ps)
                | version `withinRange` range -> case ps of
                    PSLocal lp -> allowLocal version $ addLocal lp
                    PSExtraDeps p -> allowLocal version $ addExtraDep p
                    PSSnapshot mpi -> addMPI name mpi
                    PSInstalledLib loc gid -> allowLocation loc version $ return $ Right $ ADRFound gid loc
                    PSInstalledExe loc -> allowLocation loc version $ return $ Right $ ADRFoundExe loc
                | otherwise -> do
                    addFailure $ VersionOutsideRange
                        user
                        (PackageIdentifier name version)
                        range
                    return $ Left name
      where
        allowLocation loc version inner =
            case loc of
                Local -> allowLocal version inner
                _ -> inner
        allowLocal version inner =
            case userloc of
                Local -> inner
                _ -> do
                    addFailure $ SnapshotPackageDependsOnLocal user
                        (PackageIdentifier name version)
                    return $ Left name

    toMissing (ADRToInstall pi _) = Just pi
    toMissing _ = Nothing

    toPresent (ADRFound gid _) = Just gid
    toPresent _ = Nothing

-- | Build using Shake.
build :: M env m => BuildOpts -> m ()
build bopts = do
    menv <- getMinimalEnvOverride
    cabalPkgVer <- getCabalPkgVer menv

    bconfig <- asks getBuildConfig
    inBuildPlan <- case bcResolver bconfig of
        ResolverSnapshot snapName -> do
            $logDebug $ "Checking resolver: " <> renderSnapName snapName
            mbp <- loadMiniBuildPlan snapName
            return $ mbpPackages mbp
        ResolverGhc _ -> return M.empty

    locals <- loadLocals bopts
    let localNames = Set.fromList $ map (packageName . lpPackage) locals
        localDepVersions = bcExtraDeps bconfig

    extraDeps <- loadExtraDeps menv cabalPkgVer

    let sourceMap1 = Map.unions
            [ Map.fromList $ flip map locals $ \lp ->
                let p = lpPackage lp
                 in (packageName p, (packageVersion p, PSLocal lp))
            , Map.fromList $ flip map extraDeps $ \p ->
                (packageName p, (packageVersion p, PSExtraDeps p))
            , flip fmap inBuildPlan $ \mpi -> (mpiVersion mpi, PSSnapshot mpi)
            ]

    sourceMap2 <- getInstalled menv profiling sourceMap1

    plan <- constructPlan locals sourceMap2

    if boptsDryrun bopts
        then mapM_ ($logInfo . displayTask) $ Map.elems plan
        else executeTasks $ M.elems plan
  where
    profiling = boptsLibProfile bopts || boptsExeProfile bopts

loadExtraDeps :: M env m
              => EnvOverride
              -> PackageIdentifier -- ^ Cabal version
              -> m [Package]
loadExtraDeps menv cabalPkgVer = do
    bconfig <- asks getBuildConfig
    unpackDir <- configLocalUnpackDir
    dist <- distRelativeDir cabalPkgVer
    paths <- unpackPackageIdents menv unpackDir (Just dist)
        $ Set.fromList
        $ map fromTuple
        $ M.toList
        $ bcExtraDeps bconfig
    forM (Map.toList paths) $ \(ident, dir) -> do
        cabalfp <- getCabalFileName dir
        -- FIXME confirm this matches name <- parsePackageNameFromFilePath cabalfp
        let name = packageIdentifierName ident
            flags = fromMaybe M.empty (M.lookup name $ bcFlags bconfig)
            pc = depPackageConfig bconfig flags
        readPackage pc cabalfp PTDep
        -- FIXME confirm that the ident matches with what we just read?

-- | Package config to be used for dependencies
depPackageConfig :: BuildConfig -> Map FlagName Bool -> PackageConfig
depPackageConfig bconfig flags = PackageConfig
    { packageConfigEnableTests = False
    , packageConfigEnableBenchmarks = False
    , packageConfigFlags = flags
    , packageConfigGhcVersion = bcGhcVersion bconfig
    , packageConfigPlatform = configPlatform (getConfig bconfig)
    }

-- | For a dry run
displayTask :: Task -> Text
displayTask task = T.pack $ concat
    [ packageIdentifierString $ taskProvides task
    , ": database="
    , case taskLocation task of
        Global -> assert False "global"
        Snap -> "snapshot"
        Local -> "local"
    , ", source="
    , case taskType task of
        TTPackage lp -> toFilePath $ packageDir $ lpPackage lp
        TTMPI _ -> "package index"
    , if Set.null $ taskRequiresMissing task
        then ""
        else ", after: " ++ intercalate "," (map packageIdentifierString $ Set.toList $ taskRequiresMissing task)
    ]

-- | Perform the actual plan
executeTasks :: a
executeTasks = error "executeTasks"

{- FIXME
    cabalPkgVer <- getMinimalEnvOverride >>= getCabalPkgVer

    -- FIXME currently this will install all dependencies for the entire
    -- project even if just building a subset of the project
    locals <- determineLocals cabalPkgVer bopts
    localsWanted <- checkWanted locals bopts
    ranges <- getDependencyRanges locals
    dependencies <- getDependencies locals $
        M.unionWith (M.unionWith intersectVersionRanges)
            ranges
            (case boptsTargets bopts of
                Left _ -> M.empty
                Right names -> M.fromList $ map (, M.empty) names)

    installDependencies cabalPkgVer bopts dependencies
    toRemove <- getPackagesToRemove (Set.map packageIdentifier (S.fromList locals))
    buildLocals bopts localsWanted toRemove

-- | Get currently user-local-db-installed packages that need to be
-- removed before we install the new package set.
getPackagesToRemove :: (MonadIO m, MonadLogger m, MonadReader env m, HasBuildConfig env, MonadThrow m, MonadCatch m)
                    => Set PackageIdentifier -> m (Set PackageIdentifier)
getPackagesToRemove toInstall = do
    menv <- getMinimalEnvOverride
    localDB <- packageDatabaseLocal
    depDB <- packageDatabaseDeps
    globalDB <- getGlobalDB menv
    let allDBs =
            [localDB, depDB, globalDB]
    installed <-
        getPackageVersionsSet menv allDBs (== localDB)
    $logDebug
        ("Installed: " <>
         T.pack (show installed))
    $logDebug
        ("Package databases: " <>
         T.pack (show allDBs))
    let toRemove =
            Set.filter
                (\ident ->
                      Set.member
                          (packageIdentifierName ident)
                          (Set.map packageIdentifierName toInstall) &&
                      not (Set.member ident toInstall))
                installed
    $logDebug
        ("To remove: " <>
         T.pack (show toRemove))
    return toRemove

-- | Determine all of the local packages we wish to install. This does not
-- include any dependencies.
determineLocals
    :: (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
    => PackageIdentifier -- ^ Cabal version
    -> BuildOpts
    -> m [Package]
determineLocals cabalPkgVer bopts = do
    bconfig <- asks getBuildConfig

    $logDebug "Unpacking packages as necessary"
    menv <- getMinimalEnvOverride
    mdist <- liftM Just $ distRelativeDir cabalPkgVer
    paths2 <- unpackPackageIdents menv (configLocalUnpackDir bconfig) mdist
            $ Set.fromList
            $ map fromTuple
            $ M.toList
            $ bcExtraDeps bconfig
    let paths = M.fromList (map (, PTUser) $ Set.toList $ bcPackages bconfig)
             <> M.fromList (map (, PTDep) $ M.elems paths2)
    $logDebug $ "Installing from local directories: " <> T.pack (show paths)
    locals <- forM (M.toList paths) $ \(dir, ptype) -> do
        cabalfp <- getCabalFileName dir
        name <- parsePackageNameFromFilePath cabalfp
        readPackage (packageConfig name bconfig ptype) cabalfp ptype
    $logDebug $ "Local packages to install: " <> T.intercalate ", "
        (map (packageIdentifierText . packageIdentifier) locals)
    return locals
  where
    finalAction = boptsFinalAction bopts

    packageConfig name bconfig PTDep = PackageConfig
        { packageConfigEnableTests = False
        , packageConfigEnableBenchmarks = False
        , packageConfigFlags =
               fromMaybe M.empty (M.lookup name $ bcFlags bconfig)
        , packageConfigGhcVersion = bcGhcVersion bconfig
        , packageConfigPlatform = configPlatform (getConfig bconfig)
        }
    packageConfig name bconfig PTUser = PackageConfig
        { packageConfigEnableTests =
            case finalAction of
                DoTests -> True
                _ -> False
        , packageConfigEnableBenchmarks =
            case finalAction of
                DoBenchmarks -> True
                _ -> False
        , packageConfigFlags =
               fromMaybe M.empty (M.lookup name $ bcFlags bconfig)
        , packageConfigGhcVersion = bcGhcVersion bconfig
        , packageConfigPlatform = configPlatform (getConfig bconfig)
        }

-- | Get the version ranges for all dependencies. This takes care of checking
-- for consistency amongst the local packages themselves, and removing locally
-- provided dependencies from that list.
--
-- Note that we return a Map from the package name of the dependency, to a Map
-- of the user and the required range. This allows us to give user friendly
-- error messages.
getDependencyRanges
    :: (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
    => [Package] -- ^ locals
    -> m (Map PackageName (Map PackageName VersionRange))
getDependencyRanges locals = do
    -- All version ranges demanded by our local packages. We keep track of where the range came from for nicer error messages
    let allRanges =
            M.unionsWith M.union $ flip map locals $ \l ->
            fmap (M.singleton (packageName l)) (packageDeps l)

    -- Check and then strip out any dependencies provided by a local package
    let stripLocal (errs, ranges) local' =
            (errs', ranges')
          where
            name = packageName local'
            errs' = checkMismatches local' (fromMaybe M.empty $ M.lookup name ranges)
                 ++ errs
            ranges' = M.delete name ranges
        checkMismatches :: Package
                        -> Map PackageName VersionRange
                        -> [StackBuildException]
        checkMismatches pkg users =
            mapMaybe go (M.toList users)
          where
            version = packageVersion pkg
            go (user, range)
                | withinRange version range = Nothing
                | otherwise = Just $ MismatchedLocalDep
                    (packageName pkg)
                    (packageVersion pkg)
                    user
                    range

    case foldl' stripLocal ([], allRanges) locals of
        ([], ranges) -> return ranges
        (errs, _) -> throwM $ DependencyIssues errs

-- | Determine all of the dependencies which need to be available.
--
-- This function checks that the dependency ranges will all be satisfies
getDependencies
    :: (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
    => [Package] -- ^ locals
    -> Map PackageName (Map PackageName VersionRange) -- ^ ranges
    -> m (Map PackageName (Version, Map FlagName Bool))
getDependencies locals ranges = do
    -- Get global packages
    menv <- getMinimalEnvOverride


    bconfig <- asks getBuildConfig
    dependencies <- case bcResolver bconfig of
        ResolverSnapshot snapName -> do
            $logDebug $ "Checking resolver: " <> renderSnapName snapName
            mbp0 <- loadMiniBuildPlan snapName
            globals <- getPackageVersionMapWithGlobalDb menv (Just mbp0) []
            let mbp = mbp0
                    { mbpPackages = mbpPackages mbp0 `Map.union`
                        fmap (\v -> MiniPackageInfo
                            { mpiVersion = v
                            , mpiFlags = Map.empty
                            , mpiPackageDeps = Set.empty
                            , mpiToolDeps = Set.empty
                            , mpiExes = Set.empty
                            }) globals
                    }

            let toolMap = getToolMap mbp
                shadowed = Set.fromList $ map packageName locals
                isShadowed = (`Set.member` shadowed)
                toolDeps = M.unionsWith Set.union
                         $ flip concatMap locals
                         $ \local -> flip concatMap (packageTools local)
                         $ \(Dependency name' _) ->
                             let name = packageNameByteString $ fromCabalPackageName name'
                              in case M.lookup name toolMap of
                                     Nothing -> []
                                     Just pkgs -> map
                                        (\pkg -> M.singleton pkg (Set.singleton $ packageName local))
                                        (Set.toList pkgs)
                localTools = M.fromList $ map (\p -> (packageName p, ())) locals
                toolDeps' = M.difference toolDeps localTools

            (deps, users) <- resolveBuildPlan menv mbp isShadowed $ M.unionWith Set.union
                (fmap M.keysSet ranges)
                toolDeps'
            forM_ (M.toList users) $ \(name, users') -> $logDebug $
                T.concat
                    [ packageNameText name
                    , " used by "
                    , T.intercalate ", " $ map packageNameText
                                         $ Set.toList users'
                    ]
            return deps
        ResolverGhc _ -> do
            globals <- getPackageVersionMapWithGlobalDb menv Nothing []
            return $ fmap (, M.empty) globals

    let checkDepRange (dep, users) =
            concatMap go $ M.toList users
          where
            go (user, range) =
                case M.lookup dep dependencies of
                    Nothing -> [MissingDep2 user dep range]
                    Just (version, _)
                        | withinRange version range -> []
                        | otherwise -> [MismatchedDep dep version user range]
    case concatMap checkDepRange $ M.toList ranges of
        [] -> return ()
        errs -> throwM $ DependencyIssues errs
    return dependencies

-- | Install the given set of dependencies into the dependency database, if missing.
installDependencies
    :: (MonadIO m,MonadReader env m,HasLogLevel env,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
    => PackageIdentifier -- ^ Cabal version
    -> BuildOpts
    -> Map PackageName (Version, Map FlagName Bool)
    -> m ()
installDependencies cabalPkgVer bopts deps' = do
    bconfig <- asks getBuildConfig
    mgr <- asks getHttpManager
    logLevel <- asks getLogLevel
    pkgDbs <- getPackageDatabases bconfig BTDeps
    menv <- getMinimalEnvOverride

    installed <- liftM toIdents $ getPackageVersionMapWithGlobalDb menv Nothing pkgDbs
    let toInstall' = M.difference deps installed

    -- Get rid of non-library dependencies which are already installed
    toInstall <- liftM M.unions $ forM (M.toList toInstall') $ \(ident, flags) -> do
        dest <- configPackageInstalled ident
        exists <- liftIO $ doesFileExist $ toFilePath dest
        return $ if exists
            then M.empty
            else M.singleton ident flags

    configureResource <- newResource "cabal configure" 1
    installResource <- newResource "cabal install" 1
    cfgVar <- liftIO $ newMVar ConfigLock

    if M.null toInstall
        then $logDebug "All dependencies are already installed"
        else do
            if boptsDryrun bopts
               then dryRunPrint "dependencies" mempty (S.fromList (M.keys toInstall))
               else do
                 $logInfo $ "Installing dependencies: " <> T.intercalate ", " (map packageIdentifierText (M.keys toInstall))
                 withTempUnpacked cabalPkgVer (M.keys toInstall) $ \newPkgDirs -> do
                   $logInfo "All dependencies unpacked"
                   packages <- liftM S.fromList $ forM newPkgDirs $ \dir -> do
                       cabalfp <- getCabalFileName dir
                       name <- parsePackageNameFromFilePath cabalfp
                       flags <- case M.lookup name deps' of
                           Nothing -> assert False $ return M.empty
                           Just (_, flags) -> return flags
                       readPackage (packageConfig flags bconfig) cabalfp PTDep
                   plans <- forM (S.toList packages) $ \package -> do
                       let gconfig = GenConfig -- FIXME
                               { gconfigOptimize = False
                               , gconfigLibProfiling = True
                               , gconfigExeProfiling = False
                               , gconfigGhcOptions = []
                               , gconfigFlags = packageFlags package
                               , gconfigPkgId = error "gconfigPkgId"
                               }
                       return $ makePlan -- FIXME dedupe this code with buildLocals
                           mgr
                           logLevel
                           cabalPkgVer
                           M.empty
                           Wanted
                           bopts
                           bconfig
                           BTDeps
                           0 -- no wanted locals while building deps
                           gconfig
                           packages
                           package
                           configureResource
                           installResource
                           (userDocsDir (bcConfig bconfig))
                           cfgVar
                   runPlans bopts
                      (M.fromList $ map (, Wanted) $ Set.toList packages)
                      plans
                      (userDocsDir (bcConfig bconfig))
  where
    deps = M.fromList $ map (\(name, (version, flags)) -> (PackageIdentifier name version, flags))
                      $ M.toList deps'
    toIdents = M.fromList . map (\(name, version) -> (PackageIdentifier name version, ())) . M.toList

    packageConfig flags bconfig = PackageConfig
        { packageConfigEnableTests = False
        , packageConfigEnableBenchmarks = False
        , packageConfigFlags = flags
        , packageConfigGhcVersion = bcGhcVersion bconfig
        , packageConfigPlatform = configPlatform (getConfig bconfig)
        }

-- | Build all of the given local packages, assuming all necessary dependencies
-- are already installed.
buildLocals
    :: (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m,HasLogLevel env)
    => BuildOpts
    -> Map Package Wanted
    -> Set PackageIdentifier
    -> m ()
buildLocals bopts packagesToInstall packagesToRemove = do
     env <- ask
     bconfig <- asks getBuildConfig
     mgr <- asks getHttpManager
     logLevel <- asks getLogLevel
     menv <- getMinimalEnvOverride
     localDB <- packageDatabaseLocal
     depDB <- packageDatabaseDeps
     globalDB <- getGlobalDB menv
     -- Note that this unregistering must come before getting the list
     -- of 'pkgIds' below, because those ids are used for calculation
     -- of when a user package has been unregistered in the package
     -- database and therefore should be rebuilt and installed.
     unless (boptsDryrun bopts)
            (unregisterPackages menv [localDB,globalDB,depDB] (==localDB) packagesToRemove)
     pkgIds <- getGhcPkgIds menv [localDB]
                (map packageName (M.keys packagesToInstall))
     cabalPkgVer <- getCabalPkgVer menv
     configureResource <- newResource "cabal configure" 1
     installResource <- newResource "cabal install" 1
     cfgVar <- liftIO $ newMVar ConfigLock
     plans <-
       forM (M.toList packagesToInstall)
            (\(package, wantedTarget) ->
               do when (wantedTarget == Wanted && boptsFinalAction bopts /= DoNothing &&
                        packageType package == PTUser)
                       (liftIO (deleteGenFile cabalPkgVer (packageDir package)))
                  gconfig <- readGenConfigFile
                      cabalPkgVer
                      pkgIds
                      bopts
                      wantedTarget
                      package
                      cfgVar
                  return (makePlan mgr
                                   logLevel
                                   cabalPkgVer
                                   pkgIds
                                   wantedTarget
                                   bopts
                                   (getBuildConfig env)
                                   BTLocals
                                   (length $ filter (== Wanted) $ Map.elems packagesToInstall)
                                   gconfig
                                   (M.keysSet packagesToInstall)
                                   package
                                   configureResource
                                   installResource
                                   (userDocsDir (bcConfig bconfig))
                                   cfgVar))

     if boptsDryrun bopts
        then dryRunPrint "local packages" packagesToRemove (Set.map packageIdentifier (M.keysSet packagesToInstall))
        else runPlans bopts packagesToInstall plans (userDocsDir (bcConfig bconfig))

-- FIXME clean up this function to make it more nicely shareable
runPlans :: (MonadIO m, MonadReader env m, HasBuildConfig env, HasLogLevel env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
         => BuildOpts
         -> Map Package Wanted
         -> [(PkgDepsOracle -> Action [PackageIdentifier]) -> Rules ()]
         -> Path Abs Dir
         -> m ()
runPlans _bopts packagesToInstall plans _docLoc = do
    shakeDir <- asks configShakeFilesDir
    shakeArgs
        shakeDir
        defaultShakeThreads
        (do getPkgDeps <- makeOracle
            mapM_ ($ getPkgDeps) plans)
        {- EKB FIXME: doc generation for stack-doc-server
        when
            (boptsFinalAction bopts == DoHaddock)
            (buildDocIndex
                 (wanted pwd)
                 docLoc
                 packages
                 mgr
                 logLevel)
                                  --}
  where makeOracle =
            addOracle
                (\(PkgDeps name) ->
                      case find ((==name).packageName) (M.keys packagesToInstall) of
                        Just package ->
                            return
                                (map packageIdentifier
                                     (mapMaybe
                                          (\depname ->
                                                find
                                                    ((== depname) . packageName)
                                                    (M.keys packagesToInstall))
                                          (M.keys (packageDeps package))))
                        Nothing -> return [])

-- | Dry run output.
dryRunPrint :: MonadLogger m => Text -> Set PackageIdentifier -> Set PackageIdentifier -> m ()
dryRunPrint label toRemove toInstall = do
    unless
        (Set.null toRemove)
        (do $logInfo ("The following " <> label <> " will be unregistered:")
            forM_
                (S.toList toRemove)
                ($logInfo .
                 packageIdentifierText))
    unless
        (Set.null toInstall)
        (do $logInfo ("The following " <> label <> " will be installed:")
            forM_
                (S.toList toInstall)
                ($logInfo .
                 packageIdentifierText))

-- | Reset the build (remove Shake database and .gen files).
clean :: forall m env.
         (MonadIO m, MonadReader env m, HasHttpManager env, HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
      => m ()
clean =
  do env <- ask
     menv <- getMinimalEnvOverride
     cabalPkgVer <- getCabalPkgVer menv
     forM_ (S.toList (bcPackages $ getBuildConfig env))
           (\pkgdir ->
              do deleteGenFile cabalPkgVer pkgdir
                 distDir' <- liftM FL.toFilePath
                       (distDirFromDir cabalPkgVer pkgdir)
                 liftIO $ do
                     exists <- doesDirectoryExist distDir'
                     when exists (removeDirectoryRecursive distDir'))
     shakeDir <- asks configShakeFilesDir
     liftIO (do exists <- doesDirectoryExist (toFilePath shakeDir)
                when exists
                     (removeDirectoryRecursive (toFilePath shakeDir)))

--------------------------------------------------------------------------------
-- Shake plan

-- | Whether the target is wanted or not.
data Wanted
  = NotWanted
  | Wanted
  deriving (Eq)

-- | Make a Shake plan for a package.
makePlan :: Manager
         -> LogLevel
         -> PackageIdentifier
         -> Map PackageName GhcPkgId
         -> Wanted
         -> BuildOpts
         -> BuildConfig
         -> BuildType
         -> Int -- ^ number of wanted locals
         -> GenConfig
         -> Set Package
         -> Package
         -> Resource
         -> Resource
         -> Path Abs Dir
         -> MVar ConfigLock
         -> (PkgDepsOracle -> Action [PackageIdentifier])
         -> Rules ()
makePlan mgr logLevel cabalPkgVer pkgIds wanted bopts bconfig buildType wantedLocals gconfig packages package installResource configureResource docLoc cfgVar getPkgDeps = do
    configureTarget <-
        either throw return $
        liftM FL.toFilePath
            (configuredFileFromDir cabalPkgVer dir)
    buildTarget <-
        either throw return $
        liftM FL.toFilePath
            (builtFileFromDir cabalPkgVer dir)
    when
        (wanted == Wanted)
        (want [buildTarget])
    configureTarget %>
        const (do void (getPkgDeps (PkgDeps (packageName package)))
                  runWithLogging configureAction)
    buildTarget %> const (runWithLogging (buildAction configureTarget))
  where
    needSourceFiles =
        need (map FL.toFilePath (S.toList (packageFiles package)))
    dir =
        packageDir package
    runWithLogging =
        runStackLoggingT mgr logLevel
    configureAction = do
        needDependencies cabalPkgVer pkgIds bopts packages package cfgVar
        need
            [ toFilePath
                  (packageCabalFile package)]
        (setuphs,removeAfterwards) <-
            liftIO (ensureSetupHs dir)
        actionFinally
            (configurePackage
                 cabalPkgVer
                 bconfig
                 configureResource
                 setuphs
                 buildType
                 package
                 gconfig
                 (if wanted == Wanted && packageType package == PTUser
                      then boptsFinalAction bopts
                      else DoNothing))
            removeAfterwards
    buildAction configureTarget = do
        need [configureTarget]
        needSourceFiles
        (setuphs,removeAfterwards) <-
            liftIO (ensureSetupHs dir)
        actionFinally
            (buildPackage
                 cabalPkgVer
                 bopts
                 bconfig
                 setuphs
                 wanted
                 wantedLocals
                 buildType
                 packages
                 package
                 gconfig
                 (if wanted == Wanted && packageType package == PTUser
                      then boptsFinalAction bopts
                      else DoNothing)
                 installResource
                 docLoc)
            removeAfterwards
        writeFinalFiles cabalPkgVer cfgVar gconfig bconfig buildType dir package

-- | Specify that the given package needs the following other
-- packages.
needDependencies :: MonadAction m
                 => PackageIdentifier -- ^ Cabal version
                 -> Map PackageName GhcPkgId
                 -> BuildOpts
                 -> Set Package
                 -> Package
                 -> MVar ConfigLock
                 -> m ()
needDependencies cabalPkgVer pkgIds bopts packages package cfgVar =
  do deps <- mapM (\package' ->
                     let dir' = packageDir package'
                     in do genFile <- liftIO $ builtFileFromDir
                                cabalPkgVer
                                dir'
                           void (readGenConfigFile cabalPkgVer
                                                   pkgIds
                                                   bopts
                                                   NotWanted
                                                   package'
                                                   cfgVar)
                           return (FL.toFilePath genFile))
                  (mapMaybe (\name ->
                               find ((== name) . packageName)
                                    (S.toList packages))
                            (M.keys (packageDeps package)))
     need deps

--------------------------------------------------------------------------------
-- Build actions

getPackageDatabases :: MonadIO m => BuildConfig -> BuildType -> m [Path Abs Dir]
getPackageDatabases bconfig BTDeps =
    liftIO $ liftM return $ runReaderT packageDatabaseDeps bconfig
getPackageDatabases bconfig BTLocals = liftIO $ flip runReaderT bconfig $
    sequence
        [ packageDatabaseDeps
        , packageDatabaseLocal
        ]

getInstallRoot :: MonadIO m => BuildConfig -> BuildType -> m (Path Abs Dir)
getInstallRoot bconfig BTDeps = liftIO $ runReaderT installationRootDeps bconfig
getInstallRoot bconfig BTLocals = liftIO $ runReaderT installationRootLocal bconfig

-- | Write the final generated files after a build successfully
-- completes.
writeFinalFiles :: (MonadIO m)
                => PackageIdentifier -- ^ Cabal version
                -> MVar ConfigLock
                -> GenConfig
                -> BuildConfig
                -> BuildType
                -> Path Abs Dir
                -> Package
                -> m ()
writeFinalFiles cabalPkgVer cfgVar gconfig bconfig buildType dir package = liftIO $
         (do pkgDbs <- getPackageDatabases bconfig buildType
             menv <- runReaderT getMinimalEnvOverride bconfig
             mpkgid <- runNoLoggingT
                      $ flip runReaderT bconfig
                      $ findGhcPkgId
                            menv
                            pkgDbs
                            (packageName package)
             when (packageHasLibrary package && isNothing mpkgid)
                (throwIO (Couldn'tFindPkgId (packageName package)))

             -- Write out some record that we installed the package
             when (buildType == BTDeps && not (packageHasLibrary package)) $ do
                 dest <- flip runReaderT bconfig
                       $ configPackageInstalled $ PackageIdentifier
                            (packageName package)
                            (packageVersion package)
                 createDirectoryIfMissing True $ toFilePath $ parent dest
                 writeFile (toFilePath dest) "Installed"

             withMVar cfgVar (writeGenConfigFile
                                       cabalPkgVer
                                       dir
                                       gconfig {gconfigPkgId = mpkgid})
             updateGenFile cabalPkgVer dir)

-- | Build the given package with the given configuration.
configurePackage :: (MonadAction m)
                 => PackageIdentifier
                 -> BuildConfig
                 -> Resource
                 -> Path Abs File -- ^ Setup.hs file
                 -> BuildType
                 -> Package
                 -> GenConfig
                 -> FinalAction
                 -> m ()
configurePackage cabalPkgVer bconfig configureResource setuphs buildType package gconfig setupAction =
  do logPath <- liftIO $ runReaderT (buildLogPath package) bconfig
     liftIO (void (try (removeFile (FL.toFilePath logPath)) :: IO (Either IOException ())))
     pkgDbs <- getPackageDatabases bconfig buildType
     installRoot <- getInstallRoot bconfig buildType
     let runhaskell' = runhaskell False
                                  cabalPkgVer package setuphs bconfig buildType
     -- Uncertain as to why we cannot run configures in parallel. This appears
     -- to be a Cabal library bug. Original issue:
     -- https://github.com/fpco/stack/issues/84. Ideally we'd be able to remove
     -- this call to withResource.
     withResource configureResource 1 $ runhaskell'
       (concat [["configure","--user"]
               ,["--package-db=clear","--package-db=global"]
               ,map (("--package-db=" ++) . toFilePath) pkgDbs
               ,["--libdir=" ++ toFilePath (installRoot </> $(mkRelDir "lib"))
                ,"--bindir=" ++ toFilePath (installRoot </> bindirSuffix)
                ,"--datadir=" ++ toFilePath (installRoot </> $(mkRelDir "share"))
                ,"--docdir=" ++ toFilePath (installRoot </> $(mkRelDir "doc"))
                ]
               ,["--enable-library-profiling" | gconfigLibProfiling gconfig]
               ,["--enable-executable-profiling" | gconfigExeProfiling gconfig]
               ,["--enable-tests" | setupAction == DoTests]
               ,["--enable-benchmarks" | setupAction == DoBenchmarks]
               ,map (\(name,enabled) ->
                       "-f" <>
                       (if enabled
                           then ""
                           else "-") <>
                       flagNameString name)
                    (M.toList (packageFlags package))])

-- | Remove the dist/ dir of a package.
cleanPackage :: PackageIdentifier -- ^ Cabal version
             -> Package
             -> ConfigLock -- ^ Needed because this affects the config directory.
             -> IO ()
cleanPackage cabalPkgVer package _ = do
    dist <- distRelativeDir cabalPkgVer
    handleIO onErr $ removeDirectoryRecursive
        (toFilePath
             (packageDir package </> dist))
  where
    onErr e = putStrLn $ "FIXME Race condition https://github.com/fpco/stack/issues/155 triggered: " ++ show e

-- | Whether we're building dependencies (separate database and build
-- process), or locally specified packages.
data BuildType = BTDeps | BTLocals
    deriving (Eq)

-- | Build the given package with the given configuration.
buildPackage :: MonadAction m
             => PackageIdentifier
             -> BuildOpts
             -> BuildConfig
             -> Path Abs File -- ^ Setup.hs file
             -> Wanted
             -> Int -- ^ number of wanted locals
             -> BuildType
             -> Set Package
             -> Package
             -> GenConfig
             -> FinalAction
             -> Resource
             -> Path Abs Dir
             -> m ()
buildPackage cabalPkgVer bopts bconfig setuphs wanted wantedLocals buildType _packages package gconfig setupAction installResource _docLoc =
  do logPath <- liftIO $ runReaderT (buildLogPath package) bconfig
     liftIO (void (try (removeFile (FL.toFilePath logPath)) :: IO (Either IOException ())))
     let runhaskell' live = runhaskell live cabalPkgVer package setuphs bconfig buildType
         -- The purpose of singularBuild is to say whether we should print
         -- build output to the console (as opposed to a log file). The
         -- goal is to only do so when building a single local target
         -- package. The semantics are:
         --
         -- * Is there only one wanted local package? If so, we know that
         -- package will be the last one to be built, so we know its output
         -- won't end up interleaved with other builds
         --
         -- * Is it a user package? We never print information on dependencies
         --
         -- * Is this the wanted package? We don't want to print information
         -- on one of the local packages that was just pulled in as a
         -- dependency of the current target
         singularBuild = wantedLocals == 1 && packageType package == PTUser && wanted == Wanted
     runhaskell'
       singularBuild
       (concat [["build"]
               ,["--ghc-options=-O2" | gconfigOptimize gconfig]
               ,concat [["--ghc-options",T.unpack opt]
                        | opt <- boptsGhcOptions bopts
                        , packageType package == PTUser]])

     case setupAction of
       DoTests -> do
         let pkgRoot = packageDir package
         distRelativeDir' <- liftIO $ distRelativeDir cabalPkgVer
         let buildDir = pkgRoot </> distRelativeDir'
         menv <- liftIO $ configEnvOverride (getConfig bconfig) EnvSettings
            { esIncludeLocals = True
            , esIncludeGhcPackagePath = True
            }
         forM_ (Set.toList $ packageTests package) $ \testName -> do
           let exeExtension =
                case configPlatform $ getConfig bconfig of
                    Platform _ Windows -> ".exe"
                    _ -> ""
           nameDir <- liftIO $ parseRelDir $ T.unpack testName
           nameExe <- liftIO $ parseRelFile $ T.unpack testName ++ exeExtension
           let exeName = buildDir </> $(mkRelDir "build") </> nameDir </> nameExe
           exists <- liftIO $ doesFileExist $ toFilePath exeName
           if exists
               then runTestSuite
                        menv
                        (if singularBuild then Nothing else Just logPath)
                        pkgRoot
                        exeName
                        package
                        testName
               else $logInfo $ T.concat
                    [ "Test suite "
                    , testName
                    , " executable not found found for "
                    , T.pack $ packageNameString $ packageName package
                    ]
           -- Previously just used this, but see https://github.com/fpco/stack/issues/167
           -- runhaskell' singularBuild ["test"]
       DoHaddock ->
           do
              {- EKB FIXME: doc generation for stack-doc-server
 #ifndef mingw32_HOST_OS
              liftIO (removeDocLinks docLoc package)
 #endif
              ifcOpts <- liftIO (haddockInterfaceOpts docLoc package packages)
              --}
              runhaskell'
                         singularBuild
                         ["haddock"
                         ,"--html"]
              {- EKB FIXME: doc generation for stack-doc-server
                         ,"--hoogle"
                         ,"--hyperlink-source"
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
                             mempty -- FIXME: ?
                             "hoogle"
                             ["convert"
                             ,"--haddock"
                             ,hoogleTxtPath
                             ,hoogleDbPath])
              --}
       DoBenchmarks -> runhaskell' singularBuild ["bench"]
       _ -> return ()
     withResource installResource 1 (runhaskell' False ["install"])
     {- EKB FIXME: doc generation for stack-doc-server
 #ifndef mingw32_HOST_OS
     case setupAction of
       DoHaddock -> liftIO (createDocLinks docLoc package)
       _ -> return ()
 #endif
     --}

-- | Run a single test suite
runTestSuite :: (MonadIO m, MonadLogger m)
             => EnvOverride
             -> Maybe (Path Abs File) -- ^ optional log file, otherwise use console
             -> Path Abs Dir -- ^ working directory
             -> Path Abs File -- ^ executable
             -> Package
             -> Text -- ^ test name
             -> m ()
runTestSuite menv mlogFile pkgRoot fp package testName = do
    $logInfo display
    ec <- liftIO $ case mlogFile of
        Nothing -> go Inherit
        Just logFile -> withBinaryFile (toFilePath logFile) AppendMode $ go . UseHandle
    case ec of
        ExitSuccess -> return ()
        _ -> do
            $logError $ T.concat
                [ display
                , ": ERROR"
                , case mlogFile of
                    Nothing -> ""
                    Just logFile -> T.concat
                        [ " (see "
                        , T.pack $ toFilePath logFile
                        , ")"
                        ]
                ]
            liftIO $ throwM $ TestSuiteFailure fp mlogFile ec
  where
    display = T.concat
        [ packageIdentifierText $ packageIdentifier package
        , ": test "
        , testName
        ]
    go outerr = do
        (Just stdin', Nothing, Nothing, ph) <- createProcess (proc (toFilePath fp) [])
            { cwd = Just $ toFilePath pkgRoot
            , Process.env = envHelper menv
            , std_in = CreatePipe
            , std_out = outerr
            , std_err = outerr
            }
        hClose stdin'
        waitForProcess ph

-- | Run the Haskell command for the given package.
runhaskell :: (HasBuildConfig config,MonadAction m)
           => Bool
           -> PackageIdentifier
           -> Package
           -> Path Abs File -- ^ Setup.hs or Setup.lhs file
           -> config
           -> BuildType
           -> [String]
           -> m ()
runhaskell liveOutput cabalPkgVer package setuphs config' buildType args =
  do buildDir <- liftIO (stackageBuildDir cabalPkgVer package)
     liftIO (createDirectoryIfMissing True (FL.toFilePath buildDir))
     $logInfo display
     outRef <- liftIO (newIORef mempty)
     errRef <- liftIO (newIORef mempty)
     join (liftIO (catch (runWithRefs outRef errRef)
                         (\e@ProcessExitedUnsuccessfully{} ->
                              return (dumpLog outRef errRef e))))
  where
    runWithRefs outRef errRef = do
        menv <- liftIO $ iomenv
        exeName <- liftIO $ join $ findExecutable menv "runhaskell"
        distRelativeDir' <- liftIO $ distRelativeDir cabalPkgVer
        let subEnv =
                 fmap (filter (\(x, _) -> x /= "GHC_PACKAGE_PATH"))
               $ envHelper menv
        withSink $ \sink -> withCheckedProcess
          (cp exeName distRelativeDir')
             {cwd = Just (FL.toFilePath (packageDir package))
             ,Process.env = subEnv}
          (\ClosedStream stdout' stderr' -> runConcurrently $
                Concurrently (logFrom stdout' sink outRef) A.*>
                Concurrently (logFrom stderr' sink errRef))
        return (return ())
    dumpLog outRef errRef e = do
        if liveOutput
           then return ()
           else do $logError (display <> ": ERROR")
                   errs <- liftIO (readIORef errRef)
                   outs <- liftIO (readIORef outRef)
                   unless (S8.null outs)
                          (do $logError "Stdout was:"
                              $logError (T.decodeUtf8 outs))
                   unless (S8.null errs)
                          (do $logError "Stderr was:"
                              $logError (T.decodeUtf8 errs))
        liftIO (throwIO e)
    withSink inner = do
         logPath <- liftIO $ runReaderT (buildLogPath package) config'
         liftIO $ createDirectoryIfMissing True $ FL.toFilePath
                $ parent logPath
         withBinaryFile (FL.toFilePath logPath) AppendMode (inner . stdoutToo)
      where stdoutToo h
                | not liveOutput = sinkHandle h
                | configHideTHLoading (getConfig config') =
                        CL.iterM (S8.hPut h)
                    =$= CB.lines
                    =$= CL.filter (not . isTHLoading)
                    =$= CL.mapM_ S8.putStrLn
                | otherwise = CL.iterM S8.putStr =$= sinkHandle h
    logFrom src sink ref =
        src $=
        CL.mapM (\chunk ->
                   do liftIO (modifyIORef' ref (<> chunk))
                      return chunk) $$
        sink
    display =
      packageIdentifierText (packageIdentifier package) <>
      ": " <>
      case args of
        (cname:_) -> T.pack cname
        _ -> mempty
    cp exeName distRelativeDir' =
      proc (toFilePath exeName)
        (("-package=" ++ packageIdentifierString cabalPkgVer)
         : "-clear-package-db"
         : "-global-package-db"
         -- TODO: Perhaps we want to include the snapshot package database here
         -- as well
         : toFilePath setuphs
         : ("--builddir=" ++ toFilePath distRelativeDir')
         : args)
    iomenv = configEnvOverride (getConfig config') EnvSettings
            { esIncludeLocals =
                case buildType of
                    BTDeps -> False
                    BTLocals -> True
            , esIncludeGhcPackagePath = False
            }

-- | Is this line a Template Haskell "Loading package" line
-- ByteString
isTHLoading :: S8.ByteString -> Bool
isTHLoading bs =
    "Loading package " `S8.isPrefixOf` bs &&
    ("done." `S8.isSuffixOf` bs || "done.\r" `S8.isSuffixOf` bs)

-- | Ensure Setup.hs exists in the given directory. Returns an action
-- to remove it later.
ensureSetupHs :: Path Abs Dir -> IO (Path Abs File, IO ())
ensureSetupHs dir =
  do exists1 <- doesFileExist (FL.toFilePath fp1)
     exists2 <- doesFileExist (FL.toFilePath fp2)
     if exists1 || exists2
        then return (if exists1 then fp1 else fp2, return ())
        else do writeFile (FL.toFilePath fp1) "import Distribution.Simple\nmain = defaultMain"
                return (fp1, removeFile (FL.toFilePath fp1))
  where fp1 = dir </> $(mkRelFile "Setup.hs")
        fp2 = dir </> $(mkRelFile "Setup.lhs")

{- EKB FIXME: doc generation for stack-doc-server
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
             destPath = FL.toFilePath destHoogleDbLoc
         want [destPath]
         destPath %> \_ ->
           runWithLogging
               (do needDeps
                   srcHoogleDbs <- liftIO (fmap concat (mapM toSrcHoogleDb (S.toList packages)))
                   callProcess
                        mempty -- FIXME: ?
                        "hoogle"
                        ("combine" :
                         "-o" :
                         FL.toFilePath destHoogleDbLoc :
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
                                         in [FL.toFilePath (builtFileFromDir dir)]
                                    else [])
                      (S.toList packages))

#ifndef mingw32_HOST_OS
-- | Remove existing links docs for package from @~/.shake/doc@.
removeDocLinks :: Path Abs Dir -> Package -> IO ()
removeDocLinks docLoc package =
  --EKB FIXME: only when running in Docker, for now.
  do createDirectoryIfMissing True
                              (FL.toFilePath docLoc)
     userDocLs <-
       fmap (map (FL.toFilePath docLoc ++))
            (getDirectoryContents (FL.toFilePath docLoc))
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
  --EKB FIXME: only when running in Docker, for now.
  do let pkgVer =
           joinPkgVer (packageName package,(packageVersion package))
     pkgVerLoc <- liftIO (parseRelDir pkgVer)
     let pkgDestDocLoc = docLoc </> pkgVerLoc
         pkgDestDocPath =
           FilePath.dropTrailingPathSeparator (FL.toFilePath pkgDestDocLoc)
         cabalDocLoc = parent docLoc </>
                      --EKB FIXME: this does not work with .stack-work
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
         case FL.stripDir (parent docLoc)
                          haddockLoc of
           Just relHaddockPath ->
             do let srcRelPathCollapsed =
                      FilePath.takeDirectory (FilePath.dropTrailingPathSeparator (FL.toFilePath relHaddockPath))
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
               do let destPath = (FL.toFilePath userDocLoc ++ "/" ++
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
--}

--------------------------------------------------------------------------------
-- Generated files

-- | Should the generated config be considered invalid?
genFileInvalidated :: Map PackageName GhcPkgId
                   -> BuildOpts
                   -> GenConfig
                   -> PackageName
                   -> Package
                   -> Bool
genFileInvalidated pkgIds bopts gconfig pname package =
  or [installedPkgIdChanged
     ,optimizationsChanged
     ,profilingChanged
     ,ghcOptsChanged
     ,flagsChanged]
  where installedPkgIdChanged =
          gconfigPkgId gconfig /=
          M.lookup pname pkgIds
        ghcOptsChanged = boptsGhcOptions bopts /= gconfigGhcOptions gconfig
        profilingChanged =
          (boptsLibProfile bopts &&
           not (gconfigLibProfiling gconfig)) ||
          (boptsExeProfile bopts &&
           not (gconfigExeProfiling gconfig))
        optimizationsChanged =
          case boptsEnableOptimizations bopts of
            Just optimize
              | optimize /= gconfigOptimize gconfig && optimize -> True
            _ -> False
        flagsChanged = packageFlags package /= gconfigFlags gconfig

-- | Should the generated config be updated?
genFileChanged :: Map PackageName GhcPkgId
               -> BuildOpts
               -> GenConfig
               -> Package
               -> Bool
genFileChanged pkgIds bopts gconfig package =
  or [installedPkgIdChanged
     ,optimizationsChanged && not isDependency
     ,profilingChanged && not isDependency
     ,ghcOptsChanged && not isDependency
     ,flagsChanged]
  where pname = packageName package
        isDependency = packageType package == PTDep
        installedPkgIdChanged =
          gconfigPkgId gconfig /=
          M.lookup pname pkgIds
        ghcOptsChanged = boptsGhcOptions bopts /= gconfigGhcOptions gconfig
        profilingChanged =
          (boptsLibProfile bopts &&
           not (gconfigLibProfiling gconfig)) ||
          (boptsExeProfile bopts &&
           not (gconfigExeProfiling gconfig))
        optimizationsChanged =
          maybe False (/= gconfigOptimize gconfig) (boptsEnableOptimizations bopts)
        flagsChanged =
          packageFlags package /=
          gconfigFlags gconfig

-- | Write out the gen file for the build dir.
updateGenFile :: MonadIO m
              => PackageIdentifier -- ^ Cabal version
              -> Path Abs Dir -> m ()
updateGenFile cabalPkgVer dir = liftIO $ do
  fp <- builtFileFromDir cabalPkgVer dir
  L.writeFile (FL.toFilePath fp) ""

-- | Delete the gen file, which will cause a rebuild.
deleteGenFile :: MonadIO m
              => PackageIdentifier -- ^ Cabal version
              -> Path Abs Dir -> m ()
deleteGenFile cabalPkgVer dir = liftIO $ do
  fp <- configuredFileFromDir cabalPkgVer dir
  catch (removeFile (FL.toFilePath fp))
        (\(_ :: IOException) -> return ())

-- | Save generated configuration.
writeGenConfigFile :: MonadIO m
                   => PackageIdentifier -- ^ Cabal version
                   -> Path Abs Dir
                   -> GenConfig
                   -> ConfigLock -- ^ Needed because this affects the config directory.
                   -> m ()
writeGenConfigFile cabalPkgVer dir gconfig _ = liftIO $
  do built <- builtConfigFileFromDir cabalPkgVer dir
     createDirectoryIfMissing True (FL.toFilePath (FL.parent built))
     fp <- builtConfigFileFromDir cabalPkgVer dir
     L.writeFile (FL.toFilePath fp) (encode gconfig)

-- | Read the generated config file, or return a default based on the
-- build configuration.
readGenConfigFile :: MonadIO m
                  => PackageIdentifier -- ^ Cabal version
                  -> Map PackageName GhcPkgId
                  -> BuildOpts
                  -> Wanted
                  -> Package
                  -> MVar ConfigLock
                  -> m GenConfig
readGenConfigFile cabalPkgVer pkgIds bopts wanted package cfgVar = liftIO (withMVar cfgVar go)
  where name = packageName package
        dir = packageDir package
        go configLock =
          do fp <- builtConfigFileFromDir cabalPkgVer dir
             bytes <-
               catch (fmap Just
                           (S.readFile (FL.toFilePath fp)))
                     (\(_ :: IOException) ->
                        return Nothing)
             case bytes >>= decode . L.fromStrict of
               Just gconfig ->
                 if genFileChanged pkgIds bopts gconfig package
                    then
                         -- If the build config has changed such that the gen
                         -- config needs to be regenerated...
                         do let invalidated =
                                  genFileInvalidated pkgIds bopts gconfig name package
                            when (invalidated || wanted == Wanted)
                                 (deleteGenFile cabalPkgVer dir)
                            let gconfig' =
                                  (newConfig gconfig bopts package)
                            -- When a file has been invalidated it means the
                            -- configuration has changed such that things need
                            -- to be recompiled, hence the above setting of force
                            -- recomp.
                            cleanPackage cabalPkgVer package configLock
                            writeGenConfigFile cabalPkgVer dir gconfig' configLock
                            return gconfig'
                    else return gconfig -- No change, the generated config is consistent with the build config.
               Nothing ->
                 do maybe (return ())
                          (const (putStrLn ("Warning: Couldn't parse config file for " ++
                                            packageNameString name ++
                                            ", migrating to latest configuration format. This will force a rebuild.")))
                          bytes
                    deleteGenFile cabalPkgVer dir
                    let gconfig' =
                          newConfig defaultGenConfig bopts package
                    writeGenConfigFile cabalPkgVer dir gconfig' configLock
                    return gconfig' -- Probably doesn't exist or is out of date (not parseable.)

-- | Update a gen configuration using the build configuration.
newConfig :: GenConfig -- ^ Build configuration.
          -> BuildOpts -- ^ A base gen configuration.
          -> Package
          -> GenConfig
newConfig gconfig bopts package =
  defaultGenConfig
      {gconfigOptimize =
         maybe (gconfigOptimize gconfig)
               id
               (boptsEnableOptimizations bopts)
      ,gconfigLibProfiling = boptsLibProfile bopts ||
                             gconfigLibProfiling gconfig
      ,gconfigExeProfiling = boptsExeProfile bopts ||
                             gconfigExeProfiling gconfig
      ,gconfigGhcOptions = boptsGhcOptions bopts
      ,gconfigFlags = packageFlags package
      ,gconfigPkgId = gconfigPkgId gconfig}

--------------------------------------------------------------------------------
-- Package fetching

-- | Fetch and unpack the package.
withTempUnpacked :: (MonadIO m,MonadThrow m,MonadLogger m,MonadMask m,MonadReader env m,HasHttpManager env,HasConfig env,MonadBaseControl IO m)
                 => PackageIdentifier -- ^ Cabal version
                 -> [PackageIdentifier]
                 -> ([Path Abs Dir] -> m a)
                 -> m a
withTempUnpacked cabalPkgVer pkgs inner = withSystemTempDirectory "stack-unpack" $ \tmp -> do
    dest <- parseAbsDir tmp
    menv <- getMinimalEnvOverride
    mdist <- liftM Just $ distRelativeDir cabalPkgVer
    m <- unpackPackageIdents menv dest mdist $ Set.fromList pkgs
    inner $ M.elems m

--------------------------------------------------------------------------------
-- Paths

{- EKB FIXME: doc generation for stack-doc-server
-- | Returns true for paths whose last directory component begins with ".".
isHiddenDir :: Path b Dir -> Bool
isHiddenDir = isPrefixOf "." . toFilePath . dirname
--}
        -}

-- | Get the version of Cabal from the global package database.
getCabalPkgVer :: (MonadThrow m,MonadIO m,MonadLogger m)
               => EnvOverride -> m PackageIdentifier
getCabalPkgVer menv = do
    db <- getGlobalDB menv
    findGhcPkgId
        menv
        [db]
        cabalName >>=
        maybe
            (throwM (Couldn'tFindPkgId cabalName))
            (return . ghcPkgIdPackageIdentifier)
  where
    cabalName =
        $(mkPackageName "Cabal")

clean :: a
clean = error "clean"
