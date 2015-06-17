{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
-- | Construct a @Plan@ for how to build
module Stack.Build.ConstructPlan
    ( constructPlan
    ) where

import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger (MonadLogger)
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as S8
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Distribution.Package (Dependency (..))
import           Distribution.Version         (anyVersion,
                                               intersectVersionRanges)
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Prelude hiding (FilePath, pi, writeFile)
import           Stack.Build.Cache
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Build.Types
import           Stack.BuildPlan

import           Stack.Package
import           Stack.PackageIndex
import           Stack.Types

data PackageInfo
    = PIOnlyInstalled Version Location Installed
    | PIOnlySource PackageSource
    | PIBoth PackageSource Installed

combineSourceInstalled :: PackageSource
                       -> (Version, Location, Installed)
                       -> PackageInfo
combineSourceInstalled ps (version, location, installed) =
    assert (piiVersion ps == version) $
    assert (piiLocation ps == location) $
    case location of
        -- Always trust something in the snapshot
        Snap -> PIOnlyInstalled version location installed
        Local -> PIBoth ps installed

type CombinedMap = Map PackageName PackageInfo

combineMap :: SourceMap -> InstalledMap -> CombinedMap
combineMap = Map.mergeWithKey
    (\_ s i -> Just $ combineSourceInstalled s i)
    (fmap PIOnlySource)
    (fmap (\(v, l, i) -> PIOnlyInstalled v l i))

data AddDepRes
    = ADRToInstall Task
    | ADRFound Version Installed
    deriving Show

type M = RWST
    Ctx
    ( Map PackageName (Either ConstructPlanException Task) -- finals
    , Map Text Location -- executable to be installed, and location where the binary is placed
    )
    (Map PackageName (Either ConstructPlanException AddDepRes))
    IO

data Ctx = Ctx
    { mbp            :: !MiniBuildPlan
    , baseConfigOpts :: !BaseConfigOpts
    , loadPackage    :: !(PackageName -> Version -> Map FlagName Bool -> IO Package)
    , combinedMap    :: !CombinedMap
    , toolToPackages :: !(Dependency -> Map PackageName VersionRange)
    , ctxBuildConfig :: !BuildConfig
    , callStack      :: ![PackageName]
    , extraToBuild   :: !(Set PackageName)
    , latestVersions :: !(Map PackageName Version)
    }

instance HasStackRoot Ctx
instance HasPlatform Ctx
instance HasConfig Ctx
instance HasBuildConfig Ctx where
    getBuildConfig = ctxBuildConfig

constructPlan :: forall env m.
                 (MonadCatch m, MonadReader env m, HasBuildConfig env, MonadIO m, MonadLogger m, MonadBaseControl IO m, HasHttpManager env)
              => MiniBuildPlan
              -> BaseConfigOpts
              -> [LocalPackage]
              -> Set PackageName -- ^ additional packages that must be built
              -> Set GhcPkgId -- ^ locally registered
              -> (PackageName -> Version -> Map FlagName Bool -> IO Package) -- ^ load upstream package
              -> SourceMap
              -> InstalledMap
              -> m Plan
constructPlan mbp0 baseConfigOpts0 locals extraToBuild0 locallyRegistered loadPackage0 sourceMap installedMap = do
    menv <- getMinimalEnvOverride
    caches <- getPackageCaches menv
    let latest = Map.fromListWith max $ map toTuple $ Map.keys caches

    bconfig <- asks getBuildConfig
    let onWanted =
            case boptsFinalAction $ bcoBuildOpts baseConfigOpts0 of
                DoNothing -> void . addDep . packageName . lpPackage
                _ -> addFinal
    let inner = do
            mapM_ onWanted $ filter lpWanted locals
            mapM_ addDep $ Set.toList extraToBuild0
    ((), m, (efinals, installExes)) <- liftIO $ runRWST inner (ctx bconfig latest) M.empty
    let toEither (_, Left e)  = Left e
        toEither (k, Right v) = Right (k, v)
        (errlibs, adrs) = partitionEithers $ map toEither $ M.toList m
        (errfinals, finals) = partitionEithers $ map toEither $ M.toList efinals
        errs = errlibs ++ errfinals
    if null errs
        then do
            let toTask (_, ADRFound _ _) = Nothing
                toTask (name, ADRToInstall task) = Just (name, task)
                tasks = M.fromList $ mapMaybe toTask adrs
                maybeStripLocals
                    | boptsOnlySnapshot $ bcoBuildOpts baseConfigOpts0 =
                        stripLocals
                    | otherwise = id
            return $ maybeStripLocals Plan
                { planTasks = tasks
                , planFinals = M.fromList finals
                , planUnregisterLocal = mkUnregisterLocal tasks locallyRegistered
                , planInstallExes =
                    if boptsInstallExes $ bcoBuildOpts baseConfigOpts0
                        then installExes
                        else Map.empty
                }
        else throwM $ ConstructPlanExceptions errs (bcStackYaml bconfig)
  where
    ctx bconfig latest = Ctx
        { mbp = mbp0
        , baseConfigOpts = baseConfigOpts0
        , loadPackage = loadPackage0
        , combinedMap = combineMap sourceMap installedMap
        , toolToPackages = \ (Dependency name _) ->
          maybe Map.empty (Map.fromSet (\_ -> anyVersion)) $
          Map.lookup (S8.pack . packageNameString . fromCabalPackageName $ name) toolMap
        , ctxBuildConfig = bconfig
        , callStack = []
        , extraToBuild = extraToBuild0
        , latestVersions = latest
        }
    toolMap = getToolMap mbp0

-- | Determine which packages to unregister based on the given tasks and
-- already registered local packages
mkUnregisterLocal :: Map PackageName Task -> Set GhcPkgId -> Set GhcPkgId
mkUnregisterLocal tasks locallyRegistered =
    Set.filter toUnregister locallyRegistered
  where
    toUnregister gid =
        case M.lookup name tasks of
            Nothing -> False
            Just _ -> True
      where
        ident = ghcPkgIdPackageIdentifier gid
        name = packageIdentifierName ident

addFinal :: LocalPackage -> M ()
addFinal lp = do
    depsRes <- addPackageDeps package
    res <- case depsRes of
        Left e -> return $ Left e
        Right (missing, present) -> do
            ctx <- ask
            return $ Right Task
                { taskProvides = PackageIdentifier
                    (packageName package)
                    (packageVersion package)
                , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                    let allDeps = Set.union present missing'
                     in configureOpts
                            (getConfig ctx)
                            (baseConfigOpts ctx)
                            allDeps
                            True -- wanted
                            Local
                            (packageFlags package)
                , taskPresent = present
                , taskType = TTLocal lp
                }
    tell (Map.singleton (packageName package) res, mempty)
  where
    package = lpPackageFinal lp

addDep :: PackageName -> M (Either ConstructPlanException AddDepRes)
addDep name = do
    m <- get
    case Map.lookup name m of
        Just res -> return res
        Nothing -> do
            res <- addDep' name
            modify $ Map.insert name res
            return res

addDep' :: PackageName -> M (Either ConstructPlanException AddDepRes)
addDep' name = do
    ctx <- ask
    if name `elem` callStack ctx
        then return $ Left $ DependencyCycleDetected $ name : callStack ctx
        else local
            (\ctx' -> ctx' { callStack = name : callStack ctx' }) $ do
            (addDep'' name)

addDep'' :: PackageName -> M (Either ConstructPlanException AddDepRes)
addDep'' name = do
    ctx <- ask
    case Map.lookup name $ combinedMap ctx of
        -- TODO look up in the package index and see if there's a
        -- recommendation available
        Nothing -> return $ Left $ UnknownPackage name
        Just (PIOnlyInstalled version loc installed) -> do
            tellExecutablesUpstream name version loc Map.empty -- slightly hacky, no flags since they likely won't affect executable names
            return $ Right $ ADRFound version installed
        Just (PIOnlySource ps) -> do
            tellExecutables name ps
            installPackage name ps
        Just (PIBoth ps installed) -> do
            tellExecutables name ps
            needInstall <- checkNeedInstall name ps installed
            if needInstall
                then installPackage name ps
                else return $ Right $ ADRFound (piiVersion ps) installed

tellExecutables :: PackageName -> PackageSource -> M () -- TODO merge this with addFinal above?
tellExecutables _ (PSLocal lp)
    | lpWanted lp = tellExecutablesPackage Local $ lpPackage lp
    | otherwise = return ()
tellExecutables name (PSUpstream version loc flags) = do
    tellExecutablesUpstream name version loc flags

tellExecutablesUpstream :: PackageName -> Version -> Location -> Map FlagName Bool -> M ()
tellExecutablesUpstream name version loc flags = do
    ctx <- ask
    when (name `Set.member` extraToBuild ctx) $ do
        p <- liftIO $ loadPackage ctx name version flags
        tellExecutablesPackage loc p

tellExecutablesPackage :: Location -> Package -> M ()
tellExecutablesPackage loc p =
    tell (Map.empty, m)
  where
    m = Map.fromList $ map (, loc) $ Set.toList $ packageExes p

-- TODO There are a lot of duplicated computations below. I've kept that for
-- simplicity right now

installPackage :: PackageName -> PackageSource -> M (Either ConstructPlanException AddDepRes)
installPackage name ps = do
    ctx <- ask
    package <- psPackage name ps
    depsRes <- addPackageDeps package
    case depsRes of
        Left e -> return $ Left e
        Right (missing, present) -> do
            return $ Right $ ADRToInstall Task
                { taskProvides = PackageIdentifier
                    (packageName package)
                    (packageVersion package)
                , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                    let allDeps = Set.union present missing'
                     in configureOpts
                            (getConfig ctx)
                            (baseConfigOpts ctx)
                            allDeps
                            (psWanted ps)
                            (piiLocation ps)
                            (packageFlags package)
                , taskPresent = present
                , taskType =
                    case ps of
                        PSLocal lp -> TTLocal lp
                        PSUpstream _ loc _ -> TTUpstream package loc
                }

checkNeedInstall :: PackageName -> PackageSource -> Installed -> M Bool
checkNeedInstall name ps installed = assert (piiLocation ps == Local) $ do
    package <- psPackage name ps
    depsRes <- addPackageDeps package
    case depsRes of
        Left _e -> return True -- installPackage will find the error again
        Right (missing, present)
            | Set.null missing -> checkDirtiness ps installed package present
            | otherwise -> return True

addPackageDeps :: Package -> M (Either ConstructPlanException (Set PackageIdentifier, Set GhcPkgId))
addPackageDeps package = do
    ctx <- ask
    deps' <- packageDepsWithTools package
    deps <- forM (Map.toList deps') $ \(depname, range) -> do
        eres <- addDep depname
        case eres of
            Left e ->
                let bd =
                        case e of
                            UnknownPackage name ->
                                NotInBuildPlan $ Map.lookup name $ latestVersions ctx
                            _ -> Couldn'tResolveItsDependencies
                 in return $ Left (depname, (range, bd))
            Right adr | not $ adrVersion adr `withinRange` range ->
                return $ Left (depname, (range, DependencyMismatch $ adrVersion adr))
            Right (ADRToInstall task) -> return $ Right
                (Set.singleton $ taskProvides task, Set.empty)
            Right (ADRFound _ (Executable _)) -> return $ Right
                (Set.empty, Set.empty)
            Right (ADRFound _ (Library gid)) -> return $ Right
                (Set.empty, Set.singleton gid)
    case partitionEithers deps of
        ([], pairs) -> return $ Right $ mconcat pairs
        (errs, _) -> return $ Left $ DependencyPlanFailures
            (PackageIdentifier
                (packageName package)
                (packageVersion package))
            (Map.fromList errs)
  where
    adrVersion (ADRToInstall task) = packageIdentifierVersion $ taskProvides task
    adrVersion (ADRFound v _) = v

checkDirtiness :: PackageSource
               -> Installed
               -> Package
               -> Set GhcPkgId
               -> M Bool
checkDirtiness ps installed package present = do
    ctx <- ask
    let configOpts = configureOpts
            (getConfig ctx)
            (baseConfigOpts ctx)
            present
            (psWanted ps)
            (piiLocation ps) -- should be Local always
            (packageFlags package)
        configCache = ConfigCache
            { configCacheOpts = map encodeUtf8 configOpts
            , configCacheDeps = present
            , configCacheComponents =
                case ps of
                    PSLocal lp -> Set.map encodeUtf8 $ lpComponents lp
                    PSUpstream _ _ _ -> Set.empty
            }
    moldOpts <- tryGetFlagCache installed
    case moldOpts of
        Nothing -> return True
        Just oldOpts -> return $ oldOpts /= configCache || psDirty ps

psDirty :: PackageSource -> Bool
psDirty (PSLocal lp) = lpDirtyFiles lp
psDirty (PSUpstream _ _ _) = False -- files never change in an upstream package

psWanted :: PackageSource -> Bool
psWanted (PSLocal lp) = lpWanted lp
psWanted (PSUpstream _ _ _) = False

psPackage :: PackageName -> PackageSource -> M Package
psPackage _ (PSLocal lp) = return $ lpPackage lp
psPackage name (PSUpstream version _ flags) = do
    ctx <- ask
    liftIO $ loadPackage ctx name version flags

-- | Get all of the dependencies for a given package, including guessed build
-- tool dependencies.
packageDepsWithTools :: Package -> M (Map PackageName VersionRange)
packageDepsWithTools p = do
    ctx <- ask
    return $ Map.unionsWith intersectVersionRanges
           $ packageDeps p
           : map (toolToPackages ctx) (packageTools p)

-- | Strip out anything from the @Plan@ intended for the local database
stripLocals :: Plan -> Plan
stripLocals plan = plan
    { planTasks = Map.filter checkTask $ planTasks plan
    , planFinals = Map.empty
    , planUnregisterLocal = Set.empty
    , planInstallExes = Map.filter (/= Local) $ planInstallExes plan
    }
  where
    checkTask task =
        case taskType task of
            TTLocal _ -> False
            TTUpstream _ Local -> False
            TTUpstream _ Snap -> True
