{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Distribution.Package (Dependency (..))
import           Distribution.Version         (anyVersion,
                                               intersectVersionRanges)
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Prelude hiding (FilePath, pi, writeFile)
import           Stack.Build.Cache
import           Stack.Build.Haddock
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Build.Types
import           Stack.BuildPlan

import           Stack.Package
import           Stack.PackageIndex
import           Stack.Types

data PackageInfo
    = PIOnlyInstalled Version InstallLocation Installed
    | PIOnlySource PackageSource
    | PIBoth PackageSource Installed

combineSourceInstalled :: PackageSource
                       -> (Version, InstallLocation, Installed)
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
    | ADRFound InstallLocation Version Installed
    deriving Show

type M = RWST
    Ctx
    ( Map PackageName (Either ConstructPlanException Task) -- finals
    , Map Text InstallLocation -- executable to be installed, and location where the binary is placed
    , Map PackageName Text -- why a local package is considered dirty
    )
    (Map PackageName (Either ConstructPlanException AddDepRes))
    IO

data Ctx = Ctx
    { mbp            :: !MiniBuildPlan
    , baseConfigOpts :: !BaseConfigOpts
    , loadPackage    :: !(PackageName -> Version -> Map FlagName Bool -> IO Package)
    , combinedMap    :: !CombinedMap
    , toolToPackages :: !(Dependency -> Map PackageName VersionRange)
    , ctxEnvConfig   :: !EnvConfig
    , callStack      :: ![PackageName]
    , extraToBuild   :: !(Set PackageName)
    , latestVersions :: !(Map PackageName Version)
    , wanted         :: !(Set PackageName)
    }

instance HasStackRoot Ctx
instance HasPlatform Ctx
instance HasConfig Ctx
instance HasBuildConfig Ctx where
    getBuildConfig = getBuildConfig . getEnvConfig
instance HasEnvConfig Ctx where
    getEnvConfig = ctxEnvConfig

constructPlan :: forall env m.
                 (MonadCatch m, MonadReader env m, HasEnvConfig env, MonadIO m, MonadLogger m, MonadBaseControl IO m, HasHttpManager env)
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

    econfig <- asks getEnvConfig
    let onWanted =
            case boptsFinalAction $ bcoBuildOpts baseConfigOpts0 of
                DoNothing -> void . addDep . packageName . lpPackage
                _ -> addFinal
    let inner = do
            mapM_ onWanted $ filter lpWanted locals
            mapM_ addDep $ Set.toList extraToBuild0
    ((), m, (efinals, installExes, dirtyReason)) <- liftIO $ runRWST inner (ctx econfig latest) M.empty
    let toEither (_, Left e)  = Left e
        toEither (k, Right v) = Right (k, v)
        (errlibs, adrs) = partitionEithers $ map toEither $ M.toList m
        (errfinals, finals) = partitionEithers $ map toEither $ M.toList efinals
        errs = errlibs ++ errfinals
    if null errs
        then do
            let toTask (_, ADRFound _ _ _) = Nothing
                toTask (name, ADRToInstall task) = Just (name, task)
                tasks = M.fromList $ mapMaybe toTask adrs
                maybeStripLocals
                    | boptsOnlySnapshot $ bcoBuildOpts baseConfigOpts0 =
                        stripLocals
                    | otherwise = id
            return $ maybeStripLocals Plan
                { planTasks = tasks
                , planFinals = M.fromList finals
                , planUnregisterLocal = mkUnregisterLocal tasks dirtyReason locallyRegistered
                , planInstallExes =
                    if boptsInstallExes $ bcoBuildOpts baseConfigOpts0
                        then installExes
                        else Map.empty
                }
        else throwM $ ConstructPlanExceptions errs (bcStackYaml $ getBuildConfig econfig)
  where
    ctx econfig latest = Ctx
        { mbp = mbp0
        , baseConfigOpts = baseConfigOpts0
        , loadPackage = loadPackage0
        , combinedMap = combineMap sourceMap installedMap
        , toolToPackages = \ (Dependency name _) ->
          maybe Map.empty (Map.fromSet (\_ -> anyVersion)) $
          Map.lookup (S8.pack . packageNameString . fromCabalPackageName $ name) toolMap
        , ctxEnvConfig = econfig
        , callStack = []
        , extraToBuild = extraToBuild0
        , latestVersions = latest
        , wanted = wantedLocalPackages locals
        }
    -- TODO Currently, this will only consider and install tools from the
    -- snapshot. It will not automatically install build tools from extra-deps
    -- or local packages.
    toolMap = getToolMap mbp0

-- | Determine which packages to unregister based on the given tasks and
-- already registered local packages
mkUnregisterLocal :: Map PackageName Task
                  -> Map PackageName Text
                  -> Set GhcPkgId
                  -> Map GhcPkgId Text
mkUnregisterLocal tasks dirtyReason locallyRegistered =
    Map.unions $ map toUnregisterMap $ Set.toList locallyRegistered
  where
    toUnregisterMap gid =
        case M.lookup name tasks of
            Nothing -> Map.empty
            Just _ -> Map.singleton gid
                    $ fromMaybe "likely unregistering due to a version change"
                    $ Map.lookup name dirtyReason
      where
        ident = ghcPkgIdPackageIdentifier gid
        name = packageIdentifierName ident

addFinal :: LocalPackage -> M ()
addFinal lp = do
    depsRes <- addPackageDeps package
    res <- case depsRes of
        Left e -> return $ Left e
        Right (missing, present, _minLoc) -> do
            ctx <- ask
            return $ Right Task
                { taskProvides = PackageIdentifier
                    (packageName package)
                    (packageVersion package)
                , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                    let allDeps = Set.union present missing'
                     in configureOpts
                            (getEnvConfig ctx)
                            (baseConfigOpts ctx)
                            allDeps
                            True -- wanted
                            Local
                            package
                , taskPresent = present
                , taskType = TTLocal lp
                }
    tell (Map.singleton (packageName package) res, mempty, mempty)
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
            return $ Right $ ADRFound loc version installed
        Just (PIOnlySource ps) -> do
            tellExecutables name ps
            installPackage name ps
        Just (PIBoth ps installed) -> do
            tellExecutables name ps
            needInstall <- checkNeedInstall name ps installed (wanted ctx)
            if needInstall
                then installPackage name ps
                else return $ Right $ ADRFound (piiLocation ps) (piiVersion ps) installed

tellExecutables :: PackageName -> PackageSource -> M () -- TODO merge this with addFinal above?
tellExecutables _ (PSLocal lp)
    | lpWanted lp = tellExecutablesPackage Local $ lpPackage lp
    | otherwise = return ()
tellExecutables name (PSUpstream version loc flags) = do
    tellExecutablesUpstream name version loc flags

tellExecutablesUpstream :: PackageName -> Version -> InstallLocation -> Map FlagName Bool -> M ()
tellExecutablesUpstream name version loc flags = do
    ctx <- ask
    when (name `Set.member` extraToBuild ctx) $ do
        p <- liftIO $ loadPackage ctx name version flags
        tellExecutablesPackage loc p

tellExecutablesPackage :: InstallLocation -> Package -> M ()
tellExecutablesPackage loc p =
    tell (Map.empty, m, Map.empty)
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
        Right (missing, present, minLoc) -> do
            return $ Right $ ADRToInstall Task
                { taskProvides = PackageIdentifier
                    (packageName package)
                    (packageVersion package)
                , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                    let allDeps = Set.union present missing'
                        destLoc = piiLocation ps <> minLoc
                     in configureOpts
                            (getEnvConfig ctx)
                            (baseConfigOpts ctx)
                            allDeps
                            (psWanted ps)
                            -- An assertion to check for a recurrence of
                            -- https://github.com/commercialhaskell/stack/issues/345
                            (assert (destLoc == piiLocation ps) destLoc)
                            package
                , taskPresent = present
                , taskType =
                    case ps of
                        PSLocal lp -> TTLocal lp
                        PSUpstream _ loc _ -> TTUpstream package $ loc <> minLoc
                }

checkNeedInstall :: PackageName -> PackageSource -> Installed -> Set PackageName -> M Bool
checkNeedInstall name ps installed wanted = assert (piiLocation ps == Local) $ do
    package <- psPackage name ps
    depsRes <- addPackageDeps package
    case depsRes of
        Left _e -> return True -- installPackage will find the error again
        Right (missing, present, _loc)
            | Set.null missing -> checkDirtiness ps installed package present wanted
            | otherwise -> do
                tell (Map.empty, Map.empty, Map.singleton name $
                    let t = T.intercalate ", " $ map (T.pack . packageNameString . packageIdentifierName) (Set.toList missing)
                     in T.append "missing dependencies: " $
                            if T.length t < 100
                                then t
                                else T.take 97 t <> "...")
                return True

addPackageDeps :: Package -> M (Either ConstructPlanException (Set PackageIdentifier, Set GhcPkgId, InstallLocation))
addPackageDeps package = do
    ctx <- ask
    deps' <- packageDepsWithTools package
    deps <- forM (Map.toList deps') $ \(depname, range) -> do
        eres <- addDep depname
        let mlatest = Map.lookup depname $ latestVersions ctx
        case eres of
            Left e ->
                let bd =
                        case e of
                            UnknownPackage name -> assert (name == depname) NotInBuildPlan
                            _ -> Couldn'tResolveItsDependencies
                 in return $ Left (depname, (range, mlatest, bd))
            Right adr | not $ adrVersion adr `withinRange` range ->
                return $ Left (depname, (range, mlatest, DependencyMismatch $ adrVersion adr))
            Right (ADRToInstall task) -> return $ Right
                (Set.singleton $ taskProvides task, Set.empty, taskLocation task)
            Right (ADRFound loc _ (Executable _)) -> return $ Right
                (Set.empty, Set.empty, loc)
            Right (ADRFound loc _ (Library gid)) -> return $ Right
                (Set.empty, Set.singleton gid, loc)
    case partitionEithers deps of
        ([], pairs) -> return $ Right $ mconcat pairs
        (errs, _) -> return $ Left $ DependencyPlanFailures
            (PackageIdentifier
                (packageName package)
                (packageVersion package))
            (Map.fromList errs)
  where
    adrVersion (ADRToInstall task) = packageIdentifierVersion $ taskProvides task
    adrVersion (ADRFound _ v _) = v

checkDirtiness :: PackageSource
               -> Installed
               -> Package
               -> Set GhcPkgId
               -> Set PackageName
               -> M Bool
checkDirtiness ps installed package present wanted = do
    ctx <- ask
    moldOpts <- tryGetFlagCache installed
    let configOpts = configureOpts
            (getEnvConfig ctx)
            (baseConfigOpts ctx)
            present
            (psWanted ps)
            (piiLocation ps) -- should be Local always
            package
        buildOpts = bcoBuildOpts (baseConfigOpts ctx)
        wantConfigCache = ConfigCache
            { configCacheOpts = map encodeUtf8 configOpts
            , configCacheDeps = present
            , configCacheComponents =
                case ps of
                    PSLocal lp -> Set.map encodeUtf8 $ lpComponents lp
                    PSUpstream _ _ _ -> Set.empty
            , configCacheHaddock =
                shouldHaddockPackage buildOpts wanted (packageName package) ||
                -- Disabling haddocks when old config had haddocks doesn't make dirty.
                maybe False configCacheHaddock moldOpts
            }
    let mreason =
            case moldOpts of
                Nothing -> Just "old configure information not found"
                Just oldOpts
                    | oldOpts /= wantConfigCache -> Just $ describeConfigDiff oldOpts wantConfigCache
                    | psDirty ps -> Just "local file changes"
                    | otherwise -> Nothing
    case mreason of
        Nothing -> return False
        Just reason -> do
            tell (Map.empty, Map.empty, Map.singleton (packageName package) reason)
            return True

describeConfigDiff :: ConfigCache -> ConfigCache -> Text
describeConfigDiff old new
    | configCacheDeps old /= configCacheDeps new = "dependencies changed"
    | configCacheComponents old /= configCacheComponents new = "components changed"
    | configCacheHaddock old && not (configCacheHaddock new) = "no longer building haddocks"
    | not (configCacheHaddock old) && configCacheHaddock new = "building haddocks"
    | oldOpts /= newOpts = T.pack $ concat
        [ "flags changed from "
        , show oldOpts
        , " to "
        , show newOpts
        ]
    | otherwise = "unknown config cache difference"
  where
    -- options set by stack
    isStackOpt t = any (`T.isPrefixOf` t)
        [ "--dependency="
        , "--constraint="
        , "--package-db="
        , "--libdir="
        , "--bindir="
        ]

    userOpts = filter (not . isStackOpt)
             . map (decodeUtf8With lenientDecode)
             . configCacheOpts

    (oldOpts, newOpts) = removeMatching (userOpts old) (userOpts new)

    removeMatching (x:xs) (y:ys)
        | x == y = removeMatching xs ys
    removeMatching xs ys = (xs, ys)

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
    , planUnregisterLocal = Map.empty
    , planInstallExes = Map.filter (/= Local) $ planInstallExes plan
    }
  where
    checkTask task =
        case taskType task of
            TTLocal _ -> False
            TTUpstream _ Local -> False
            TTUpstream _ Snap -> True

taskLocation :: Task -> InstallLocation
taskLocation =
    go . taskType
  where
    go (TTLocal _) = Local
    go (TTUpstream _ loc) = loc
