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
import           Control.Monad.IO.Class
import           Control.Monad.Reader         (MonadReader, ReaderT, ask, asks,
                                               local, runReaderT)
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8        as S8
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import           Data.Monoid                  (mconcat)
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Text.Encoding           (encodeUtf8)
import           Distribution.Package         (Dependency (..))
import           Distribution.Version         (anyVersion,
                                               intersectVersionRanges)
import           Prelude                      hiding (FilePath, pi, writeFile)
import           Stack.Build.Cache
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Build.Types
import           Stack.BuildPlan
import           Stack.Package
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

type S = Map PackageName (Either ConstructPlanException AddDepRes)
type M = StateT S (ReaderT Ctx IO)

data Ctx = Ctx
    { mbp            :: !MiniBuildPlan
    , baseConfigOpts :: !BaseConfigOpts
    , loadPackage    :: !(PackageName -> Version -> Map FlagName Bool -> IO Package)
    , combinedMap    :: !CombinedMap
    , toolToPackages :: !(Dependency -> Map PackageName VersionRange)
    , ctxBuildConfig :: !BuildConfig
    , callStack      :: ![PackageName]
    }

instance HasStackRoot Ctx
instance HasPlatform Ctx
instance HasConfig Ctx
instance HasBuildConfig Ctx where
    getBuildConfig = ctxBuildConfig

constructPlan :: forall env m.
                 (MonadThrow m, MonadReader env m, HasBuildConfig env, MonadIO m)
              => MiniBuildPlan
              -> BaseConfigOpts
              -> [LocalPackage]
              -> [PackageName] -- ^ additional packages that must be built
              -> Set GhcPkgId -- ^ locally registered
              -> (PackageName -> Version -> Map FlagName Bool -> IO Package) -- ^ load upstream package
              -> SourceMap
              -> InstalledMap
              -> m Plan
constructPlan mbp0 baseConfigOpts0 locals extraToBuild locallyRegistered loadPackage0 sourceMap installedMap = do
    bconfig <- asks getBuildConfig
    m <- liftIO $ flip runReaderT (ctx bconfig) $ flip execStateT M.empty $ do
        let allTargets = Set.fromList
                       $ map (packageName . lpPackage) locals ++ extraToBuild
        mapM_ addDep $ Set.toList allTargets
    let toEither (_, Left e)  = Left e
        toEither (k, Right v) = Right (k, v)
    case partitionEithers $ map toEither $ M.toList m of
        ([], adrs) -> do
            let toTask (_, ADRFound _ _) = Nothing
                toTask (name, ADRToInstall task) = Just (name, task)
                tasks = M.fromList $ mapMaybe toTask adrs
            return Plan
                { planTasks = tasks -- FIXME add in JustFinal for all missing wanteds
                , planUnregisterLocal = mkUnregisterLocal tasks locallyRegistered
                }
        (errs, _) -> throwM $ ConstructPlanExceptions errs
  where
    ctx bconfig = Ctx
        { mbp = mbp0
        , baseConfigOpts = baseConfigOpts0
        , loadPackage = loadPackage0
        , combinedMap = combineMap sourceMap installedMap
        , toolToPackages = \ (Dependency name _) ->
            Map.fromList
          $ map (, anyVersion)
          $ maybe [] Set.toList
          $ Map.lookup (S8.pack . packageNameString . fromCabalPackageName $ name) toolMap
        , ctxBuildConfig = bconfig
        , callStack = []
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
            Just task ->
                case taskType task of
                    TTLocal _ JustFinal -> False
                    _ -> True
      where
        ident = ghcPkgIdPackageIdentifier gid
        name = packageIdentifierName ident

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
        Just (PIOnlyInstalled version _ installed) ->
            return $ Right $ ADRFound version installed
        Just (PIOnlySource ps) -> installPackage AllSteps name ps
        Just (PIBoth ps installed) -> do
            mneededSteps <- checkNeededSteps name ps installed
            case mneededSteps of
                Nothing -> return $ Right $ ADRFound (piiVersion ps) installed
                Just neededSteps -> installPackage neededSteps name ps

-- TODO There are a lot of duplicated computations below. I've kept that for
-- simplicity right now

installPackage :: NeededSteps -> PackageName -> PackageSource -> M (Either ConstructPlanException AddDepRes)
installPackage neededSteps name ps = assert (neededSteps == AllSteps || piiLocation ps == Local) $ do
    ctx <- ask
    package <- psPackage name ps
    depsRes <- addPackageDeps package
    return $ case depsRes of
        Left e -> Left e
        Right (missing, present) -> Right $ ADRToInstall Task
            { taskProvides = PackageIdentifier
                (packageName package)
                (packageVersion package)
            , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                let allDeps = Set.union present missing'
                 in configureOpts
                        (baseConfigOpts ctx)
                        allDeps
                        (psWanted ps)
                        (piiLocation ps)
                        (packageFlags package)
            , taskType =
                case ps of
                    PSLocal lp -> TTLocal lp neededSteps
                    PSUpstream _ loc _ ->
                        assert (neededSteps == AllSteps) $
                        TTUpstream package loc
            }

checkNeededSteps :: PackageName -> PackageSource -> Installed -> M (Maybe NeededSteps)
checkNeededSteps name ps installed = assert (piiLocation ps == Local) $ do
    package <- psPackage name ps
    depsRes <- addPackageDeps package
    case depsRes of
        Left _e -> return $ Just AllSteps -- installPackage will find the error again
        Right (missing, present)
            | Set.null missing -> checkDirtiness ps installed package present
            | otherwise -> return $ Just AllSteps

addPackageDeps :: Package -> M (Either ConstructPlanException (Set PackageIdentifier, Set GhcPkgId))
addPackageDeps package = do
    deps' <- packageDepsWithTools package
    deps <- forM (Map.toList deps') $ \(depname, range) -> do
        -- TODO need to check version ranges
        eres <- addDep depname
        case eres of
            Left _ -> return $ Left depname
            Right (ADRToInstall task) -> return $ Right
                (Set.singleton $ taskProvides task, Set.empty)
            Right (ADRFound _ Executable) -> return $ Right
                (Set.empty, Set.empty)
            Right (ADRFound _ (Library gid)) -> return $ Right
                (Set.empty, Set.singleton gid)
    case partitionEithers deps of
        ([], pairs) -> return $ Right $ mconcat pairs
        (errs, _) -> return $ Left $ DependencyPlanFailures (packageName package) (Set.fromList errs)

checkDirtiness :: PackageSource
               -> Installed
               -> Package
               -> Set GhcPkgId
               -> M (Maybe NeededSteps)
checkDirtiness _ Executable _ _ = return Nothing -- TODO reinstall executables in the future
checkDirtiness ps (Library installed) package present = do
    ctx <- ask
    let configOpts = configureOpts
            (baseConfigOpts ctx)
            present
            (psWanted ps)
            (piiLocation ps) -- should be Local always
            (packageFlags package)
    moldOpts <- psOldOpts ps installed
    return $ case moldOpts of
        Nothing -> Just AllSteps
        Just oldOpts
            | oldOpts /= map encodeUtf8 configOpts -> Just AllSteps
            | psDirty ps -> Just SkipConfig
            | otherwise -> Nothing

psDirty :: PackageSource -> Bool
psDirty (PSLocal lp) = lpDirtyFiles lp
psDirty (PSUpstream _ _ _) = False -- files never change in an upstream package

psOldOpts :: PackageSource -> GhcPkgId -> M (Maybe [S8.ByteString])
psOldOpts (PSLocal lp) _ = return $ lpLastConfigOpts lp
psOldOpts (PSUpstream _ _ _) installed = tryGetFlagCache installed

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
