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
import           Control.Monad.Reader         (MonadReader)
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
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Distribution.Package         (Dependency (..))
import           Distribution.Version         (anyVersion,
                                               intersectVersionRanges)
import           Prelude                      hiding (FilePath, writeFile)
import           Stack.Build.Cache
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Build.Types
import           Stack.BuildPlan
import           Stack.Package
import           Stack.Types

data AddDepRes
    = ADRToInstall Task
    | ADRFound Version Installed
    deriving Show

type S = Map PackageName (Either ConstructPlanException AddDepRes)

adrVersion :: AddDepRes -> Version
adrVersion (ADRToInstall task) = packageIdentifierVersion $ taskProvides task
adrVersion (ADRFound v _) = v

data Ctx m = Ctx
    { mbp :: !MiniBuildPlan
    , baseConfigOpts :: !BaseConfigOpts
    , loadPackage :: !(PackageName -> Version -> Map FlagName Bool -> m Package)
    , sourceMap :: !SourceMap
    , installedMap :: !InstalledMap
    }

constructPlan :: forall env m.
                 (MonadThrow m, MonadReader env m, HasBuildConfig env, MonadIO m)
              => MiniBuildPlan
              -> BaseConfigOpts
              -> [LocalPackage]
              -> [PackageName] -- ^ additional packages that must be built
              -> Set GhcPkgId -- ^ locally registered
              -> (PackageName -> Version -> Map FlagName Bool -> m Package) -- ^ load upstream package
              -> SourceMap
              -> InstalledMap
              -> m Plan
constructPlan mbp0 baseConfigOpts0 locals extraToBuild locallyRegistered loadPackage0 sourceMap0 installedMap0 = do
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
    _ctx@Ctx {..} = Ctx
        { mbp = mbp0
        , baseConfigOpts = baseConfigOpts0
        , loadPackage = loadPackage0
        , sourceMap = sourceMap0
        , installedMap = installedMap0
        }
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
                , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                    let allDeps = Set.union present missing'
                     in configureOpts
                            baseConfigOpts
                            allDeps
                            (lpWanted lp)
                            Local
                            (packageFlags $ lpPackage lp)
                , taskType = TTLocal lp AllSteps
                }

    installUpstream callStack name version loc flags = do
        package <- lift $ loadPackage name version flags
        eres <- checkPackage callStack package
        case eres of
            Left e -> return $ Left e
            Right (present, missing) -> return $ Right $ ADRToInstall Task
                { taskProvides = PackageIdentifier name version
                , taskType = TTUpstream package loc
                , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                    let allDeps = Set.union present missing'
                     in configureOpts
                            baseConfigOpts
                            allDeps
                            False
                            loc
                            flags
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
                                    , taskType = TTLocal lp neededSteps
                                    , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                                        let allDeps = Set.union present missing'
                                         in configureOpts
                                                baseConfigOpts
                                                allDeps
                                                (lpWanted lp)
                                                Local
                                                (packageFlags $ lpPackage lp)
                                    }
            Just (PSUpstream version' loc flags) -> assert (version == version') $
                case loc of
                    Snap -> return $ Right $ ADRFound version installed
                    Local -> do
                        package <- lift $ loadPackage name version flags
                        eres <- checkPackage callStack package
                        let toInstall present missing = Right $ ADRToInstall Task
                                    { taskProvides = PackageIdentifier name version
                                    , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                                        let allDeps = Set.union present missing'
                                         in configureOpts
                                                baseConfigOpts
                                                allDeps
                                                False
                                                Local
                                                (packageFlags package)
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
                Left _e -> return $ Left name -- FIXME do something better with this?
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
