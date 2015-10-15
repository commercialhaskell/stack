{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
-- | Construct a @Plan@ for how to build
module Stack.Build.ConstructPlan
    ( constructPlan
    ) where

import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger (MonadLogger, logWarn)
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
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Distribution.Package (Dependency (..))
import           Distribution.Version         (anyVersion)
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Prelude hiding (pi, writeFile)
import           Stack.Build.Cache
import           Stack.Build.Haddock
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Types.Build
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

data W = W
    { wFinals :: !(Map PackageName (Either ConstructPlanException (Task, LocalPackageTB)))
    , wInstall :: !(Map Text InstallLocation)
    -- ^ executable to be installed, and location where the binary is placed
    , wDirty :: !(Map PackageName Text)
    -- ^ why a local package is considered dirty
    , wDeps :: !(Set PackageName)
    -- ^ Packages which count as dependencies
    , wWarnings :: !([Text] -> [Text])
    -- ^ Warnings
    }
instance Monoid W where
    mempty = W mempty mempty mempty mempty mempty
    mappend (W a b c d e) (W w x y z z') = W (mappend a w) (mappend b x) (mappend c y) (mappend d z) (mappend e z')

type M = RWST
    Ctx
    W
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
instance HasGHCVariant Ctx
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
              -> Map GhcPkgId PackageIdentifier -- ^ locally registered
              -> (PackageName -> Version -> Map FlagName Bool -> IO Package) -- ^ load upstream package
              -> SourceMap
              -> InstalledMap
              -> m Plan
constructPlan mbp0 baseConfigOpts0 locals extraToBuild0 locallyRegistered loadPackage0 sourceMap installedMap = do
    menv <- getMinimalEnvOverride
    caches <- getPackageCaches menv
    let latest = Map.fromListWith max $ map toTuple $ Map.keys caches

    econfig <- asks getEnvConfig
    let onWanted lp = do
            case lpExeComponents lp of
                Nothing -> return ()
                Just _ -> void $ addDep False $ packageName $ lpPackage lp

            case lpTestBench lp of
                Just tb -> addFinal lp tb
                Nothing -> return ()
    let inner = do
            mapM_ onWanted $ filter lpWanted locals
            mapM_ (addDep False) $ Set.toList extraToBuild0
    ((), m, W efinals installExes dirtyReason deps warnings) <-
        liftIO $ runRWST inner (ctx econfig latest) M.empty
    mapM_ $logWarn (warnings [])
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
                takeSubset =
                    case boptsBuildSubset $ bcoBuildOpts baseConfigOpts0 of
                        BSAll -> id
                        BSOnlySnapshot -> stripLocals
                        BSOnlyDependencies -> stripNonDeps deps
            return $ takeSubset Plan
                { planTasks = tasks
                , planFinals = M.fromList finals
                , planUnregisterLocal = mkUnregisterLocal tasks dirtyReason locallyRegistered sourceMap
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
                  -> Map GhcPkgId PackageIdentifier
                  -> SourceMap
                  -> Map GhcPkgId (PackageIdentifier, Maybe Text)
mkUnregisterLocal tasks dirtyReason locallyRegistered sourceMap =
    Map.unions $ map toUnregisterMap $ Map.toList locallyRegistered
  where
    toUnregisterMap (gid, ident) =
        case M.lookup name tasks of
            Nothing ->
                case M.lookup name sourceMap of
                    Just (PSUpstream _ Snap _) -> Map.singleton gid
                        ( ident
                        , Just "Switching to snapshot installed package"
                        )
                    _ -> Map.empty
            Just _ -> Map.singleton gid
                ( ident
                , Map.lookup name dirtyReason
                )
      where
        name = packageIdentifierName ident

addFinal :: LocalPackage -> LocalPackageTB -> M ()
addFinal lp lptb = do
    depsRes <- addPackageDeps False package
    res <- case depsRes of
        Left e -> return $ Left e
        Right (missing, present, _minLoc) -> do
            ctx <- ask
            return $ Right (Task
                { taskProvides = PackageIdentifier
                    (packageName package)
                    (packageVersion package)
                , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                    let allDeps = Map.union present missing'
                     in configureOpts
                            (getEnvConfig ctx)
                            (baseConfigOpts ctx)
                            allDeps
                            True -- wanted
                            True -- local
                            Local
                            package
                , taskPresent = present
                , taskType = TTLocal lp
                }, lptb)
    tell mempty { wFinals = Map.singleton (packageName package) res }
  where
    package = lptbPackage lptb

addDep :: Bool -- ^ is this being used by a dependency?
       -> PackageName -> M (Either ConstructPlanException AddDepRes)
addDep treatAsDep' name = do
    ctx <- ask
    let treatAsDep = treatAsDep' || name `Set.notMember` wanted ctx
    when treatAsDep $ markAsDep name
    m <- get
    case Map.lookup name m of
        Just res -> return res
        Nothing -> do
            res <- addDep' treatAsDep name
            modify $ Map.insert name res
            return res

addDep' :: Bool -- ^ is this being used by a dependency?
        -> PackageName -> M (Either ConstructPlanException AddDepRes)
addDep' treatAsDep name = do
    ctx <- ask
    if name `elem` callStack ctx
        then return $ Left $ DependencyCycleDetected $ name : callStack ctx
        else local
            (\ctx' -> ctx' { callStack = name : callStack ctx' }) $ do
            (addDep'' treatAsDep name)

addDep'' :: Bool -- ^ is this being used by a dependency?
         -> PackageName -> M (Either ConstructPlanException AddDepRes)
addDep'' treatAsDep name = do
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
            installPackage treatAsDep name ps
        Just (PIBoth ps installed) -> do
            tellExecutables name ps
            needInstall <- checkNeedInstall treatAsDep name ps installed (wanted ctx)
            if needInstall
                then installPackage treatAsDep name ps
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
tellExecutablesPackage loc p = do
    cm <- asks combinedMap
    -- Determine which components are enabled so we know which ones to copy
    let myComps =
            case Map.lookup (packageName p) cm of
                Nothing -> assert False Set.empty
                Just (PIOnlyInstalled _ _ _) -> Set.empty
                Just (PIOnlySource ps) -> goSource ps
                Just (PIBoth ps _) -> goSource ps

        goSource (PSLocal lp) = fromMaybe Set.empty $ lpExeComponents lp
        goSource (PSUpstream _ _ _) = Set.empty

    tell mempty { wInstall = m myComps }
  where
    m myComps = Map.fromList $ map (, loc) $ Set.toList
              $ filterComps myComps $ packageExes p

    filterComps myComps x
        | Set.null myComps = x
        | otherwise = Set.intersection x $ Set.map toExe myComps

    toExe x = fromMaybe x $ T.stripPrefix "exe:" x

-- TODO There are a lot of duplicated computations below. I've kept that for
-- simplicity right now

installPackage :: Bool -- ^ is this being used by a dependency?
               -> PackageName -> PackageSource -> M (Either ConstructPlanException AddDepRes)
installPackage treatAsDep name ps = do
    ctx <- ask
    package <- psPackage name ps
    depsRes <- addPackageDeps treatAsDep package
    case depsRes of
        Left e -> return $ Left e
        Right (missing, present, minLoc) -> do
            return $ Right $ ADRToInstall Task
                { taskProvides = PackageIdentifier
                    (packageName package)
                    (packageVersion package)
                , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                    let allDeps = Map.union present missing'
                        destLoc = piiLocation ps <> minLoc
                     in configureOpts
                            (getEnvConfig ctx)
                            (baseConfigOpts ctx)
                            allDeps
                            (psWanted ps)
                            (psLocal ps)
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

checkNeedInstall :: Bool
                 -> PackageName -> PackageSource -> Installed -> Set PackageName -> M Bool
checkNeedInstall treatAsDep name ps installed wanted = assert (piiLocation ps == Local) $ do
    package <- psPackage name ps
    depsRes <- addPackageDeps treatAsDep package
    case depsRes of
        Left _e -> return True -- installPackage will find the error again
        Right (missing, present, _loc)
            | Set.null missing -> checkDirtiness ps installed package present wanted
            | otherwise -> do
                tell mempty { wDirty = Map.singleton name $
                    let t = T.intercalate ", " $ map (T.pack . packageNameString . packageIdentifierName) (Set.toList missing)
                     in T.append "missing dependencies: " $ addEllipsis t }
                return True

addEllipsis :: Text -> Text
addEllipsis t
    | T.length t < 100 = t
    | otherwise = T.take 97 t <> "..."

addPackageDeps :: Bool -- ^ is this being used by a dependency?
               -> Package -> M (Either ConstructPlanException (Set PackageIdentifier, Map PackageIdentifier GhcPkgId, InstallLocation))
addPackageDeps treatAsDep package = do
    ctx <- ask
    deps' <- packageDepsWithTools package
    deps <- forM (Map.toList deps') $ \(depname, range) -> do
        eres <- addDep treatAsDep depname
        let mlatest = Map.lookup depname $ latestVersions ctx
        case eres of
            Left e ->
                let bd =
                        case e of
                            UnknownPackage name -> assert (name == depname) NotInBuildPlan
                            _ -> Couldn'tResolveItsDependencies
                 in return $ Left (depname, (range, mlatest, bd))
            Right adr -> do
                inRange <- if adrVersion adr `withinRange` range
                    then return True
                    else do
                        let warn reason = do
                                tell mempty { wWarnings = (msg:) }
                              where
                                msg = T.concat
                                    [ "WARNING: Ignoring out of range dependency"
                                    , reason
                                    , ": "
                                    , T.pack $ packageIdentifierString $ PackageIdentifier depname (adrVersion adr)
                                    , ". "
                                    , T.pack $ packageNameString $ packageName package
                                    , " requires: "
                                    , versionRangeText range
                                    ]
                        allowNewer <- asks $ configAllowNewer . getConfig
                        if allowNewer
                            then do
                                warn " (allow-newer enabled)"
                                return True
                            else do
                                x <- inSnapshot (packageName package) (packageVersion package)
                                y <- inSnapshot depname (adrVersion adr)
                                if x && y
                                    then do
                                        warn " (trusting snapshot over Hackage revisions)"
                                        return True
                                    else return False
                if inRange
                    then case adr of
                        ADRToInstall task -> return $ Right
                            (Set.singleton $ taskProvides task, Map.empty, taskLocation task)
                        ADRFound loc _ (Executable _) -> return $ Right
                            (Set.empty, Map.empty, loc)
                        ADRFound loc _ (Library ident gid) -> return $ Right
                            (Set.empty, Map.singleton ident gid, loc)
                    else return $ Left (depname, (range, mlatest, DependencyMismatch $ adrVersion adr))
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
               -> Map PackageIdentifier GhcPkgId
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
            (psLocal ps)
            (piiLocation ps) -- should be Local always
            package
        buildOpts = bcoBuildOpts (baseConfigOpts ctx)
        wantConfigCache = ConfigCache
            { configCacheOpts = configOpts
            , configCacheDeps = Set.fromList $ Map.elems present
            , configCacheComponents =
                case ps of
                    PSLocal lp -> Set.map renderComponent $ lpComponents lp
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
                    | Just reason <- describeConfigDiff config oldOpts wantConfigCache -> Just reason
                    | Just files <- psDirty ps -> Just $ "local file changes: " <>
                                                         addEllipsis (T.pack $ unwords $ Set.toList files)
                    | otherwise -> Nothing
        config = getConfig ctx
    case mreason of
        Nothing -> return False
        Just reason -> do
            tell mempty { wDirty = Map.singleton (packageName package) reason }
            return True

describeConfigDiff :: Config -> ConfigCache -> ConfigCache -> Maybe Text
describeConfigDiff config old new
    | configCacheDeps old /= configCacheDeps new = Just "dependencies changed"
    | not $ Set.null $ Set.filter isLibExe newComponents =
        Just $ "components added: " `T.append` T.intercalate ", "
            (map (decodeUtf8With lenientDecode) (Set.toList newComponents))
    | not (configCacheHaddock old) && configCacheHaddock new = Just "rebuilding with haddocks"
    | oldOpts /= newOpts = Just $ T.pack $ concat
        [ "flags changed from "
        , show oldOpts
        , " to "
        , show newOpts
        ]
    | otherwise = Nothing
  where
    isLibExe t = not $ any (`S8.isPrefixOf` t)
        [ "test:"
        , "bench:"
        ]

    -- options set by stack
    isStackOpt t = any (`T.isPrefixOf` t)
        [ "--dependency="
        , "--constraint="
        , "--package-db="
        , "--libdir="
        , "--bindir="
        , "--enable-tests"
        , "--enable-benchmarks"
        ]

    stripGhcOptions =
        go
      where
        go [] = []
        go ("--ghc-option":_:xs) = go xs
        go ("--ghc-options":_:xs) = go xs
        go (x:xs)
          | isPrefixed x = go xs
          | otherwise = x : go xs

        isPrefixed t = any (`T.isPrefixOf` t)
            [ "--ghc-option="
            , "--ghc-options="
            ]

    userOpts = filter (not . isStackOpt)
             . (if configRebuildGhcOptions config
                   then id
                   else stripGhcOptions)
             . map T.pack
             . (\(ConfigureOpts x y) -> x ++ y)
             . configCacheOpts

    (oldOpts, newOpts) = removeMatching (userOpts old) (userOpts new)

    removeMatching (x:xs) (y:ys)
        | x == y = removeMatching xs ys
    removeMatching xs ys = (xs, ys)

    newComponents = configCacheComponents new `Set.difference` configCacheComponents old

psDirty :: PackageSource -> Maybe (Set FilePath)
psDirty (PSLocal lp) = lpDirtyFiles lp
psDirty (PSUpstream _ _ _) = Nothing -- files never change in an upstream package

psWanted :: PackageSource -> Bool
psWanted (PSLocal lp) = lpWanted lp
psWanted (PSUpstream _ _ _) = False

psLocal :: PackageSource -> Bool
psLocal (PSLocal _) = True
psLocal (PSUpstream _ _ _) = False

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

stripNonDeps :: Set PackageName -> Plan -> Plan
stripNonDeps deps plan = plan
    { planTasks = Map.filter checkTask $ planTasks plan
    , planFinals = Map.empty
    , planInstallExes = Map.empty -- TODO maybe don't disable this?
    }
  where
    checkTask task = packageIdentifierName (taskProvides task) `Set.member` deps

markAsDep :: PackageName -> M ()
markAsDep name = tell mempty { wDeps = Set.singleton name }

-- | Is the given package/version combo defined in the snapshot?
inSnapshot :: PackageName -> Version -> M Bool
inSnapshot name version = do
    p <- asks mbp
    return $ fromMaybe False $ do
        mpi <- Map.lookup name (mbpPackages p)
        return $ mpiVersion mpi == version
