{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
-- | Construct a @Plan@ for how to build
module Stack.Build.ConstructPlan
    ( constructPlan
    ) where

import           Control.Arrow ((&&&))
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Resource
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Typeable
import qualified Distribution.Package as Cabal
import qualified Distribution.Text as Cabal
import qualified Distribution.Version as Cabal
import           GHC.Generics (Generic)
import           Generics.Deriving.Monoid (memptydefault, mappenddefault)
import           Lens.Micro (lens)
import           Path
import           Prelude hiding (pi, writeFile)
import           Stack.Build.Cache
import           Stack.Build.Haddock
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.BuildPlan
import           Stack.Constants
import           Stack.Package
import           Stack.PackageDump
import           Stack.PackageIndex
import           Stack.PrettyPrint
import           Stack.Types.Build
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.FlagName
import           Stack.Types.GhcPkgId
import           Stack.Types.Package
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.StackT (StackM)
import           Stack.Types.Version
import           System.Process.Read (findExecutable)

data PackageInfo
    = PIOnlyInstalled InstallLocation Installed
    | PIOnlySource PackageSource
    | PIBoth PackageSource Installed
    deriving (Show)

combineSourceInstalled :: PackageSource
                       -> (InstallLocation, Installed)
                       -> PackageInfo
combineSourceInstalled ps (location, installed) =
    assert (piiVersion ps == installedVersion installed) $
    assert (piiLocation ps == location) $
    case location of
        -- Always trust something in the snapshot
        Snap -> PIOnlyInstalled location installed
        Local -> PIBoth ps installed

type CombinedMap = Map PackageName PackageInfo

combineMap :: SourceMap -> InstalledMap -> CombinedMap
combineMap = Map.mergeWithKey
    (\_ s i -> Just $ combineSourceInstalled s i)
    (fmap PIOnlySource)
    (fmap (uncurry PIOnlyInstalled))

data AddDepRes
    = ADRToInstall Task
    | ADRFound InstallLocation Installed
    deriving Show

type ParentMap = MonoidMap PackageName (First Version, [(PackageIdentifier, VersionRange)])

data W = W
    { wFinals :: !(Map PackageName (Either ConstructPlanException Task))
    , wInstall :: !(Map Text InstallLocation)
    -- ^ executable to be installed, and location where the binary is placed
    , wDirty :: !(Map PackageName Text)
    -- ^ why a local package is considered dirty
    , wDeps :: !(Set PackageName)
    -- ^ Packages which count as dependencies
    , wWarnings :: !([Text] -> [Text])
    -- ^ Warnings
    , wParents :: !ParentMap
    -- ^ Which packages a given package depends on, along with the package's version
    } deriving Generic
instance Monoid W where
    mempty = memptydefault
    mappend = mappenddefault

type M = RWST
    Ctx
    W
    (Map PackageName (Either ConstructPlanException AddDepRes))
    IO

data Ctx = Ctx
    { mbp            :: !MiniBuildPlan
    , baseConfigOpts :: !BaseConfigOpts
    , loadPackage    :: !(PackageName -> Version -> Map FlagName Bool -> [Text] -> IO Package)
    , combinedMap    :: !CombinedMap
    , toolToPackages :: !(Cabal.Dependency -> Map PackageName VersionRange)
    , ctxEnvConfig   :: !EnvConfig
    , callStack      :: ![PackageName]
    , extraToBuild   :: !(Set PackageName)
    , getVersions    :: !(PackageName -> IO (Set Version))
    , wanted         :: !(Set PackageName)
    , localNames     :: !(Set PackageName)
    , logFunc        :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    }

instance HasPlatform Ctx
instance HasGHCVariant Ctx
instance HasConfig Ctx
instance HasBuildConfigNoLocal Ctx
instance HasBuildConfig Ctx
instance HasEnvConfigNoLocal Ctx where
    envConfigNoLocalL = envConfigL.envConfigNoLocalL
instance HasEnvConfig Ctx where
    envConfigL = lens ctxEnvConfig (\x y -> x { ctxEnvConfig = y })

constructPlan :: forall env m. (StackM env m, HasEnvConfig env)
              => MiniBuildPlan
              -> BaseConfigOpts
              -> [LocalPackage]
              -> Set PackageName -- ^ additional packages that must be built
              -> [DumpPackage () () ()] -- ^ locally registered
              -> (PackageName -> Version -> Map FlagName Bool -> [Text] -> IO Package) -- ^ load upstream package
              -> SourceMap
              -> InstalledMap
              -> m Plan
constructPlan mbp0 baseConfigOpts0 locals extraToBuild0 localDumpPkgs loadPackage0 sourceMap installedMap = do
    $logDebug "Constructing the build plan"
    let locallyRegistered = Map.fromList $ map (dpGhcPkgId &&& dpPackageIdent) localDumpPkgs
    getVersions0 <- getPackageVersionsIO

    econfig <- view envConfigL
    let onWanted = void . addDep False . packageName . lpPackage
    let inner = do
            mapM_ onWanted $ filter lpWanted locals
            mapM_ (addDep False) $ Set.toList extraToBuild0
    lf <- askLoggerIO
    ((), m, W efinals installExes dirtyReason deps warnings parents) <-
        liftIO $ runRWST inner (ctx econfig getVersions0 lf) M.empty
    mapM_ $logWarn (warnings [])
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
                takeSubset =
                    case boptsCLIBuildSubset $ bcoBuildOptsCLI baseConfigOpts0 of
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
        else do
            planDebug $ show errs
            stackYaml <- view stackYamlL
            $prettyError $ pprintExceptions errs stackYaml parents (wantedLocalPackages locals)
            throwM $ ConstructPlanFailed "Plan construction failed."
  where
    ctx econfig getVersions0 lf = Ctx
        { mbp = mbp0
        , baseConfigOpts = baseConfigOpts0
        , loadPackage = loadPackage0
        , combinedMap = combineMap sourceMap installedMap
        , toolToPackages = \(Cabal.Dependency name _) ->
          maybe Map.empty (Map.fromSet (const Cabal.anyVersion)) $
          Map.lookup (T.pack . packageNameString . fromCabalPackageName $ name) toolMap
        , ctxEnvConfig = econfig
        , callStack = []
        , extraToBuild = extraToBuild0
        , getVersions = getVersions0
        , wanted = wantedLocalPackages locals
        , localNames = Set.fromList $ map (packageName . lpPackage) locals
        , logFunc = lf
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
                    Just (PSUpstream _ Snap _ _ _) -> Map.singleton gid
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

addFinal :: LocalPackage -> Package -> Bool -> M ()
addFinal lp package isAllInOne = do
    depsRes <- addPackageDeps False package
    res <- case depsRes of
        Left e -> return $ Left e
        Right (missing, present, _minLoc) -> do
            ctx <- ask
            return $ Right Task
                { taskProvides = PackageIdentifier
                    (packageName package)
                    (packageVersion package)
                , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                    let allDeps = Map.union present missing'
                     in configureOpts
                            (view envConfigL ctx)
                            (baseConfigOpts ctx)
                            allDeps
                            True -- local
                            Local
                            package
                , taskPresent = present
                , taskType = TTLocal lp
                , taskAllInOne = isAllInOne
                }
    tell mempty { wFinals = Map.singleton (packageName package) res }

addDep :: Bool -- ^ is this being used by a dependency?
       -> PackageName
       -> M (Either ConstructPlanException AddDepRes)
addDep treatAsDep' name = do
    ctx <- ask
    let treatAsDep = treatAsDep' || name `Set.notMember` wanted ctx
    when treatAsDep $ markAsDep name
    m <- get
    case Map.lookup name m of
        Just res -> do
            planDebug $ "addDep: Using cached result for " ++ show name ++ ": " ++ show res
            return res
        Nothing -> do
            res <- if name `elem` callStack ctx
                then do
                    planDebug $ "addDep: Detected cycle " ++ show name ++ ": " ++ show (callStack ctx)
                    return $ Left $ DependencyCycleDetected $ name : callStack ctx
                else local (\ctx' -> ctx' { callStack = name : callStack ctx' }) $ do
                    let mpackageInfo = Map.lookup name $ combinedMap ctx
                    planDebug $ "addDep: Package info for " ++ show name ++ ": " ++ show mpackageInfo
                    case mpackageInfo of
                        -- TODO look up in the package index and see if there's a
                        -- recommendation available
                        Nothing -> return $ Left $ UnknownPackage name
                        Just (PIOnlyInstalled loc installed) -> do
                            -- slightly hacky, no flags since they likely won't affect executable names
                            tellExecutablesUpstream name (installedVersion installed) loc Map.empty
                            return $ Right $ ADRFound loc installed
                        Just (PIOnlySource ps) -> do
                            tellExecutables name ps
                            installPackage treatAsDep name ps Nothing
                        Just (PIBoth ps installed) -> do
                            tellExecutables name ps
                            installPackage treatAsDep name ps (Just installed)
            updateLibMap name res
            return res

tellExecutables :: PackageName -> PackageSource -> M ()
tellExecutables _ (PSLocal lp)
    | lpWanted lp = tellExecutablesPackage Local $ lpPackage lp
    | otherwise = return ()
-- Ignores ghcOptions because they don't matter for enumerating
-- executables.
tellExecutables name (PSUpstream version loc flags _ghcOptions _gitSha) =
    tellExecutablesUpstream name version loc flags

tellExecutablesUpstream :: PackageName -> Version -> InstallLocation -> Map FlagName Bool -> M ()
tellExecutablesUpstream name version loc flags = do
    ctx <- ask
    when (name `Set.member` extraToBuild ctx) $ do
        p <- liftIO $ loadPackage ctx name version flags []
        tellExecutablesPackage loc p

tellExecutablesPackage :: InstallLocation -> Package -> M ()
tellExecutablesPackage loc p = do
    cm <- asks combinedMap
    -- Determine which components are enabled so we know which ones to copy
    let myComps =
            case Map.lookup (packageName p) cm of
                Nothing -> assert False Set.empty
                Just (PIOnlyInstalled _ _) -> Set.empty
                Just (PIOnlySource ps) -> goSource ps
                Just (PIBoth ps _) -> goSource ps

        goSource (PSLocal lp)
            | lpWanted lp = exeComponents (lpComponents lp)
            | otherwise = Set.empty
        goSource PSUpstream{} = Set.empty

    tell mempty { wInstall = Map.fromList $ map (, loc) $ Set.toList $ filterComps myComps $ packageExes p }
  where
    filterComps myComps x
        | Set.null myComps = x
        | otherwise = Set.intersection x myComps

installPackage :: Bool -- ^ is this being used by a dependency?
               -> PackageName
               -> PackageSource
               -> Maybe Installed
               -> M (Either ConstructPlanException AddDepRes)
installPackage treatAsDep name ps minstalled = do
    ctx <- ask
    case ps of
        PSUpstream version _ flags ghcOptions _ -> do
            planDebug $ "installPackage: Doing all-in-one build for upstream package " ++ show name
            package <- liftIO $ loadPackage ctx name version flags ghcOptions
            resolveDepsAndInstall True treatAsDep ps package minstalled
        PSLocal lp ->
            case lpTestBench lp of
                Nothing -> do
                    planDebug $ "installPackage: No test / bench component for " ++ show name ++ " so doing an all-in-one build."
                    resolveDepsAndInstall True treatAsDep ps (lpPackage lp) minstalled
                Just tb -> do
                    -- Attempt to find a plan which performs an all-in-one
                    -- build.  Ignore the writer action + reset the state if
                    -- it fails.
                    s <- get
                    res <- pass $ do
                        res <- addPackageDeps treatAsDep tb
                        let writerFunc w = case res of
                                Left _ -> mempty
                                _ -> w
                        return (res, writerFunc)
                    case res of
                        Right deps -> do
                          planDebug $ "installPackage: For " ++ show name ++ ", successfully added package deps"
                          adr <- installPackageGivenDeps True ps tb minstalled deps
                          -- FIXME: this redundantly adds the deps (but
                          -- they'll all just get looked up in the map)
                          addFinal lp tb True
                          return $ Right adr
                        Left _ -> do
                            -- Reset the state to how it was before
                            -- attempting to find an all-in-one build
                            -- plan.
                            planDebug $ "installPackage: Before trying cyclic plan, resetting lib result map to " ++ show s
                            put s
                            -- Otherwise, fall back on building the
                            -- tests / benchmarks in a separate step.
                            res' <- resolveDepsAndInstall False treatAsDep ps (lpPackage lp) minstalled
                            when (isRight res') $ do
                                -- Insert it into the map so that it's
                                -- available for addFinal.
                                updateLibMap name res'
                                addFinal lp tb False
                            return res'

resolveDepsAndInstall :: Bool
                      -> Bool
                      -> PackageSource
                      -> Package
                      -> Maybe Installed
                      -> M (Either ConstructPlanException AddDepRes)
resolveDepsAndInstall isAllInOne treatAsDep ps package minstalled = do
    res <- addPackageDeps treatAsDep package
    case res of
        Left err -> return $ Left err
        Right deps -> liftM Right $ installPackageGivenDeps isAllInOne ps package minstalled deps

installPackageGivenDeps :: Bool
                        -> PackageSource
                        -> Package
                        -> Maybe Installed
                        -> ( Set PackageIdentifier
                           , Map PackageIdentifier GhcPkgId
                           , InstallLocation )
                        -> M AddDepRes
installPackageGivenDeps isAllInOne ps package minstalled (missing, present, minLoc) = do
    let name = packageName package
    ctx <- ask
    mRightVersionInstalled <- case (minstalled, Set.null missing) of
        (Just installed, True) -> do
            shouldInstall <- checkDirtiness ps installed package present (wanted ctx)
            return $ if shouldInstall then Nothing else Just installed
        (Just _, False) -> do
            let t = T.intercalate ", " $ map (T.pack . packageNameString . packageIdentifierName) (Set.toList missing)
            tell mempty { wDirty = Map.singleton name $ "missing dependencies: " <> addEllipsis t }
            return Nothing
        (Nothing, _) -> return Nothing
    return $ case mRightVersionInstalled of
        Just installed -> ADRFound (piiLocation ps) installed
        Nothing -> ADRToInstall Task
            { taskProvides = PackageIdentifier
                (packageName package)
                (packageVersion package)
            , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                let allDeps = Map.union present missing'
                    destLoc = piiLocation ps <> minLoc
                 in configureOpts
                        (view envConfigL ctx)
                        (baseConfigOpts ctx)
                        allDeps
                        (psLocal ps)
                        -- An assertion to check for a recurrence of
                        -- https://github.com/commercialhaskell/stack/issues/345
                        (assert (destLoc == piiLocation ps) destLoc)
                        package
            , taskPresent = present
            , taskType =
                case ps of
                    PSLocal lp -> TTLocal lp
                    PSUpstream _ loc _ _ sha -> TTUpstream package (loc <> minLoc) sha
            , taskAllInOne = isAllInOne
            }

-- Update response in the lib map. If it is an error, and there's
-- already an error about cyclic dependencies, prefer the cyclic error.
updateLibMap :: PackageName -> Either ConstructPlanException AddDepRes -> M ()
updateLibMap name val = modify $ \mp ->
    case (M.lookup name mp, val) of
        (Just (Left DependencyCycleDetected{}), Left _) -> mp
        _ -> M.insert name val mp

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
        let getLatestApplicable = do
                vs <- liftIO $ getVersions ctx depname
                return (latestApplicableVersion range vs)
        case eres of
            Left e -> do
                addParent depname range Nothing
                let bd =
                        case e of
                            UnknownPackage name -> assert (name == depname) NotInBuildPlan
                            _ -> Couldn'tResolveItsDependencies (packageVersion package)
                mlatestApplicable <- getLatestApplicable
                return $ Left (depname, (range, mlatestApplicable, bd))
            Right adr -> do
                addParent depname range Nothing
                inRange <- if adrVersion adr `withinRange` range
                    then return True
                    else do
                        let warn_ reason =
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
                        allowNewer <- view $ configL.to configAllowNewer
                        if allowNewer
                            then do
                                warn_ " (allow-newer enabled)"
                                return True
                            else do
                                x <- inSnapshot (packageName package) (packageVersion package)
                                y <- inSnapshot depname (adrVersion adr)
                                if x && y
                                    then do
                                        warn_ " (trusting snapshot over Hackage revisions)"
                                        return True
                                    else return False
                if inRange
                    then case adr of
                        ADRToInstall task -> return $ Right
                            (Set.singleton $ taskProvides task, Map.empty, taskLocation task)
                        ADRFound loc (Executable _) -> return $ Right
                            (Set.empty, Map.empty, loc)
                        ADRFound loc (Library ident gid) -> return $ Right
                            (Set.empty, Map.singleton ident gid, loc)
                    else do
                        mlatestApplicable <- getLatestApplicable
                        return $ Left (depname, (range, mlatestApplicable, DependencyMismatch $ adrVersion adr))
    case partitionEithers deps of
        ([], pairs) -> return $ Right $ mconcat pairs
        (errs, _) -> return $ Left $ DependencyPlanFailures
            package
            (Map.fromList errs)
  where
    adrVersion (ADRToInstall task) = packageIdentifierVersion $ taskProvides task
    adrVersion (ADRFound _ installed) = installedVersion installed
    addParent depname range mversion = tell mempty { wParents = MonoidMap $ M.singleton depname val }
      where
        val = (First mversion, [(packageIdentifier package, range)])

checkDirtiness :: PackageSource
               -> Installed
               -> Package
               -> Map PackageIdentifier GhcPkgId
               -> Set PackageName
               -> M Bool
checkDirtiness ps installed package present wanted = do
    ctx <- ask
    moldOpts <- flip runLoggingT (logFunc ctx) $ tryGetFlagCache installed
    let configOpts = configureOpts
            (view envConfigL ctx)
            (baseConfigOpts ctx)
            present
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
                    PSUpstream{} -> Set.empty
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
                    | True <- psForceDirty ps -> Just "--force-dirty specified"
                    | Just files <- psDirty ps -> Just $ "local file changes: " <>
                                                         addEllipsis (T.pack $ unwords $ Set.toList files)
                    | otherwise -> Nothing
        config = view configL ctx
    case mreason of
        Nothing -> return False
        Just reason -> do
            tell mempty { wDirty = Map.singleton (packageName package) reason }
            return True

describeConfigDiff :: Config -> ConfigCache -> ConfigCache -> Maybe Text
describeConfigDiff config old new
    | not (configCacheDeps new `Set.isSubsetOf` configCacheDeps old) = Just "dependencies changed"
    | not $ Set.null newComponents =
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
    stripGhcOptions =
        go
      where
        go [] = []
        go ("--ghc-option":x:xs) = go' Ghc x xs
        go ("--ghc-options":x:xs) = go' Ghc x xs
        go ((T.stripPrefix "--ghc-option=" -> Just x):xs) = go' Ghc x xs
        go ((T.stripPrefix "--ghc-options=" -> Just x):xs) = go' Ghc x xs
        go ("--ghcjs-option":x:xs) = go' Ghcjs x xs
        go ("--ghcjs-options":x:xs) = go' Ghcjs x xs
        go ((T.stripPrefix "--ghcjs-option=" -> Just x):xs) = go' Ghcjs x xs
        go ((T.stripPrefix "--ghcjs-options=" -> Just x):xs) = go' Ghcjs x xs
        go (x:xs) = x : go xs

        go' wc x xs = checkKeepers wc x $ go xs

        checkKeepers wc x xs =
            case filter isKeeper $ T.words x of
                [] -> xs
                keepers -> T.pack (compilerOptionsCabalFlag wc) : T.unwords keepers : xs

        -- GHC options which affect build results and therefore should always
        -- force a rebuild
        --
        -- For the most part, we only care about options generated by Stack
        -- itself
        isKeeper = (== "-fhpc") -- more to be added later

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

psForceDirty :: PackageSource -> Bool
psForceDirty (PSLocal lp) = lpForceDirty lp
psForceDirty PSUpstream{} = False

psDirty :: PackageSource -> Maybe (Set FilePath)
psDirty (PSLocal lp) = lpDirtyFiles lp
psDirty PSUpstream{} = Nothing -- files never change in an upstream package

psLocal :: PackageSource -> Bool
psLocal (PSLocal _) = True
psLocal PSUpstream{} = False

-- | Get all of the dependencies for a given package, including guessed build
-- tool dependencies.
packageDepsWithTools :: Package -> M (Map PackageName VersionRange)
packageDepsWithTools p = do
    ctx <- ask
    -- TODO: it would be cool to defer these warnings until there's an
    -- actual issue building the package.
    let toEither (Cabal.Dependency (Cabal.PackageName name) _) mp =
            case Map.toList mp of
                [] -> Left (NoToolFound name (packageName p))
                [_] -> Right mp
                xs -> Left (AmbiguousToolsFound name (packageName p) (map fst xs))
        (warnings0, toolDeps) =
             partitionEithers $
             map (\dep -> toEither dep (toolToPackages ctx dep)) (packageTools p)
    -- Check whether the tool is on the PATH before warning about it.
    warnings <- fmap catMaybes $ forM warnings0 $ \warning -> do
        let toolName = case warning of
                NoToolFound tool _ -> tool
                AmbiguousToolsFound tool _ _ -> tool
        config <- view configL
        menv <- liftIO $ configEnvOverride config minimalEnvSettings { esIncludeLocals = True }
        mfound <- findExecutable menv toolName
        case mfound of
            Nothing -> return (Just warning)
            Just _ -> return Nothing
    tell mempty { wWarnings = (map toolWarningText warnings ++) }
    when (any isNoToolFound warnings) $ do
        let msg = T.unlines
                [ "Missing build-tools may be caused by dependencies of the build-tool being overridden by extra-deps."
                , "This should be fixed soon - see this issue https://github.com/commercialhaskell/stack/issues/595"
                ]
        tell mempty { wWarnings = (msg:) }
    return $ Map.unionsWith intersectVersionRanges
           $ packageDeps p
           : toolDeps

data ToolWarning
    = NoToolFound String PackageName
    | AmbiguousToolsFound String PackageName [PackageName]

isNoToolFound :: ToolWarning -> Bool
isNoToolFound NoToolFound{} = True
isNoToolFound _ = False

toolWarningText :: ToolWarning -> Text
toolWarningText (NoToolFound toolName pkgName) =
    "No packages found in snapshot which provide a " <>
    T.pack (show toolName) <>
    " executable, which is a build-tool dependency of " <>
    T.pack (show (packageNameString pkgName))
toolWarningText (AmbiguousToolsFound toolName pkgName options) =
    "Multiple packages found in snapshot which provide a " <>
    T.pack (show toolName) <>
    " exeuctable, which is a build-tool dependency of " <>
    T.pack (show (packageNameString pkgName)) <>
    ", so none will be installed.\n" <>
    "Here's the list of packages which provide it: " <>
    T.intercalate ", " (map packageNameText options) <>
    "\nSince there's no good way to choose, you may need to install it manually."

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
            TTUpstream _ Local _ -> False
            TTUpstream _ Snap _ -> True

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
    ls <- asks localNames
    return $ fromMaybe False $ do
        guard $ not $ name `Set.member` ls
        mpi <- Map.lookup name (mbpPackages p)
        return $ mpiVersion mpi == version

data ConstructPlanException
    = DependencyCycleDetected [PackageName]
    | DependencyPlanFailures Package (Map PackageName (VersionRange, LatestApplicableVersion, BadDependency))
    | UnknownPackage PackageName -- TODO perhaps this constructor will be removed, and BadDependency will handle it all
    -- ^ Recommend adding to extra-deps, give a helpful version number?
    deriving (Typeable, Eq, Show)

-- | For display purposes only, Nothing if package not found
type LatestApplicableVersion = Maybe Version

-- | Reason why a dependency was not used
data BadDependency
    = NotInBuildPlan
    | Couldn'tResolveItsDependencies Version
    | DependencyMismatch Version
    deriving (Typeable, Eq, Show)

-- TODO: Consider intersecting version ranges for multiple deps on a
-- package.  This is why VersionRange is in the parent map.

pprintExceptions
    :: [ConstructPlanException]
    -> Path Abs File
    -> ParentMap
    -> Set PackageName
    -> AnsiDoc
pprintExceptions exceptions stackYaml parentMap wanted =
    "While constructing the build plan, the following exceptions were encountered:" <> line <> line <>
    mconcat (intersperse (line <> line) (mapMaybe pprintException exceptions')) <> line <>
    if Map.null extras then "" else
        line <>
        "Recommended action: try adding the following to your extra-deps in" <+>
        toAnsiDoc (display stackYaml) <> ":" <>
        line <>
        vsep (map pprintExtra (Map.toList extras)) <>
        line <>
        line <>
        "You may also want to try the 'stack solver' command"
  where
    exceptions' = nub exceptions

    extras = Map.unions $ map getExtras exceptions'
    getExtras (DependencyCycleDetected _) = Map.empty
    getExtras (UnknownPackage _) = Map.empty
    getExtras (DependencyPlanFailures _ m) =
       Map.unions $ map go $ Map.toList m
     where
       go (name, (_range, Just version, NotInBuildPlan)) =
           Map.singleton name version
       go _ = Map.empty
    pprintExtra (name, version) =
      fromString (concat ["- ", packageNameString name, "-", versionString version])

    pprintException (DependencyCycleDetected pNames) = Just $
        "Dependency cycle detected in packages:" <> line <>
        indent 4 (encloseSep "[" "]" "," (map (errorRed . fromString . packageNameString) pNames))
    pprintException (DependencyPlanFailures pkg (Map.toList -> pDeps)) =
        case mapMaybe pprintDep pDeps of
            [] -> Nothing
            depErrors -> Just $
                "In the dependencies for" <+> pkgIdent <>
                pprintFlags (packageFlags pkg) <> ":" <> line <>
                indent 4 (vsep depErrors) <>
                case getShortestDepsPath parentMap wanted (packageName pkg) of
                    [] -> mempty
                    [target,_] -> line <> "needed since" <+> displayTargetPkgId target <+> "is a build target."
                    (target:path) -> line <> "needed due to " <> encloseSep "" "" " -> " pathElems
                      where
                        pathElems =
                            [displayTargetPkgId target] ++
                            map display path ++
                            [pkgIdent]
              where
                pkgIdent = displayCurrentPkgId (packageIdentifier pkg)
        -- TODO: optionally show these?
    -- Skip these because they are redundant with 'NotInBuildPlan' info.
    pprintException (UnknownPackage _) = Nothing

    pprintFlags flags
        | Map.null flags = ""
        | otherwise = parens $ sep $ map pprintFlag $ Map.toList flags
    pprintFlag (name, True) = "+" <> fromString (show name)
    pprintFlag (name, False) = "-" <> fromString (show name)

    pprintDep (name, (range, mlatestApplicable, badDep)) = case badDep of
        NotInBuildPlan -> Just $
            errorRed (display name) <+>
            align ("must match" <+> goodRange <> "," <> softline <>
                   "but the stack configuration has no specified version" <>
                   latestApplicable Nothing)
        -- TODO: For local packages, suggest editing constraints
        DependencyMismatch version -> Just $
            displayErrorPkgId (PackageIdentifier name version) <+>
            align ("must match" <+> goodRange <>
                   latestApplicable (Just version))
        -- I think the main useful info is these explain why missing
        -- packages are needed. Instead lets give the user the shortest
        -- path from a target to the package.
        Couldn'tResolveItsDependencies _version -> Nothing
      where
        goodRange = goodGreen (fromString (Cabal.display range))
        latestApplicable mversion =
            case mlatestApplicable of
                Nothing -> ""
                Just la
                    | mlatestApplicable == mversion -> softline <>
                        "(latest applicable is specified)"
                    | otherwise -> softline <>
                        "(latest applicable is " <> goodGreen (display la) <> ")"

-- | Get the shortest reason for the package to be in the build plan. In
-- other words, trace the parent dependencies back to a 'wanted'
-- package.
getShortestDepsPath
    :: ParentMap
    -> Set PackageName
    -> PackageName
    -> [PackageIdentifier]
getShortestDepsPath (MonoidMap parentsMap) wanted name =
    case M.lookup name parentsMap of
        Nothing -> []
        Just (_, parents) -> findShortest 256 paths0
          where
            paths0 = M.fromList $ map (\(ident, _) -> (packageIdentifierName ident, startDepsPath ident)) parents
  where
    -- The 'paths' map is a map from PackageName to the shortest path
    -- found to get there. It is the frontier of our breadth-first
    -- search of dependencies.
    findShortest :: Int -> Map PackageName DepsPath -> [PackageIdentifier]
    findShortest fuel _ | fuel <= 0 =
        [PackageIdentifier $(mkPackageName "stack-ran-out-of-jet-fuel") $(mkVersion "0")]
    findShortest _ paths | M.null paths = []
    findShortest fuel paths =
        case targets of
            [] -> findShortest (fuel - 1) $ M.fromListWith chooseBest $ concatMap extendPath recurses
            _ -> let (DepsPath _ _ path) = minimum (map snd targets) in path
      where
        (targets, recurses) = partition (\(n, _) -> n `Set.member` wanted) (M.toList paths)
    chooseBest :: DepsPath -> DepsPath -> DepsPath
    chooseBest x y = if x > y then x else y
    -- Extend a path to all its parents.
    extendPath :: (PackageName, DepsPath) -> [(PackageName, DepsPath)]
    extendPath (n, dp) =
        case M.lookup n parentsMap of
            Nothing -> []
            Just (_, parents) -> map (\(pkgId, _) -> (packageIdentifierName pkgId, extendDepsPath pkgId dp)) parents

data DepsPath = DepsPath
    { dpLength :: Int -- ^ Length of dpPath
    , dpNameLength :: Int -- ^ Length of package names combined
    , dpPath :: [PackageIdentifier] -- ^ A path where the packages later
                                    -- in the list depend on those that
                                    -- come earlier
    }
    deriving (Eq, Ord, Show)

startDepsPath :: PackageIdentifier -> DepsPath
startDepsPath ident = DepsPath
    { dpLength = 1
    , dpNameLength = T.length (packageNameText (packageIdentifierName ident))
    , dpPath = [ident]
    }

extendDepsPath :: PackageIdentifier -> DepsPath -> DepsPath
extendDepsPath ident dp = DepsPath
    { dpLength = dpLength dp + 1
    , dpNameLength = dpNameLength dp + T.length (packageNameText (packageIdentifierName ident))
    , dpPath = [ident]
    }

-- Utility newtype wrapper to make make Map's Monoid also use the
-- element's Monoid.

newtype MonoidMap k a = MonoidMap (Map k a)
    deriving (Eq, Ord, Read, Show, Generic, Functor)

instance (Ord k, Monoid a) => Monoid (MonoidMap k a) where
    mappend (MonoidMap mp1) (MonoidMap mp2) = MonoidMap (M.unionWith mappend mp1 mp2)
    mempty = MonoidMap mempty

-- Switch this to 'True' to enable some debugging putStrLn in this module
planDebug :: MonadIO m => String -> m ()
planDebug = if False then liftIO . putStrLn else \_ -> return ()
