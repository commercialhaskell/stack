{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Build the project.

module Stack.Build
  (build
  ,withLoadPackage
  ,mkBaseConfigOpts
  ,queryBuildInfo
  ,splitObjsWarning
  ,CabalVersionException(..))
  where

import           Control.Exception (Exception)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Unlift (MonadBaseUnlift)
import           Data.Aeson (Value (Object, Array), (.=), object)
import           Data.Function
import qualified Data.HashMap.Strict as HM
import           Data.List ((\\))
import           Data.List.Extra (groupSort)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import           Data.Text.Read (decimal)
import           Data.Typeable (Typeable)
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import           Path
import           Prelude hiding (FilePath, writeFile)
import           Stack.Build.ConstructPlan
import           Stack.Build.Execute
import           Stack.Build.Haddock
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Build.Target
import           Stack.Fetch as Fetch
import           Stack.GhcPkg
import           Stack.Package
import           Stack.PackageIndex
import           Stack.PrettyPrint
import           Stack.Types.Build
import           Stack.Types.Config
import           Stack.Types.FlagName
import           Stack.Types.Package
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.StackT
import           Stack.Types.Version

#ifdef WINDOWS
import           Stack.Types.Compiler
#endif
import           System.FileLock (FileLock, unlockFile)

#ifdef WINDOWS
import           System.Win32.Console (setConsoleCP, setConsoleOutputCP, getConsoleCP, getConsoleOutputCP)
import qualified Control.Monad.Catch as Catch
#endif

-- | Build.
--
--   If a buildLock is passed there is an important contract here.  That lock must
--   protect the snapshot, and it must be safe to unlock it if there are no further
--   modifications to the snapshot to be performed by this build.
build :: (StackM env m, HasEnvConfig env, MonadBaseUnlift IO m)
      => (Set (Path Abs File) -> IO ()) -- ^ callback after discovering all local files
      -> Maybe FileLock
      -> BuildOptsCLI
      -> m ()
build setLocalFiles mbuildLk boptsCli = fixCodePage $ do
    bopts <- view buildOptsL
    let profiling = boptsLibProfile bopts || boptsExeProfile bopts
    let symbols = not (boptsLibStrip bopts || boptsExeStrip bopts)
    menv <- getMinimalEnvOverride

    (targets, mbp, locals, extraToBuild, extraDeps, sourceMap) <- loadSourceMapFull NeedTargets boptsCli

    -- Set local files, necessary for file watching
    stackYaml <- view stackYamlL
    liftIO $ setLocalFiles
           $ Set.insert stackYaml
           $ Set.unions
           $ map lpFiles locals

    (installedMap, globalDumpPkgs, snapshotDumpPkgs, localDumpPkgs) <-
        getInstalled menv
                     GetInstalledOpts
                         { getInstalledProfiling = profiling
                         , getInstalledHaddock   = shouldHaddockDeps bopts
                         , getInstalledSymbols   = symbols }
                     sourceMap

    warnMissingExtraDeps installedMap extraDeps

    baseConfigOpts <- mkBaseConfigOpts boptsCli
    plan <- withLoadPackage menv $ \loadPackage ->
        constructPlan mbp baseConfigOpts locals extraToBuild localDumpPkgs loadPackage sourceMap installedMap

    -- If our work to do is all local, let someone else have a turn with the snapshot.
    -- They won't damage what's already in there.
    case (mbuildLk, allLocal plan) of
       -- NOTE: This policy is too conservative.  In the future we should be able to
       -- schedule unlocking as an Action that happens after all non-local actions are
       -- complete.
      (Just lk,True) -> do $logDebug "All installs are local; releasing snapshot lock early."
                           liftIO $ unlockFile lk
      _ -> return ()

    checkCabalVersion
    warnAboutSplitObjs bopts
    warnIfExecutablesWithSameNameCouldBeOverwritten locals plan

    when (boptsPreFetch bopts) $
        preFetch plan

    if boptsCLIDryrun boptsCli
        then printPlan plan
        else executePlan menv boptsCli baseConfigOpts locals
                         globalDumpPkgs
                         snapshotDumpPkgs
                         localDumpPkgs
                         installedMap
                         targets
                         plan

-- | If all the tasks are local, they don't mutate anything outside of our local directory.
allLocal :: Plan -> Bool
allLocal =
    all (== Local) .
    map taskLocation .
    Map.elems .
    planTasks

checkCabalVersion :: (StackM env m, HasEnvConfig env) => m ()
checkCabalVersion = do
    allowNewer <- view $ configL.to configAllowNewer
    cabalVer <- view cabalVersionL
    -- https://github.com/haskell/cabal/issues/2023
    when (allowNewer && cabalVer < $(mkVersion "1.22")) $ throwM $
        CabalVersionException $
            "Error: --allow-newer requires at least Cabal version 1.22, but version " ++
            versionString cabalVer ++
            " was found."

data CabalVersionException = CabalVersionException { unCabalVersionException :: String }
    deriving (Typeable)

instance Show CabalVersionException where show = unCabalVersionException
instance Exception CabalVersionException

warnMissingExtraDeps
    :: (StackM env m, HasConfig env)
    => InstalledMap -> Map PackageName Version -> m ()
warnMissingExtraDeps installed extraDeps = do
    missingExtraDeps <-
        fmap catMaybes $ forM (Map.toList extraDeps) $ \(n, v) ->
            if Map.member n installed
                then return Nothing
                else do
                    vs <- getPackageVersions n
                    if Set.null vs
                        then return $ Just $
                            fromString (packageNameString n ++ "-" ++ versionString v)
                        else return Nothing
    unless (null missingExtraDeps) $
        $prettyWarn $
            "Some extra-deps are neither installed nor in the index:" <> line <>
            indent 4 (bulletedList missingExtraDeps)

-- | See https://github.com/commercialhaskell/stack/issues/1198.
warnIfExecutablesWithSameNameCouldBeOverwritten
    :: MonadLogger m => [LocalPackage] -> Plan -> m ()
warnIfExecutablesWithSameNameCouldBeOverwritten locals plan = do
    $logDebug "Checking if we are going to build multiple executables with the same name"
    forM_ (Map.toList warnings) $ \(exe,(toBuild,otherLocals)) -> do
        let exe_s
                | length toBuild > 1 = "several executables with the same name:"
                | otherwise = "executable"
            exesText pkgs =
                T.intercalate
                    ", "
                    ["'" <> packageNameText p <> ":" <> exe <> "'" | p <- pkgs]
        ($logWarn . T.unlines . concat)
            [ [ "Building " <> exe_s <> " " <> exesText toBuild <> "." ]
            , [ "Only one of them will be available via 'stack exec' or locally installed."
              | length toBuild > 1
              ]
            , [ "Other executables with the same name might be overwritten: " <>
                exesText otherLocals <> "."
              | not (null otherLocals)
              ]
            ]
  where
    -- Cases of several local packages having executables with the same name.
    -- The Map entries have the following form:
    --
    --  executable name: ( package names for executables that are being built
    --                   , package names for other local packages that have an
    --                     executable with the same name
    --                   )
    warnings :: Map Text ([PackageName],[PackageName])
    warnings =
        Map.mapMaybe
            (\(pkgsToBuild,localPkgs) ->
                case (pkgsToBuild,NE.toList localPkgs \\ NE.toList pkgsToBuild) of
                    (_ :| [],[]) ->
                        -- We want to build the executable of single local package
                        -- and there are no other local packages with an executable of
                        -- the same name. Nothing to warn about, ignore.
                        Nothing
                    (_,otherLocals) ->
                        -- We could be here for two reasons (or their combination):
                        -- 1) We are building two or more executables with the same
                        --    name that will end up overwriting each other.
                        -- 2) In addition to the executable(s) that we want to build
                        --    there are other local packages with an executable of the
                        --    same name that might get overwritten.
                        -- Both cases warrant a warning.
                        Just (NE.toList pkgsToBuild,otherLocals))
            (Map.intersectionWith (,) exesToBuild localExes)
    exesToBuild :: Map Text (NonEmpty PackageName)
    exesToBuild =
        collect
            [ (exe,pkgName)
            | (pkgName,task) <- Map.toList (planTasks plan)
            , isLocal task
            , exe <- (Set.toList . exeComponents . lpComponents . taskLP) task
            ]
      where
        isLocal Task{taskType = (TTLocal _)} = True
        isLocal _ = False
        taskLP Task{taskType = (TTLocal lp)} = lp
        taskLP _ = error "warnIfExecutablesWithSameNameCouldBeOverwritten/taskLP: task isn't local"
    localExes :: Map Text (NonEmpty PackageName)
    localExes =
        collect
            [ (exe,packageName pkg)
            | pkg <- map lpPackage locals
            , exe <- Set.toList (packageExes pkg)
            ]
    collect :: Ord k => [(k,v)] -> Map k (NonEmpty v)
    collect = Map.map NE.fromList . Map.fromDistinctAscList . groupSort

warnAboutSplitObjs :: MonadLogger m => BuildOpts -> m ()
warnAboutSplitObjs bopts | boptsSplitObjs bopts = do
    $logWarn $ "Building with --split-objs is enabled. " <> T.pack splitObjsWarning
warnAboutSplitObjs _ = return ()

splitObjsWarning :: String
splitObjsWarning = unwords
     [ "Note that this feature is EXPERIMENTAL, and its behavior may be changed and improved."
     , "You will need to clean your workdirs before use. If you want to compile all dependencies"
     , "with split-objs, you will need to delete the snapshot (and all snapshots that could"
     , "reference that snapshot)."
     ]

-- | Get the @BaseConfigOpts@ necessary for constructing configure options
mkBaseConfigOpts :: (MonadIO m, MonadReader env m, HasEnvConfig env, MonadThrow m)
                 => BuildOptsCLI -> m BaseConfigOpts
mkBaseConfigOpts boptsCli = do
    bopts <- view buildOptsL
    snapDBPath <- packageDatabaseDeps
    localDBPath <- packageDatabaseLocal
    snapInstallRoot <- installationRootDeps
    localInstallRoot <- installationRootLocal
    packageExtraDBs <- packageDatabaseExtra
    return BaseConfigOpts
        { bcoSnapDB = snapDBPath
        , bcoLocalDB = localDBPath
        , bcoSnapInstallRoot = snapInstallRoot
        , bcoLocalInstallRoot = localInstallRoot
        , bcoBuildOpts = bopts
        , bcoBuildOptsCLI = boptsCli
        , bcoExtraDBs = packageExtraDBs
        }

-- | Provide a function for loading package information from the package index
withLoadPackage :: (StackM env m, HasEnvConfig env, MonadBaseUnlift IO m)
                => EnvOverride
                -> ((PackageName -> Version -> Map FlagName Bool -> [Text] -> IO Package) -> m a)
                -> m a
withLoadPackage menv inner = do
    econfig <- view envConfigL
    withCabalLoader menv $ \cabalLoader ->
        inner $ \name version flags ghcOptions -> do
            bs <- cabalLoader $ PackageIdentifier name version

            -- Intentionally ignore warnings, as it's not really
            -- appropriate to print a bunch of warnings out while
            -- resolving the package index.
            (_warnings,pkg) <- readPackageBS (depPackageConfig econfig flags ghcOptions) bs
            return pkg
  where
    -- | Package config to be used for dependencies
    depPackageConfig :: EnvConfig -> Map FlagName Bool -> [Text] -> PackageConfig
    depPackageConfig econfig flags ghcOptions = PackageConfig
        { packageConfigEnableTests = False
        , packageConfigEnableBenchmarks = False
        , packageConfigFlags = flags
        , packageConfigGhcOptions = ghcOptions
        , packageConfigCompilerVersion = view actualCompilerVersionL econfig
        , packageConfigPlatform = view platformL econfig
        }

-- | Set the code page for this process as necessary. Only applies to Windows.
-- See: https://github.com/commercialhaskell/stack/issues/738
#ifdef WINDOWS
fixCodePage :: (StackM env m, HasBuildConfig env, HasEnvConfig env) => m a -> m a
fixCodePage inner = do
    mcp <- view $ configL.to configModifyCodePage
    ghcVersion <- view $ actualCompilerVersionL.to getGhcVersion
    if mcp && ghcVersion < $(mkVersion "7.10.3")
        then fixCodePage'
        -- GHC >=7.10.3 doesn't need this code page hack.
        else inner
  where
    fixCodePage' = do
        origCPI <- liftIO getConsoleCP
        origCPO <- liftIO getConsoleOutputCP

        let setInput = origCPI /= expected
            setOutput = origCPO /= expected
            fixInput
                | setInput = Catch.bracket_
                    (liftIO $ do
                        setConsoleCP expected)
                    (liftIO $ setConsoleCP origCPI)
                | otherwise = id
            fixOutput
                | setInput = Catch.bracket_
                    (liftIO $ do
                        setConsoleOutputCP expected)
                    (liftIO $ setConsoleOutputCP origCPO)
                | otherwise = id

        case (setInput, setOutput) of
            (False, False) -> return ()
            (True, True) -> warn ""
            (True, False) -> warn " input"
            (False, True) -> warn " output"

        fixInput $ fixOutput inner
    expected = 65001 -- UTF-8
    warn typ = $logInfo $ T.concat
        [ "Setting"
        , typ
        , " codepage to UTF-8 (65001) to ensure correct output from GHC"
        ]
#else
fixCodePage :: a -> a
fixCodePage = id
#endif

-- | Query information about the build and print the result to stdout in YAML format.
queryBuildInfo :: (StackM env m, HasEnvConfig env)
               => [Text] -- ^ selectors
               -> m ()
queryBuildInfo selectors0 =
        rawBuildInfo
    >>= select id selectors0
    >>= liftIO . TIO.putStrLn . decodeUtf8 . Yaml.encode
  where
    select _ [] value = return value
    select front (sel:sels) value =
        case value of
            Object o ->
                case HM.lookup sel o of
                    Nothing -> err "Selector not found"
                    Just value' -> cont value'
            Array v ->
                case decimal sel of
                    Right (i, "")
                        | i >= 0 && i < V.length v -> cont $ v V.! i
                        | otherwise -> err "Index out of range"
                    _ -> err "Encountered array and needed numeric selector"
            _ -> err $ "Cannot apply selector to " ++ show value
      where
        cont = select (front . (sel:)) sels
        err msg = error $ msg ++ ": " ++ show (front [sel])

-- | Get the raw build information object
rawBuildInfo :: (StackM env m, HasEnvConfig env) => m Value
rawBuildInfo = do
    (locals, _sourceMap) <- loadSourceMap NeedTargets defaultBuildOptsCLI
    return $ object
        [ "locals" .= Object (HM.fromList $ map localToPair locals)
        ]
  where
    localToPair lp =
        (T.pack $ packageNameString $ packageName p, value)
      where
        p = lpPackage lp
        value = object
            [ "version" .= packageVersion p
            , "path" .= toFilePath (lpDir lp)
            ]
