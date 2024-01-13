{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Perform a build
module Stack.Build.ExecutePackage
  ( singleBuild
  , singleTest
  , singleBench
  ) where

import           Control.Concurrent.Execute
                   ( ActionContext (..), ActionId (..)
                   )
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as BL
import           Conduit ( runConduitRes )
import qualified Data.Conduit.Filesystem as CF
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process.Typed ( createSource )
import qualified Data.Conduit.Text as CT
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import qualified Distribution.PackageDescription as C
import           Distribution.System ( OS (Windows), Platform (Platform) )
import qualified Distribution.Text as C
import           Distribution.Types.MungedPackageName
                   ( encodeCompatPackageName )
import           Distribution.Types.UnqualComponentName
                   ( mkUnqualComponentName )
import           Distribution.Version ( mkVersion )
import           Path
                   ( (</>), addExtension, filename
                   , isProperPrefixOf, parent, parseRelDir, parseRelFile
                   , stripProperPrefix
                   )
import           Path.Extra
                   ( toFilePathNoTrailingSep
                   )
import           Path.IO
                   ( copyFile, doesFileExist, ensureDir
                   , ignoringAbsence, removeDirRecur, removeFile

                   )
import           RIO.NonEmpty ( nonEmpty )
import           RIO.Process
                   ( byteStringInput, findExecutable, getStderr, getStdout, inherit
                   , modifyEnvVars, proc, setStderr, setStdin
                   , setStdout, showProcessArgDebug, useHandleOpen, waitExitCode
                   , withProcessWait, withWorkingDir
                   )
import           Stack.Build.Cache
                   ( TestStatus (..), deleteCaches, getTestStatus
                   , markExeInstalled, markExeNotInstalled, readPrecompiledCache
                   , setTestStatus, tryGetCabalMod, tryGetConfigCache
                   , tryGetPackageProjectRoot, tryGetSetupConfigMod
                   , writeBuildCache, writeCabalMod, writeConfigCache
                   , writeFlagCache, writePrecompiledCache
                   , writePackageProjectRoot, writeSetupConfigMod
                   )
import           Stack.Build.Source ( addUnlistedToBuildCache )
import           Stack.Build.Target (  )
import           Stack.Config.ConfigureScript ( ensureConfigureScript )
import           Stack.Constants
                   ( bindirSuffix, compilerOptionsCabalFlag
                   , relDirBuild
                   , testGhcEnvRelFile
                   )
import           Stack.Constants.Config
                   ( distDirFromDir, distRelativeDir, hpcDirFromDir
                   , hpcRelativeDir, setupConfigFromDir
                   )
import           Stack.Coverage
                   ( generateHpcReport
                   , updateTixFile
                   )
import           Stack.GhcPkg ( ghcPkg, unregisterGhcPkgIds )
import           Stack.Package
                   ( buildLogPath, buildableExes, buildableSubLibs
                   , hasBuildableMainLibrary, mainLibraryHasExposedModules
                   )
import           Stack.PackageDump ( conduitDumpPackage, ghcPkgDescribe )
import           Stack.Prelude
import           Stack.Types.Build
                   ( ConfigCache (..), PrecompiledCache (..)
                   , Task (..), TaskConfigOpts (..), TaskType (..)
                   , taskAnyMissing, taskIsTarget
                   , taskLocation, taskProvides
                   , taskTypePackageIdentifier
                   )
import qualified  Stack.Types.Build as ConfigCache ( ConfigCache (..) )
import           Stack.Types.Build.Exception
                   ( BuildException (..), BuildPrettyException (..) )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..), projectRootL )
import           Stack.Types.BuildOpts
                   ( BenchmarkOpts (..), BuildOpts (..), BuildOptsCLI (..)
                   , HaddockOpts (..), TestOpts (..)
                   )
import           Stack.Types.CompCollection
                   ( collectionKeyValueList, collectionLookup
                   , getBuildableListAs, getBuildableListText
                   )
import           Stack.Types.Compiler
                   ( ActualCompiler (..), WhichCompiler (..)
                   , getGhcVersion, whichCompilerL
                   )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), GhcPkgExe (..), HasCompiler (..)
                   , cpWhich, getGhcPkgExe
                   )
import qualified Stack.Types.Component as Component
import           Stack.Types.Config
                   ( Config (..), HasConfig (..) )
import           Stack.Types.ConfigureOpts
                   ( BaseConfigOpts (..), ConfigureOpts (..) )
import           Stack.Types.DumpPackage ( DumpPackage (..) )
import           Stack.Types.EnvConfig
                   ( HasEnvConfig (..), actualCompilerVersionL
                   , appropriateGhcColorFlag
                   )
import           Stack.Types.EnvSettings ( EnvSettings (..) )
import           Stack.Types.GhcPkgId ( GhcPkgId, unGhcPkgId )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.Installed
                   ( InstallLocation (..), Installed (..), InstalledMap
                   , InstalledLibraryInfo (..)
                   )
import           Stack.Types.IsMutable ( IsMutable (..) )
import           Stack.Types.NamedComponent
                   ( NamedComponent, exeComponents, isCBench
                   , isCTest, renderComponent
                   )
import           Stack.Types.Package
                   ( LocalPackage (..), Package (..), installedMapGhcPkgId
                   , runMemoizedWith, simpleInstalledLib
                   , toCabalMungedPackageName
                   )
import           Stack.Types.PackageFile ( PackageWarning (..) )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Curator ( Curator (..) )
import           Stack.Types.Runner ( HasRunner, globalOptsL )
import           System.IO.Error ( isDoesNotExistError )
import           System.PosixCompat.Files
                   ( createLink, getFileStatus, modificationTime )
import           System.Random ( randomIO )
import           Stack.Build.ExecuteEnv

-- | Generate the ConfigCache
getConfigCache ::
     HasEnvConfig env
  => ExecuteEnv
  -> Task
  -> InstalledMap
  -> Bool
  -> Bool
  -> RIO env (Map PackageIdentifier GhcPkgId, ConfigCache)
getConfigCache ee task installedMap enableTest enableBench = do
  let extra =
        -- We enable tests if the test suite dependencies are already
        -- installed, so that we avoid unnecessary recompilation based on
        -- cabal_macros.h changes when switching between 'stack build' and
        -- 'stack test'. See:
        -- https://github.com/commercialhaskell/stack/issues/805
        case task.taskType of
          TTLocalMutable _ ->
            -- FIXME: make this work with exact-configuration.
            -- Not sure how to plumb the info atm. See
            -- https://github.com/commercialhaskell/stack/issues/2049
            [ "--enable-tests" | enableTest] ++
            [ "--enable-benchmarks" | enableBench]
          TTRemotePackage{} -> []
  idMap <- liftIO $ readTVarIO ee.ghcPkgIds
  let getMissing ident =
        case Map.lookup ident idMap of
          Nothing
              -- Expect to instead find it in installedMap if it's
              -- an initialBuildSteps target.
              | ee.buildOptsCLI.initialBuildSteps && taskIsTarget task
              , Just (_, installed) <- Map.lookup (pkgName ident) installedMap
                  -> pure $ installedToGhcPkgId ident installed
          Just installed -> pure $ installedToGhcPkgId ident installed
          _ -> throwM $ PackageIdMissingBug ident
      installedToGhcPkgId ident (Library ident' libInfo) =
        assert (ident == ident') (installedMapGhcPkgId ident libInfo)
      installedToGhcPkgId _ (Executable _) = mempty
      TaskConfigOpts missing mkOpts = task.configOpts
  missingMapList <- traverse getMissing $ toList missing
  let missing' = Map.unions missingMapList
      opts = mkOpts missing'
      allDeps = Set.fromList $ Map.elems missing' ++ Map.elems task.present
      cache = ConfigCache
        { opts = opts { coNoDirs = opts.coNoDirs ++ map T.unpack extra }
        , deps = allDeps
        , components =
            case task.taskType of
              TTLocalMutable lp ->
                Set.map (encodeUtf8 . renderComponent) lp.components
              TTRemotePackage{} -> Set.empty
        , haddock = task.buildHaddock
        , pkgSrc = task.cachePkgSrc
        , pathEnvVar = ee.pathEnvVar
        }
      allDepsMap = Map.union missing' task.present
  pure (allDepsMap, cache)

-- | Ensure that the configuration for the package matches what is given
ensureConfig :: HasEnvConfig env
             => ConfigCache -- ^ newConfigCache
             -> Path Abs Dir -- ^ package directory
             -> BuildOpts
             -> RIO env () -- ^ announce
             -> (ExcludeTHLoading -> [String] -> RIO env ()) -- ^ cabal
             -> Path Abs File -- ^ Cabal file
             -> Task
             -> RIO env Bool
ensureConfig newConfigCache pkgDir buildOpts announce cabal cabalfp task = do
  newCabalMod <-
    liftIO $ modificationTime <$> getFileStatus (toFilePath cabalfp)
  setupConfigfp <- setupConfigFromDir pkgDir
  let getNewSetupConfigMod =
        liftIO $ either (const Nothing) (Just . modificationTime) <$>
        tryJust
          (guard . isDoesNotExistError)
          (getFileStatus (toFilePath setupConfigfp))
  newSetupConfigMod <- getNewSetupConfigMod
  newProjectRoot <- S8.pack . toFilePath <$> view projectRootL
  -- See https://github.com/commercialhaskell/stack/issues/3554. This can be
  -- dropped when Stack drops support for GHC < 8.4.
  taskAnyMissingHackEnabled <-
    view $ actualCompilerVersionL . to getGhcVersion . to (< mkVersion [8, 4])
  needConfig <-
    if buildOpts.reconfigure
          -- The reason 'taskAnyMissing' is necessary is a bug in Cabal. See:
          -- <https://github.com/haskell/cabal/issues/4728#issuecomment-337937673>.
          -- The problem is that Cabal may end up generating the same package ID
          -- for a dependency, even if the ABI has changed. As a result, without
          -- check, Stack would think that a reconfigure is unnecessary, when in
          -- fact we _do_ need to reconfigure. The details here suck. We really
          -- need proper hashes for package identifiers.
       || (taskAnyMissingHackEnabled && taskAnyMissing task)
      then pure True
      else do
        -- We can ignore the components portion of the config
        -- cache, because it's just used to inform 'construct
        -- plan that we need to plan to build additional
        -- components. These components don't affect the actual
        -- package configuration.
        let ignoreComponents :: ConfigCache -> ConfigCache
            ignoreComponents cc = cc { ConfigCache.components = Set.empty }
        -- Determine the old and new configuration in the local directory, to
        -- determine if we need to reconfigure.
        mOldConfigCache <- tryGetConfigCache pkgDir

        mOldCabalMod <- tryGetCabalMod pkgDir

        -- Cabal's setup-config is created per OS/Cabal version, multiple
        -- projects using the same package could get a conflict because of this
        mOldSetupConfigMod <- tryGetSetupConfigMod pkgDir
        mOldProjectRoot <- tryGetPackageProjectRoot pkgDir

        pure $
                fmap ignoreComponents mOldConfigCache
             /= Just (ignoreComponents newConfigCache)
          || mOldCabalMod /= Just newCabalMod
          || mOldSetupConfigMod /= newSetupConfigMod
          || mOldProjectRoot /= Just newProjectRoot
  let ConfigureOpts dirs nodirs = newConfigCache.opts

  when task.buildTypeConfig $
    -- When build-type is Configure, we need to have a configure script in the
    -- local directory. If it doesn't exist, build it with autoreconf -i. See:
    -- https://github.com/commercialhaskell/stack/issues/3534
    ensureConfigureScript pkgDir

  when needConfig $ do
    deleteCaches pkgDir
    announce
    cp <- view compilerPathsL
    let (GhcPkgExe pkgPath) = cp.cpPkg
    let programNames =
          case cpWhich cp of
            Ghc ->
              [ ("ghc", toFilePath cp.cpCompiler)
              , ("ghc-pkg", toFilePath pkgPath)
              ]
    exes <- forM programNames $ \(name, file) -> do
      mpath <- findExecutable file
      pure $ case mpath of
          Left _ -> []
          Right x -> pure $ concat ["--with-", name, "=", x]
    -- Configure cabal with arguments determined by
    -- Stack.Types.Build.ureOpts
    cabal KeepTHLoading $ "configure" : concat
      [ concat exes
      , dirs
      , nodirs
      ]
    -- Only write the cache for local packages.  Remote packages are built in a
    -- temporary directory so the cache would never be used anyway.
    case task.taskType of
      TTLocalMutable{} -> writeConfigCache pkgDir newConfigCache
      TTRemotePackage{} -> pure ()
    writeCabalMod pkgDir newCabalMod
    -- This file gets updated one more time by the configure step, so get the
    -- most recent value. We could instead change our logic above to check if
    -- our config mod file is newer than the file above, but this seems
    -- reasonable too.
    getNewSetupConfigMod >>= writeSetupConfigMod pkgDir
    writePackageProjectRoot pkgDir newProjectRoot
  pure needConfig

-- | Make a padded prefix for log messages
packageNamePrefix :: ExecuteEnv -> PackageName -> String
packageNamePrefix ee name' =
  let name = packageNameString name'
      paddedName =
        case ee.largestPackageName of
          Nothing -> name
          Just len ->
            assert (len >= length name) $ take len $ name ++ L.repeat ' '
  in  paddedName <> "> "

announceTask ::
     HasLogFunc env
  => ExecuteEnv
  -> TaskType
  -> Utf8Builder
  -> RIO env ()
announceTask ee taskType action = logInfo $
     fromString
       (packageNamePrefix ee (pkgName (taskTypePackageIdentifier taskType)))
  <> action

-- Implements running a package's build, used to implement 'ATBuild' and
-- 'ATBuildFinal' tasks.  In particular this does the following:
--
-- * Checks if the package exists in the precompiled cache, and if so,
--   add it to the database instead of performing the build.
--
-- * Runs the configure step if needed ('ensureConfig')
--
-- * Runs the build step
--
-- * Generates haddocks
--
-- * Registers the library and copies the built executables into the
--   local install directory. Note that this is literally invoking Cabal
--   with @copy@, and not the copying done by @stack install@ - that is
--   handled by 'copyExecutables'.
singleBuild :: forall env. (HasEnvConfig env, HasRunner env)
            => ActionContext
            -> ExecuteEnv
            -> Task
            -> InstalledMap
            -> Bool             -- ^ Is this a final build?
            -> RIO env ()
singleBuild
    ac
    ee
    task
    installedMap
    isFinalBuild
  = do
    (allDepsMap, cache) <-
      getConfigCache ee task installedMap enableTests enableBenchmarks
    mprecompiled <- getPrecompiled cache
    minstalled <-
      case mprecompiled of
        Just precompiled -> copyPreCompiled precompiled
        Nothing -> do
          mcurator <- view $ buildConfigL . to (.curator)
          realConfigAndBuild cache mcurator allDepsMap
    case minstalled of
      Nothing -> pure ()
      Just installed -> do
        writeFlagCache installed cache
        liftIO $ atomically $
          modifyTVar ee.ghcPkgIds $ Map.insert pkgId installed
 where
  pkgId = taskProvides task
  PackageIdentifier pname pversion = pkgId
  doHaddock mcurator package =
       task.buildHaddock
    && not isFinalBuild
       -- Works around haddock failing on bytestring-builder since it has no
       -- modules when bytestring is new enough.
    && mainLibraryHasExposedModules package
       -- Special help for the curator tool to avoid haddocks that are known
       -- to fail
    && maybe True (Set.notMember pname . (.curatorSkipHaddock)) mcurator
  expectHaddockFailure =
      maybe False (Set.member pname . (.curatorExpectHaddockFailure))
  isHaddockForHackage = ee.buildOpts.haddockForHackage
  fulfillHaddockExpectations mcurator action
    | expectHaddockFailure mcurator = do
        eres <- tryAny $ action KeepOpen
        case eres of
          Right () -> prettyWarnL
            [ style Current (fromPackageName pname) <> ":"
            , flow "unexpected Haddock success."
            ]
          Left _ -> pure ()
  fulfillHaddockExpectations _ action = action CloseOnException

  buildingFinals = isFinalBuild || task.allInOne
  enableTests = buildingFinals && any isCTest (taskComponents task)
  enableBenchmarks = buildingFinals && any isCBench (taskComponents task)

  annSuffix executableBuildStatuses =
    if result == "" then "" else " (" <> result <> ")"
   where
    result = T.intercalate " + " $ concat
      [ ["lib" | task.allInOne && hasLib]
      , ["sub-lib" | task.allInOne && hasSubLib]
      , ["exe" | task.allInOne && hasExe]
      , ["test" | enableTests]
      , ["bench" | enableBenchmarks]
      ]
    (hasLib, hasSubLib, hasExe) = case task.taskType of
      TTLocalMutable lp ->
        let package = lp.package
            hasLibrary = hasBuildableMainLibrary package
            hasSubLibraries = not $ null package.subLibraries
            hasExecutables =
              not . Set.null $ exesToBuild executableBuildStatuses lp
        in  (hasLibrary, hasSubLibraries, hasExecutables)
      -- This isn't true, but we don't want to have this info for upstream deps.
      _ -> (False, False, False)

  getPrecompiled cache =
    case task.taskType of
      TTRemotePackage Immutable _ loc -> do
        mpc <- readPrecompiledCache
                 loc
                 cache.opts
                 cache.haddock
        case mpc of
          Nothing -> pure Nothing
          -- Only pay attention to precompiled caches that refer to packages
          -- within the snapshot.
          Just pc
            | maybe False
                (ee.baseConfigOpts.bcoSnapInstallRoot `isProperPrefixOf`)
                pc.pcLibrary -> pure Nothing
          -- If old precompiled cache files are left around but snapshots are
          -- deleted, it is possible for the precompiled file to refer to the
          -- very library we're building, and if flags are changed it may try to
          -- copy the library to itself. This check prevents that from
          -- happening.
          Just pc -> do
            let allM _ [] = pure True
                allM f (x:xs) = do
                  b <- f x
                  if b then allM f xs else pure False
            b <- liftIO $
                   allM doesFileExist $ maybe id (:) pc.pcLibrary pc.pcExes
            pure $ if b then Just pc else Nothing
      _ -> pure Nothing

  copyPreCompiled (PrecompiledCache mlib subLibs exes) = do
    announceTask ee task.taskType "using precompiled package"

    -- We need to copy .conf files for the main library and all sub-libraries
    -- which exist in the cache, from their old snapshot to the new one.
    -- However, we must unregister any such library in the new snapshot, in case
    -- it was built with different flags.
    let
      subLibNames = Set.toList $ buildableSubLibs $ case task.taskType of
        TTLocalMutable lp -> lp.package
        TTRemotePackage _ p _ -> p
      toMungedPackageId :: Text -> MungedPackageId
      toMungedPackageId subLib =
        let subLibName = LSubLibName $ mkUnqualComponentName $ T.unpack subLib
        in  MungedPackageId (MungedPackageName pname subLibName) pversion
      toPackageId :: MungedPackageId -> PackageIdentifier
      toPackageId (MungedPackageId n v) =
        PackageIdentifier (encodeCompatPackageName n) v
      allToUnregister :: [Either PackageIdentifier GhcPkgId]
      allToUnregister = mcons
        (Left pkgId <$ mlib)
        (map (Left . toPackageId . toMungedPackageId) subLibNames)
      allToRegister = mcons mlib subLibs

    unless (null allToRegister) $
      withMVar ee.installLock $ \() -> do
        -- We want to ignore the global and user package databases. ghc-pkg
        -- allows us to specify --no-user-package-db and --package-db=<db> on
        -- the command line.
        let pkgDb = ee.baseConfigOpts.bcoSnapDB
        ghcPkgExe <- getGhcPkgExe
        -- First unregister, silently, everything that needs to be unregistered.
        case nonEmpty allToUnregister of
          Nothing -> pure ()
          Just allToUnregister' -> catchAny
            (unregisterGhcPkgIds False ghcPkgExe pkgDb allToUnregister')
            (const (pure ()))
        -- Now, register the cached conf files.
        forM_ allToRegister $ \libpath ->
          ghcPkg ghcPkgExe [pkgDb] ["register", "--force", toFilePath libpath]

    liftIO $ forM_ exes $ \exe -> do
      ensureDir bindir
      let dst = bindir </> filename exe
      createLink (toFilePath exe) (toFilePath dst) `catchIO` \_ -> copyFile exe dst
    case (mlib, exes) of
      (Nothing, _:_) -> markExeInstalled (taskLocation task) pkgId
      _ -> pure ()

    -- Find the package in the database
    let pkgDbs = [ee.baseConfigOpts.bcoSnapDB]

    case mlib of
      Nothing -> pure $ Just $ Executable pkgId
      Just _ -> do
        mpkgid <- loadInstalledPkg pkgDbs ee.snapshotDumpPkgs pname

        pure $ Just $
          case mpkgid of
            Nothing -> assert False $ Executable pkgId
            Just pkgid -> simpleInstalledLib pkgId pkgid mempty
   where
    bindir = ee.baseConfigOpts.bcoSnapInstallRoot </> bindirSuffix

  realConfigAndBuild cache mcurator allDepsMap =
    withSingleContext ac ee task.taskType allDepsMap Nothing $
      \package cabalfp pkgDir cabal0 announce _outputType -> do
        let cabal = cabal0 CloseOnException
        executableBuildStatuses <- getExecutableBuildStatuses package pkgDir
        when (  not (cabalIsSatisfied executableBuildStatuses)
             && taskIsTarget task
             ) $
          prettyInfoL
            [ flow "Building all executables for"
            , style Current (fromPackageName package.name)
            , flow "once. After a successful build of all of them, only \
                   \specified executables will be rebuilt."
            ]
        _neededConfig <-
          ensureConfig
            cache
            pkgDir
            ee.buildOpts
            ( announce
                (  "configure"
                <> display (annSuffix executableBuildStatuses)
                )
            )
            cabal
            cabalfp
            task
        let installedMapHasThisPkg :: Bool
            installedMapHasThisPkg =
              case Map.lookup package.name installedMap of
                Just (_, Library ident _) -> ident == pkgId
                Just (_, Executable _) -> True
                _ -> False

        case ( ee.buildOptsCLI.onlyConfigure
             , ee.buildOptsCLI.initialBuildSteps && taskIsTarget task
             ) of
          -- A full build is done if there are downstream actions,
          -- because their configure step will require that this
          -- package is built. See
          -- https://github.com/commercialhaskell/stack/issues/2787
          (True, _) | null ac.acDownstream -> pure Nothing
          (_, True) | null ac.acDownstream || installedMapHasThisPkg -> do
            initialBuildSteps executableBuildStatuses cabal announce
            pure Nothing
          _ -> fulfillCuratorBuildExpectations
                 pname
                 mcurator
                 enableTests
                 enableBenchmarks
                 Nothing
                 (Just <$>
                    realBuild cache package pkgDir cabal0 announce executableBuildStatuses)

  initialBuildSteps executableBuildStatuses cabal announce = do
    announce
      (  "initial-build-steps"
      <> display (annSuffix executableBuildStatuses)
      )
    cabal KeepTHLoading ["repl", "stack-initial-build-steps"]

  realBuild ::
       ConfigCache
    -> Package
    -> Path Abs Dir
    -> (KeepOutputOpen -> ExcludeTHLoading -> [String] -> RIO env ())
    -> (Utf8Builder -> RIO env ())
       -- ^ A plain 'announce' function
    -> Map Text ExecutableBuildStatus
    -> RIO env Installed
  realBuild cache package pkgDir cabal0 announce executableBuildStatuses = do
    let cabal = cabal0 CloseOnException
    wc <- view $ actualCompilerVersionL . whichCompilerL

    markExeNotInstalled (taskLocation task) pkgId
    case task.taskType of
      TTLocalMutable lp -> do
        when enableTests $ setTestStatus pkgDir TSUnknown
        caches <- runMemoizedWith lp.newBuildCaches
        mapM_
          (uncurry (writeBuildCache pkgDir))
          (Map.toList caches)
      TTRemotePackage{} -> pure ()

    -- FIXME: only output these if they're in the build plan.

    let postBuildCheck _succeeded = do
          mlocalWarnings <- case task.taskType of
            TTLocalMutable lp -> do
                warnings <- checkForUnlistedFiles task.taskType pkgDir
                -- TODO: Perhaps only emit these warnings for non extra-dep?
                pure (Just (lp.cabalFile, warnings))
            _ -> pure Nothing
          -- NOTE: once
          -- https://github.com/commercialhaskell/stack/issues/2649
          -- is resolved, we will want to partition the warnings
          -- based on variety, and output in different lists.
          let showModuleWarning (UnlistedModulesWarning comp modules) =
                "- In" <+>
                fromString (T.unpack (renderComponent comp)) <>
                ":" <> line <>
                indent 4 ( mconcat
                         $ L.intersperse line
                         $ map
                             (style Good . fromString . C.display)
                             modules
                         )
          forM_ mlocalWarnings $ \(cabalfp, warnings) ->
            unless (null warnings) $ prettyWarn $
                 flow "The following modules should be added to \
                      \exposed-modules or other-modules in" <+>
                      pretty cabalfp
              <> ":"
              <> line
              <> indent 4 ( mconcat
                          $ L.intersperse line
                          $ map showModuleWarning warnings
                          )
              <> blankLine
              <> flow "Missing modules in the Cabal file are likely to cause \
                      \undefined reference errors from the linker, along with \
                      \other problems."

    actualCompiler <- view actualCompilerVersionL
    () <- announce
      (  "build"
      <> display (annSuffix executableBuildStatuses)
      <> " with "
      <> display actualCompiler
      )
    config <- view configL
    extraOpts <- extraBuildOptions wc ee.buildOpts
    let stripTHLoading
          | config.hideTHLoading = ExcludeTHLoading
          | otherwise                  = KeepTHLoading
    cabal stripTHLoading (("build" :) $ (++ extraOpts) $
        case (task.taskType, task.allInOne, isFinalBuild) of
            (_, True, True) -> throwM AllInOneBuildBug
            (TTLocalMutable lp, False, False) ->
              primaryComponentOptions executableBuildStatuses lp
            (TTLocalMutable lp, False, True) -> finalComponentOptions lp
            (TTLocalMutable lp, True, False) ->
                 primaryComponentOptions executableBuildStatuses lp
              ++ finalComponentOptions lp
            (TTRemotePackage{}, _, _) -> [])
      `catch` \ex -> case ex of
        CabalExitedUnsuccessfully{} ->
          postBuildCheck False >> prettyThrowM ex
        _ -> throwM ex
    postBuildCheck True

    mcurator <- view $ buildConfigL . to (.curator)
    when (doHaddock mcurator package) $ do
      announce $ if isHaddockForHackage
        then "haddock for Hackage"
        else "haddock"

      -- For GHC 8.4 and later, provide the --quickjump option.
      let quickjump =
            case actualCompiler of
              ACGhc ghcVer
                | ghcVer >= mkVersion [8, 4] -> ["--haddock-option=--quickjump"]
              _ -> []

      fulfillHaddockExpectations mcurator $ \keep -> do
        let args = concat
              (  if isHaddockForHackage
                   then
                     [ [ "--for-hackage" ] ]
                   else
                     [ [ "--html"
                       , "--hoogle"
                       , "--html-location=../$pkg-$version/"
                       ]
                     , [ "--haddock-option=--hyperlinked-source"
                       | ee.buildOpts.haddockHyperlinkSource
                       ]
                     , [ "--internal" | ee.buildOpts.haddockInternal  ]
                     , quickjump
                     ]
              <> [ [ "--haddock-option=" <> opt
                   | opt <- ee.buildOpts.haddockOpts.hoAdditionalArgs
                   ]
                 ]
              )

        cabal0 keep KeepTHLoading $ "haddock" : args

    let hasLibrary = hasBuildableMainLibrary package
        hasSubLibraries = not $ null package.subLibraries
        hasExecutables = not $ null package.executables
        shouldCopy =
             not isFinalBuild
          && (hasLibrary || hasSubLibraries || hasExecutables)
    when shouldCopy $ withMVar ee.installLock $ \() -> do
      announce "copy/register"
      eres <- try $ cabal KeepTHLoading ["copy"]
      case eres of
        Left err@CabalExitedUnsuccessfully{} ->
          throwM $ CabalCopyFailed
                     (package.buildType == C.Simple)
                     (displayException err)
        _ -> pure ()
      when (hasLibrary || hasSubLibraries) $ cabal KeepTHLoading ["register"]

    -- copy ddump-* files
    case T.unpack <$> ee.buildOpts.ddumpDir of
      Just ddumpPath | buildingFinals && not (null ddumpPath) -> do
        distDir <- distRelativeDir
        ddumpDir <- parseRelDir ddumpPath

        logDebug $ fromString ("ddump-dir: " <> toFilePath ddumpDir)
        logDebug $ fromString ("dist-dir: " <> toFilePath distDir)

        runConduitRes
          $ CF.sourceDirectoryDeep False (toFilePath distDir)
         .| CL.filter (L.isInfixOf ".dump-")
         .| CL.mapM_ (\src -> liftIO $ do
              parentDir <- parent <$> parseRelDir src
              destBaseDir <-
                (ddumpDir </>) <$> stripProperPrefix distDir parentDir
              -- exclude .stack-work dir
              unless (".stack-work" `L.isInfixOf` toFilePath destBaseDir) $ do
                ensureDir destBaseDir
                src' <- parseRelFile src
                copyFile src' (destBaseDir </> filename src'))
      _ -> pure ()

    let (installedPkgDb, installedDumpPkgsTVar) =
          case taskLocation task of
            Snap ->
              ( ee.baseConfigOpts.bcoSnapDB
              , ee.snapshotDumpPkgs )
            Local ->
              ( ee.baseConfigOpts.bcoLocalDB
              , ee.localDumpPkgs )
    let ident = PackageIdentifier package.name package.version
    -- only pure the sub-libraries to cache them if we also cache the main
    -- library (that is, if it exists)
    (mpkgid, subLibsPkgIds) <- if hasBuildableMainLibrary package
      then do
        subLibsPkgIds' <- fmap catMaybes $
          forM (getBuildableListAs id package.subLibraries) $ \subLib -> do
            let subLibName = toCabalMungedPackageName package.name subLib
            maybeGhcpkgId <- loadInstalledPkg
              [installedPkgDb]
              installedDumpPkgsTVar
              (encodeCompatPackageName subLibName)
            pure $ (subLib, ) <$> maybeGhcpkgId
        let subLibsPkgIds = snd <$> subLibsPkgIds'
        mpkgid <- loadInstalledPkg
                    [installedPkgDb]
                    installedDumpPkgsTVar
                    package.name
        let makeInstalledLib pkgid =
              simpleInstalledLib ident pkgid (Map.fromList subLibsPkgIds')
        case mpkgid of
          Nothing -> throwM $ Couldn'tFindPkgId package.name
          Just pkgid -> pure (makeInstalledLib pkgid, subLibsPkgIds)
      else do
        markExeInstalled (taskLocation task) pkgId -- TODO unify somehow
                                                   -- with writeFlagCache?
        pure (Executable ident, []) -- don't pure sublibs in this case

    case task.taskType of
      TTRemotePackage Immutable _ loc ->
        writePrecompiledCache
          ee.baseConfigOpts
          loc
          cache.opts
          cache.haddock
          mpkgid
          subLibsPkgIds
          (buildableExes package)
      _ -> pure ()

    case task.taskType of
      -- For packages from a package index, pkgDir is in the tmp directory. We
      -- eagerly delete it if no other tasks require it, to reduce space usage
      -- in tmp (#3018).
      TTRemotePackage{} -> do
        let remaining =
              filter
                (\(ActionId x _) -> x == pkgId)
                (Set.toList ac.acRemaining)
        when (null remaining) $ removeDirRecur pkgDir
      TTLocalMutable{} -> pure ()

    pure mpkgid

  loadInstalledPkg ::
       [Path Abs Dir]
    -> TVar (Map GhcPkgId DumpPackage)
    -> PackageName
    -> RIO env (Maybe GhcPkgId)
  loadInstalledPkg pkgDbs tvar name = do
    pkgexe <- getGhcPkgExe
    dps <- ghcPkgDescribe pkgexe name pkgDbs $ conduitDumpPackage .| CL.consume
    case dps of
      [] -> pure Nothing
      [dp] -> do
        liftIO $ atomically $ modifyTVar' tvar (Map.insert dp.ghcPkgId dp)
        pure $ Just dp.ghcPkgId
      _ -> throwM $ MultipleResultsBug name dps

-- | Get the build status of all the package executables. Do so by
-- testing whether their expected output file exists, e.g.
--
-- .stack-work/dist/x86_64-osx/Cabal-1.22.4.0/build/alpha/alpha
-- .stack-work/dist/x86_64-osx/Cabal-1.22.4.0/build/alpha/alpha.exe
-- .stack-work/dist/x86_64-osx/Cabal-1.22.4.0/build/alpha/alpha.jsexe/ (NOTE: a dir)
getExecutableBuildStatuses ::
     HasEnvConfig env
  => Package
  -> Path Abs Dir
  -> RIO env (Map Text ExecutableBuildStatus)
getExecutableBuildStatuses package pkgDir = do
  distDir <- distDirFromDir pkgDir
  platform <- view platformL
  fmap
    Map.fromList
    (mapM (checkExeStatus platform distDir) (Set.toList (buildableExes package)))

-- | Check whether the given executable is defined in the given dist directory.
checkExeStatus ::
     HasLogFunc env
  => Platform
  -> Path b Dir
  -> Text
  -> RIO env (Text, ExecutableBuildStatus)
checkExeStatus platform distDir name = do
  exename <- parseRelDir (T.unpack name)
  exists <- checkPath (distDir </> relDirBuild </> exename)
  pure
    ( name
    , if exists
        then ExecutableBuilt
        else ExecutableNotBuilt)
 where
  checkPath base =
    case platform of
      Platform _ Windows -> do
        fileandext <- parseRelFile (file ++ ".exe")
        doesFileExist (base </> fileandext)
      _ -> do
        fileandext <- parseRelFile file
        doesFileExist (base </> fileandext)
   where
    file = T.unpack name

-- | Check if any unlisted files have been found, and add them to the build cache.
checkForUnlistedFiles ::
     HasEnvConfig env
  => TaskType
  -> Path Abs Dir
  -> RIO env [PackageWarning]
checkForUnlistedFiles (TTLocalMutable lp) pkgDir = do
  caches <- runMemoizedWith lp.newBuildCaches
  (addBuildCache,warnings) <-
    addUnlistedToBuildCache
      lp.package
      lp.cabalFile
      lp.components
      caches
  forM_ (Map.toList addBuildCache) $ \(component, newToCache) -> do
    let cache = Map.findWithDefault Map.empty component caches
    writeBuildCache pkgDir component $
      Map.unions (cache : newToCache)
  pure warnings
checkForUnlistedFiles TTRemotePackage{} _ = pure []

-- | Implements running a package's tests. Also handles producing
-- coverage reports if coverage is enabled.
singleTest :: HasEnvConfig env
           => TestOpts
           -> [Text]
           -> ActionContext
           -> ExecuteEnv
           -> Task
           -> InstalledMap
           -> RIO env ()
singleTest topts testsToRun ac ee task installedMap = do
  -- FIXME: Since this doesn't use cabal, we should be able to avoid using a
  -- full blown 'withSingleContext'.
  (allDepsMap, _cache) <- getConfigCache ee task installedMap True False
  mcurator <- view $ buildConfigL . to (.curator)
  let pname = pkgName $ taskProvides task
      expectFailure = expectTestFailure pname mcurator
  withSingleContext ac ee task.taskType allDepsMap (Just "test") $
    \package _cabalfp pkgDir _cabal announce outputType -> do
      config <- view configL
      let needHpc = topts.toCoverage
      toRun <-
        if topts.toDisableRun
          then do
            announce "Test running disabled by --no-run-tests flag."
            pure False
          else if topts.toRerunTests
            then pure True
            else do
              status <- getTestStatus pkgDir
              case status of
                TSSuccess -> do
                  unless (null testsToRun) $
                    announce "skipping already passed test"
                  pure False
                TSFailure
                  | expectFailure -> do
                      announce "skipping already failed test that's expected to fail"
                      pure False
                  | otherwise -> do
                      announce "rerunning previously failed test"
                      pure True
                TSUnknown -> pure True

      when toRun $ do
        buildDir <- distDirFromDir pkgDir
        hpcDir <- hpcDirFromDir pkgDir
        when needHpc (ensureDir hpcDir)

        let suitesToRun
              = [ testSuitePair
                | testSuitePair <-
                    ((fmap . fmap) (.interface) <$> collectionKeyValueList)
                      package.testSuites
                , let testName = fst testSuitePair
                , testName `elem` testsToRun
                ]

        errs <- fmap Map.unions $ forM suitesToRun $ \(testName, suiteInterface) -> do
          let stestName = T.unpack testName
          (testName', isTestTypeLib) <-
            case suiteInterface of
              C.TestSuiteLibV09{} -> pure (stestName ++ "Stub", True)
              C.TestSuiteExeV10{} -> pure (stestName, False)
              interface -> throwM (TestSuiteTypeUnsupported interface)

          let exeName = testName' ++
                case config.platform of
                  Platform _ Windows -> ".exe"
                  _ -> ""
          tixPath <- fmap (pkgDir </>) $ parseRelFile $ exeName ++ ".tix"
          exePath <-
            fmap (buildDir </>) $ parseRelFile $
              "build/" ++ testName' ++ "/" ++ exeName
          exists <- doesFileExist exePath
          -- in Stack.Package.packageFromPackageDescription we filter out
          -- package itself of any dependencies so any tests requiring loading
          -- of their own package library will fail so to prevent this we return
          -- it back here but unfortunately unconditionally
          installed <- case Map.lookup pname installedMap of
            Just (_, installed) -> pure $ Just installed
            Nothing -> do
              idMap <- liftIO $ readTVarIO ee.ghcPkgIds
              pure $ Map.lookup (taskProvides task) idMap
          let pkgGhcIdList = case installed of
                               Just (Library _ libInfo) -> [libInfo.iliId]
                               _ -> []
          -- doctest relies on template-haskell in QuickCheck-based tests
          thGhcId <-
            case L.find ((== "template-haskell") . pkgName . (.packageIdent) . snd)
                   (Map.toList ee.globalDumpPkgs) of
              Just (ghcId, _) -> pure ghcId
              Nothing -> throwIO TemplateHaskellNotFoundBug
          -- env variable GHC_ENVIRONMENT is set for doctest so module names for
          -- packages with proper dependencies should no longer get ambiguous
          -- see e.g. https://github.com/doctest/issues/119
          -- also we set HASKELL_DIST_DIR to a package dist directory so
          -- doctest will be able to load modules autogenerated by Cabal
          let setEnv f pc = modifyEnvVars pc $ \envVars ->
                Map.insert "HASKELL_DIST_DIR" (T.pack $ toFilePath buildDir) $
                Map.insert "GHC_ENVIRONMENT" (T.pack f) envVars
              fp' = ee.tempDir </> testGhcEnvRelFile
          -- Add a random suffix to avoid conflicts between parallel jobs
          -- See https://github.com/commercialhaskell/stack/issues/5024
          randomInt <- liftIO (randomIO :: IO Int)
          let randomSuffix = "." <> show (abs randomInt)
          fp <- toFilePath <$> addExtension randomSuffix fp'
          let snapDBPath =
                toFilePathNoTrailingSep ee.baseConfigOpts.bcoSnapDB
              localDBPath =
                toFilePathNoTrailingSep ee.baseConfigOpts.bcoLocalDB
              ghcEnv =
                   "clear-package-db\n"
                <> "global-package-db\n"
                <> "package-db "
                <> fromString snapDBPath
                <> "\n"
                <> "package-db "
                <> fromString localDBPath
                <> "\n"
                <> foldMap
                     ( \ghcId ->
                            "package-id "
                         <> display (unGhcPkgId ghcId)
                         <> "\n"
                     )
                     (pkgGhcIdList ++ thGhcId:Map.elems allDepsMap)
          writeFileUtf8Builder fp ghcEnv
          menv <- liftIO $
            setEnv fp =<< config.processContextSettings EnvSettings
              { esIncludeLocals = taskLocation task == Local
              , esIncludeGhcPackagePath = True
              , esStackExe = True
              , esLocaleUtf8 = False
              , esKeepGhcRts = False
              }
          let emptyResult = Map.singleton testName Nothing
          withProcessContext menv $ if exists
            then do
                -- We clear out the .tix files before doing a run.
                when needHpc $ do
                  tixexists <- doesFileExist tixPath
                  when tixexists $
                    prettyWarnL
                      [ flow "Removing HPC file"
                      , pretty tixPath <> "."
                      ]
                  liftIO $ ignoringAbsence (removeFile tixPath)

                let args = topts.toAdditionalArgs
                    argsDisplay = case args of
                      [] -> ""
                      _ ->    ", args: "
                           <> T.intercalate " " (map showProcessArgDebug args)
                announce $
                     "test (suite: "
                  <> display testName
                  <> display argsDisplay
                  <> ")"

                -- Clear "Progress: ..." message before
                -- redirecting output.
                case outputType of
                  OTConsole _ -> do
                    logStickyDone ""
                    liftIO $ hFlush stdout
                    liftIO $ hFlush stderr
                  OTLogFile _ _ -> pure ()

                let output = case outputType of
                      OTConsole Nothing -> Nothing <$ inherit
                      OTConsole (Just prefix) -> fmap
                        ( \src -> Just $
                               runConduit $ src
                            .| CT.decodeUtf8Lenient
                            .| CT.lines
                            .| CL.map stripCR
                            .| CL.mapM_ (\t -> logInfo $ prefix <> display t)
                        )
                        createSource
                      OTLogFile _ h -> Nothing <$ useHandleOpen h
                    optionalTimeout action
                      | Just maxSecs <- topts.toMaximumTimeSeconds, maxSecs > 0 =
                          timeout (maxSecs * 1000000) action
                      | otherwise = Just <$> action

                mec <- withWorkingDir (toFilePath pkgDir) $
                  optionalTimeout $ proc (toFilePath exePath) args $ \pc0 -> do
                    changeStdin <-
                      if isTestTypeLib
                        then do
                          logPath <- buildLogPath package (Just stestName)
                          ensureDir (parent logPath)
                          pure $
                              setStdin
                            $ byteStringInput
                            $ BL.fromStrict
                            $ encodeUtf8 $ fromString $
                            show ( logPath
                                 , mkUnqualComponentName (T.unpack testName)
                                 )
                        else do
                          isTerminal <- view $ globalOptsL . to (.globalTerminal)
                          if topts.toAllowStdin && isTerminal
                            then pure id
                            else pure $ setStdin $ byteStringInput mempty
                    let pc = changeStdin
                           $ setStdout output
                           $ setStderr output
                             pc0
                    withProcessWait pc $ \p -> do
                      case (getStdout p, getStderr p) of
                        (Nothing, Nothing) -> pure ()
                        (Just x, Just y) -> concurrently_ x y
                        (x, y) -> assert False $
                          concurrently_
                            (fromMaybe (pure ()) x)
                            (fromMaybe (pure ()) y)
                      waitExitCode p
                -- Add a trailing newline, incase the test
                -- output didn't finish with a newline.
                case outputType of
                  OTConsole Nothing -> prettyInfo blankLine
                  _ -> pure ()
                -- Move the .tix file out of the package
                -- directory into the hpc work dir, for
                -- tidiness.
                when needHpc $
                  updateTixFile package.name tixPath testName'
                let announceResult result =
                      announce $
                           "Test suite "
                        <> display testName
                        <> " "
                        <> result
                case mec of
                  Just ExitSuccess -> do
                    announceResult "passed"
                    pure Map.empty
                  Nothing -> do
                    announceResult "timed out"
                    if expectFailure
                    then pure Map.empty
                    else pure $ Map.singleton testName Nothing
                  Just ec -> do
                    announceResult "failed"
                    if expectFailure
                    then pure Map.empty
                    else pure $ Map.singleton testName (Just ec)
              else do
                unless expectFailure $
                  logError $
                    displayShow $ TestSuiteExeMissing
                      (package.buildType == C.Simple)
                      exeName
                      (packageNameString package.name)
                      (T.unpack testName)
                pure emptyResult

        when needHpc $ do
          let testsToRun' = map f testsToRun
              f tName =
                case (.interface) <$> mComponent of
                  Just C.TestSuiteLibV09{} -> tName <> "Stub"
                  _ -> tName
               where
                mComponent = collectionLookup tName package.testSuites
          generateHpcReport pkgDir package testsToRun'

        bs <- liftIO $
          case outputType of
            OTConsole _ -> pure ""
            OTLogFile logFile h -> do
              hClose h
              S.readFile $ toFilePath logFile

        let succeeded = Map.null errs
        unless (succeeded || expectFailure) $
          throwM $ TestSuiteFailure
            (taskProvides task)
            errs
            (case outputType of
               OTLogFile fp _ -> Just fp
               OTConsole _ -> Nothing)
            bs

        setTestStatus pkgDir $ if succeeded then TSSuccess else TSFailure

-- | Implements running a package's benchmarks.
singleBench :: HasEnvConfig env
            => BenchmarkOpts
            -> [Text]
            -> ActionContext
            -> ExecuteEnv
            -> Task
            -> InstalledMap
            -> RIO env ()
singleBench beopts benchesToRun ac ee task installedMap = do
  (allDepsMap, _cache) <- getConfigCache ee task installedMap False True
  withSingleContext ac ee task.taskType allDepsMap (Just "bench") $
    \_package _cabalfp _pkgDir cabal announce _outputType -> do
      let args = map T.unpack benchesToRun <> maybe []
                       ((:[]) . ("--benchmark-options=" <>))
                       beopts.beoAdditionalArgs

      toRun <-
        if beopts.beoDisableRun
          then do
            announce "Benchmark running disabled by --no-run-benchmarks flag."
            pure False
          else pure True

      when toRun $ do
        announce "benchmarks"
        cabal CloseOnException KeepTHLoading ("bench" : args)

-- Do not pass `-hpcdir` as GHC option if the coverage is not enabled.
-- This helps running stack-compiled programs with dynamic interpreters like
-- `hint`. Cfr: https://github.com/commercialhaskell/stack/issues/997
extraBuildOptions :: (HasEnvConfig env, HasRunner env)
                  => WhichCompiler -> BuildOpts -> RIO env [String]
extraBuildOptions wc bopts = do
  colorOpt <- appropriateGhcColorFlag
  let optsFlag = compilerOptionsCabalFlag wc
      baseOpts = maybe "" (" " ++) colorOpt
  if bopts.testOpts.toCoverage
    then do
      hpcIndexDir <- toFilePathNoTrailingSep <$> hpcRelativeDir
      pure [optsFlag, "-hpcdir " ++ hpcIndexDir ++ baseOpts]
    else
      pure [optsFlag, baseOpts]

-- Library, sub-library, foreign library and executable build components.
primaryComponentOptions ::
     Map Text ExecutableBuildStatus
  -> LocalPackage
  -> [String]
primaryComponentOptions executableBuildStatuses lp =
  -- TODO: get this information from target parsing instead, which will allow
  -- users to turn off library building if desired
     ( if hasBuildableMainLibrary package
         then map T.unpack
           $ T.append "lib:" (T.pack (packageNameString package.name))
           : map
               (T.append "flib:")
               (getBuildableListText package.foreignLibraries)
         else []
     )
  ++ map
       (T.unpack . T.append "lib:")
       (getBuildableListText package.subLibraries)
  ++ map
       (T.unpack . T.append "exe:")
       (Set.toList $ exesToBuild executableBuildStatuses lp)
 where
  package = lp.package

-- | History of this function:
--
-- * Normally it would do either all executables or if the user specified
--   requested components, just build them. Afterwards, due to this Cabal bug
--   <https://github.com/haskell/cabal/issues/2780>, we had to make Stack build
--   all executables every time.
--
-- * In <https://github.com/commercialhaskell/stack/issues/3229> this was
--   flagged up as very undesirable behavior on a large project, hence the
--   behavior below that we build all executables once (modulo success), and
--   thereafter pay attention to user-wanted components.
--
exesToBuild :: Map Text ExecutableBuildStatus -> LocalPackage -> Set Text
exesToBuild executableBuildStatuses lp =
  if cabalIsSatisfied executableBuildStatuses && lp.wanted
    then exeComponents lp.components
    else buildableExes lp.package

-- | Do the current executables satisfy Cabal's bugged out requirements?
cabalIsSatisfied :: Map k ExecutableBuildStatus -> Bool
cabalIsSatisfied = all (== ExecutableBuilt) . Map.elems

-- Test-suite and benchmark build components.
finalComponentOptions :: LocalPackage -> [String]
finalComponentOptions lp =
  map (T.unpack . renderComponent) $
  Set.toList $
  Set.filter (\c -> isCTest c || isCBench c) lp.components

taskComponents :: Task -> Set NamedComponent
taskComponents task =
  case task.taskType of
    TTLocalMutable lp -> lp.components -- FIXME probably just want lpWanted
    TTRemotePackage{} -> Set.empty

expectTestFailure :: PackageName -> Maybe Curator -> Bool
expectTestFailure pname =
  maybe False (Set.member pname . (.curatorExpectTestFailure))

expectBenchmarkFailure :: PackageName -> Maybe Curator -> Bool
expectBenchmarkFailure pname =
  maybe False (Set.member pname . (.curatorExpectBenchmarkFailure))

fulfillCuratorBuildExpectations ::
     (HasCallStack, HasTerm env)
  => PackageName
  -> Maybe Curator
  -> Bool
  -> Bool
  -> b
  -> RIO env b
  -> RIO env b
fulfillCuratorBuildExpectations pname mcurator enableTests _ defValue action
  | enableTests && expectTestFailure pname mcurator = do
      eres <- tryAny action
      case eres of
        Right res -> do
          prettyWarnL
            [ style Current (fromPackageName pname) <> ":"
            , flow "unexpected test build success."
            ]
          pure res
        Left _ -> pure defValue
fulfillCuratorBuildExpectations pname mcurator _ enableBench defValue action
  | enableBench && expectBenchmarkFailure pname mcurator = do
      eres <- tryAny action
      case eres of
        Right res -> do
          prettyWarnL
            [ style Current (fromPackageName pname) <> ":"
            , flow "unexpected benchmark build success."
            ]
          pure res
        Left _ -> pure defValue
fulfillCuratorBuildExpectations _ _ _ _ _ action = action
