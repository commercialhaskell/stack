{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Stack.Build.ExecutePackage
Description : Perform a build.
License     : BSD-3-Clause

Perform a build.
-}

module Stack.Build.ExecutePackage
  ( singleBuild
  , singleTest
  , singleBench
  , componentTarget
  , componentEnableTests
  , componentEnableBenchmarks
  , dispatchBuildOpts
    -- * Backpack helpers (exported for testing)
  , findGhcPkgId
  , mkInstantiateWithOpts
  ) where

import           Control.Concurrent.Execute
                   ( ActionContext (..), ActionId (..) )
import           Control.Monad.Extra ( whenJust )
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
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
import           Distribution.ModuleName ( ModuleName )
import qualified Distribution.PackageDescription as C
import           Distribution.System ( OS (..), Platform (..) )
import qualified Distribution.Text as C
import           Distribution.Types.MungedPackageName
                   ( encodeCompatPackageName )
import           Path
                   ( (</>), addExtension, filename, isProperPrefixOf, parent
                   , parseRelDir, parseRelFile, stripProperPrefix
                   )
import           Path.Extra ( toFilePathNoTrailingSep )
import           Path.IO
                   ( copyFile, doesFileExist, ensureDir, ignoringAbsence
                   , removeDirRecur, removeFile
                   )
import           RIO.NonEmpty ( nonEmpty )
import           RIO.Process
                   ( HasProcessContext, byteStringInput, findExecutable
                   , getStderr, getStdout, inherit, modifyEnvVars, proc
                   , setStderr, setStdin, setStdout, showProcessArgDebug
                   , useHandleOpen, waitExitCode, withModifyEnvVars
                   , withProcessWait, withWorkingDir
                   )
import           Stack.Build.ConstructPlan ( shouldSplitComponents )
import           Stack.Build.Cache
                   ( TestStatus (..), deleteCaches, getTestStatus
                   , markExeInstalled, markExeNotInstalled, readPrecompiledCache
                   , setTestStatus, tryGetCabalMod, tryGetConfigCache
                   , tryGetPackageProjectRoot, tryGetSetupConfigMod
                   , writeBuildCache, writeCabalMod, writeConfigCache
                   , writeFlagCache, writePrecompiledCache
                   , writePackageProjectRoot, writeSetupConfigMod
                   )
import           Stack.Build.ExecuteEnv
                   ( ExcludeTHLoading (..), ExecuteEnv (..), KeepOutputOpen (..)
                   , OutputType (..), withSingleContext
                   )
import           Stack.Build.TestSuiteTimeout
                   ( forceKill, prepareForEscalation, terminateGracefully )
import           Stack.Build.Source ( addUnlistedToBuildCache )
import           Stack.Config.ConfigureScript ( ensureConfigureScript )
import           Stack.ConfigureOpts
                   ( configureOptsFromBase, renderConfigureOpts )
import           Stack.Constants
                   ( bindirSuffix, compilerOptionsCabalFlag, testGhcEnvRelFile )
import           Stack.Constants.Config
                   ( distDirFromDir, distRelativeDir, hpcDirFromDir
                   , hpcRelativeDir, setupConfigFromDir
                   )
import           Stack.Coverage ( generateHpcReport, updateTixFile )
import           Stack.GhcPkg ( ghcPkg, ghcPkgPathEnvVar, unregisterGhcPkgIds )
import           Stack.Package
                   ( buildLogPath, buildableExes, buildableSubLibs
                   , hasBuildableMainLibrary, hasIntraPackageDeps
                   )
import           Stack.PackageDump ( conduitDumpPackage, ghcPkgDescribe )
import           Stack.Prelude
import           Stack.Types.Build.Exception
                   ( BuildException (..), BuildPrettyException (..) )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..), configFileRootL )
import           Stack.Types.BuildOpts
                   ( BenchmarkOpts (..), BuildOpts (..), HaddockOpts (..)
                   , TestOpts (..)
                   )
import           Stack.Types.BuildOptsCLI ( BuildOptsCLI (..) )
import           Stack.Types.Cache
                   ( ConfigCache (..), ConfigCacheType (..)
                   , PrecompiledCache (..)
                   )
import qualified Stack.Types.Cache as ConfigCache ( ConfigCache (..) )
import           Stack.Types.CompCollection
                   ( collectionKeyValueList, collectionLookup
                   , foldComponentToAnotherCollection, getBuildableListText
                   )
import           Stack.Types.Compiler
                   ( WhichCompiler (..), whichCompiler, whichCompilerL )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), GhcPkgExe (..), HasCompiler (..)
                   , cpWhich, getGhcPkgExe
                   )
import qualified Stack.Types.Component as Component
import           Stack.Types.ComponentUtils
                   ( StackUnqualCompName, toCabalName, unqualCompToString
                   , unqualCompToText
                   )
import           Stack.Types.Config ( Config (..), HasConfig (..) )
import           Stack.Types.ConfigureOpts
                   ( BaseConfigOpts (..), ConfigureOpts (..) )
import           Stack.Types.Curator ( Curator (..) )
import           Stack.Types.DumpPackage ( DumpPackage (..) )
import           Stack.Types.EnvConfig
                   ( EnvConfig (..), HasEnvConfig (..), actualCompilerVersionL
                   , appropriateGhcColorFlag
                   )
import           Stack.Types.EnvSettings ( EnvSettings (..) )
import           Stack.Types.GhcPkgId
                   ( GhcPkgId, ghcPkgIdString, ghcPkgIdToText )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.Installed
                   ( InstallLocation (..), Installed (..), InstalledMap
                   , InstalledLibraryInfo (..)
                   )
import           Stack.Types.IsMutable ( IsMutable (..) )
import           Stack.Types.NamedComponent
                   ( NamedComponent (..), exeComponents, isCBench, isCTest
                   , renderComponent
                   )
import           Stack.Types.Package
                   ( LocalPackage (..), Package (..), installedPackageToGhcPkgId
                   , runMemoizedWith, simpleInstalledLib
                   , toCabalMungedPackageName
                   )
import           Stack.Types.PackageFile ( PackageWarning (..) )
import           Stack.Types.Plan
                   ( ComponentKey (..), Task (..), TaskConfigOpts (..)
                   , TaskType (..), componentKeyPkgName, taskIsTarget
                   , taskLocation, taskProvides, taskTargetIsMutable
                   , taskTypePackageIdentifier
                   )
import           Stack.Types.Runner ( HasRunner, globalOptsL )
import           Stack.Types.SourceMap ( SourceMap (..) )
import           System.IO.Error ( isDoesNotExistError )
import           System.PosixCompat.Files
                   ( createLink, getFileStatus, modificationTime )
import           System.Random ( randomIO )
import           System.Semaphore ( Semaphore (..), SemaphoreName (..) )

-- | Generate the t'ConfigCache' value.
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
                  -> pure $ installedPackageToGhcPkgId ident installed
          Just installed -> pure $ installedPackageToGhcPkgId ident installed
          _ -> throwM $ PackageIdMissingBug ident
  let cOpts = task.configOpts
  missingMapList <- traverse getMissing $ toList cOpts.missing
  let pcOpts = cOpts.pkgConfigOpts
      missing' = Map.unions missingMapList
      -- Historically the leftermost was missing' for union preference in case of
      -- collision for the return here. But unifying things with configureOpts
      -- where it was the opposite resulted in this. It doesn't seem to make any
      -- difference anyway.
      allDepsMap = Map.union missing' task.present
      configureOpts' = configureOptsFromBase
        cOpts.envConfig
        cOpts.baseConfigOpts
        allDepsMap
        cOpts.isLocalNonExtraDep
        cOpts.isMutable
        pcOpts
      instWithOpts =
        mkInstantiateWithOpts task.backpackInstEntries allDepsMap
      configureOpts = configureOpts'
        { nonPathRelated =
            configureOpts'.nonPathRelated
            ++ map T.unpack extra
            ++ instWithOpts
        }
      deps = Set.fromList $ Map.elems missing' ++ Map.elems task.present
      components = case task.taskType of
        TTLocalMutable lp ->
          Set.map (encodeUtf8 . renderComponent) lp.components
        TTRemotePackage{} -> Set.empty
      cache = ConfigCache
        { configureOpts
        , deps
        , components
        , buildHaddocks = task.buildHaddocks
        , pkgSrc = task.cachePkgSrc
        , pathEnvVar = ee.pathEnvVar
        }
  pure (allDepsMap, cache)

-- | Look up the 'GhcPkgId' for a package by its 'PackageName' in a dependency
-- map keyed by 'PackageIdentifier'. Used to resolve implementing packages for
-- Backpack @--instantiate-with@ flags.
findGhcPkgId ::
     Map PackageIdentifier GhcPkgId
  -> PackageName
  -> Maybe GhcPkgId
findGhcPkgId depsMap pn =
  case [gid | (PackageIdentifier n _, gid) <- Map.toList depsMap, n == pn] of
    (gid:_) -> Just gid
    [] -> Nothing

-- | Generate @--instantiate-with@ configure flags for CInst (Backpack
-- instantiation) tasks. Each entry maps a signature name to an implementing
-- module identified by its package's 'GhcPkgId'.
--
-- Format: @--instantiate-with=SigName=\<unit-id\>:ImplModuleName@
mkInstantiateWithOpts ::
     [(ModuleName, PackageName, ModuleName)]
     -- ^ Backpack instantiation entries: (sigName, implPkgName, implModuleName)
  -> Map PackageIdentifier GhcPkgId
     -- ^ All dependency GhcPkgIds
  -> [String]
mkInstantiateWithOpts entries depsMap =
  [ "--instantiate-with="
    ++ C.display sigName
    ++ "="
    ++ ghcPkgIdString implGhcPkgId
    ++ ":"
    ++ C.display implModuleName
  | (sigName, implPkgName, implModuleName) <- entries
  , Just implGhcPkgId <- [findGhcPkgId depsMap implPkgName]
  ]

-- | Ensure that the configuration for the package matches what is given
ensureConfig ::
     HasEnvConfig env
  => ConfigCache
     -- ^ newConfigCache
  -> Path Abs Dir
     -- ^ package directory
  -> BuildOpts
  -> Maybe Text
     -- ^ CInst hash suffix (Nothing for normal builds)
  -> RIO env ()
     -- ^ announce
  -> (ExcludeTHLoading -> [String] -> RIO env ())
     -- ^ cabal
  -> Path Abs File
     -- ^ Cabal file
  -> Task
  -> RIO env Bool
ensureConfig newConfigCache pkgDir buildOpts mInstSuffix announce cabal cabalFP task = do
  -- CInst (Backpack instantiation) tasks share a source directory with the
  -- indefinite package but use a separate --builddir. They use a separate
  -- config cache entry (ConfigCacheTypeInstantiation) to avoid colliding with
  -- the indefinite build's cache. File-based caches (cabalMod,
  -- projectRoot) are shared since the .cabal file is the same, but
  -- setup-config lives in the inst builddir and must be checked there.
  let configCacheType =
        maybe ConfigCacheTypeConfig ConfigCacheTypeInstantiation mInstSuffix
  newCabalMod <-
    liftIO $ modificationTime <$> getFileStatus (toFilePath cabalFP)
  setupConfigfp <- case mInstSuffix of
    Nothing -> setupConfigFromDir pkgDir
    Just suffix -> do
      dist <- distDirFromDir pkgDir
      instSubDir <- parseRelDir ("inst-" ++ T.unpack suffix)
      setupConfig <- parseRelFile "setup-config"
      pure $ dist </> instSubDir </> setupConfig
  let getNewSetupConfigMod =
        liftIO $ either (const Nothing) (Just . modificationTime) <$>
        tryJust
          (guard . isDoesNotExistError)
          (getFileStatus (toFilePath setupConfigfp))
  newSetupConfigMod <- getNewSetupConfigMod
  newConfigFileRoot <- S8.pack . toFilePath <$> view configFileRootL
  needConfig <-
    if buildOpts.reconfigure
          -- The reason 'taskAnyMissing' is necessary is a bug in Cabal. See:
          -- <https://github.com/haskell/cabal/issues/4728#issuecomment-337937673>.
          -- The problem is that Cabal may end up generating the same package ID
          -- for a dependency, even if the ABI has changed. As a result, without
          -- check, Stack would think that a reconfigure is unnecessary, when in
          -- fact we _do_ need to reconfigure. The details here suck. We really
          -- need proper hashes for package identifiers.
      then pure True
      else do
        -- We can ignore the components field of the Cabal configuration cache,
        -- because it is only used to inform 'construct plan' that we need to
        -- plan to build additional components. These components don't affect
        -- the Cabal configuration for the package.
        let ignoreComponents :: ConfigCache -> ConfigCache
            ignoreComponents cc = cc { ConfigCache.components = Set.empty }
        -- Determine the old and new Cabal configuration for the package
        -- directory, to determine if we need to reconfigure.
        mOldConfigCache <- tryGetConfigCache pkgDir configCacheType

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
          || mOldProjectRoot /= Just newConfigFileRoot

  when task.buildTypeConfig $
    -- When build-type is Configure, we need to have a configure script in the
    -- local directory. If it doesn't exist, build it with autoreconf -i. See:
    -- https://github.com/commercialhaskell/stack/issues/3534
    ensureConfigureScript pkgDir

  when needConfig $ do
    deleteCaches pkgDir configCacheType
    announce
    cp <- view compilerPathsL
    let (GhcPkgExe pkgPath) = cp.pkg
    let programNames =
          case cpWhich cp of
            Ghc ->
              [ ("ghc", toFilePath cp.compiler)
              , ("ghc-pkg", toFilePath pkgPath)
              ]
    exes <- forM programNames $ \(name, file) ->
      findExecutable file <&> \case
        Left _ -> []
        Right x -> pure $ concat ["--with-", name, "=", x]
    let allOpts =
             concat exes
          <> renderConfigureOpts newConfigCache.configureOpts
    -- Configure cabal with arguments determined by
    -- Stack.Types.Build.configureOpts
    cabal KeepTHLoading $ "configure" : allOpts
    -- Only write the cache for local packages. Remote packages are built in a
    -- temporary directory so the cache would never be used anyway.
    case task.taskType of
      TTLocalMutable{} -> writeConfigCache pkgDir configCacheType newConfigCache
      TTRemotePackage{} -> pure ()
    -- File-based caches are shared with the indefinite build (same .cabal
    -- file, same pkgDir). Only write them for normal (non-CInst) tasks to
    -- avoid redundant writes.
    when (isNothing mInstSuffix) $ do
      writeCabalMod pkgDir newCabalMod
      -- This file gets updated one more time by the configure step, so get the
      -- most recent value. We could instead change our logic above to check if
      -- our config mod file is newer than the file above, but this seems
      -- reasonable too.
      getNewSetupConfigMod >>= writeSetupConfigMod pkgDir
      writePackageProjectRoot pkgDir newConfigFileRoot
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

-- | Implements running a package's build, used to implement
-- 'Control.Concurrent.Execute.ATBuild' tasks.
--
-- In particular this does the following:
--
-- * Checks if the package exists in the precompiled cache, and if so, add it to
--   the database instead of performing the build.
--
-- * Runs the configure step if needed (@ensureConfig@)
--
-- * Runs the build step
--
-- * Generates haddocks
--
-- * Registers the library and copies the built executables into the local
--   install directory. Note that this is literally invoking Cabal with @copy@,
--   and not the copying done by @stack install@ - that is handled by
--   'Stack.Build.copyExecutables'.
singleBuild ::
     forall env. (HasEnvConfig env, HasRunner env)
  => ActionContext
  -> ExecuteEnv
  -> Task
  -> InstalledMap
  -> Bool
     -- ^ Is this a final build? (Controls enable-tests/--enable-benchmarks.)
  -> Bool
     -- ^ Is this a merged primary+final build? True only when a non-split
     --   package's primary task is folded together with its final task in a
     --   single Setup invocation, so lib/exe components are also built and
     --   installed. Ignored when isFinalBuild is False.
  -> ComponentKey
     -- ^ The component key identifying which component this build is for.
  -> RIO env ()
singleBuild
    ac
    ee
    task
    installedMap
    isFinalBuild
    isMergedBuild
    ck
  = do
    (allDepsMap, cache) <-
      getConfigCache ee task installedMap enableTests enableBenchmarks
    let bcoSnapInstallRoot = ee.baseConfigOpts.snapInstallRoot
        isCInstTask = case ck of
          ComponentKey _ (CInst _) -> True
          _ -> False
    mprecompiled <- getPrecompiled isCInstTask cache task.taskType bcoSnapInstallRoot
    minstalled <-
      case mprecompiled of
        Just precompiled ->
          copyPreCompiled isCInstTask ee task pkgId precompiled
        Nothing -> do
          curator <- view $ buildConfigL . to (.curator)
          realConfigAndBuild
            ac
            ee
            task
            installedMap
            (enableTests, enableBenchmarks)
            (isFinalBuild, buildingFinals, isMergedBuild)
            cache
            curator
            allDepsMap
            ck
    -- For CInst (Backpack instantiation) tasks, do NOT update the ghcPkgIds
    -- TVar. The consumer's --dependency flag must reference the indefinite
    -- GhcPkgId (stored by the CLib task). Cabal resolves the instantiation
    -- by looking up the package DB using mixin declarations. Writing the
    -- flag cache is also skipped to avoid clobbering the indefinite build's
    -- cache.
    unless isCInstTask $
      whenJust minstalled $ \installed -> do
        writeFlagCache installed cache
        liftIO $ atomically $ modifyTVar ee.ghcPkgIds $ Map.insert pkgId installed
 where
  pkgId = taskProvides task
  buildingFinals = isFinalBuild
  enableTests = buildingFinals
    && componentEnableTests ck (taskComponents task)
  enableBenchmarks = buildingFinals
    && componentEnableBenchmarks ck (taskComponents task)

realConfigAndBuild ::
     forall env a. HasEnvConfig env
  => ActionContext
  -> ExecuteEnv
  -> Task
  -> Map PackageName (a, Installed)
  -> (Bool, Bool)
     -- ^ (enableTests, enableBenchmarks)
  -> (Bool, Bool, Bool)
     -- ^ (isFinalBuild, buildingFinals, isMergedBuild)
  -> ConfigCache
  -> Maybe Curator
  -> Map PackageIdentifier GhcPkgId
  -> ComponentKey
  -> RIO env (Maybe Installed)
realConfigAndBuild
    ac
    ee
    task
    installedMap
    (enableTests, enableBenchmarks)
    (isFinalBuild, buildingFinals, isMergedBuild)
    cache
    mcurator0
    allDepsMap
    ck
  = withSingleContext ac ee task.taskType allDepsMap Nothing mInstSuffix $
      \package cabalFP pkgDir cabal0 announce _outputType -> do
        let cabal = cabal0 CloseOnException
        _neededConfig <-
          ensureConfig
            cache
            pkgDir
            ee.buildOpts
            mInstSuffix
            (announce ("configure" <> display annSuffix))
            cabal
            cabalFP
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
          (True, _) | null ac.downstream -> pure Nothing
          (_, True) | null ac.downstream || installedMapHasThisPkg -> do
            initialBuildSteps cabal announce
            pure Nothing
          _ -> fulfillCuratorBuildExpectations
                 pname
                 mcurator0
                 enableTests
                 enableBenchmarks
                 Nothing
                 (Just <$> realBuild package pkgDir cabal0 announce)
 where
  pkgId = taskProvides task
  PackageIdentifier pname _ = pkgId
  mInstSuffix = case ck of
    ComponentKey _ (CInst hashSuffix) -> Just hashSuffix
    _ -> Nothing
  doHaddock curator =
       task.buildHaddocks
       -- Skip haddock only for pure finals-only builds (split CTest/CBench
       -- or the non-split finalBuild action with no primary to go with).
       -- Merged non-split builds still produce primary components, so run
       -- haddock for them as we would for a plain primary build.
    && not (isFinalBuild && not isMergedBuild)
       -- Special help for the curator tool to avoid haddocks that are known
       -- to fail
    && maybe True (Set.notMember pname . (.skipHaddock)) curator

  annSuffix = buildAnnSuffix ck task enableTests enableBenchmarks
  initialBuildSteps cabal announce = do
    announce ("initial-build-steps" <> display annSuffix)
    cabal KeepTHLoading ["repl", "stack-initial-build-steps"]

  realBuild ::
       Package
    -> Path Abs Dir
    -> (KeepOutputOpen -> ExcludeTHLoading -> [String] -> RIO env ())
    -> (Utf8Builder -> RIO env ())
       -- ^ A plain 'announce' function
    -> RIO env Installed
  realBuild package pkgDir cabal0 announce = do
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
                pure (Just (lp.cabalFP, warnings))
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
          forM_ mlocalWarnings $ \(cabalFP, warnings) ->
            unless (null warnings) $ prettyWarn $
                 flow "The following modules should be added to \
                      \exposed-modules or other-modules in" <+>
                      pretty cabalFP
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
      <> display annSuffix
      <> " with "
      <> display actualCompiler
      )
    config <- view configL
    extraOpts <- extraBuildOptions wc ee.buildOpts ee.semaphore
    let stripTHLoading
          | config.hideTHLoading = ExcludeTHLoading
          | otherwise                  = KeepTHLoading
    let (buildOpts, copyOpts) =
          buildAndCopyOpts task.taskType ck isFinalBuild isMergedBuild
    cabal stripTHLoading ("build" : buildOpts <> extraOpts)
      `catch` \ex -> case ex of
        CabalExitedUnsuccessfully{} ->
          postBuildCheck False >> prettyThrowM ex
        _ -> throwM ex
    postBuildCheck True

    mcurator <- view $ buildConfigL . to (.curator)
    when (doHaddock mcurator) $ do
      let isTaskTargetMutable = taskTargetIsMutable task == Mutable
          isHaddockForHackage =
            ee.buildOpts.haddockForHackage && isTaskTargetMutable
      announce $ if isHaddockForHackage
        then "haddock for Hackage"
        else "haddock"

      -- For GHC 8.4 and later, provide the --quickjump option.
      let quickjump = ["--haddock-option=--quickjump"]

      fulfillHaddockExpectations pname mcurator $ \keep -> do
        let args = concat
              (  ( if isHaddockForHackage
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
                      , [ "--executables" | ee.buildOpts.haddockExecutables ]
                      , [ "--tests" | ee.buildOpts.haddockTests ]
                      , [ "--benchmarks" | ee.buildOpts.haddockBenchmarks ]
                      , [ "--internal" | ee.buildOpts.haddockInternal  ]
                      , quickjump
                      ]
                 )
              <> [ [ "--haddock-option=" <> opt
                   | opt <- ee.buildOpts.haddockOpts.additionalArgs
                   ]
                 ]
              )

        cabal0 keep KeepTHLoading $ "haddock" : args

    let hasLibrary = hasBuildableMainLibrary package
        hasSubLibraries = not $ null package.subLibraries
        hasExecutables = not $ null package.executables
        -- Skip copy/install only for pure finals-only builds: split
        -- CTest/CBench components and non-split finalBuild actions that
        -- follow a clean primary. Every other path — plain primary
        -- builds, split primary components, merged primary+final non-split
        -- builds, and intra-package Backpack CLib/CInst builds — copies
        -- and registers as before.
        shouldCopy =
             not (isFinalBuild && not isMergedBuild)
          && (hasLibrary || hasSubLibraries || hasExecutables)
    when shouldCopy $ withMVar ee.installLock $ \() -> do
      announce "copy/register"
      try (cabal KeepTHLoading $ "copy" : copyOpts) >>= \case
        Left err@CabalExitedUnsuccessfully{} ->
          prettyThrowM $ CabalCopyFailed
            (package.buildType == C.Simple)
            err
        _ -> pure ()
      when (hasLibrary || hasSubLibraries) $ cabal KeepTHLoading ["register"]

    copyDdumpFilesIfNeeded buildingFinals ee.buildOpts.ddumpDir
    installedPkg <-
      fetchAndMarkInstalledPackage ee (taskLocation task) package pkgId
    postProcessRemotePackage
      task.taskType
      ac
      cache
      ee
      installedPkg
      package
      pkgId
      pkgDir
    pure installedPkg

-- | Action in the case that the task relates to a remote package.
postProcessRemotePackage ::
     (HasEnvConfig env)
  => TaskType
  -> ActionContext
  -> ConfigCache
  -> ExecuteEnv
  -> Installed
  -> Package
  -> PackageIdentifier
  -> Path b Dir
  -> RIO env ()
postProcessRemotePackage
    taskType
    ac
    cache
    ee
    installedPackage
    package
    pkgId
    pkgDir
  = case taskType of
      TTRemotePackage isMutable _ loc -> do
        when (isMutable == Immutable) $ writePrecompiledCache
          ee.baseConfigOpts
          loc
          cache.configureOpts
          cache.buildHaddocks
          installedPackage
          (buildableExes package)
        -- For packages from a package index, pkgDir is in the tmp directory. We
        -- eagerly delete it if no other tasks require it, to reduce space usage
        -- in tmp (#3018).
        let remaining =
              Set.filter
                (\(ActionId ck _) -> componentKeyPkgName ck == pkgName pkgId)
                ac.remaining
        when (null remaining) $ removeDirRecur pkgDir
      _ -> pure ()

-- | Once all the Cabal-related tasks have run for a package, we should be able
-- to gather the information needed to create an 'Installed' package value. For
-- now, either there's a main library (in which case we consider the 'GhcPkgId'
-- values of the package's libraries) or we just consider it's an executable
-- (and mark all the executables as installed, if any).
--
-- Note that this also modifies the installedDumpPkgsTVar which is used for
-- generating Haddocks.
--
fetchAndMarkInstalledPackage ::
     (HasEnvConfig env, HasTerm env)
  => ExecuteEnv
  -> InstallLocation
  -> Package
  -> PackageIdentifier
  -> RIO env Installed
fetchAndMarkInstalledPackage ee taskInstallLocation package pkgId = do
  let ghcPkgIdLoader = fetchGhcPkgIdForLib ee taskInstallLocation package.name
  -- Only pure the sub-libraries to cache them if we also cache the main
  -- library (that is, if it exists)
  if hasBuildableMainLibrary package
    then do
      let foldSubLibToMap subLib mapInMonad = do
            maybeGhcpkgId <- ghcPkgIdLoader (Just subLib.name)
            mapInMonad <&> case maybeGhcpkgId of
              Just v -> Map.insert subLib.name v
              _ -> id
      subLibsPkgIds <- foldComponentToAnotherCollection
        package.subLibraries
        foldSubLibToMap
        mempty
      ghcPkgIdLoader Nothing >>= \case
        Nothing -> throwM $ Couldn'tFindPkgId package.name
        Just ghcPkgId -> pure $ simpleInstalledLib pkgId ghcPkgId subLibsPkgIds
    else do
      markExeInstalled taskInstallLocation pkgId -- TODO unify somehow
                                                  -- with writeFlagCache?
      pure $ Executable pkgId

fetchGhcPkgIdForLib ::
     (HasTerm env, HasEnvConfig env)
  => ExecuteEnv
  -> InstallLocation
  -> PackageName
  -> Maybe Component.StackUnqualCompName
  -> RIO env (Maybe GhcPkgId)
fetchGhcPkgIdForLib ee installLocation pkgName libName = do
  let baseConfigOpts = ee.baseConfigOpts
      (installedPkgDb, installedDumpPkgsTVar) =
        case installLocation of
          Snap ->
            ( baseConfigOpts.snapDB
            , ee.snapshotDumpPkgs )
          Local ->
            ( baseConfigOpts.localDB
            , ee.localDumpPkgs )
  let commonLoader = loadInstalledPkg [installedPkgDb] installedDumpPkgsTVar
  case libName of
    Nothing -> commonLoader pkgName
    Just v -> do
      let mungedName = encodeCompatPackageName $ toCabalMungedPackageName pkgName v
      commonLoader mungedName

-- | Copy ddump-* files, if we are building finals and a non-empty ddump-dir
-- has been specified.
copyDdumpFilesIfNeeded :: HasEnvConfig env => Bool -> Maybe Text -> RIO env ()
copyDdumpFilesIfNeeded buildingFinals mDdumpPath = when buildingFinals $
  whenJust mDdumpPath $ \ddumpPath -> unless (T.null ddumpPath) $ do
    distDir <- distRelativeDir
    ddumpRelDir <- parseRelDir $ T.unpack ddumpPath
    prettyDebugL
      [ "ddump-dir:"
      , pretty ddumpRelDir
      ]
    prettyDebugL
      [ "dist-dir:"
      , pretty distDir
      ]
    runConduitRes
      $ CF.sourceDirectoryDeep False (toFilePath distDir)
      .| CL.filter (L.isInfixOf ".dump-")
      .| CL.mapM_ (\src -> liftIO $ do
          parentDir <- parent <$> parseRelDir src
          destBaseDir <-
            (ddumpRelDir </>) <$> stripProperPrefix distDir parentDir
          -- exclude .stack-work dir
          unless (".stack-work" `L.isInfixOf` toFilePath destBaseDir) $ do
            ensureDir destBaseDir
            src' <- parseRelFile src
            copyFile src' (destBaseDir </> filename src'))

getPrecompiled ::
     HasEnvConfig env
  => Bool
     -- ^ Is this a CInst (Backpack instantiation) task? CInst tasks are
     -- always created by addInstantiationTasks even when the instantiated
     -- package is already registered. Skip the self-reference check so
     -- the precompiled cache can short-circuit the build.
  -> ConfigCache
  -> TaskType
  -> Path Abs Dir
  -> RIO env (Maybe (PrecompiledCache Abs))
getPrecompiled isCInst cache taskType bcoSnapInstallRoot =
  case taskType of
    TTRemotePackage Immutable _ loc ->
      readPrecompiledCache loc cache.configureOpts cache.buildHaddocks >>= \case
        Nothing -> pure Nothing
        -- Only pay attention to precompiled caches that refer to packages
        -- within the snapshot. For CInst tasks, skip this check: CInst
        -- tasks are always created regardless of whether the instantiated
        -- package is already installed, and re-registering with --force is
        -- harmless.
        Just pc
          | not isCInst
          , maybe False
              (bcoSnapInstallRoot `isProperPrefixOf`)
              pc.library -> pure Nothing
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
                  allM doesFileExist $ maybe id (:) pc.library pc.exes
          pure $ if b then Just pc else Nothing
    _ -> pure Nothing

copyPreCompiled ::
     ( HasLogFunc env
     , HasCompiler env
     , HasTerm env
     , HasProcessContext env
     , HasEnvConfig env
     )
  => Bool
     -- ^ Is this a CInst (Backpack instantiation) task? If so, skip
     -- unregistration — the indefinite package and other instantiations
     -- must remain in the DB.
  -> ExecuteEnv
  -> Task
  -> PackageIdentifier
  -> PrecompiledCache b0
  -> RIO env (Maybe Installed)
copyPreCompiled isCInst ee task pkgId (PrecompiledCache mlib subLibs exes) = do
  let PackageIdentifier pname pversion = pkgId
  announceTask ee task.taskType "using precompiled package"

  -- We need to copy .conf files for the main library and all sub-libraries
  -- which exist in the cache, from their old snapshot to the new one.
  -- However, we must unregister any such library in the new snapshot, in case
  -- it was built with different flags. For CInst tasks, we skip unregistration
  -- because the indefinite package and other instantiations share the same
  -- package name and must remain in the DB. The --force flag on register
  -- handles any conflicts.
  let
    subLibNames = Set.toList $ buildableSubLibs $ case task.taskType of
      TTLocalMutable lp -> lp.package
      TTRemotePackage _ p _ -> p
    toMungedPackageId :: StackUnqualCompName -> MungedPackageId
    toMungedPackageId subLib =
      let subLibName = LSubLibName $ toCabalName subLib
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
      let pkgDb = ee.baseConfigOpts.snapDB
      ghcPkgExe <- getGhcPkgExe
      -- First unregister, silently, everything that needs to be unregistered.
      -- Skip for CInst tasks to preserve the indefinite and other instantiated
      -- entries.
      unless isCInst $
        whenJust (nonEmpty allToUnregister) $ \allToUnregister' -> do
          logLevel <- view $ globalOptsL . to (.logLevel)
          let isDebug = logLevel == LevelDebug
          catchAny
            (unregisterGhcPkgIds isDebug ghcPkgExe pkgDb allToUnregister')
            (const (pure ()))
      -- There appears to be a bug in the ghc-pkg executable such that, on
      -- Windows only, it cannot register a package into a package database that
      -- is also listed in the GHC_PACKAGE_PATH environment variable. See:
      -- https://gitlab.haskell.org/ghc/ghc/-/issues/25962. We work around that
      -- by removing GHC_PACKAGE_PATH from the environment for the register
      -- step.
      wc <- view $ envConfigL . to (.sourceMap.compiler) . to whichCompiler
      withModifyEnvVars (Map.delete $ ghcPkgPathEnvVar wc) $
        forM_ allToRegister $ \libpath -> do
          let args = ["register", "--force", toFilePath libpath]
          ghcPkg ghcPkgExe [pkgDb] args >>= \case
            Left e -> prettyWarn $
              "[S-4541]"
              <> line
              <> fillSep
                   [ flow "While registering"
                   , pretty libpath
                   , "in"
                   , pretty pkgDb <> ","
                   , flow "Stack encountered the following error:"
                   ]
              <> blankLine
              <> string (displayException e)
            Right _ -> pure ()
  liftIO $ forM_ exes $ \exe -> do
    ensureDir bindir
    let dst = bindir </> filename exe
    createLink (toFilePath exe) (toFilePath dst) `catchIO` \_ -> copyFile exe dst
  case (mlib, exes) of
    (Nothing, _:_) -> markExeInstalled (taskLocation task) pkgId
    _ -> pure ()

  -- Find the package in the database
  let pkgDbs = [ee.baseConfigOpts.snapDB]

  case mlib of
    Nothing -> pure $ Just $ Executable pkgId
    Just _ -> do
      mpkgid <- loadInstalledPkg pkgDbs ee.snapshotDumpPkgs pname

      pure $ Just $
        case mpkgid of
          Nothing -> assert False $ Executable pkgId
          Just pkgid -> simpleInstalledLib pkgId pkgid mempty
 where
  bindir = ee.baseConfigOpts.snapInstallRoot </> bindirSuffix

loadInstalledPkg ::
     (HasCompiler env, HasProcessContext env, HasTerm env)
  => [Path Abs Dir]
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
    -- For Backpack packages, ghc-pkg may return multiple entries for the same
    -- component name: an indefinite (signature-only) package and one or more
    -- instantiated packages. Pick the last instantiated one (has '+' in its
    -- ID) since that contains the compiled code. Multiple instantiations
    -- arise when different consumers fill the same signature with different
    -- implementations. Register all entries in the dump.
    _ -> case filter isInstantiated dps of
      [] -> throwM $ MultipleResultsBug name dps
      instantiated -> do
        forM_ dps $ \d ->
          liftIO $ atomically $ modifyTVar' tvar (Map.insert d.ghcPkgId d)
        -- Pick the last instantiated entry. With multiple instantiations, the
        -- order doesn't matter — the caller (CInst) discards the result anyway.
        let dp = L.last instantiated
        pure $ Just dp.ghcPkgId
 where
  isInstantiated dp =
    '+' `elem` ghcPkgIdString dp.ghcPkgId

fulfillHaddockExpectations ::
     (MonadUnliftIO m, HasTerm env, MonadReader env m)
  => PackageName
  -> Maybe Curator
  -> (KeepOutputOpen -> m ())
  -> m ()
fulfillHaddockExpectations pname mcurator action
  | expectHaddockFailure mcurator =
      tryAny (action KeepOpen) >>= \case
        Right () -> prettyWarnL
          [ style Current (fromPackageName pname) <> ":"
          , flow "unexpected Haddock success."
          ]
        Left _ -> pure ()
 where
  expectHaddockFailure = maybe False (Set.member pname . (.expectHaddockFailure))
fulfillHaddockExpectations _ _ action = action CloseOnException

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
      lp.cabalFP
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
singleTest ::
     HasEnvConfig env
  => TestOpts
  -> [StackUnqualCompName]
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
  withSingleContext ac ee task.taskType allDepsMap (Just "test") Nothing $
    \package _cabalfp pkgDir _cabal announce outputType -> do
      config <- view configL
      let needHpc = topts.coverage
      toRun <-
        if topts.runTests
          then if topts.rerunTests
            then pure True
            else
              getTestStatus pkgDir >>= \case
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
          else prettyThrowM $ ActionNotFilteredBug "singleTest"
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
          let stestName = unqualCompToString testName
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
                               Just (Library _ libInfo) -> [libInfo.ghcPkgId]
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
                toFilePathNoTrailingSep ee.baseConfigOpts.snapDB
              localDBPath =
                toFilePathNoTrailingSep ee.baseConfigOpts.localDB
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
                         <> display (ghcPkgIdToText ghcId)
                         <> "\n"
                     )
                     (pkgGhcIdList ++ thGhcId:Map.elems allDepsMap)
          writeFileUtf8Builder fp ghcEnv
          menv <- liftIO $
            setEnv fp =<< config.processContextSettings EnvSettings
              { includeLocals = taskLocation task == Local
              , includeGhcPackagePath = True
              , stackExe = True
              , localeUtf8 = False
              , keepGhcRts = False
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

                let args = topts.additionalArgs
                    argsDisplay = case args of
                      [] -> ""
                      _ ->    ", args: "
                           <> T.intercalate " " (map showProcessArgDebug args)
                announce $
                     "test (suite: "
                  <> display (unqualCompToText testName)
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
                    runOutput p =
                      case (getStdout p, getStderr p) of
                        (Nothing, Nothing) -> pure ()
                        (Just x, Just y) -> concurrently_ x y
                        (x, y) -> assert False $
                          concurrently_
                            (fromMaybe (pure ()) x)
                            (fromMaybe (pure ()) y)
                    timeoutWithGrace p maxSecs graceSecs = do
                      mExit <- timeout (maxSecs * 1000000) (waitExitCode p)
                      case mExit of
                        Just ec -> pure (Just ec)
                        Nothing -> do
                          terminateGracefully p
                          mGraceExit <- timeout (graceSecs * 1000000)
                            (waitExitCode p)
                          case mGraceExit of
                            Just _ -> pure Nothing
                            Nothing -> do
                              forceKill p
                              void $ waitExitCode p
                              pure Nothing
                    runWithTimeout pc
                      | Just maxSecs <- topts.maximumTimeSeconds, maxSecs > 0
                      , Just graceSecs <- topts.timeoutGraceSeconds
                      , graceSecs > 0 =
                          withProcessWait (prepareForEscalation pc) $ \p -> do
                            (_, mec') <- concurrently
                              (runOutput p)
                              (timeoutWithGrace p maxSecs graceSecs)
                            pure mec'
                      | Just maxSecs <- topts.maximumTimeSeconds, maxSecs > 0 =
                          timeout (maxSecs * 1000000) $
                            withProcessWait pc $ \p -> do
                              runOutput p
                              waitExitCode p
                      | otherwise =
                          Just <$> withProcessWait pc (\p -> runOutput p *> waitExitCode p)

                mec <- withWorkingDir (toFilePath pkgDir) $
                  proc (toFilePath exePath) args $ \pc0 -> do
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
                                 , toCabalName testName
                                 )
                        else do
                          isTerminal <- view $ globalOptsL . to (.terminal)
                          if topts.allowStdin && isTerminal
                            then pure id
                            else pure $ setStdin $ byteStringInput mempty
                    let pc = changeStdin
                           $ setStdout output
                           $ setStderr output
                             pc0
                    runWithTimeout pc
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
                        <> display (unqualCompToText testName)
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
                  prettyError $
                    pretty $ TestSuiteExeMissing
                      (package.buildType == C.Simple)
                      exeName
                      package.name
                      testName
                pure emptyResult

        when needHpc $ do
          let testsToRun' = map f testsToRun
              f tName =
                case (.interface) <$> mComponent of
                  Just C.TestSuiteLibV09{} -> unqualCompToText tName <> "Stub"
                  _ -> unqualCompToText tName
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
singleBench ::
     HasEnvConfig env
  => BenchmarkOpts
  -> [StackUnqualCompName]
  -> ActionContext
  -> ExecuteEnv
  -> Task
  -> InstalledMap
  -> RIO env ()
singleBench beopts benchesToRun ac ee task installedMap = do
  (allDepsMap, _cache) <- getConfigCache ee task installedMap False True
  withSingleContext ac ee task.taskType allDepsMap (Just "bench") Nothing $
    \_package _cabalfp _pkgDir cabal announce _outputType -> do
      let args = map unqualCompToString benchesToRun <> maybe []
                       ((:[]) . ("--benchmark-options=" <>))
                       beopts.additionalArgs
      toRun <-
        if beopts.runBenchmarks
          then pure True
          else prettyThrowM $ ActionNotFilteredBug "singleBench"
      when toRun $ do
        announce "benchmarks"
        cabal CloseOnException KeepTHLoading ("bench" : args)

-- Do not pass `-hpcdir` as GHC option if the coverage is not enabled.
-- This helps running stack-compiled programs with dynamic interpreters like
-- `hint`. Cfr: https://github.com/commercialhaskell/stack/issues/997
extraBuildOptions ::
     (HasEnvConfig env, HasRunner env)
  => WhichCompiler
  -> BuildOpts
  -> Maybe Semaphore
  -> RIO env [String]
extraBuildOptions wc bopts semaphore = do
  colorOpt <- appropriateGhcColorFlag
  let optsFlag = compilerOptionsCabalFlag wc
      semaphoreFlag = maybe
        []
        (("--semaphore":) . L.singleton . getSemaphoreName . semaphoreName)
        semaphore
      baseOpts = maybe "" (" " ++) colorOpt
  if bopts.testOpts.coverage
    then do
      hpcIndexDir <- toFilePathNoTrailingSep <$> hpcRelativeDir
      pure $ semaphoreFlag ++ [optsFlag, "-hpcdir " ++ hpcIndexDir ++ baseOpts]
    else
      pure $ semaphoreFlag ++ [optsFlag, baseOpts]

-- | Compute the announce suffix for build/configure log messages. For CInst
-- tasks, shows the instantiation details (e.g. @inst:941095d7: Str = impl-pkg@).
-- For other per-component builds, shows the component type. For final builds,
-- shows test/bench.
buildAnnSuffix ::
     ComponentKey
  -> Task
  -> Bool -- ^ enableTests
  -> Bool -- ^ enableBenchmarks
  -> Text
buildAnnSuffix ck task enableTests enableBenchmarks = case ck of
  ComponentKey _ (CInst hash) ->
    " (inst:" <> T.take 8 hash <> instDetail <> ")"
  ComponentKey _ comp
    | not (isCLib comp) -> " (" <> renderComponent comp <> ")"
  _ | result /= "" -> " (" <> result <> ")"
  _ -> ""
 where
  instDetail
    | null task.backpackInstEntries = ""
    | otherwise = ": " <> T.intercalate ", "
        [ T.pack (C.display sig)
          <> " = "
          <> T.pack (packageNameString impl)
          <> if sig == implMod then "" else ":" <> T.pack (C.display implMod)
        | (sig, impl, implMod) <- task.backpackInstEntries
        ]
  result = T.intercalate " + " $
    ["test" | enableTests] ++ ["bench" | enableBenchmarks]
  isCLib CLib = True
  isCLib _ = False

-- | Compute the Cabal build and copy target options for a component. For
-- packages with intra-package deps (e.g. Backpack), omits targets so Cabal
-- handles ordering. For per-component builds, passes a single target. For
-- remote packages, no targets are needed.
buildAndCopyOpts ::
     TaskType
  -> ComponentKey
  -> Bool -- ^ isFinalBuild
  -> Bool -- ^ isMergedBuild (primary+final folded together)
  -> ([String], [String])
     -- ^ (buildOpts, copyOpts)
buildAndCopyOpts taskType ck isFinalBuild isMergedBuild = case taskType of
  TTLocalMutable lp
    | hasIntraPackageDeps lp.package -> ([], [])
    | shouldSplitComponents lp.package
    , ComponentKey pn comp <- ck ->
        dispatchBuildOpts
          (Just (componentTarget pn comp))
          isFinalBuild
          isMergedBuild
          []
          []
    | otherwise ->
        dispatchBuildOpts
          Nothing
          isFinalBuild
          isMergedBuild
          (primaryComponentOptions lp)
          (finalComponentOptions lp)
  TTRemotePackage{} -> ([], [])

-- | Pure dispatcher for 'buildAndCopyOpts'. Given the build mode and the
-- pre-computed Cabal target lists, decide which targets to pass to
-- @setup build@ and which to @setup copy@. Exported for unit testing —
-- 'buildAndCopyOpts' is the production caller.
--
-- Mode summary:
--
-- * @Just target@ + @isFinalBuild=False@ (split primary): build and copy
--   the single target.
-- * @Just target@ + @isFinalBuild=True@ (split final): build the single
--   target, skip copy.
-- * @Nothing@ + @isFinalBuild=False@ (non-split primary-only): build and
--   copy all primary components.
-- * @Nothing@ + @isFinalBuild=True@ + @isMergedBuild=True@ (non-split
--   primary+final folded into one Setup invocation): build primary and
--   final components together, copy only primary.
-- * @Nothing@ + @isFinalBuild=True@ + @isMergedBuild=False@ (non-split
--   finals-only, primary was clean): build final components, skip copy.
dispatchBuildOpts ::
     Maybe String
     -- ^ Split-component target (rendered Cabal target string) when
     --   'shouldSplitComponents' holds; 'Nothing' for non-split packages.
  -> Bool     -- ^ isFinalBuild
  -> Bool     -- ^ isMergedBuild
  -> [String] -- ^ primary component options (ignored on the split path)
  -> [String] -- ^ final component options (ignored on the split path)
  -> ([String], [String])
     -- ^ (buildOpts, copyOpts)
dispatchBuildOpts (Just target) isFinalBuild _ _ _ =
  if isFinalBuild then ([target], []) else ([target], [target])
dispatchBuildOpts Nothing True True primary finals =
  (primary ++ finals, primary)
dispatchBuildOpts Nothing True False _ finals =
  (finals, [])
dispatchBuildOpts Nothing False _ primary _ =
  (primary, primary)

-- | Render a 'NamedComponent' as a Cabal build target string. This uses
-- Cabal's target syntax (e.g. @lib:pkg-name@, @exe:my-exe@, @test:my-test@).
componentTarget :: PackageName -> NamedComponent -> String
componentTarget pn CLib = "lib:" ++ packageNameString pn
componentTarget _ (CSubLib x) = "lib:" ++ unqualCompToString x
componentTarget _ (CFlib x) = "flib:" ++ unqualCompToString x
componentTarget _ (CExe x) = "exe:" ++ unqualCompToString x
componentTarget _ (CTest x) = "test:" ++ unqualCompToString x
componentTarget _ (CBench x) = "bench:" ++ unqualCompToString x
componentTarget _ (CInst _) = ""

-- | Should tests be enabled (Cabal @--enable-tests@) for a given
-- 'ComponentKey'? For per-component CTest keys, always True. For non-split
-- CLib keys, checks the full component set. For all other keys, False.
componentEnableTests :: ComponentKey -> Set NamedComponent -> Bool
componentEnableTests ck comps = case ck of
  ComponentKey _ (CTest _) -> True
  ComponentKey _ CLib -> any isCTest comps
  _ -> False

-- | Should benchmarks be enabled (Cabal @--enable-benchmarks@) for a given
-- 'ComponentKey'? For per-component CBench keys, always True. For non-split
-- CLib keys, checks the full component set. For all other keys, False.
componentEnableBenchmarks :: ComponentKey -> Set NamedComponent -> Bool
componentEnableBenchmarks ck comps = case ck of
  ComponentKey _ (CBench _) -> True
  ComponentKey _ CLib -> any isCBench comps
  _ -> False

-- Library, sub-library, foreign library and executable build components.
primaryComponentOptions :: LocalPackage -> [String]
primaryComponentOptions lp =
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
  ++ Set.toList
       ( Set.mapMonotonic
           (\s -> "exe:" ++ unqualCompToString s)
           (exesToBuild lp)
       )
 where
  package = lp.package

-- | Either build all executables or, if the user specifies requested
-- components, just build them.
exesToBuild :: LocalPackage -> Set StackUnqualCompName
exesToBuild lp = if lp.wanted
  then exeComponents lp.components
  else buildableExes lp.package

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
  maybe False (Set.member pname . (.expectTestFailure))

expectBenchmarkFailure :: PackageName -> Maybe Curator -> Bool
expectBenchmarkFailure pname =
  maybe False (Set.member pname . (.expectBenchmarkFailure))

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
  | enableTests && expectTestFailure pname mcurator =
      tryAny action >>= \case
        Right res -> do
          prettyWarnL
            [ style Current (fromPackageName pname) <> ":"
            , flow "unexpected test build success."
            ]
          pure res
        Left _ -> pure defValue
fulfillCuratorBuildExpectations pname mcurator _ enableBench defValue action
  | enableBench && expectBenchmarkFailure pname mcurator =
      tryAny action >>= \case
        Right res -> do
          prettyWarnL
            [ style Current (fromPackageName pname) <> ":"
            , flow "unexpected benchmark build success."
            ]
          pure res
        Left _ -> pure defValue
fulfillCuratorBuildExpectations _ _ _ _ _ action = action
