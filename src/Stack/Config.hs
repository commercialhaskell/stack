{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

-- | The general Stack configuration that starts everything off. This should
-- be smart to falback if there is no stack.yaml, instead relying on
-- whatever files are available.
--
-- If there is no stack.yaml, and there is a cabal.config, we
-- read in those constraints, and if there's a cabal.sandbox.config,
-- we read any constraints from there and also find the package
-- database from there, etc. And if there's nothing, we should
-- probably default to behaving like cabal, possibly with spitting out
-- a warning that "you should run `stk init` to make things better".
module Stack.Config
  (MiniConfig
  ,loadConfig
  ,loadConfigMaybeProject
  ,loadMiniConfig
  ,packagesParser
  ,getLocalPackages
  ,resolvePackageEntry
  ,getImplicitGlobalProjectDir
  ,getStackYaml
  ,getSnapshots
  ,makeConcreteResolver
  ,checkOwnership
  ,getInContainer
  ,getInNixShell
  ,defaultConfigYaml
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import           Control.Applicative
import           Control.Arrow ((***))
import           Control.Exception (assert)
import           Control.Monad (liftM, unless, when, filterM)
import           Control.Monad.Catch (MonadThrow, MonadCatch, catchAll, throwM, catch)
import           Control.Monad.Extra (firstJustM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger hiding (Loc)
import           Control.Monad.Reader (ask, runReaderT)
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.Aeson.Extended
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as L
import           Data.Foldable (forM_)
import           Data.IORef
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid.Extra
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Yaml as Yaml
import           Distribution.System (OS (..), Platform (..), buildPlatform)
import qualified Distribution.Text
import           Distribution.Version (simplifyVersionRange)
import           GHC.Conc (getNumProcessors)
import           Lens.Micro (lens)
import           Network.HTTP.Client (parseUrlThrow)
import           Network.HTTP.Download (download)
import           Network.HTTP.Simple (httpJSON, getResponseBody)
import           Options.Applicative (Parser, strOption, long, help)
import           Path
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.Find (findInParents)
import           Path.IO
import qualified Paths_stack as Meta
import           Stack.BuildPlan
import           Stack.Config.Build
import           Stack.Config.Docker
import           Stack.Config.Nix
import           Stack.Config.Urls
import           Stack.Constants
import qualified Stack.Image as Image
import           Stack.Types.BuildPlan
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.Docker
import           Stack.Types.Internal
import           Stack.Types.Nix
import           Stack.Types.Resolver
import           Stack.Types.StackT
import           Stack.Types.Urls
import           Stack.Types.Version
import           System.Environment
import           System.IO
import           System.PosixCompat.Files (fileOwner, getFileStatus)
import           System.PosixCompat.User (getEffectiveUserID)
import           System.Process.Read
import           System.Process.Run

-- | If deprecated path exists, use it and print a warning.
-- Otherwise, return the new path.
tryDeprecatedPath
    :: (MonadIO m, MonadLogger m)
    => Maybe T.Text -- ^ Description of file for warning (if Nothing, no deprecation warning is displayed)
    -> (Path Abs a -> m Bool) -- ^ Test for existence
    -> Path Abs a -- ^ New path
    -> Path Abs a -- ^ Deprecated path
    -> m (Path Abs a, Bool) -- ^ (Path to use, whether it already exists)
tryDeprecatedPath mWarningDesc exists new old = do
    newExists <- exists new
    if newExists
        then return (new, True)
        else do
            oldExists <- exists old
            if oldExists
                then do
                    case mWarningDesc of
                        Nothing -> return ()
                        Just desc ->
                            $logWarn $ T.concat
                                [ "Warning: Location of ", desc, " at '"
                                , T.pack (toFilePath old)
                                , "' is deprecated; rename it to '"
                                , T.pack (toFilePath new)
                                , "' instead" ]
                    return (old, True)
                else return (new, False)

-- | Get the location of the implicit global project directory.
-- If the directory already exists at the deprecated location, its location is returned.
-- Otherwise, the new location is returned.
getImplicitGlobalProjectDir
    :: (MonadIO m, MonadLogger m)
    => Config -> m (Path Abs Dir)
getImplicitGlobalProjectDir config =
    --TEST no warning printed
    liftM fst $ tryDeprecatedPath
        Nothing
        doesDirExist
        (implicitGlobalProjectDir stackRoot)
        (implicitGlobalProjectDirDeprecated stackRoot)
  where
    stackRoot = configStackRoot config

-- | This is slightly more expensive than @'asks' ('bcStackYaml' '.' 'getBuildConfig')@
-- and should only be used when no 'BuildConfig' is at hand.
getStackYaml
    :: (StackMiniM env m, HasConfig env)
    => m (Path Abs File)
getStackYaml = do
    config <- view configL
    case configMaybeProject config of
        Just (_project, stackYaml) -> return stackYaml
        Nothing -> liftM (</> stackDotYaml) (getImplicitGlobalProjectDir config)

-- | Download the 'Snapshots' value from stackage.org.
getSnapshots
    :: (StackMiniM env m, HasConfig env)
    => m Snapshots
getSnapshots = do
    latestUrlText <- askLatestSnapshotUrl
    latestUrl <- parseUrlThrow (T.unpack latestUrlText)
    $logDebug $ "Downloading snapshot versions file from " <> latestUrlText
    result <- httpJSON latestUrl
    $logDebug $ "Done downloading and parsing snapshot versions file"
    return $ getResponseBody result

-- | Turn an 'AbstractResolver' into a 'Resolver'.
makeConcreteResolver
    :: (StackMiniM env m, HasConfig env)
    => AbstractResolver
    -> m Resolver
makeConcreteResolver (ARResolver r) = return r
makeConcreteResolver ar = do
    snapshots <- getSnapshots
    r <-
        case ar of
            ARResolver r -> assert False $ return r
            ARGlobal -> do
                config <- view configL
                implicitGlobalDir <- getImplicitGlobalProjectDir config
                let fp = implicitGlobalDir </> stackDotYaml
                WithJSONWarnings (ProjectAndConfigMonoid project _) _warnings <-
                    liftIO (Yaml.decodeFileEither $ toFilePath fp)
                    >>= either throwM return
                return $ projectResolver project
            ARLatestNightly -> return $ ResolverSnapshot $ Nightly $ snapshotsNightly snapshots
            ARLatestLTSMajor x ->
                case IntMap.lookup x $ snapshotsLts snapshots of
                    Nothing -> error $ "No LTS release found with major version " ++ show x
                    Just y -> return $ ResolverSnapshot $ LTS x y
            ARLatestLTS
                | IntMap.null $ snapshotsLts snapshots -> error "No LTS releases found"
                | otherwise ->
                    let (x, y) = IntMap.findMax $ snapshotsLts snapshots
                     in return $ ResolverSnapshot $ LTS x y
    $logInfo $ "Selected resolver: " <> resolverName r
    return r

-- | Get the latest snapshot resolver available.
getLatestResolver :: (StackMiniM env m, HasConfig env) => m Resolver
getLatestResolver = do
    snapshots <- getSnapshots
    let mlts = do
            (x,y) <- listToMaybe (reverse (IntMap.toList (snapshotsLts snapshots)))
            return (LTS x y)
        snap = fromMaybe (Nightly (snapshotsNightly snapshots)) mlts
    return (ResolverSnapshot snap)

-- Interprets ConfigMonoid options.
configFromConfigMonoid
    :: (MonadLogger m, MonadIO m, MonadCatch m)
    => Path Abs Dir -- ^ stack root, e.g. ~/.stack
    -> Path Abs File -- ^ user config file path, e.g. ~/.stack/config.yaml
    -> Maybe AbstractResolver
    -> Maybe (Project, Path Abs File)
    -> ConfigMonoid
    -> m Config
configFromConfigMonoid configStackRoot configUserConfigPath mresolver mproject ConfigMonoid{..} = do
     let configWorkDir = fromFirst $(mkRelDir ".stack-work") configMonoidWorkDir
     -- This code is to handle the deprecation of latest-snapshot-url
     configUrls <- case (getFirst configMonoidLatestSnapshotUrl, getFirst (urlsMonoidLatestSnapshot configMonoidUrls)) of
         (Just url, Nothing) -> do
             $logWarn "The latest-snapshot-url field is deprecated in favor of 'urls' configuration"
             return (urlsFromMonoid configMonoidUrls) { urlsLatestSnapshot = url }
         _ -> return (urlsFromMonoid configMonoidUrls)
     let configConnectionCount = fromFirst 8 configMonoidConnectionCount
         configHideTHLoading = fromFirst True configMonoidHideTHLoading
         configPackageIndices = fromFirst
            [PackageIndex
                { indexName = IndexName "Hackage"
                , indexLocation = ILGitHttp
                        "https://github.com/commercialhaskell/all-cabal-hashes.git"
                        "https://s3.amazonaws.com/hackage.fpcomplete.com/01-index.tar.gz"
                , indexDownloadPrefix = "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"
                , indexGpgVerify = False
                , indexRequireHashes = False
                }]
            configMonoidPackageIndices

         configGHCVariant0 = getFirst configMonoidGHCVariant
         configGHCBuild = getFirst configMonoidGHCBuild
         configInstallGHC = fromFirst False configMonoidInstallGHC
         configSkipGHCCheck = fromFirst False configMonoidSkipGHCCheck
         configSkipMsys = fromFirst False configMonoidSkipMsys

         configExtraIncludeDirs = configMonoidExtraIncludeDirs
         configExtraLibDirs = configMonoidExtraLibDirs
         configOverrideGccPath = getFirst configMonoidOverrideGccPath

         -- Only place in the codebase where platform is hard-coded. In theory
         -- in the future, allow it to be configured.
         (Platform defArch defOS) = buildPlatform
         arch = fromMaybe defArch
              $ getFirst configMonoidArch >>= Distribution.Text.simpleParse
         os = defOS
         configPlatform = Platform arch os

         configRequireStackVersion = simplifyVersionRange (getIntersectingVersionRange configMonoidRequireStackVersion)

         configImage = Image.imgOptsFromMonoid configMonoidImageOpts

         configCompilerCheck = fromFirst MatchMinor configMonoidCompilerCheck

     configPlatformVariant <- liftIO $
         maybe PlatformVariantNone PlatformVariant <$> lookupEnv platformVariantEnvVar

     let configBuild = buildOptsFromMonoid configMonoidBuildOpts
     configDocker <-
         dockerOptsFromMonoid (fmap fst mproject) configStackRoot mresolver configMonoidDockerOpts
     configNix <- nixOptsFromMonoid configMonoidNixOpts os

     configSystemGHC <-
         case (getFirst configMonoidSystemGHC, nixEnable configNix) of
             (Just False, True) ->
                 throwM NixRequiresSystemGhc
             _ ->
                 return
                     (fromFirst
                         (dockerEnable configDocker || nixEnable configNix)
                         configMonoidSystemGHC)

     when (isJust configGHCVariant0 && configSystemGHC) $
         throwM ManualGHCVariantSettingsAreIncompatibleWithSystemGHC

     rawEnv <- liftIO getEnvironment
     pathsEnv <- augmentPathMap configMonoidExtraPath
                                (Map.fromList (map (T.pack *** T.pack) rawEnv))
     origEnv <- mkEnvOverride configPlatform pathsEnv
     let configEnvOverride _ = return origEnv

     configLocalProgramsBase <- case getFirst configMonoidLocalProgramsBase of
       Nothing -> getDefaultLocalProgramsBase configStackRoot configPlatform origEnv
       Just path -> return path
     platformOnlyDir <- runReaderT platformOnlyRelDir (configPlatform, configPlatformVariant)
     let configLocalPrograms = configLocalProgramsBase </> platformOnlyDir

     configLocalBin <-
         case getFirst configMonoidLocalBinPath of
             Nothing -> do
                 localDir <- getAppUserDataDir "local"
                 return $ localDir </> $(mkRelDir "bin")
             Just userPath ->
                 (case mproject of
                     -- Not in a project
                     Nothing -> resolveDir' userPath
                     -- Resolves to the project dir and appends the user path if it is relative
                     Just (_, configYaml) -> resolveDir (parent configYaml) userPath)
                 -- TODO: Either catch specific exceptions or add a
                 -- parseRelAsAbsDirMaybe utility and use it along with
                 -- resolveDirMaybe.
                 `catchAll`
                 const (throwM (NoSuchDirectory userPath))

     configJobs <-
        case getFirst configMonoidJobs of
            Nothing -> liftIO getNumProcessors
            Just i -> return i
     let configConcurrentTests = fromFirst True configMonoidConcurrentTests

     let configTemplateParams = configMonoidTemplateParameters
         configScmInit = getFirst configMonoidScmInit
         configGhcOptions = configMonoidGhcOptions
         configSetupInfoLocations = configMonoidSetupInfoLocations
         configPvpBounds = fromFirst PvpBoundsNone configMonoidPvpBounds
         configModifyCodePage = fromFirst True configMonoidModifyCodePage
         configExplicitSetupDeps = configMonoidExplicitSetupDeps
         configRebuildGhcOptions = fromFirst False configMonoidRebuildGhcOptions
         configApplyGhcOptions = fromFirst AGOLocals configMonoidApplyGhcOptions
         configAllowNewer = fromFirst False configMonoidAllowNewer
         configDefaultTemplate = getFirst configMonoidDefaultTemplate
         configDumpLogs = fromFirst DumpWarningLogs configMonoidDumpLogs

     configAllowDifferentUser <-
        case getFirst configMonoidAllowDifferentUser of
            Just True -> return True
            _ -> getInContainer

     configPackageCaches <- liftIO $ newIORef Nothing

     let configMaybeProject = mproject

     return Config {..}

-- | Get the default location of the local programs directory.
getDefaultLocalProgramsBase :: MonadThrow m
                            => Path Abs Dir
                            -> Platform
                            -> EnvOverride
                            -> m (Path Abs Dir)
getDefaultLocalProgramsBase configStackRoot configPlatform override =
  let
    defaultBase = configStackRoot </> $(mkRelDir "programs")
  in
    case configPlatform of
      -- For historical reasons, on Windows a subdirectory of LOCALAPPDATA is
      -- used instead of a subdirectory of STACK_ROOT. Unifying the defaults would
      -- mean that Windows users would manually have to move data from the old
      -- location to the new one, which is undesirable.
      Platform _ Windows ->
        case Map.lookup "LOCALAPPDATA" $ unEnvOverride override of
          Just t -> do
            lad <- parseAbsDir $ T.unpack t
            return $ lad </> $(mkRelDir "Programs") </> $(mkRelDir stackProgName)
          Nothing -> return defaultBase
      _ -> return defaultBase

-- | An environment with a subset of BuildConfig used for setup.
data MiniConfig = MiniConfig
    { mcGHCVariant :: !GHCVariant
    , mcConfig :: !Config
    }
instance HasConfig MiniConfig where
    configL = lens mcConfig (\x y -> x { mcConfig = y })
instance HasPlatform MiniConfig
instance HasGHCVariant MiniConfig where
    ghcVariantL = lens mcGHCVariant (\x y -> x { mcGHCVariant = y })

-- | Load the 'MiniConfig'.
loadMiniConfig
    :: MonadIO m
    => Config -> m MiniConfig
loadMiniConfig config = do
    let ghcVariant = fromMaybe GHCStandard (configGHCVariant0 config)
    return (MiniConfig ghcVariant config)

-- Load the configuration, using environment variables, and defaults as
-- necessary.
loadConfigMaybeProject
    :: StackM env m
    => ConfigMonoid
    -- ^ Config monoid from parsed command-line arguments
    -> Maybe AbstractResolver
    -- ^ Override resolver
    -> Maybe (Project, Path Abs File, ConfigMonoid)
    -- ^ Project config to use, if any
    -> m (LoadConfig m)
loadConfigMaybeProject configArgs mresolver mproject = do
    (stackRoot, userOwnsStackRoot) <- determineStackRootAndOwnership configArgs
    userConfigPath <- getDefaultUserConfigPath stackRoot
    extraConfigs0 <- getExtraConfigs userConfigPath >>= mapM loadConfigYaml
    let extraConfigs =
            -- non-project config files' existence of a docker section should never default docker
            -- to enabled, so make it look like they didn't exist
            map (\c -> c {configMonoidDockerOpts =
                              (configMonoidDockerOpts c) {dockerMonoidDefaultEnable = Any False}})
                extraConfigs0
    let mproject' = (\(project, stackYaml, _) -> (project, stackYaml)) <$> mproject
    config <- configFromConfigMonoid stackRoot userConfigPath mresolver mproject' $ mconcat $
        case mproject of
            Nothing -> configArgs : extraConfigs
            Just (_, _, projectConfig) -> configArgs : projectConfig : extraConfigs
    unless (fromCabalVersion Meta.version `withinRange` configRequireStackVersion config)
        (throwM (BadStackVersionException (configRequireStackVersion config)))

    let mprojectRoot = fmap (\(_, fp, _) -> parent fp) mproject
    unless (configAllowDifferentUser config) $ do
        unless userOwnsStackRoot $
            throwM (UserDoesn'tOwnDirectory stackRoot)
        forM_ mprojectRoot $ \dir ->
            checkOwnership (dir </> configWorkDir config)

    return LoadConfig
        { lcConfig          = config
        , lcLoadBuildConfig = loadBuildConfig mproject config mresolver
        , lcProjectRoot     = mprojectRoot
        }

-- | Load the configuration, using current directory, environment variables,
-- and defaults as necessary. The passed @Maybe (Path Abs File)@ is an
-- override for the location of the project's stack.yaml.
loadConfig :: StackM env m
           => ConfigMonoid
           -- ^ Config monoid from parsed command-line arguments
           -> Maybe AbstractResolver
           -- ^ Override resolver
           -> Maybe (Path Abs File)
           -- ^ Override stack.yaml
           -> m (LoadConfig m)
loadConfig configArgs mresolver mstackYaml =
    loadProjectConfig mstackYaml >>= loadConfigMaybeProject configArgs mresolver

-- | Load the build configuration, adds build-specific values to config loaded by @loadConfig@.
-- values.
loadBuildConfig :: StackM env m
                => Maybe (Project, Path Abs File, ConfigMonoid)
                -> Config
                -> Maybe AbstractResolver -- override resolver
                -> Maybe CompilerVersion -- override compiler
                -> m BuildConfig
loadBuildConfig mproject config mresolver mcompiler = do
    env <- ask
    miniConfig <- loadMiniConfig config

    (project', stackYamlFP) <- case mproject of
      Just (project, fp, _) -> do
          forM_ (projectUserMsg project) ($logWarn . T.pack)
          return (project, fp)
      Nothing -> do
            $logDebug "Run from outside a project, using implicit global project config"
            destDir <- getImplicitGlobalProjectDir config
            let dest :: Path Abs File
                dest = destDir </> stackDotYaml
                dest' :: FilePath
                dest' = toFilePath dest
            ensureDir destDir
            exists <- doesFileExist dest
            if exists
               then do
                   ProjectAndConfigMonoid project _ <- loadConfigYaml dest
                   when (view terminalL env) $
                       case mresolver of
                           Nothing ->
                               $logDebug ("Using resolver: " <> resolverName (projectResolver project) <>
                                         " from implicit global project's config file: " <> T.pack dest')
                           Just aresolver -> do
                               let name =
                                        case aresolver of
                                            ARResolver resolver -> resolverName resolver
                                            ARLatestNightly -> "nightly"
                                            ARLatestLTS -> "lts"
                                            ARLatestLTSMajor x -> T.pack $ "lts-" ++ show x
                                            ARGlobal -> "global"
                               $logDebug ("Using resolver: " <> name <>
                                         " specified on command line")
                   return (project, dest)
               else do
                   r <- case mresolver of
                       Just aresolver -> do
                           r' <- runReaderT (makeConcreteResolver aresolver) miniConfig
                           $logInfo ("Using resolver: " <> resolverName r' <> " specified on command line")
                           return r'
                       Nothing -> do
                           r'' <- runReaderT getLatestResolver miniConfig
                           $logInfo ("Using latest snapshot resolver: " <> resolverName r'')
                           return r''
                   $logInfo ("Writing implicit global project config file to: " <> T.pack dest')
                   $logInfo "Note: You can change the snapshot via the resolver field there."
                   let p = Project
                           { projectUserMsg = Nothing
                           , projectPackages = mempty
                           , projectExtraDeps = mempty
                           , projectFlags = mempty
                           , projectResolver = r
                           , projectCompiler = Nothing
                           , projectExtraPackageDBs = []
                           }
                   liftIO $ do
                       S.writeFile dest' $ S.concat
                           [ "# This is the implicit global project's config file, which is only used when\n"
                           , "# 'stack' is run outside of a real project.  Settings here do _not_ act as\n"
                           , "# defaults for all projects.  To change stack's default settings, edit\n"
                           , "# '", encodeUtf8 (T.pack $ toFilePath $ configUserConfigPath config), "' instead.\n"
                           , "#\n"
                           , "# For more information about stack's configuration, see\n"
                           , "# http://docs.haskellstack.org/en/stable/yaml_configuration/\n"
                           , "#\n"
                           , Yaml.encode p]
                       S.writeFile (toFilePath $ parent dest </> $(mkRelFile "README.txt")) $ S.concat
                           [ "This is the implicit global project, which is used only when 'stack' is run\n"
                           , "outside of a real project.\n" ]
                   return (p, dest)
    resolver <-
        case mresolver of
            Nothing -> return $ projectResolver project'
            Just aresolver ->
                runReaderT (makeConcreteResolver aresolver) miniConfig
    let project = project'
            { projectResolver = resolver
            , projectCompiler = mcompiler <|> projectCompiler project'
            }

    (mbp0, loadedResolver) <- flip runReaderT miniConfig $
        loadResolver (Just stackYamlFP) (projectResolver project)
    let mbp = case projectCompiler project of
            Just compiler -> mbp0 { mbpCompilerVersion = compiler }
            Nothing -> mbp0

    extraPackageDBs <- mapM resolveDir' (projectExtraPackageDBs project)

    return BuildConfig
        { bcNoLocal = BuildConfigNoLocal
            { bcConfig = config
            , bcResolver = loadedResolver
            , bcWantedMiniBuildPlan = mbp
            , bcGHCVariant = view ghcVariantL miniConfig
            }
        , bcLocal = BuildConfigLocal
            { bcPackageEntries = projectPackages project
            , bcExtraDeps = projectExtraDeps project
            , bcExtraPackageDBs = extraPackageDBs
            , bcStackYaml = stackYamlFP
            , bcFlags = projectFlags project
            , bcImplicitGlobal = isNothing mproject
            }
        }

-- | Get packages from EnvConfig, downloading and cloning as necessary.
-- If the packages have already been downloaded, this uses a cached value (
getLocalPackages
    :: (StackMiniM env m, HasEnvConfig env)
    => m (Map.Map (Path Abs Dir) TreatLikeExtraDep)
getLocalPackages = do
    cacheRef <- view $ envConfigLocalL.to envConfigPackagesRef
    mcached <- liftIO $ readIORef cacheRef
    case mcached of
        Just cached -> return cached
        Nothing -> do
            menv <- getMinimalEnvOverride
            root <- view projectRootL
            entries <- view $ buildConfigLocalL.to bcPackageEntries
            liftM (Map.fromList . concat) $ mapM
                (resolvePackageEntry menv root)
                entries

-- | Resolve a PackageEntry into a list of paths, downloading and cloning as
-- necessary.
resolvePackageEntry
    :: (StackMiniM env m, HasConfig env)
    => EnvOverride
    -> Path Abs Dir -- ^ project root
    -> PackageEntry
    -> m [(Path Abs Dir, TreatLikeExtraDep)]
resolvePackageEntry menv projRoot pe = do
    entryRoot <- resolvePackageLocation menv projRoot (peLocation pe)
    paths <-
        case peSubdirs pe of
            [] -> return [entryRoot]
            subs -> mapM (resolveDir entryRoot) subs
    extraDep <-
        case peExtraDepMaybe pe of
            Just e -> return e
            Nothing ->
                case peLocation pe of
                    PLFilePath _ ->
                        -- we don't give a warning on missing explicit
                        -- value here, user intent is almost always
                        -- the default for a local directory
                        return False
                    PLRemote url _ -> do
                        $logWarn $ mconcat
                            [ "No extra-dep setting found for package at URL:\n\n"
                            , url
                            , "\n\n"
                            , "This is usually a mistake, external packages "
                            , "should typically\nbe treated as extra-deps to avoid "
                            , "spurious test case failures."
                            ]
                        return False
    return $ map (, extraDep) paths

-- | Resolve a PackageLocation into a path, downloading and cloning as
-- necessary.
resolvePackageLocation
    :: (StackMiniM env m, HasConfig env)
    => EnvOverride
    -> Path Abs Dir -- ^ project root
    -> PackageLocation
    -> m (Path Abs Dir)
resolvePackageLocation _ projRoot (PLFilePath fp) = resolveDir projRoot fp
resolvePackageLocation menv projRoot (PLRemote url remotePackageType) = do
    workDir <- view workDirL
    let nameBeforeHashing = case remotePackageType of
            RPTHttp{} -> url
            RPTGit commit -> T.unwords [url, commit]
            RPTHg commit -> T.unwords [url, commit, "hg"]
        -- TODO: dedupe with code for snapshot hash?
        name = T.unpack $ decodeUtf8 $ S.take 12 $ B64URL.encode $ SHA256.hash $ encodeUtf8 nameBeforeHashing
        root = projRoot </> workDir </> $(mkRelDir "downloaded")
        fileExtension = case remotePackageType of
            RPTHttp -> ".http-archive"
            _       -> ".unused"

    fileRel <- parseRelFile $ name ++ fileExtension
    dirRel <- parseRelDir name
    dirRelTmp <- parseRelDir $ name ++ ".tmp"
    let file = root </> fileRel
        dir = root </> dirRel

    exists <- doesDirExist dir
    unless exists $ do
        ignoringAbsence (removeDirRecur dir)

        let cloneAndExtract commandName cloneArgs resetCommand commit = do
                ensureDir root
                callProcessInheritStderrStdout Cmd
                    { cmdDirectoryToRunIn = Just root
                    , cmdCommandToRun = commandName
                    , cmdEnvOverride = menv
                    , cmdCommandLineArguments =
                        "clone" :
                        cloneArgs ++
                        [ T.unpack url
                        , toFilePathNoTrailingSep dir
                        ]
                    }
                created <- doesDirExist dir
                unless created $ throwM $ FailedToCloneRepo commandName
                readProcessNull (Just dir) menv commandName
                    (resetCommand ++ [T.unpack commit, "--"])
                    `catch` \case
                        ex@ProcessFailed{} -> do
                            $logInfo $ "Please ensure that commit " <> commit <> " exists within " <> url
                            throwM ex
                        ex -> throwM ex

        case remotePackageType of
            RPTHttp -> do
                let dirTmp = root </> dirRelTmp
                ignoringAbsence (removeDirRecur dirTmp)

                let fp = toFilePath file
                req <- parseUrlThrow $ T.unpack url
                _ <- download req file

                let tryTar = do
                        $logDebug $ "Trying to untar " <> T.pack fp
                        liftIO $ withBinaryFile fp ReadMode $ \h -> do
                            lbs <- L.hGetContents h
                            let entries = Tar.read $ GZip.decompress lbs
                            Tar.unpack (toFilePath dirTmp) entries
                    tryZip = do
                        $logDebug $ "Trying to unzip " <> T.pack fp
                        archive <- fmap Zip.toArchive $ liftIO $ L.readFile fp
                        liftIO $  Zip.extractFilesFromArchive [Zip.OptDestination
                                                               (toFilePath dirTmp)] archive
                    err = throwM $ UnableToExtractArchive url file

                    catchAllLog goodpath handler =
                        catchAll goodpath $ \e -> do
                            $logDebug $ "Got exception: " <> T.pack (show e)
                            handler

                tryTar `catchAllLog` tryZip `catchAllLog` err
                renameDir dirTmp dir

            -- Passes in --git-dir to git and --repository to hg, in order
            -- to avoid the update commands being applied to the user's
            -- repo.  See https://github.com/commercialhaskell/stack/issues/2748
            RPTGit commit -> cloneAndExtract "git" ["--recursive"] ["--git-dir=.git", "reset", "--hard"] commit
            RPTHg  commit -> cloneAndExtract "hg"  []              ["--repository", ".", "update", "-C"] commit

    case remotePackageType of
        RPTHttp -> do
            x <- listDir dir
            case x of
                ([dir'], []) -> return dir'
                (dirs, files) -> do
                    ignoringAbsence (removeFile file)
                    ignoringAbsence (removeDirRecur dir)
                    throwM $ UnexpectedArchiveContents dirs files
        _ -> return dir


-- | Get the stack root, e.g. @~/.stack@, and determine whether the user owns it.
--
-- On Windows, the second value is always 'True'.
determineStackRootAndOwnership
    :: (MonadIO m, MonadCatch m)
    => ConfigMonoid
    -- ^ Parsed command-line arguments
    -> m (Path Abs Dir, Bool)
determineStackRootAndOwnership clArgs = do
    stackRoot <- do
        case getFirst (configMonoidStackRoot clArgs) of
            Just x -> return x
            Nothing -> do
                mstackRoot <- liftIO $ lookupEnv stackRootEnvVar
                case mstackRoot of
                    Nothing -> getAppUserDataDir stackProgName
                    Just x -> parseAbsDir x

    (existingStackRootOrParentDir, userOwnsIt) <- do
        mdirAndOwnership <- findInParents getDirAndOwnership stackRoot
        case mdirAndOwnership of
            Just x -> return x
            Nothing -> throwM (BadStackRoot stackRoot)

    when (existingStackRootOrParentDir /= stackRoot) $
        if userOwnsIt
            then liftIO $ ensureDir stackRoot
            else throwM $
                Won'tCreateStackRootInDirectoryOwnedByDifferentUser
                    stackRoot
                    existingStackRootOrParentDir

    stackRoot' <- canonicalizePath stackRoot
    return (stackRoot', userOwnsIt)

-- | @'checkOwnership' dir@ throws 'UserDoesn'tOwnDirectory' if @dir@
-- isn't owned by the current user.
--
-- If @dir@ doesn't exist, its parent directory is checked instead.
-- If the parent directory doesn't exist either, @'NoSuchDirectory' ('parent' dir)@
-- is thrown.
checkOwnership :: (MonadIO m, MonadCatch m) => Path Abs Dir -> m ()
checkOwnership dir = do
    mdirAndOwnership <- firstJustM getDirAndOwnership [dir, parent dir]
    case mdirAndOwnership of
        Just (_, True) -> return ()
        Just (dir', False) -> throwM (UserDoesn'tOwnDirectory dir')
        Nothing ->
            (throwM . NoSuchDirectory) $ (toFilePathNoTrailingSep . parent) dir

-- | @'getDirAndOwnership' dir@ returns @'Just' (dir, 'True')@ when @dir@
-- exists and the current user owns it in the sense of 'isOwnedByUser'.
getDirAndOwnership
    :: (MonadIO m, MonadCatch m)
    => Path Abs Dir
    -> m (Maybe (Path Abs Dir, Bool))
getDirAndOwnership dir = forgivingAbsence $ do
    ownership <- isOwnedByUser dir
    return (dir, ownership)

-- | Check whether the current user (determined with 'getEffectiveUserId') is
-- the owner for the given path.
--
-- Will always return 'True' on Windows.
isOwnedByUser :: MonadIO m => Path Abs t -> m Bool
isOwnedByUser path = liftIO $ do
    if osIsWindows
        then return True
        else do
            fileStatus <- getFileStatus (toFilePath path)
            user <- getEffectiveUserID
            return (user == fileOwner fileStatus)
  where
#ifdef WINDOWS
    osIsWindows = True
#else
    osIsWindows = False
#endif

-- | 'True' if we are currently running inside a Docker container.
getInContainer :: (MonadIO m) => m Bool
getInContainer = liftIO (isJust <$> lookupEnv inContainerEnvVar)

-- | 'True' if we are currently running inside a Nix.
getInNixShell :: (MonadIO m) => m Bool
getInNixShell = liftIO (isJust <$> lookupEnv inNixShellEnvVar)

-- | Determine the extra config file locations which exist.
--
-- Returns most local first
getExtraConfigs :: (MonadIO m, MonadLogger m)
                => Path Abs File -- ^ use config path
                -> m [Path Abs File]
getExtraConfigs userConfigPath = do
  defaultStackGlobalConfigPath <- getDefaultGlobalConfigPath
  liftIO $ do
    env <- getEnvironment
    mstackConfig <-
        maybe (return Nothing) (fmap Just . parseAbsFile)
      $ lookup "STACK_CONFIG" env
    mstackGlobalConfig <-
        maybe (return Nothing) (fmap Just . parseAbsFile)
      $ lookup "STACK_GLOBAL_CONFIG" env
    filterM doesFileExist
        $ fromMaybe userConfigPath mstackConfig
        : maybe [] return (mstackGlobalConfig <|> defaultStackGlobalConfigPath)

-- | Load and parse YAML from the given conig file. Throws
-- 'ParseConfigFileException' when there's a decoding error.
loadConfigYaml
    :: (FromJSON (WithJSONWarnings a), MonadIO m, MonadLogger m)
    => Path Abs File -> m a
loadConfigYaml path = do
    eres <- loadYaml path
    case eres of
        Left err -> liftIO $ throwM (ParseConfigFileException path err)
        Right res -> return res

-- | Load and parse YAML from the given file.
loadYaml
    :: (FromJSON (WithJSONWarnings a), MonadIO m, MonadLogger m)
    => Path Abs File -> m (Either Yaml.ParseException a)
loadYaml path = do
    eres <- liftIO $ Yaml.decodeFileEither (toFilePath path)
    case eres  of
        Left err -> return (Left err)
        Right (WithJSONWarnings res warnings) -> do
            logJSONWarnings (toFilePath path) warnings
            return (Right res)

-- | Get the location of the project config file, if it exists.
getProjectConfig :: (MonadIO m, MonadThrow m, MonadLogger m)
                 => Maybe (Path Abs File)
                 -- ^ Override stack.yaml
                 -> m (Maybe (Path Abs File))
getProjectConfig (Just stackYaml) = return $ Just stackYaml
getProjectConfig Nothing = do
    env <- liftIO getEnvironment
    case lookup "STACK_YAML" env of
        Just fp -> do
            $logInfo "Getting project config file from STACK_YAML environment"
            liftM Just $ resolveFile' fp
        Nothing -> do
            currDir <- getCurrentDir
            findInParents getStackDotYaml currDir
  where
    getStackDotYaml dir = do
        let fp = dir </> stackDotYaml
            fp' = toFilePath fp
        $logDebug $ "Checking for project config at: " <> T.pack fp'
        exists <- doesFileExist fp
        if exists
            then return $ Just fp
            else return Nothing

-- | Find the project config file location, respecting environment variables
-- and otherwise traversing parents. If no config is found, we supply a default
-- based on current directory.
loadProjectConfig :: (MonadIO m, MonadThrow m, MonadLogger m)
                  => Maybe (Path Abs File)
                  -- ^ Override stack.yaml
                  -> m (Maybe (Project, Path Abs File, ConfigMonoid))
loadProjectConfig mstackYaml = do
    mfp <- getProjectConfig mstackYaml
    case mfp of
        Just fp -> do
            currDir <- getCurrentDir
            $logDebug $ "Loading project config file " <>
                        T.pack (maybe (toFilePath fp) toFilePath (stripDir currDir fp))
            load fp
        Nothing -> do
            $logDebug $ "No project config file found, using defaults."
            return Nothing
  where
    load fp = do
        ProjectAndConfigMonoid project config <- loadConfigYaml fp
        return $ Just (project, fp, config)

-- | Get the location of the default stack configuration file.
-- If a file already exists at the deprecated location, its location is returned.
-- Otherwise, the new location is returned.
getDefaultGlobalConfigPath
    :: (MonadIO m, MonadLogger m)
    => m (Maybe (Path Abs File))
getDefaultGlobalConfigPath =
    case (defaultGlobalConfigPath, defaultGlobalConfigPathDeprecated) of
        (Just new,Just old) ->
            liftM (Just . fst ) $
            tryDeprecatedPath
                (Just "non-project global configuration file")
                doesFileExist
                new
                old
        (Just new,Nothing) -> return (Just new)
        _ -> return Nothing

-- | Get the location of the default user configuration file.
-- If a file already exists at the deprecated location, its location is returned.
-- Otherwise, the new location is returned.
getDefaultUserConfigPath
    :: (MonadIO m, MonadLogger m)
    => Path Abs Dir -> m (Path Abs File)
getDefaultUserConfigPath stackRoot = do
    (path, exists) <- tryDeprecatedPath
        (Just "non-project configuration file")
        doesFileExist
        (defaultUserConfigPath stackRoot)
        (defaultUserConfigPathDeprecated stackRoot)
    unless exists $ do
        ensureDir (parent path)
        liftIO $ S.writeFile (toFilePath path) defaultConfigYaml
    return path

packagesParser :: Parser [String]
packagesParser = many (strOption (long "package" <> help "Additional packages that must be installed"))

defaultConfigYaml :: S.ByteString
defaultConfigYaml = S.intercalate "\n"
     [ "# This file contains default non-project-specific settings for 'stack', used"
     , "# in all projects.  For more information about stack's configuration, see"
     , "# http://docs.haskellstack.org/en/stable/yaml_configuration/"
     , ""
     , "# The following parameters are used by \"stack new\" to automatically fill fields"
     , "# in the cabal config. We recommend uncommenting them and filling them out if"
     , "# you intend to use 'stack new'."
     , "# See https://docs.haskellstack.org/en/stable/yaml_configuration/#templates"
     , "templates:"
     , "  params:"
     , "#    author-name:"
     , "#    author-email:"
     , "#    copyright:"
     , "#    github-username:"
     ]
