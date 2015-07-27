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
  (loadConfig
  ,packagesParser
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.Applicative
import           Control.Concurrent (getNumCapabilities)
import           Control.Exception (IOException)
import           Control.Monad
import           Control.Monad.Catch (Handler(..), MonadCatch, MonadThrow, catches, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger hiding (Loc)
import           Control.Monad.Reader (MonadReader, ask, asks, runReaderT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.Aeson.Extended
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as L
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Yaml as Yaml
import           Distribution.System (OS (..), Platform (..), buildPlatform)
import qualified Distribution.Text
import           Distribution.Version (simplifyVersionRange)
import           Network.HTTP.Client.Conduit (HasHttpManager, getHttpManager, Manager, parseUrl)
import           Network.HTTP.Download (download)
import           Options.Applicative (Parser, strOption, long, help)
import           Path
import           Path.IO
import qualified Paths_stack as Meta
import           Stack.BuildPlan
import           Stack.Constants
import qualified Stack.Docker as Docker
import qualified Stack.Image as Image
import           Stack.Init
import           Stack.Types
import           Stack.Types.Internal
import           System.Directory (getAppUserDataDirectory, createDirectoryIfMissing, canonicalizePath)
import           System.Environment
import           System.IO
import           System.Process.Read (getEnvOverride, EnvOverride, unEnvOverride, readInNull)

-- | Get the latest snapshot resolver available.
getLatestResolver
    :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m)
    => m Resolver
getLatestResolver = do
    snapshots <- getSnapshots
    let mlts = do
            (x,y) <- listToMaybe (reverse (IntMap.toList (snapshotsLts snapshots)))
            return (LTS x y)
        snap =
            case mlts of
                Nothing -> Nightly (snapshotsNightly snapshots)
                Just lts -> lts
    return (ResolverSnapshot snap)

-- | Note that this will be @Nothing@ on Windows, which is by design.
defaultStackGlobalConfig :: Maybe (Path Abs File)
defaultStackGlobalConfig = parseAbsFile "/etc/stack/config"

-- Interprets ConfigMonoid options.
configFromConfigMonoid
    :: (MonadLogger m, MonadIO m, MonadCatch m, MonadReader env m, HasHttpManager env)
    => Path Abs Dir -- ^ stack root, e.g. ~/.stack
    -> Maybe Project
    -> ConfigMonoid
    -> m Config
configFromConfigMonoid configStackRoot mproject configMonoid@ConfigMonoid{..} = do
     let configDocker = Docker.dockerOptsFromMonoid mproject configStackRoot configMonoidDockerOpts
         configConnectionCount = fromMaybe 8 configMonoidConnectionCount
         configHideTHLoading = fromMaybe True configMonoidHideTHLoading
         configLatestSnapshotUrl = fromMaybe
            "https://s3.amazonaws.com/haddock.stackage.org/snapshots.json"
            configMonoidLatestSnapshotUrl
         configPackageIndices = fromMaybe
            [PackageIndex
                { indexName = IndexName "Hackage"
                , indexLocation = ILGitHttp
                        "https://github.com/commercialhaskell/all-cabal-hashes.git"
                        "https://s3.amazonaws.com/hackage.fpcomplete.com/00-index.tar.gz"
                , indexDownloadPrefix = "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"
                , indexGpgVerify = False
                , indexRequireHashes = False
                }]
            configMonoidPackageIndices

         configSystemGHC = fromMaybe True configMonoidSystemGHC
         configInstallGHC = fromMaybe False configMonoidInstallGHC
         configSkipGHCCheck = fromMaybe False configMonoidSkipGHCCheck
         configSkipMsys = fromMaybe False configMonoidSkipMsys

         configExtraIncludeDirs = configMonoidExtraIncludeDirs
         configExtraLibDirs = configMonoidExtraLibDirs

         -- Only place in the codebase where platform is hard-coded. In theory
         -- in the future, allow it to be configured.
         (Platform defArch defOS) = buildPlatform
         arch = fromMaybe defArch
              $ configMonoidArch >>= Distribution.Text.simpleParse
         os = fromMaybe defOS
            $ configMonoidOS >>= Distribution.Text.simpleParse
         configPlatform = Platform arch os

         configRequireStackVersion = simplifyVersionRange configMonoidRequireStackVersion

         configConfigMonoid = configMonoid

         configImage = Image.imgOptsFromMonoid configMonoidImageOpts

     origEnv <- getEnvOverride configPlatform
     let configEnvOverride _ = return origEnv

     platform <- runReaderT platformRelDir configPlatform

     configLocalPrograms <-
        case configPlatform of
            Platform _ Windows -> do
                progsDir <- getWindowsProgsDir configStackRoot origEnv
                return $ progsDir </> $(mkRelDir stackProgName) </> platform
            _ -> return $ configStackRoot </> $(mkRelDir "programs") </> platform

     configLocalBin <-
         case configMonoidLocalBinPath of
             Nothing -> do
                 localDir <- liftIO (getAppUserDataDirectory "local") >>= parseAbsDir
                 return $ localDir </> $(mkRelDir "bin")
             Just userPath ->
                 (liftIO $ canonicalizePath userPath >>= parseAbsDir)
                 `catches`
                 [Handler (\(_ :: IOException) -> throwM $ NoSuchDirectory userPath)
                 ,Handler (\(_ :: PathParseException) -> throwM $ NoSuchDirectory userPath)
                 ]
     configJobs <-
        case configMonoidJobs of
            Nothing -> liftIO getNumCapabilities
            Just i -> return i
     let configConcurrentTests = fromMaybe True configMonoidConcurrentTests

     return Config {..}

-- | Get the directory on Windows where we should install extra programs. For
-- more information, see discussion at:
-- https://github.com/fpco/minghc/issues/43#issuecomment-99737383
getWindowsProgsDir :: MonadThrow m
                   => Path Abs Dir
                   -> EnvOverride
                   -> m (Path Abs Dir)
getWindowsProgsDir stackRoot m =
    case Map.lookup "LOCALAPPDATA" $ unEnvOverride m of
        Just t -> do
            lad <- parseAbsDir $ T.unpack t
            return $ lad </> $(mkRelDir "Programs")
        Nothing -> return $ stackRoot </> $(mkRelDir "Programs")

data MiniConfig = MiniConfig Manager Config
instance HasConfig MiniConfig where
    getConfig (MiniConfig _ c) = c
instance HasStackRoot MiniConfig
instance HasHttpManager MiniConfig where
    getHttpManager (MiniConfig man _) = man
instance HasPlatform MiniConfig

-- | Load the configuration, using current directory, environment variables,
-- and defaults as necessary.
loadConfig :: (MonadLogger m,MonadIO m,MonadCatch m,MonadThrow m,MonadBaseControl IO m,MonadReader env m,HasHttpManager env,HasTerminal env)
           => ConfigMonoid
           -- ^ Config monoid from parsed command-line arguments
           -> Maybe (Path Abs File)
           -- ^ Override stack.yaml
           -> m (LoadConfig m)
loadConfig configArgs mstackYaml = do
    stackRoot <- determineStackRoot
    extraConfigs <- getExtraConfigs stackRoot >>= mapM loadYaml
    mproject <- loadProjectConfig mstackYaml
    config <- configFromConfigMonoid stackRoot (fmap (\(proj, _, _) -> proj) mproject) $ mconcat $
        case mproject of
            Nothing -> configArgs : extraConfigs
            Just (_, _, projectConfig) -> configArgs : projectConfig : extraConfigs
    unless (fromCabalVersion Meta.version `withinRange` configRequireStackVersion config)
        (throwM (BadStackVersionException (configRequireStackVersion config)))
    menv <- runReaderT getMinimalEnvOverride config
    return $ LoadConfig
        { lcConfig          = config
        , lcLoadBuildConfig = loadBuildConfig menv mproject config stackRoot
        , lcProjectRoot     = fmap (\(_, fp, _) -> parent fp) mproject
        }

-- | Load the build configuration, adds build-specific values to config loaded by @loadConfig@.
-- values.
loadBuildConfig :: (MonadLogger m, MonadIO m, MonadCatch m, MonadReader env m, HasHttpManager env, MonadBaseControl IO m, HasTerminal env)
                => EnvOverride
                -> Maybe (Project, Path Abs File, ConfigMonoid)
                -> Config
                -> Path Abs Dir
                -> Maybe AbstractResolver -- override resolver
                -> m BuildConfig
loadBuildConfig menv mproject config stackRoot mresolver = do
    env <- ask
    let miniConfig = MiniConfig (getHttpManager env) config
    (project', stackYamlFP) <- case mproject of
      Just (project, fp, _) -> return (project, fp)
      Nothing -> do
            $logInfo "Run from outside a project, using implicit global config"
            let dest :: Path Abs File
                dest = destDir </> stackDotYaml
                destDir = implicitGlobalDir stackRoot
                dest' :: FilePath
                dest' = toFilePath dest
            createTree destDir
            exists <- fileExists dest
            if exists
               then do
                   ProjectAndConfigMonoid project _ <- loadYaml dest
                   when (getTerminal env) $
                       case mresolver of
                           Nothing ->
                               $logInfo ("Using resolver: " <> resolverName (projectResolver project) <>
                                         " from global config file: " <> T.pack dest')
                           Just aresolver -> do
                               let name =
                                        case aresolver of
                                            ARResolver resolver -> resolverName resolver
                                            ARLatestNightly -> "nightly"
                                            ARLatestLTS -> "lts"
                                            ARLatestLTSMajor x -> T.pack $ "lts-" ++ show x
                                            ARGlobal -> "global"
                               $logInfo ("Using resolver: " <> name <>
                                         " specified on command line")
                   return (project, dest)
               else do
                   r <- runReaderT getLatestResolver miniConfig
                   $logInfo ("Using latest snapshot resolver: " <> resolverName r)
                   $logInfo ("Writing global (non-project-specific) config file to: " <> T.pack dest')
                   $logInfo "Note: You can change the snapshot via the resolver field there."
                   let p = Project
                           { projectPackages = mempty
                           , projectExtraDeps = mempty
                           , projectFlags = mempty
                           , projectResolver = r
                           }
                   liftIO $ Yaml.encodeFile dest' p
                   return (p, dest)
    resolver <-
        case mresolver of
            Nothing -> return $ projectResolver project'
            Just aresolver -> do
                manager <- asks getHttpManager
                runReaderT
                    (makeConcreteResolver aresolver)
                    (MiniConfig manager config)
    let project = project' { projectResolver = resolver }

    ghcVersion <-
        case projectResolver project of
            ResolverSnapshot snapName -> do
                mbp <- runReaderT (loadMiniBuildPlan snapName) miniConfig
                return $ mbpGhcVersion mbp
            ResolverGhc m -> return $ fromMajorVersion m
            ResolverCustom _name url -> do
                mbp <- runReaderT (parseCustomMiniBuildPlan stackYamlFP url) miniConfig
                return $ mbpGhcVersion mbp

    let root = parent stackYamlFP
    packages' <- mapM (resolvePackageEntry menv root) (projectPackages project)
    let packages = Map.fromList $ concat packages'

    return BuildConfig
        { bcConfig = config
        , bcResolver = projectResolver project
        , bcGhcVersionExpected = ghcVersion
        , bcPackages = packages
        , bcExtraDeps = projectExtraDeps project
        , bcRoot = root
        , bcStackYaml = stackYamlFP
        , bcFlags = projectFlags project
        }

-- | Resolve a PackageEntry into a list of paths, downloading and cloning as
-- necessary.
resolvePackageEntry
    :: (MonadIO m, MonadThrow m, MonadReader env m, HasHttpManager env, MonadLogger m, MonadCatch m
       ,MonadBaseControl IO m)
    => EnvOverride
    -> Path Abs Dir -- ^ project root
    -> PackageEntry
    -> m [(Path Abs Dir, Bool)]
resolvePackageEntry menv projRoot pe = do
    entryRoot <- resolvePackageLocation menv projRoot (peLocation pe)
    paths <-
        case peSubdirs pe of
            [] -> return [entryRoot]
            subs -> mapM (resolveDir entryRoot) subs
    case peValidWanted pe of
        Nothing -> return ()
        Just _ -> $logWarn "Warning: you are using the deprecated valid-wanted field. You should instead use extra-dep. See: https://github.com/commercialhaskell/stack/wiki/stack.yaml#packages"
    return $ map (, not $ peExtraDep pe) paths

-- | Resolve a PackageLocation into a path, downloading and cloning as
-- necessary.
resolvePackageLocation
    :: (MonadIO m, MonadThrow m, MonadReader env m, HasHttpManager env, MonadLogger m, MonadCatch m
       ,MonadBaseControl IO m)
    => EnvOverride
    -> Path Abs Dir -- ^ project root
    -> PackageLocation
    -> m (Path Abs Dir)
resolvePackageLocation _ projRoot (PLFilePath fp) = resolveDir projRoot fp
resolvePackageLocation _ projRoot (PLHttpTarball url) = do
    let name = T.unpack $ decodeUtf8 $ B16.encode $ SHA256.hash $ encodeUtf8 url
        root = projRoot </> workDirRel </> $(mkRelDir "downloaded")
    fileRel <- parseRelFile $ name ++ ".tar.gz"
    dirRel <- parseRelDir name
    dirRelTmp <- parseRelDir $ name ++ ".tmp"
    let file = root </> fileRel
        dir = root </> dirRel
        dirTmp = root </> dirRelTmp

    exists <- dirExists dir
    unless exists $ do
        req <- parseUrl $ T.unpack url
        _ <- download req file

        removeTreeIfExists dirTmp
        liftIO $ withBinaryFile (toFilePath file) ReadMode $ \h -> do
            lbs <- L.hGetContents h
            let entries = Tar.read $ GZip.decompress lbs
            Tar.unpack (toFilePath dirTmp) entries
            renameDir dirTmp dir

    x <- listDirectory dir
    case x of
        ([dir'], []) -> return dir'
        (dirs, files) -> do
            removeFileIfExists file
            removeTreeIfExists dir
            throwM $ UnexpectedTarballContents dirs files

resolvePackageLocation menv projRoot (PLGit url commit) = do
    let name = T.unpack $ decodeUtf8 $ B16.encode $ SHA256.hash $ encodeUtf8 $ T.unwords [url, commit]
        root = projRoot </> workDirRel </> $(mkRelDir "downloaded")
    dirRel <- parseRelDir $ name ++ ".git"
    dirRelTmp <- parseRelDir $ name ++ ".git.tmp"
    let dir = root </> dirRel
        dirTmp = root </> dirRelTmp

    exists <- dirExists dir
    unless exists $ do
        removeTreeIfExists dirTmp
        createTree (parent dirTmp)
        readInNull (parent dirTmp) "git" menv
            [ "clone"
            , T.unpack url
            , toFilePath dirTmp
            ]
            Nothing
        readInNull dirTmp "git" menv
            [ "reset"
            , "--hard"
            , T.unpack commit
            ]
            Nothing
        renameDir dirTmp dir

    return dir

-- | Get the stack root, e.g. ~/.stack
determineStackRoot :: (MonadIO m, MonadThrow m) => m (Path Abs Dir)
determineStackRoot = do
    env <- liftIO getEnvironment
    case lookup stackRootEnvVar env of
        Nothing -> do
            x <- liftIO $ getAppUserDataDirectory stackProgName
            parseAbsDir x
        Just x -> do
            y <- liftIO $ do
                createDirectoryIfMissing True x
                canonicalizePath x
            parseAbsDir y

-- | Determine the extra config file locations which exist.
--
-- Returns most local first
getExtraConfigs :: MonadIO m
                => Path Abs Dir -- ^ stack root
                -> m [Path Abs File]
getExtraConfigs stackRoot = liftIO $ do
    env <- getEnvironment
    mstackConfig <-
        maybe (return Nothing) (fmap Just . parseAbsFile)
      $ lookup "STACK_CONFIG" env
    mstackGlobalConfig <-
        maybe (return Nothing) (fmap Just . parseAbsFile)
      $ lookup "STACK_GLOBAL_CONFIG" env
    filterM fileExists
        $ fromMaybe (stackRoot </> stackDotYaml) mstackConfig
        : maybe [] return (mstackGlobalConfig <|> defaultStackGlobalConfig)

-- | Load and parse YAML from the given file.
loadYaml :: (FromJSON (a, [JSONWarning]), MonadIO m, MonadLogger m) => Path Abs File -> m a
loadYaml path = do
    (result,warnings) <-
        liftIO $
        Yaml.decodeFileEither (toFilePath path) >>=
        either (throwM . ParseConfigFileException path) return
    logJSONWarnings (toFilePath path) warnings
    return result

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
            liftM Just $ case parseAbsFile fp of
                Left _ -> do
                    currDir <- getWorkingDir
                    resolveFile currDir fp
                Right path -> return path
        Nothing -> do
            currDir <- getWorkingDir
            search currDir
  where
    search dir = do
        let fp = dir </> stackDotYaml
            fp' = toFilePath fp
        $logDebug $ "Checking for project config at: " <> T.pack fp'
        exists <- fileExists fp
        if exists
            then return $ Just fp
            else do
                let dir' = parent dir
                if dir == dir'
                    -- fully traversed, give up
                    then return Nothing
                    else search dir'

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
            currDir <- getWorkingDir
            $logDebug $ "Loading project config file " <>
                        T.pack (maybe (toFilePath fp) toFilePath (stripDir currDir fp))
            load fp
        Nothing -> do
            $logDebug $ "No project config file found, using defaults."
            return Nothing
  where
    load fp = do
        ProjectAndConfigMonoid project config <- loadYaml fp
        return $ Just (project, fp, config)

packagesParser :: Parser [String]
packagesParser = many (strOption (long "package" <> help "Additional packages that must be installed"))
