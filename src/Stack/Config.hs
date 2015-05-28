{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  ( loadConfig
  , loadBuildConfig
  , stackDotYaml
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger hiding (Loc)
import           Control.Monad.Reader (MonadReader, ask, runReaderT)
import           Data.Aeson
import           Data.Either (partitionEithers)
import           Data.Map (Map)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Distribution.Package as C
import qualified Distribution.PackageDescription as C
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import           Distribution.System (OS (Windows), Platform (..), buildPlatform)
import           Network.HTTP.Client.Conduit (HasHttpManager, getHttpManager, Manager)
import           Path
import           Path.IO
import           Stack.BuildPlan
import           Stack.Package
import           Stack.Types
import           System.Directory
import           System.Environment
import           System.Process.Read (getEnvOverride, EnvOverride, unEnvOverride)

-- An uninterpreted representation of configuration options.
-- Configurations may be "cascaded" using mappend (left-biased).
data ConfigMonoid =
  ConfigMonoid
    { configMonoidDockerOpts     :: !DockerOpts
    , configMonoidUrls           :: !(Map Text Text)
    -- ^ Various URLs for downloading things. Yes, this is stringly typed,
    -- making it easy to extend. Please make sure to only access it using
    -- helper functions.
    , configMonoidGpgVerifyIndex :: !(Maybe Bool)
    -- ^ Controls how package index updating occurs
    , configMonoidConnectionCount :: !(Maybe Int)
    -- ^ See: 'configConnectionCount'
    , configMonoidHideTHLoading :: !(Maybe Bool)
    -- ^ See: 'configHideTHLoading'
    }
  deriving Show

-- | Get the default resolver value
getDefaultResolver :: (MonadIO m, MonadCatch m, MonadReader env m, HasConfig env, HasUrls env, HasHttpManager env, MonadLogger m)
                   => Path Abs Dir
                   -> m (Resolver, Map PackageName (Map FlagName Bool))
getDefaultResolver dir = do
    ecabalfp <- try $ getCabalFileName dir
    msnap <- case ecabalfp of
        Left e -> do
            $logWarn $ T.pack $ show (e :: PackageException)
            return Nothing
        Right cabalfp -> do
            gpd <- readPackageUnresolved cabalfp
            mpair <- findBuildPlan cabalfp gpd
            let name =
                    case C.package $ C.packageDescription gpd of
                        C.PackageIdentifier cname _ ->
                            fromCabalPackageName cname
            return $ fmap (, name) mpair
    case msnap of
        Just ((snap, flags), name) ->
            return (ResolverSnapshot snap, Map.singleton name flags)
        Nothing -> do
            s <- getSnapshots
            let snap = case IntMap.maxViewWithKey (snapshotsLts s) of
                    Just ((x, y), _) -> LTS x y
                    Nothing -> Nightly $ snapshotsNightly s
            return (ResolverSnapshot snap, Map.empty)

-- | Dummy type to support this monoid business.
newtype DockerOpts = DockerOpts Docker
  deriving (Show)

-- | Left-biased instance.
instance Monoid DockerOpts where
  --EKB FIXME: implement proper configuration inheritance
  mappend x _ = x
  mempty = DockerOpts defaultDocker

instance Monoid ConfigMonoid where
  mempty = ConfigMonoid
    { configMonoidDockerOpts = mempty
    , configMonoidUrls = mempty
    , configMonoidGpgVerifyIndex = Nothing
    , configMonoidConnectionCount = Nothing
    , configMonoidHideTHLoading = Nothing
    }
  mappend l r = ConfigMonoid
    { configMonoidDockerOpts = configMonoidDockerOpts l <> configMonoidDockerOpts r
    , configMonoidUrls = configMonoidUrls l <> configMonoidUrls r
    , configMonoidGpgVerifyIndex = configMonoidGpgVerifyIndex l <|> configMonoidGpgVerifyIndex r
    , configMonoidConnectionCount = configMonoidConnectionCount l <|> configMonoidConnectionCount r
    , configMonoidHideTHLoading = configMonoidHideTHLoading l <|> configMonoidHideTHLoading r
    }

instance FromJSON ConfigMonoid where
  parseJSON =
    withObject "ConfigMonoid" $
    \obj ->
      do getTheDocker <- obj .:? "docker" .!= defaultDocker
         configMonoidUrls <- obj .:? "urls" .!= mempty
         configMonoidGpgVerifyIndex <- obj .:? "gpg-verify-index"
         configMonoidConnectionCount <- obj .:? "connection-count"
         configMonoidHideTHLoading <- obj .:? "hide-th-loading"
         let configMonoidDockerOpts = DockerOpts getTheDocker
         return ConfigMonoid {..}

-- | A project is a collection of packages. We can have multiple stack.yaml
-- files, but only one of them may contain project information.
data Project = Project
    { projectPackages :: ![FilePath]
    -- ^ Components of the package list which refer to local directories
    --
    -- Note that we use @FilePath@ and not @Path@s. The goal is: first parse
    -- the value raw, and then use @canonicalizePath@ and @parseAbsDir@.
    , projectExtraDeps :: !(Map PackageName Version)
    -- ^ Components of the package list referring to package/version combos,
    -- see: https://github.com/fpco/stack/issues/41
    , projectFlags :: !(Map PackageName (Map FlagName Bool))
    -- ^ Per-package flag overrides
    , projectResolver :: !Resolver
    -- ^ How we resolve which dependencies to use
    }
  deriving Show

instance ToJSON Project where
    toJSON p = object
        [ "packages"   .= projectPackages p
        , "extra-deps" .= map fromTuple (Map.toList $ projectExtraDeps p)
        , "flags"      .= projectFlags p
        , "resolver"   .= projectResolver p
        ]
instance FromJSON (Project, ConfigMonoid) where
    parseJSON = withObject "Project, ConfigMonoid" $ \o -> do
        dirs <- o .:? "packages" .!= ["."]
        extraDeps' <- o .:? "extra-deps" .!= []
        extraDeps <-
            case partitionEithers $ goDeps extraDeps' of
                ([], x) -> return $ Map.fromList x
                (errs, _) -> fail $ unlines errs

        flags <- o .:? "flags" .!= mempty
        resolver <- o .: "resolver"
        config <- parseJSON $ Object o
        let project = Project
                { projectPackages = dirs
                , projectExtraDeps = extraDeps
                , projectFlags = flags
                , projectResolver = resolver
                }
        return (project, config)
      where
        goDeps =
            map toSingle . Map.toList . Map.unionsWith S.union . map toMap
          where
            toMap i = Map.singleton
                (packageIdentifierName i)
                (S.singleton (packageIdentifierVersion i))

        toSingle (k, s) =
            case S.toList s of
                [x] -> Right (k, x)
                xs -> Left $ concat
                    [ "Multiple versions for package "
                    , packageNameString k
                    , ": "
                    , unwords $ map versionString xs
                    ]

-- | Note that this will be @Nothing@ on Windows, which is by design.
defaultStackGlobalConfig :: Maybe (Path Abs File)
defaultStackGlobalConfig = parseAbsFile "/etc/stack/config"

-- Interprets ConfigMonoid options.
configFromConfigMonoid :: (MonadLogger m, MonadIO m, MonadCatch m, MonadReader env m, HasHttpManager env)
  => Path Abs Dir -- ^ stack root, e.g. ~/.stack
  -> ConfigMonoid
  -> m Config
configFromConfigMonoid configStackRoot ConfigMonoid{..} = do
     let configDocker = case configMonoidDockerOpts of
                 DockerOpts x -> x
         configUrls = configMonoidUrls
         configGpgVerifyIndex = fromMaybe False configMonoidGpgVerifyIndex
         configConnectionCount = fromMaybe 8 configMonoidConnectionCount
         configHideTHLoading = fromMaybe True configMonoidHideTHLoading

         -- Only place in the codebase where platform is hard-coded. In theory
         -- in the future, allow it to be configured.
         configPlatform = buildPlatform

     origEnv <- getEnvOverride configPlatform
     let configEnvOverride _ = return origEnv

     platform <- runReaderT platformRelDir configPlatform

     configLocalGHCs <-
        case configPlatform of
            Platform _ Windows -> do
                progsDir <- getWindowsProgsDir configStackRoot origEnv
                return $ progsDir </> $(mkRelDir "stack") </> platform
            _ -> return $ configStackRoot </> $(mkRelDir "ghc") </> platform

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
instance HasUrls MiniConfig
instance HasPlatform MiniConfig

-- | Load the configuration, using current directory, environment variables,
-- and defaults as necessary.
loadConfig :: (MonadLogger m,MonadIO m,MonadCatch m,MonadReader env m,HasHttpManager env)
           => m Config
loadConfig = do
    stackRoot <- determineStackRoot
    extraConfigs <- getExtraConfigs stackRoot
    mproject <- getProjectConfig
    let fps = maybe id (:) mproject extraConfigs
    configs <- mapM loadConfigMonoid fps
    configFromConfigMonoid stackRoot $ mconcat configs

-- | Load the configuration, like in @loadConfig@, but gets project-specific
-- values.
loadBuildConfig :: (MonadLogger m,MonadIO m,MonadCatch m,MonadReader env m,HasHttpManager env)
                => m BuildConfig
loadBuildConfig = do
    stackRoot <- determineStackRoot
    extraConfigs <- getExtraConfigs stackRoot >>= mapM loadConfigMonoid
    mproject <- loadProjectConfig
    config <- configFromConfigMonoid stackRoot $ mconcat $
        case mproject of
            Nothing -> extraConfigs
            Just (_, _, config) -> config : extraConfigs

    env <- ask
    let miniConfig = MiniConfig (getHttpManager env) config
    (project, stackYamlFP) <- case mproject of
        Just (project, fp, _) -> return (project, fp)
        Nothing -> do
            currDir <- getWorkingDir
            (r, flags) <- runReaderT (getDefaultResolver currDir) miniConfig
            let dest = currDir </> stackDotYaml
                dest' = toFilePath dest
            exists <- liftIO $ doesFileExist dest'
            when exists $ error "Invariant violated: in toBuildConfig's Nothing branch, and the stack.yaml file exists"
            $logInfo $ "Writing default config file to: " <> T.pack dest'
            let p = Project
                    { projectPackages = ["."]
                    , projectExtraDeps = Map.empty
                    , projectFlags = flags
                    , projectResolver = r
                    }
            liftIO $ Yaml.encodeFile dest' p
            return (p, dest)

    ghcVersion <-
        case projectResolver project of
            ResolverSnapshot snapName -> do
                mbp <- runReaderT (loadMiniBuildPlan snapName Map.empty) miniConfig
                return $ mbpGhcVersion mbp
            ResolverGhc m -> return $ fromMajorVersion m

    let root = parent stackYamlFP
    packages' <- mapM (resolveDir root) (projectPackages project)
    let packages = S.fromList packages'

    return BuildConfig
        { bcConfig = config
        , bcResolver = projectResolver project
        , bcGhcVersion = ghcVersion
        , bcPackages = packages
        , bcExtraDeps = projectExtraDeps project
        , bcRoot = root
        , bcFlags = projectFlags project
        }

-- | Get the stack root, e.g. ~/.stack
determineStackRoot :: (MonadIO m, MonadThrow m) => m (Path Abs Dir)
determineStackRoot = do
    env <- liftIO getEnvironment
    root <-
        case lookup "STACK_ROOT" env of
            Nothing -> liftIO $ getAppUserDataDirectory "stack"
            Just x -> return x
    parseAbsDir root

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
    filterM (liftIO . doesFileExist . toFilePath)
        $ fromMaybe (stackRoot </> stackDotYaml) mstackConfig
        : maybe [] return (mstackGlobalConfig <|> defaultStackGlobalConfig)

-- | Load the value of a 'ConfigMonoid' from the given file.
loadConfigMonoid :: MonadIO m => Path Abs File -> m ConfigMonoid
loadConfigMonoid path =
    liftIO $ Yaml.decodeFileEither (toFilePath path)
         >>= either throwM return

-- | Get the location of the project config file, if it exists
getProjectConfig :: (MonadIO m, MonadThrow m, MonadLogger m)
                 => m (Maybe (Path Abs File))
getProjectConfig = do
    env <- liftIO getEnvironment
    case lookup "STACK_YAML" env of
        Just fp -> do
            $logInfo "Getting project config file from STACK_YAML environment"
            liftM Just $ parseAbsFile fp
        Nothing -> do
            currDir <- getWorkingDir
            search currDir
  where
    search dir = do
        let fp = dir </> stackDotYaml
            fp' = toFilePath fp
        $logDebug $ "Checking for project config at: " <> T.pack fp'
        exists <- liftIO $ doesFileExist fp'
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
                  => m (Maybe (Project, Path Abs File, ConfigMonoid))
loadProjectConfig = do
    mfp <- getProjectConfig
    case mfp of
        Just fp -> do
            currDir <- getWorkingDir
            $logDebug $ "Loading project config file " <>
                        T.pack (maybe (toFilePath fp) toFilePath (stripDir currDir fp))
            load fp
        Nothing -> do
            $logInfo $ "No project config file found, using defaults"
            return Nothing
  where
    load fp = do
        (project, config) <-
            liftIO (Yaml.decodeFileEither (toFilePath fp))
               >>= either throwM return
        return $ Just (project, fp, config)

-- | The filename used for the stack config file.
stackDotYaml :: Path Rel File
stackDotYaml = $(mkRelFile "stack.yaml")
