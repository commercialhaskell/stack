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
  , toBuildConfig
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
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import           Distribution.System (OS (Windows), buildOS)
import           Network.HTTP.Client.Conduit (HasHttpManager, getHttpManager, Manager)
import           Path
import           Path.IO
import           Stack.BuildPlan
import           Stack.Package
import           Stack.Types
import           System.Directory
import           System.Environment
import           System.Process.Read (getEnvOverride, EnvOverride (..))

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
    , configMonoidResolver       :: !(Maybe Resolver)
    -- ^ How we resolve which dependencies to use
    }
  deriving Show

-- | Get the default resolver value
getDefaultResolver :: (MonadIO m, MonadCatch m, MonadReader env m, HasConfig env, HasUrls env, HasHttpManager env, MonadLogger m)
                   => Path Abs Dir
                   -> m Resolver
getDefaultResolver dir = do
    ecabalfp <- try $ getCabalFileName dir
    msnap <- case ecabalfp of
        Left e -> do
            $logWarn $ T.pack $ show (e :: PackageException)
            return Nothing
        Right cabalfp -> do
            gpd <- readPackageUnresolved cabalfp
            mpair <- findBuildPlan cabalfp gpd
            return mpair
    case msnap of
        Just (snap, _FIXMEflags) -> return $ ResolverSnapshot snap
        Nothing -> return $ ResolverSnapshot $ LTS 2 9 -- FIXME

-- | Dummy type to support this monoid business.
newtype DockerOpts = DockerOpts (Maybe Docker)
  deriving (Show)

-- | Left-biased instance.
instance Monoid DockerOpts where
  mappend (DockerOpts Nothing) x = x
  mappend x (DockerOpts Nothing) = x
  mappend x _ = x
  mempty = DockerOpts Nothing

instance Monoid ConfigMonoid where
  mempty = ConfigMonoid
    { configMonoidDockerOpts = mempty
    , configMonoidUrls = mempty
    , configMonoidGpgVerifyIndex = Nothing
    , configMonoidResolver = Nothing
    }
  mappend l r = ConfigMonoid
    { configMonoidDockerOpts = configMonoidDockerOpts l <> configMonoidDockerOpts r
    , configMonoidUrls = configMonoidUrls l <> configMonoidUrls r
    , configMonoidGpgVerifyIndex = configMonoidGpgVerifyIndex l <|> configMonoidGpgVerifyIndex r
    , configMonoidResolver = configMonoidResolver l <|> configMonoidResolver r
    }

instance FromJSON ConfigMonoid where
  parseJSON =
    withObject "ConfigMonoid" $
    \obj ->
      do getTheDocker <- obj .:? "docker"
         configMonoidUrls <- obj .:? "urls" .!= mempty
         configMonoidGpgVerifyIndex <- obj .:? "gpg-verify-index"
         configMonoidResolver <- obj .:? "resolver"
         let configMonoidDockerOpts = DockerOpts getTheDocker
         return ConfigMonoid {..}

-- | A project is a collection of packages. We can have multiple stack.yaml
-- files, but only one of them may contain project information.
data Project = Project
    { projectRoot :: !(Path Abs Dir)
    -- ^ The directory containing the project's stack.yaml
    , projectPackages :: ![FilePath]
    -- ^ Components of the package list which refer to local directories
    --
    -- Note that we use @FilePath@ and not @Path@s. The goal is: first parse
    -- the value raw, and then use @canonicalizePath@ and @parseAbsDir@.
    , projectExtraDeps :: !(Map PackageName Version)
    -- ^ Components of the package list referring to package/version combos,
    -- see: https://github.com/fpco/stack/issues/41
    , projectFlags :: !(Map PackageName (Map FlagName Bool))
    -- ^ Per-package flag overrides
    , projectConfigExists :: !Bool
    -- ^ If 'True', then we actually loaded this from a real config file. If
    -- 'False', then we just made up a default.
    , projectConfigMonoid :: !ConfigMonoid
    -- ^ Extra config values found
    }
  deriving Show

instance FromJSON (Path Abs Dir -> Project) where -- FIXME get rid of Path Abs Dir
    parseJSON = withObject "Project" $ \o -> do
        dirs <- o .:? "packages" .!= ["."]
        extraDeps' <- o .:? "extra-deps" .!= []
        extraDeps <-
            case partitionEithers $ goDeps extraDeps' of
                ([], x) -> return $ Map.fromList x
                (errs, _) -> fail $ unlines errs

        flags <- o .:? "flags" .!= mempty
        config <- parseJSON $ Object o
        return $ \root -> Project
            { projectRoot = root
            , projectPackages = dirs
            , projectExtraDeps = extraDeps
            , projectFlags = flags
            , projectConfigExists = True
            , projectConfigMonoid = config
            }
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
  -> Project
  -> ConfigMonoid
  -> m Config
configFromConfigMonoid configStackRoot Project{..} ConfigMonoid{..} = do
     let configDocker = case configMonoidDockerOpts of
                 DockerOpts x -> x
         configFlags = projectFlags
         configExtraDeps = projectExtraDeps
         configDir = projectRoot
         configUrls = configMonoidUrls
         configGpgVerifyIndex = fromMaybe False configMonoidGpgVerifyIndex

     origEnv <- getEnvOverride
     let configEnvOverride _ = origEnv

     configLocalGHCs <-
        case buildOS of
            Windows -> do
                progsDir <- getWindowsProgsDir configStackRoot origEnv
                return $ progsDir </> $(mkRelDir "stack")
            _ -> return $ configStackRoot </> $(mkRelDir "ghc")

     configPackages' <- mapM (resolveDir projectRoot) projectPackages
     let configPackages = S.fromList configPackages'
         configMaybeResolver = configMonoidResolver
     return Config {..}

-- | Get the directory on Windows where we should install extra programs. For
-- more information, see discussion at:
-- https://github.com/fpco/minghc/issues/43#issuecomment-99737383
getWindowsProgsDir :: MonadThrow m
                   => Path Abs Dir
                   -> EnvOverride
                   -> m (Path Abs Dir)
getWindowsProgsDir stackRoot (EnvOverride m) =
    case Map.lookup "LOCALAPPDATA" m of
        Just t -> do
            lad <- parseAbsDir $ T.unpack t
            return $ lad </> $(mkRelDir "Programs")
        Nothing -> return $ stackRoot </> $(mkRelDir "Programs")

toBuildConfig :: (HasHttpManager env, MonadReader env m, MonadCatch m, MonadIO m, MonadLogger m)
              => Config
              -> m BuildConfig
toBuildConfig config = do
    env <- ask
    let miniConfig = MiniConfig (getHttpManager env) config
    resolver <- case configMaybeResolver config of
        Just r -> return r
        Nothing -> do
            r <- runReaderT (getDefaultResolver $ configDir config) miniConfig
            let dest = toFilePath $ configDir config </> stackDotYaml
            exists <- liftIO $ doesFileExist dest
            if exists
                then do
                    $logWarn $ T.concat
                        [ "No resolver value found in your config files. "
                        , "Without a value stated, your build will be slower and unstable. "
                        , "Please consider adding the following line to your config file:"
                        ]
                    $logWarn $ "    resolver: " <> renderResolver r
                else do
                    $logInfo $ "Writing default config file to: " <> T.pack dest
                    liftIO $ Yaml.encodeFile dest $ object
                        [ "packages" .= ["." :: Text]
                        , "resolver" .= renderResolver r
                        ]
            return r

    ghcVersion <-
        case resolver of
            ResolverSnapshot snapName -> do
                mbp <- runReaderT (loadMiniBuildPlan snapName Map.empty) miniConfig
                return $ mbpGhcVersion mbp
            ResolverGhc x y -> return $ fromMajorVersion x y

    return BuildConfig
        { bcConfig = config
        , bcResolver = resolver
        , bcGhcVersion = ghcVersion
        }

data MiniConfig = MiniConfig Manager Config
instance HasConfig MiniConfig where
    getConfig (MiniConfig _ c) = c
instance HasStackRoot MiniConfig
instance HasHttpManager MiniConfig where
    getHttpManager (MiniConfig man _) = man
instance HasUrls MiniConfig

-- | Load the configuration, using current directory, environment variables,
-- and defaults as necessary.
loadConfig :: (MonadLogger m,MonadIO m,MonadCatch m,MonadReader env m,HasHttpManager env)
           => m Config
loadConfig = do
    env <- liftIO getEnvironment
    stackRoot <- (>>= parseAbsDir) $
        case lookup "STACK_ROOT" env of
            Nothing -> liftIO $ getAppUserDataDirectory "stack"
            Just x -> return x

    extraConfigs <- getExtraConfigs stackRoot >>= mapM loadConfigMonoid
    project <- loadProjectConfig
    let config = mconcat (projectConfigMonoid project:extraConfigs)
    configFromConfigMonoid stackRoot project config

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

-- | Find the project config file location, respecting environment variables
-- and otherwise traversing parents. If no config is found, we supply a default
-- based on current directory.
loadProjectConfig :: (MonadIO m, MonadThrow m, MonadLogger m) => m Project
loadProjectConfig = do
    env <- liftIO getEnvironment
    case lookup "STACK_YAML" env of
        Just fp -> do
            $logInfo "Getting project config file from STACK_YAML environment"
            parseAbsFile fp >>= load
        Nothing -> do
            currDir <- getWorkingDir
            mfp <- search currDir
            case mfp of
                Just fp -> do
                    $logDebug $ "Loading project config file " <>
                                T.pack (maybe (toFilePath fp) toFilePath (stripDir currDir fp))
                    load fp
                Nothing -> do
                    $logInfo $ "No project config file found, using defaults"
                    return Project
                        { projectRoot = currDir
                        , projectPackages = ["."]
                        , projectExtraDeps = mempty
                        , projectFlags = mempty
                        , projectConfigExists = False
                        , projectConfigMonoid = mempty
                        }
  where
    load fp = do
        mkProject <-
            liftIO (Yaml.decodeFileEither (toFilePath fp))
               >>= either throwM return
        return $ mkProject $ parent fp

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

-- | The filename used for the stack config file.
stackDotYaml :: Path Rel File
stackDotYaml = $(mkRelFile "stack.yaml")
