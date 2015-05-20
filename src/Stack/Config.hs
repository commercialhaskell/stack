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
  ( Config(..)
  , ConfigException(..)
  , Docker(..)
  , Mount(..)
  , HasConfig (..)
  , MonadReader
  , ask
  , loadConfig
  , getDocker
  , NotYetImplemented(..)
  , dockerRepoOwnerArgName
  , dockerRepoArgName
  , dockerRepoSuffixArgName
  , dockerImageTagArgName
  , dockerImageArgName
  , dockerRegistryLoginArgName
  , dockerRegistryUsernameArgName
  , dockerRegistryPasswordArgName
  , dockerAutoPullArgName
  , dockerDetachArgName
  , dockerPersistArgName
  , dockerContainerNameArgName
  , dockerRunArgsArgName
  , dockerMountArgName
  , dockerPassHostArgName
  , configPackageIndex
  , configPackageIndexGz
  , configPackageTarball
  , askConfig
  , askLatestSnapshotUrl
  , askPackageIndexGitUrl
  , askPackageIndexHttpUrl
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger hiding (Loc)
import           Control.Monad.Reader (MonadReader, ask, runReaderT)
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString as S (readFile)
import           Data.Either (partitionEithers)
import           Data.Map (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import           Network.HTTP.Client.Conduit (HasHttpManager, getHttpManager, Manager)
import           Path
import           Path.Find
import           Stack.BuildPlan
import           Stack.Package
import           Stack.Types
import           System.Directory
import           System.Environment
import           System.FilePath ((<.>))

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
    , configMonoidInstallDeps    :: !(Maybe Bool)
    -- ^ Whether or not we should install dependencies. When using Docker,
    -- default is False, otherwise default is True
    }
  deriving Show

-- | Get the default resolver value
getDefaultResolver :: (MonadIO m, MonadCatch m, MonadReader env m, HasStackRoot env, HasUrls env, HasHttpManager env, MonadLogger m)
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
        Just (snap, _FIXMEflagsIgnored) -> return $ ResolverSnapshot snap
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

-- Note that the Maybe mappend upon two Justs
-- will mappend their contents.
appendOf :: Monoid b => (a -> b) -> a -> a -> b
appendOf getter l r = getter l <> getter r

-- For when you want to left-prefer,
-- rather than inner mappend.
altOf :: (a -> Maybe b) -> a -> a -> Maybe b
altOf getter l r = getter l <|> getter r

instance Monoid ConfigMonoid where
  mempty = ConfigMonoid
    { configMonoidDockerOpts = mempty
    , configMonoidUrls = mempty
    , configMonoidGpgVerifyIndex = Nothing
    , configMonoidResolver = Nothing
    , configMonoidInstallDeps = Nothing
    }
  mappend l r = ConfigMonoid
    { configMonoidDockerOpts = configMonoidDockerOpts l <> configMonoidDockerOpts r
    , configMonoidUrls = configMonoidUrls l <> configMonoidUrls r
    , configMonoidGpgVerifyIndex = configMonoidGpgVerifyIndex l <|> configMonoidGpgVerifyIndex r
    , configMonoidResolver = configMonoidResolver l <|> configMonoidResolver r
    , configMonoidInstallDeps = configMonoidInstallDeps l <|> configMonoidInstallDeps r
    }

instance FromJSON (Path Abs Dir -> ConfigMonoid) where
  parseJSON =
    withObject "ConfigMonoid" $
    \obj ->
      do getTheDocker <- obj .:? "docker"
         configMonoidUrls <- obj .:? "urls" .!= mempty
         configMonoidGpgVerifyIndex <- obj .:? "gpg-verify-index"
         configMonoidResolver <- obj .:? "resolver"
         configMonoidInstallDeps <- obj .:? "install-dependencies"
         return (\parentDir ->
                   let configMonoidDockerOpts = DockerOpts (fmap ($ parentDir) getTheDocker)
                   in ConfigMonoid {..})

-- | A project is a collection of packages. We can have multiple stack.yaml
-- files, but only one of them may contain project information.
data Project = Project
    { projectRoot :: !(Path Abs Dir)
    -- ^ The directory containing the project's stack.yaml
    , projectPackagesPath :: !(Set (Path Abs Dir))
    -- ^ Components of the package list which refer to local directories
    , projectPackagesIdent :: !(Set PackageIdentifier)
    -- ^ Components of the package list referring to package/version combos,
    -- see: https://github.com/fpco/stack/issues/41
    , projectGlobalFlags :: !(Map FlagName Bool)
    -- ^ Applied to all packages
    , projectPackageFlags :: !(Map PackageName (Map FlagName Bool))
    -- ^ Applied to an individual package, overriding global flags
    , projectConfigExists :: !Bool
    -- ^ If 'True', then we actually loaded this from a real config file. If
    -- 'False', then we just made up a default.
    , projectConfigMonoid :: !ConfigMonoid
    -- ^ Extra config values found
    }
  deriving Show

instance FromJSON (Path Abs Dir -> Project) where
    parseJSON = withObject "Project" $ \o -> do
        ps <- o .:? "packages" .!= ["."]
        eps <- forM ps $ \p ->
            case parsePackageIdentifier $ encodeUtf8 p of
                Right pi -> return $ Left pi
                Left e1 ->
                    case parseRelDir $ T.unpack p of
                        Right d -> return $ Right d
                        Left e2 -> fail $ "Could not parse as reldir or package identifier: " ++ T.unpack p ++ ", " ++ show (e1, e2)
        let (idents, dirs) = partitionEithers eps
        gf <- o .:? "flags" .!= mempty
        pf <- o .:? "package-flags" .!= mempty
        mkConfig <- parseJSON $ Object o
        return $ \root -> Project
            { projectRoot = root
            , projectPackagesPath = S.fromList $ map (root </>) dirs
            , projectPackagesIdent = S.fromList idents
            , projectGlobalFlags = gf
            , projectPackageFlags = pf
            , projectConfigExists = True
            , projectConfigMonoid = mkConfig root
            }

-- TODO: What about Windows?
-- FIXME: This will not build on Windows. (Good!)
defaultStackGlobalConfig :: Path Abs File
defaultStackGlobalConfig = $(mkAbsFile "/etc/stack/config")

-- | Parse configuration from the specified file, or return an empty
-- config if it doesn't exist.
getFileConfigMonoid :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Path Abs File -> m ConfigMonoid
getFileConfigMonoid configFilePath =
  do exists <-
       liftIO (doesFileExist (toFilePath configFilePath))
     if exists
        then do mconf <-
                  liftM Yaml.decodeEither (liftIO (S.readFile (toFilePath configFilePath)))
                case mconf of
                  Right c ->
                    $logDebug ("Parsed config: " <>
                               T.pack (show (c dir)))
                  Left err ->
                    $logWarn ("Parsing config file failed at " <>
                                   T.pack (show configFilePath) <>
                                   ": " <>
                                   T.pack err)
                return $
                  either (const mempty) ($ dir) mconf
        else do $logDebug ("No config file exists at " <>
                           T.pack (show configFilePath))
                return mempty
  where dir = parent configFilePath

lookupEnvText :: String -> IO (Maybe Text)
lookupEnvText var = fmap Text.pack <$> lookupEnv var

-- Interprets ConfigMonoid options.
configFromConfigMonoid :: (MonadLogger m, MonadIO m, MonadCatch m, MonadReader env m, HasHttpManager env)
  => Path Abs Dir -- ^ stack root, e.g. ~/.stack
  -> Project
  -> ConfigMonoid
  -> m Config
configFromConfigMonoid configStackRoot Project{..} ConfigMonoid{..} =
  do let configDocker = case configMonoidDockerOpts of
                 DockerOpts x -> x
         configFlags = projectGlobalFlags
         configPackageFlags = projectPackageFlags
         configPackagesPath = projectPackagesPath
         configPackagesIdent = projectPackagesIdent
         configDir = projectRoot
         configUrls = configMonoidUrls
         configGpgVerifyIndex = fromMaybe False configMonoidGpgVerifyIndex
         configInstallDeps =
            case configMonoidInstallDeps of
                Just b -> b
                Nothing ->
                    case configMonoidDockerOpts of
                        DockerOpts Nothing -> True
                        DockerOpts (Just _) -> False

     env <- ask
     let miniConfig = MiniConfig (getHttpManager env) configStackRoot configUrls
     configResolver <- case configMonoidResolver of
        Nothing -> do
            r <- runReaderT (getDefaultResolver configDir) miniConfig
            $logWarn $ T.concat
                [ "No resolver value found in your config files. "
                , "Without a value stated, your build will be slower and unstable. "
                , "Please consider adding the following line to your config file:"
                ]
            $logWarn $ "resolver: " <> renderResolver r
            return r
        Just r -> return r
     configGhcVersion <-
        case configResolver of
            ResolverSnapshot snapName -> do
                bp <- runReaderT (loadBuildPlan snapName) miniConfig
                return $ siGhcVersion $ bpSystemInfo bp
     return Config {..}

data MiniConfig = MiniConfig Manager (Path Abs Dir) (Map Text Text)
instance HasStackRoot MiniConfig where
    getStackRoot (MiniConfig _ root _) = root
instance HasHttpManager MiniConfig where
    getHttpManager (MiniConfig man _ _) = man
instance HasUrls MiniConfig where
    getUrls (MiniConfig _ _ urls) = urls

-- | Get docker configuration. Currently only looks in current/parent
-- dirs, not, e.g. $HOME/.stack.
--
-- TODO: Look in other locations.
getDocker :: (MonadIO m,MonadLogger m,MonadThrow m) => m Docker
getDocker =
  do mdocker <- getDockerLocal
     case mdocker of
       Just docker -> return docker
       Nothing -> throwM ConfigNoDockerConfig

-- | Get local directory docker configuration. Searches upwards for
-- the first parent containing the config file.
getDockerLocal :: (MonadIO m,MonadLogger m,MonadThrow m)
               => m (Maybe Docker)
getDockerLocal =
  do pwd <-
       liftIO (getCurrentDirectory >>= parseAbsDir)
     mfile <- findFileUp pwd ((== stackDotYaml) . filename) Nothing
     case mfile of
       Nothing -> throwM ConfigNoFile
       Just file -> do $logDebug ("Reading from config file: " <>
                                  T.pack (show file))
                       readDockerFrom file

-- | Read a Docker config, if there is any, from the given YAML file.
readDockerFrom :: (MonadIO m,MonadThrow m)
               => Path Abs File -> m (Maybe Docker)
readDockerFrom fp =
  do result <-
       liftM Yaml.decodeEither (liftIO (S.readFile (toFilePath fp)))
     case result of
       Left err -> throwM (ConfigInvalidYaml err)
       Right (wholeValue :: Value) ->
         case Yaml.parseEither
                (\v ->
                   do o <- parseJSON v
                      mdocker <- o .:? "docker"
                      return mdocker)
                wholeValue of
           Right (Just dockerValue) ->
             case Yaml.parseEither parseJSON dockerValue of
               Left err ->
                 throwM (ConfigInvalidYaml err)
               Right docker -> return (Just (docker (parent fp)))
           _ -> return Nothing

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
        [ fromMaybe (stackRoot </> stackDotYaml) mstackConfig
        , fromMaybe defaultStackGlobalConfig mstackGlobalConfig
        ]

-- | Load the value of a 'ConfigMonoid' from the given file.
loadConfigMonoid :: MonadIO m => Path Abs File -> m ConfigMonoid
loadConfigMonoid path =
    liftIO $ Yaml.decodeFileEither (toFilePath path)
         >>= either throwM (return . ($ parent path))

-- | Find the project config file location, respecting environment variables
-- and otherwise traversing parents. If no config is found, we supply a default
-- based on current directory.
loadProjectConfig :: (MonadIO m, MonadThrow m) => m Project
loadProjectConfig = do
    env <- liftIO getEnvironment
    case lookup "STACK_YAML" env of
        Just fp -> parseAbsFile fp >>= load
        Nothing -> do
            currDir <- liftIO $ canonicalizePath "." >>= parseAbsDir
            mfp <- liftIO $ search currDir
            case mfp of
                Just fp -> load fp
  where
    load fp = do
        mkProject <-
            liftIO (Yaml.decodeFileEither (toFilePath fp))
               >>= either throwM return
        return $ mkProject $ parent fp

    search dir = do
        let fp = dir </> stackDotYaml
        exists <- doesFileExist $ toFilePath fp
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

-- | Location of the 00-index.tar file
configPackageIndex :: Config -> Path Abs File
configPackageIndex config = configStackRoot config </> $(mkRelFile "00-index.tar")

-- | Location of the 00-index.tar.gz file
configPackageIndexGz :: Config -> Path Abs File
configPackageIndexGz config = configStackRoot config </> $(mkRelFile "00-index.tar.gz")

-- | Location of a package tarball
configPackageTarball :: MonadThrow m => Config -> PackageIdentifier -> m (Path Abs File)
configPackageTarball config ident = do
    name <- parseRelDir $ packageNameString $ packageIdentifierName ident
    ver <- parseRelDir $ versionString $ packageIdentifierVersion ident
    base <- parseRelFile $ packageIdentifierString ident <.> "tar.gz"
    return $ configStackRoot config </> $(mkRelDir "packages") </> name </> ver </> base
