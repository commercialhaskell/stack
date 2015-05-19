{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
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
  , configInDocker
  , configBinPaths
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
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import           Network.HTTP.Client.Conduit (HasHttpManager, getHttpManager, Manager)
import           Path
import           Path.Find
import           Stack.BuildPlan
import           Stack.Package
import           Stack.Types
import           System.Directory
import           System.Environment
import           System.FilePath (searchPathSeparator, (<.>))
import           System.Process

-- Some examples of stack.yaml

-- (example 1)
-- build:
--   in: docker

-- (example 2)
-- build:
--   with: lts-2

-- (example 3)
-- build:
--   with: nightly-2015-06-01

-- (example 4)
-- build:
--   with:
--     ghc: /home/dan/ghc/ghc-6.12/bin
--     cabal: /home/dan/.cabal/bin
--   in:
--     sandbox: .cabal-sandbox

-- (example 5)
-- build:
--   with:
--     ghc: '7.10'
--     cabal: detect

-- An uninterpreted representation of configuration options.
-- Configurations may be "cascaded" using mappend (left-biased).
data ConfigMonoid =
  ConfigMonoid
    { configMonoidStackRoot      :: !(Maybe (Path Abs Dir))
    , configMonoidBuildOpts      :: !BuildOpts
    , configMonoidDockerOpts     :: !DockerOpts
    , configMonoidDir            :: !(Maybe (Path Abs Dir))
    , configMonoidUrls           :: !(Map Text Text)
    , configMonoidGpgVerifyIndex :: !(Maybe Bool)
    , configMonoidResolver       :: !(Maybe Resolver)
    , configMonoidInstallDeps    :: !(Maybe Bool)
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
    { configMonoidStackRoot = Nothing
    , configMonoidBuildOpts = mempty
    , configMonoidDockerOpts = mempty
    , configMonoidDir = Nothing
    , configMonoidUrls = mempty
    , configMonoidGpgVerifyIndex = Nothing
    , configMonoidResolver = Nothing
    , configMonoidInstallDeps = Nothing
    }
  mappend l r = ConfigMonoid
    { configMonoidStackRoot = configMonoidStackRoot l <|> configMonoidStackRoot r
    , configMonoidBuildOpts = appendOf configMonoidBuildOpts l r
    , configMonoidDockerOpts = configMonoidDockerOpts l <> configMonoidDockerOpts r
    , configMonoidDir = configMonoidDir l <|> configMonoidDir r
    , configMonoidUrls = configMonoidUrls l <> configMonoidUrls r
    , configMonoidGpgVerifyIndex = configMonoidGpgVerifyIndex l <|> configMonoidGpgVerifyIndex r
    , configMonoidResolver = configMonoidResolver l <|> configMonoidResolver r
    , configMonoidInstallDeps = configMonoidInstallDeps l <|> configMonoidInstallDeps r
    }

instance FromJSON (Path Abs Dir -> ConfigMonoid) where
  parseJSON =
    withObject "ConfigMonoid" $
    \obj ->
      do configMonoidStackRoot <- obj .:? "stack-root"
                              -- FIXME shouldn't we allow stack-root to be a
                              -- relative directory as well?
                              >>= maybe
                                (return Nothing)
                                (either (fail . show) (return . Just) . parseAbsDir)
         getBuildOpts <- obj .:? "build" .!= mempty
         getTheDocker <- obj .:? "docker"
         configMonoidUrls <- obj .:? "urls" .!= mempty
         configMonoidGpgVerifyIndex <- obj .:? "gpg-verify-index"
         configMonoidResolver <- obj .:? "resolver"
         configMonoidInstallDeps <- obj .:? "install-dependencies"
         return (\parentDir ->
                   let configMonoidBuildOpts = getBuildOpts parentDir
                       configMonoidDir =
                         Just parentDir
                       configMonoidDockerOpts = DockerOpts (fmap ($ parentDir) getTheDocker)
                   in ConfigMonoid {..})

data BuildOpts =
  BuildOpts
    { buildOptsIn :: !(Maybe BuildIn)
    , buildOptsWith :: !(Maybe BuildWith)
    , buildOptsPackages :: !(Set (Path Abs Dir))
    , buildOptsFlags :: !(Map FlagName Bool)
    , buildOptsPackageFlags :: !(Map PackageName (Map FlagName Bool))
    }
  deriving Show

instance Monoid BuildOpts where
  mempty = BuildOpts
   { buildOptsIn           = Nothing
   , buildOptsWith         = Nothing
   , buildOptsPackages     = mempty
   , buildOptsFlags        = mempty
   , buildOptsPackageFlags = mempty
   }
  mappend l r = BuildOpts
    { buildOptsIn = altOf buildOptsIn l r
    , buildOptsWith = altOf buildOptsWith l r
    , buildOptsPackages = buildOptsPackages l <> buildOptsPackages r
    , buildOptsFlags = buildOptsFlags l <> buildOptsFlags r
    , buildOptsPackageFlags = buildOptsPackageFlags l <> buildOptsPackageFlags r
    }

instance FromJSON (Path Abs Dir -> BuildOpts) where
  parseJSON =
    withObject "BuildOpts" $
    \obj ->
      do buildOptsIn <- obj .:? "in"
         buildOptsWith <- obj .:? "with"
         packages <-
           do ps <- obj .:? "packages" .!= []
              fmap S.fromList
                   (mapM (\x ->
                            if x == "."
                               then return (Left ())
                               else fmap Right
                                         (either (fail . ("Unable to parse relative directory location: " ++) .
                                                         show)
                                                 return
                                                 (parseRelDir x)))
                         ps)
         buildOptsFlags <-
           fmap (fromMaybe mempty)
                (obj .:? "flags")
         buildOptsPackageFlags <-
           fmap (M.fromList .
                 mapMaybe (\(name,x) ->
                             do name' <- parsePackageNameFromString name
                                return (name',x)) .
                 M.toList)
                (fmap (fromMaybe mempty)
                      (obj .:? "package-flags"))
         return (\parentDir ->
                   let buildOptsPackages =
                         S.map (\x ->
                                  case x of
                                    Left () -> parentDir
                                    Right p ->
                                      (parentDir </> p))
                               packages
                   in BuildOpts {..})

newtype BuildIn = BuildIn Text
  deriving Show

instance FromJSON BuildIn where
  parseJSON = withText "BuildIn" $ \t ->
    pure $ BuildIn t

data BuildWith
  = BuildWithSnapshot !SnapshotEnv
  | BuildWithCustom !CustomEnvBins
  deriving Show

data CustomEnvBins
  = CustomEnvBins !(Map Text Text)
  deriving Show

newtype SnapshotEnv = SnapshotEnv Text
  deriving Show


instance FromJSON BuildWith where
  parseJSON j =
        (BuildWithSnapshot <$> parseJSON j)
    <|> (BuildWithCustom   <$> parseJSON j)
    <|> typeMismatch "BuildWith" j

instance FromJSON SnapshotEnv where
  parseJSON = withText "SnapshotEnv" $ \t ->
    pure $ SnapshotEnv t

instance FromJSON CustomEnvBins where
  parseJSON j =
        (CustomEnvBins <$> parseJSON j)
    <|> typeMismatch "CustomEnvBins" j


-- TODO: use where.exe if on Windows?
detectGhcLocation :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m (Path Abs Dir)
detectGhcLocation = do
  whichGhc <- liftIO $ readProcess "which" ["ghc"] ""
  parent `liftM` parseAbsFile whichGhc

detectCabalLocation :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m (Path Abs Dir)
detectCabalLocation = do
  whichCabal <- liftIO $ readProcess "which" ["cabal"] ""
  parent `liftM` parseAbsFile whichCabal


detectPackageDbLocation :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m (Path Abs Dir)
detectPackageDbLocation = do
  mPackageDb <- liftIO $ parsePackageDb
  case mPackageDb of
    Just dbText -> do
      parseAbsDir $ Text.unpack dbText
    Nothing -> throwM $ NotYetImplemented "detectPackageDbLocation: no cabal.sandbox.config"


parsePackageDb :: IO (Maybe Text)
parsePackageDb = do
  cabalSandboxConfigExists <- doesFileExist "cabal.sandbox.config"
  if cabalSandboxConfigExists
    then do
      t <- Text.readFile "cabal.sandbox.config"
      let packageDbLine = Text.stripPrefix "package-db: "
      return $ listToMaybe $ mapMaybe packageDbLine $ Text.lines t
    else
      return Nothing

getEnvFileConfigMonoid :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m ConfigMonoid
getEnvFileConfigMonoid = liftIO (lookupEnv "STACKAGE_CONFIG") >>= \case
  Just (parseAbsFile -> Just configFilePath) -> getFileConfigMonoid configFilePath
  _ -> return mempty

getGlobalConfigMonoid :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m ConfigMonoid
getGlobalConfigMonoid = liftIO (lookupEnv "STACKAGE_GLOBAL_CONFIG") >>= \case
  Just (parseAbsFile -> Just configFilePath) -> getFileConfigMonoid configFilePath
  _ -> getFileConfigMonoid defaultStackGlobalConfig

-- TODO: What about Windows?
-- FIXME: This will not build on Windows. (Good!)
defaultStackGlobalConfig :: Path Abs File
defaultStackGlobalConfig = $(mkAbsFile "/etc/stack/config")

getConfigMonoid :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m ConfigMonoid
getConfigMonoid = do
  envConfigMonoid <- getEnvConfigMonoid
  envFileConfigMonoid <- getEnvFileConfigMonoid
  localConfigMonoid <- getLocalConfigMonoid

  let config0 = envConfigMonoid <> envFileConfigMonoid <> localConfigMonoid

  configRootDef <- case configMonoidStackRoot config0 of
    -- Root def already present, don't do the work to find the default
    Just _ -> return mempty
    -- No root def so far, do the work to find the default
    Nothing -> do
      dir <- liftIO $ getAppUserDataDirectory "stack" >>= parseAbsDir
      return mempty { configMonoidStackRoot = Just dir }

  -- FIXME from Michael: the following is not good practice. Let the types prove that there is no partiality here by returning stackRoot from above
  let config1 = config0 <> configRootDef
      -- The above ensures this is safe
      Just stackRoot = configMonoidStackRoot config1

  rootConfig <- getFileConfigMonoid (stackRoot </> stackDotYaml)
  globalConfigMonoid <- getGlobalConfigMonoid
  return $ config1 <> rootConfig <> globalConfigMonoid

-- | Get the current directory's @stack.config@ file.
getLocalConfigMonoid :: (MonadLogger m,MonadIO m,MonadThrow m)
                     => m ConfigMonoid
getLocalConfigMonoid =
  do pwd <-
       liftIO (getCurrentDirectory >>= parseAbsDir)
     $logDebug ("Current directory is: " <>
                T.pack (show pwd))
     let filePath = pwd </> stackDotYaml
     $logDebug ("Reading from config file: " <>
                T.pack (show filePath))
     getFileConfigMonoid filePath

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

getEnvBuildOpts :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m BuildOpts
getEnvBuildOpts = liftIO $ do
  buildIn <- lookupEnvText "STACK_BUILD_IN"
  return mempty
    { buildOptsIn = BuildIn <$> buildIn
    }

-- TODO: support build opts in the env
-- Loads stack.config options from environment variables.
getEnvConfigMonoid :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m ConfigMonoid
getEnvConfigMonoid = do
  dir <- liftIO (lookupEnv "STACK_ROOT" >>= maybe (return Nothing) (fmap Just . parseAbsDir))
  buildOpts <- getEnvBuildOpts
  return mempty
    { configMonoidStackRoot = dir
    , configMonoidBuildOpts = buildOpts
    , configMonoidDir = dir
    }

-- Interprets ConfigMonoid options.
configFromConfigMonoid :: (MonadLogger m, MonadIO m, MonadCatch m, MonadReader env m, HasHttpManager env)
  => ConfigMonoid -> m Config
configFromConfigMonoid ConfigMonoid{..} =
  do configStackRoot <-
       maybe (error "No stack root.") return configMonoidStackRoot -- FIXME: This is not good.
     let BuildOpts{..} = configMonoidBuildOpts
     configBuildIn <- resolveBuildIn buildOptsIn
     (configGhcBinLocation,configCabalBinLocation,configPkgDbLocation) <-
       resolveBuildWith configStackRoot buildOptsWith
     configDocker <-
       return (case configMonoidDockerOpts of
                 DockerOpts x -> x)
     configFlags <- return buildOptsFlags
     configPackageFlags <- return buildOptsPackageFlags
     configPackages <- return buildOptsPackages
     configDir <-
       maybe (error "Couldn't determine config dir.") return configMonoidDir -- FIXME: Proper exception.
     let configUrls = configMonoidUrls
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
            -- FIXME should we assume the current directory for the default
            -- resolver like this?
            currDir <- liftIO $ canonicalizePath "." >>= parseAbsDir
            r <- runReaderT (getDefaultResolver currDir) miniConfig
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
  configMonoid <- getConfigMonoid
  configFromConfigMonoid configMonoid

resolveBuildIn :: (MonadLogger m, MonadIO m, MonadThrow m)
  => (Maybe BuildIn) -> m Text
resolveBuildIn = return . maybe defaultBuildIn (\(BuildIn t) -> t)

defaultBuildIn :: Text
defaultBuildIn = "sandbox"

resolveBuildWith :: (MonadLogger m,MonadIO m,MonadThrow m)
                 => Path Abs Dir
                 -> (Maybe BuildWith)
                 -> m (Path Abs Dir,Path Abs Dir,Path Abs Dir) -- (ghc, cabal, package-db)
resolveBuildWith sr mbw = resolveBuildWith' sr $ fromMaybe defaultBuildWith mbw

defaultBuildWith :: BuildWith
defaultBuildWith = BuildWithCustom (CustomEnvBins mempty)

-- TODO: BuildWithSnapshot
-- TODO: handle vague things like "ghc: 7.10"
resolveBuildWith' :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Path Abs Dir -> BuildWith -> m (Path Abs Dir, Path Abs Dir, Path Abs Dir)
                                             -- (ghc,          cabal,        package-db)
resolveBuildWith' _stackRoot (BuildWithSnapshot _) =
  throwM $ NotYetImplemented "resolveBuildWith': BuildWithSnapshot"
resolveBuildWith' _stackRoot (BuildWithCustom (CustomEnvBins bins)) = do
  let ghcDirText = fromMaybe "detect" $ Map.lookup "ghc" bins
  let cabalDirText = fromMaybe "detect" $ Map.lookup "cabal" bins
  ghcBinLoc <- case ghcDirText of
    "detect" -> detectGhcLocation
    _ -> parseAbsDir $ Text.unpack ghcDirText
  cabalBinLoc <- case cabalDirText of
    "detect" -> detectCabalLocation
    _ -> parseAbsDir $ Text.unpack cabalDirText
  packageDbLoc <- detectPackageDbLocation
  return (ghcBinLoc, cabalBinLoc, packageDbLoc)

-- | The filename used for the stack config file.
stackDotYaml :: Path Rel File
stackDotYaml = $(mkRelFile "stack.yaml")

-- | Get the binary locations as a string that could be used in the PATH
configBinPaths :: Config -> String
configBinPaths config =
   toFilePath (configGhcBinLocation config) <>
   [searchPathSeparator] <>
   toFilePath (configCabalBinLocation config)

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
