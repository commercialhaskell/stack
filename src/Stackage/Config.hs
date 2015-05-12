{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}


-- | The general Stackage configuration that starts everything off. This should
-- be smart to falback if there is no stackage.config, instead relying on
-- whatever files are available.
--
-- If there is no stackage.config, and there is a cabal.config, we
-- read in those constraints, and if there's a cabal.sandbox.config,
-- we read any constraints from there and also find the package
-- database from there, etc. And if there's nothing, we should
-- probably default to behaving like cabal, possibly with spitting out
-- a warning that "you should run `stk init` to make things better".
module Stackage.Config (
    Config(..)
  , configInDocker
  , Settings(..)
  , getConfig
  , NotYetImplemented(..)
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Logger hiding (Loc)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Typeable
import qualified Data.Yaml as Yaml
import Path
import System.Directory
import System.Environment
import System.Process

-- | The top-level Stackage configuration.
data Config =
  Config
    { configPkgDbLocation    :: Path Abs Dir
    , configGhcBinLocation   :: Path Abs Dir
    , configCabalBinLocation :: Path Abs Dir
    , configStackageRoot     :: Path Abs Dir
    , configStackageHost     :: String
    , configBuildIn          :: Text
    }
  deriving Show
data Settings = Settings

configInDocker :: Config -> Bool
configInDocker = (== "docker") . configBuildIn

-- TODO: eliminate occurrences of this exception.
data NotYetImplemented = NotYetImplemented Text
  deriving (Show, Typeable)
instance Exception NotYetImplemented


-- Some examples of stackage.config

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
data StackageConfig =
  StackageConfig
    { stackageConfigStackageOpts :: !StackageOpts
    , stackageConfigBuildOpts :: !BuildOpts
    }
  deriving Show

-- Note that the Maybe mappend upon two Justs
-- will mappend their contents.
appendOf :: Monoid b => (a -> b) -> a -> a -> b
appendOf getter l r = getter l <> getter r

-- For when you want to left-prefer,
-- rather than inner mappend.
altOf :: (a -> Maybe b) -> a -> a -> Maybe b
altOf getter l r = getter l <|> getter r

instance Monoid StackageConfig where
  mempty = StackageConfig
    { stackageConfigStackageOpts = mempty
    , stackageConfigBuildOpts = mempty
    }
  mappend l r = StackageConfig
    { stackageConfigStackageOpts = appendOf stackageConfigStackageOpts l r
    , stackageConfigBuildOpts = appendOf stackageConfigBuildOpts l r
    }

instance FromJSON StackageConfig where
  parseJSON = withObject "StackageConfig" $ \obj -> do
    stackageConfigStackageOpts <- obj .:? "stackage" .!= mempty
    stackageConfigBuildOpts <- obj .:? "build" .!= mempty
    return StackageConfig{..}


data StackageOpts =
  StackageOpts
    { stackageOptsRoot :: !(Maybe Text)
    , stackageOptsHost :: !(Maybe Text)
    }
  deriving Show

instance Monoid StackageOpts where
  mempty = StackageOpts
    { stackageOptsRoot = Nothing
    , stackageOptsHost = Nothing
    }
  mappend l r = StackageOpts
    { stackageOptsRoot = altOf stackageOptsRoot l r
    , stackageOptsHost = altOf stackageOptsHost l r
    }

instance FromJSON StackageOpts where
  parseJSON = withObject "StackageOpts" $ \obj -> do
    stackageOptsRoot <- obj .:? "root"
    stackageOptsHost <- obj .:? "host"
    return StackageOpts{..}

data BuildOpts =
  BuildOpts
    { buildOptsIn :: !(Maybe BuildIn)
    , buildOptsWith :: !(Maybe BuildWith)
    }
  deriving Show

instance Monoid BuildOpts where
  mempty = BuildOpts
    { buildOptsIn = Nothing
    , buildOptsWith = Nothing
    }
  mappend l r = BuildOpts
    { buildOptsIn = altOf buildOptsIn l r
    , buildOptsWith = altOf buildOptsWith l r
    }

instance FromJSON BuildOpts where
  parseJSON = withObject "BuildOpts" $ \obj -> do
    buildOptsIn <- obj .:? "in"
    buildOptsWith <- obj .:? "with"
    return BuildOpts{..}

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



--data BuildStrategy
--  = CabalSandbox CabalSandboxBuildStrategy
--  | Docker DockerBuildStrategy
--  deriving Show

--data CabalSandboxBuildStrategy
--  = BuildAgainstLTS LTSBuildStrategy
--  | BuildAgainstNightly NightlyBuildStrategy
--  | BuildAgainstSnapshot SnapshotBuildStrategy
--  | BuildAgainstCustom (CustomBuildStrategy Text)
--  deriving Show

--data DockerBuildStrategy =
--  DockerBuildStrategy
--  deriving Show

--data LTSBuildStrategy =
--  LTSBuildStrategy
--    { configLTS :: Text
--    }
--  deriving Show

--data NightlyBuildStrategy =
--  NightlyBuildStrategy
--    { configNightly :: Text
--    }
--  deriving Show

--data SnapshotBuildStrategy =
--  SnapshotBuildStrategy
--    { configSnapshot :: Text
--    }
--  deriving Show

--data CustomBuildStrategy a =
--  CustomBuildStrategy
--  { customGhcBinLocation :: a
--  , customCabalBinLocation :: a
--  , customSandboxLocation :: a
--  }
--  deriving Show

type StackageRoot = Path Abs Dir
type StackageHost = String

--instance FromJSON LTSBuildStrategy where
--  parseJSON j = parseAsText j <|> parseAsScientific j where
--    parseAsText = withText "LTSBuildStrategy" $ \t -> do
--      let configLTS = t
--      return LTSBuildStrategy{..}
--    -- allows people to not have to quote the lts field
--    parseAsScientific = withScientific "LTSBuildStrategy" $ \s -> do
--      let configLTS = Text.pack $ show s
--      return LTSBuildStrategy{..}

--instance FromJSON NightlyBuildStrategy where
--  parseJSON = withText "NightlyBuildStrategy" $ \t -> do
--    let configNightly = t
--    return NightlyBuildStrategy{..}

--instance FromJSON SnapshotBuildStrategy where
--  parseJSON = withText "SnapshotBuildStrategy" $ \t -> do
--    let configSnapshot = t
--    return SnapshotBuildStrategy{..}


--instance (a ~ Text) => FromJSON (CustomBuildStrategy a) where
--  parseJSON = withObject "CustomBuildStrategy" $ \obj -> do
--    customGhcBinLocation <- obj .: "ghc" .!= "detect"
--    customCabalBinLocation <- obj .: "cabal" .!= "detect"
--    customSandboxLocation <- obj .:? "sandbox" .!= "detect"
--    return CustomBuildStrategy{..}


--instance FromJSON DockerBuildStrategy where
--  parseJSON = withBool "DockerBuildStrategy" $ \b ->
--    if b
--      then return DockerBuildStrategy
--      else fail "docker: false"

--instance FromJSON CabalSandboxBuildStrategy where
--  parseJSON = withObject "CabalSandboxBuildStrategy" $ \obj ->
--    (BuildAgainstLTS <$> obj .: "lts") <|>
--    (BuildAgainstNightly <$> obj .: "nightly") <|>
--    (BuildAgainstSnapshot <$> obj .: "snapshot") <|>
--    (BuildAgainstCustom <$> obj .: "custom")

--parseBuildStrategy :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => StackageRoot -> CabalSandboxBuildStrategy -> m (CustomBuildStrategy (Path Abs Dir))
--parseBuildStrategy stackageRoot (BuildAgainstLTS lts) =
--  resolveLTSSnapshot stackageRoot lts >>= parseBuildStrategy stackageRoot . BuildAgainstSnapshot
--parseBuildStrategy stackageRoot (BuildAgainstNightly nightly) =
--  resolveNightlySnapshot nightly >>= parseBuildStrategy stackageRoot . BuildAgainstSnapshot
--parseBuildStrategy stackageRoot (BuildAgainstSnapshot snapshot) = do
--  customGhcBinLocation <- resolveSnapshotGhcLoc stackageRoot snapshot
--  customCabalBinLocation <- resolveSnapshotCabalLoc stackageRoot snapshot
--  customSandboxLocation <- resolveSnapshotSandboxLoc stackageRoot snapshot
--  return CustomBuildStrategy{..}
--parseBuildStrategy _stackageRoot (BuildAgainstCustom custom) = do
--  ghcBinLoc <- resolveCustomGhcLoc (customGhcBinLocation custom)
--  cabalBinLoc <- resolveCustomCabalLoc (customCabalBinLocation custom)
--  sandboxLocation <- resolveCustomSandboxLoc (customSandboxLocation custom)
--  return CustomBuildStrategy
--    { customGhcBinLocation = ghcBinLoc
--    , customCabalBinLocation = cabalBinLoc
--    , customSandboxLocation = sandboxLocation
--    }

-- TODO
-- Build strategy based on whatever ghc, cabal, and sandbox are visible.
--defaultBuildStrategy :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => m (CustomBuildStrategy (Path Abs Dir))
--defaultBuildStrategy = do
--  customGhcBinLocation <- detectGhcLocation
--  customCabalBinLocation <- detectCabalLocation
--  customSandboxLocation <- detectSandboxLocation
--  return CustomBuildStrategy{..}


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

--detectSandboxLocation :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => m (Path Abs Dir)
--detectSandboxLocation =  do
--  mPackageDb <- liftIO $ parsePackageDb
--  case mPackageDb of
--    Just dbText -> do
--      dbDir <- parseAbsDir $ Text.unpack dbText
--      return $ parentAbs dbDir
--    Nothing -> throwM $ NotYetImplemented "detectSandboxLocation: no cabal.sandbox.config"

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

getEnvFileStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m StackageConfig
getEnvFileStackageConfig = liftIO (lookupEnvText "STACKAGE_CONFIG") >>= \case
  Just file -> getFileStackageConfig file
  Nothing -> return mempty

getGlobalStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m StackageConfig
getGlobalStackageConfig = liftIO (lookupEnvText "STACKAGE_GLOBAL_CONFIG") >>= \case
  Just file -> getFileStackageConfig file
  Nothing -> getFileStackageConfig defaultStackageGlobalConfig

-- TODO: What about Windows?
defaultStackageGlobalConfig :: Text
defaultStackageGlobalConfig = "/etc/stackage/config"

getStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m StackageConfig
getStackageConfig = do
  envStackageConfig <- getEnvStackageConfig
  envFileStackageConfig <- getEnvFileStackageConfig
  localStackageConfig <- getLocalStackageConfig

  let config0 = envStackageConfig <> envFileStackageConfig <> localStackageConfig

  configRootDef <- case stackageOptsRoot (stackageConfigStackageOpts config0) of
    -- Root def already present, don't do the work to find the default
    Just _ -> return mempty
    -- No root def so far, do the work to find the default
    Nothing -> do
      dir <- liftIO $ getAppUserDataDirectory ".stackage"
      return mempty
        { stackageConfigStackageOpts = mempty
          { stackageOptsRoot = Just $ Text.pack dir
          }
        }

  let config1 = config0 <> configRootDef
      -- The above ensures this is safe
      Just stackageRoot = stackageOptsRoot (stackageConfigStackageOpts config1)

  rootConfig <- getFileStackageConfig stackageRoot
  globalStackageConfig <- getGlobalStackageConfig
  return $ config1 <> rootConfig <> globalStackageConfig

getLocalStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m StackageConfig
getLocalStackageConfig = getFileStackageConfig "stackage.config"

getFileStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Text -> m StackageConfig
getFileStackageConfig fileNameText = liftIO $ do
  let file = Text.unpack fileNameText
  exists <- doesFileExist file
  if exists
    then do
      mconf <- Yaml.decodeFile file
      return $ fromMaybe mempty mconf
    else return mempty

lookupEnvText :: String -> IO (Maybe Text)
lookupEnvText var = fmap Text.pack <$> lookupEnv var

getEnvStackageOpts :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m StackageOpts
getEnvStackageOpts = liftIO $ do
  stackageRoot <- lookupEnvText "STACKAGE_ROOT"
  stackageHost <- lookupEnvText "STACKAGE_HOST"
  return mempty
    { stackageOptsRoot = stackageRoot
    , stackageOptsHost = stackageHost
    }

getEnvBuildOpts :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m BuildOpts
getEnvBuildOpts = liftIO $ do
  buildIn <- lookupEnvText "STACKAGE_BUILD_IN"
  return mempty
    { buildOptsIn = BuildIn <$> buildIn
    }

-- TODO: support build opts in the env
-- Loads stackage.config options from environment variables.
getEnvStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m StackageConfig
getEnvStackageConfig = do
  stackageOpts <- getEnvStackageOpts
  buildOpts <- getEnvBuildOpts
  return mempty
    { stackageConfigStackageOpts = stackageOpts
    , stackageConfigBuildOpts = buildOpts
    }


-- Interprets StackageConfig options.
configFromStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => StackageConfig -> m Config
configFromStackageConfig StackageConfig{..} = do
  let StackageOpts{..} = stackageConfigStackageOpts
  configStackageHost <- resolveStackageHost stackageOptsHost
  configStackageRoot <- resolveStackageRoot stackageOptsRoot
  let BuildOpts{..} = stackageConfigBuildOpts
  configBuildIn <- resolveBuildIn buildOptsIn
  (configGhcBinLocation, configCabalBinLocation, configPkgDbLocation) <-
    resolveBuildWith configStackageRoot buildOptsWith
  return Config{..}

-- TODO: handle Settings
getConfig :: (MonadLogger m,MonadIO m,MonadThrow m)
          => Settings -> m Config
getConfig Settings = do
  stackageConfig <- getStackageConfig
  configFromStackageConfig stackageConfig

resolveBuildIn :: (MonadLogger m, MonadIO m, MonadThrow m)
  => (Maybe BuildIn) -> m Text
resolveBuildIn = return . maybe defaultBuildIn (\(BuildIn t) -> t)

defaultBuildIn :: Text
defaultBuildIn = "sandbox"

resolveBuildWith :: (MonadLogger m, MonadIO m, MonadThrow m)
  => StackageRoot -> (Maybe BuildWith) -> m (Path Abs Dir, Path Abs Dir, Path Abs Dir)
                                         -- (ghc,          cabal,        package-db)
resolveBuildWith sr mbw = resolveBuildWith' sr $ fromMaybe defaultBuildWith mbw

defaultBuildWith :: BuildWith
defaultBuildWith = BuildWithCustom (CustomEnvBins mempty)

-- TODO: BuildWithSnapshot
-- TODO: handle vague things like "ghc: 7.10"
resolveBuildWith' :: (MonadLogger m, MonadIO m, MonadThrow m)
  => StackageRoot -> BuildWith -> m (Path Abs Dir, Path Abs Dir, Path Abs Dir)
                                 -- (ghc,          cabal,        package-db)
resolveBuildWith' _stackageRoot (BuildWithSnapshot _) =
  throwM $ NotYetImplemented "resolveBuildWith': BuildWithSnapshot"
resolveBuildWith' _stackageRoot (BuildWithCustom (CustomEnvBins bins)) = do
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


--resolveLTSSnapshot :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => StackageRoot -> LTSBuildStrategy -> m SnapshotBuildStrategy
--resolveLTSSnapshot stackageRoot LTSBuildStrategy{..} = do
--  lts <- case Text.stripPrefix "lts-" configLTS of
--    Nothing -> return $ "lts-" <> configLTS
--    Just{} -> return configLTS
--  mSnapshots <- liftIO $ Yaml.decodeFile (toFilePath stackageRoot)
--  configSnapshot <- case Map.lookup lts =<< (mSnapshots :: Maybe (Map Text Text)) of
--    Nothing -> return lts
--    Just lts' -> return lts'
--  return SnapshotBuildStrategy{..}

--resolveNightlySnapshot :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => NightlyBuildStrategy -> m SnapshotBuildStrategy
--resolveNightlySnapshot _ = throwM $ NotYetImplemented "resolveNightlySnapshot"


--resolveSnapshotSandboxLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => StackageRoot -> SnapshotBuildStrategy -> m (Path Abs Dir)
--resolveSnapshotSandboxLoc _ _ = throwM $ NotYetImplemented "resolveSnapshotSandboxLoc"

--resolveSnapshotGhcLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => StackageRoot -> SnapshotBuildStrategy -> m (Path Abs Dir)
--resolveSnapshotGhcLoc _ _ = throwM $ NotYetImplemented "resolveSnapshotGhcLoc"

--resolveSnapshotCabalLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => StackageRoot -> SnapshotBuildStrategy -> m (Path Abs Dir)
--resolveSnapshotCabalLoc _ _ = throwM $ NotYetImplemented "resolveSnapshotCabalLoc"

--resolveCustomSandboxLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => Text -> m (Path Abs Dir)
--resolveCustomSandboxLoc "detect" = detectSandboxLocation
--resolveCustomSandboxLoc _ = throwM $ NotYetImplemented "resolveCustomSandboxLoc"

--resolveCustomGhcLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => Text -> m (Path Abs Dir)
--resolveCustomGhcLoc "detect" = detectGhcLocation
--resolveCustomGhcLoc _ = throwM $ NotYetImplemented "resolveCustomGhcLoc"

--resolveCustomCabalLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => Text -> m (Path Abs Dir)
--resolveCustomCabalLoc "detect" = detectCabalLocation
--resolveCustomCabalLoc _ = throwM $ NotYetImplemented "resolveCustomCabalLoc"

-- TODO: copy from stackage-sandbox or wherver this has been well defined
resolveStackageRoot :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Maybe Text -> m StackageRoot
resolveStackageRoot (Just t) = parseAbsDir (Text.unpack t)
resolveStackageRoot Nothing = throwM $ NotYetImplemented "resolveStackageRoot: unexpected error"

-- TODO: copy from stackage-sandbox or wherver this has been well defined
resolveStackageHost :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Maybe Text -> m StackageHost
resolveStackageHost = return . maybe stackageHostDefault Text.unpack

stackageHostDefault :: String
stackageHostDefault = "https://www.stackage.org"
