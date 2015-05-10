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
    Config
  , Settings
  , getConfig
  , NotYetImplemented(..)
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Catch
import Control.Monad.Logger hiding (Loc)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import qualified Data.Yaml as Yaml

import Filesystem.Loc

data Config =
  Config {configPkgDbLocation :: Loc Absolute Dir
         ,configSandboxLocation :: Loc Absolute Dir
         ,configGhcBinLocation :: Loc Absolute Dir
         ,configCabalBinLocation :: Loc Absolute Dir
         ,configInDocker :: Bool
         }
data Settings = Settings

data NotYetImplemented = NotYetImplemented Text
  deriving (Show, Typeable)
instance Exception NotYetImplemented


-- Some examples of stackage.config

-- (example 1)
-- docker: true

-- (example 2)
-- cabal-sandbox:
--   lts: 2

-- (example 3)
-- cabal-sandbox:
--   nightly: 2015-06-01

-- (example 4)
-- cabal-sandbox:
--   custom:
--     ghc: /home/dan/ghc/ghc-6.12/bin
--     cabal: /home/dan/.cabal/bin
--     sandbox: .cabal-sandbox

-- (example 5)
-- cabal-sandbox:
--   custom:
--     ghc: '7.10'
--     cabal: detect
--     sandbox: detect

data StackageConfig =
  StackageConfig
    { configStackageRoot :: Maybe Text
    , configStackageHost :: Maybe Text
    , configBuildStrategy :: BuildStrategy
    }
  deriving Show

data BuildStrategy
  = CabalSandbox CabalSandboxBuildStrategy
  | Docker DockerBuildStrategy
  deriving Show

data CabalSandboxBuildStrategy
  = BuildAgainstLTS LTSBuildStrategy
  | BuildAgainstNightly NightlyBuildStrategy
  | BuildAgainstSnapshot SnapshotBuildStrategy
  | BuildAgainstCustom (CustomBuildStrategy Text)
  deriving Show

data DockerBuildStrategy =
  DockerBuildStrategy
  deriving Show

data LTSBuildStrategy =
  LTSBuildStrategy
    { configLTS :: Text
    }
  deriving Show

data NightlyBuildStrategy =
  NightlyBuildStrategy
    { configNightly :: Text
    }
  deriving Show

data SnapshotBuildStrategy =
  SnapshotBuildStrategy
    { configSnapshot :: Text
    }
  deriving Show

data CustomBuildStrategy a =
  CustomBuildStrategy
  { customGhcBinLocation :: a
  , customCabalBinLocation :: a
  , customSandboxLocation :: a
  }
  deriving Show


instance FromJSON LTSBuildStrategy where
  parseJSON j = parseAsText j <|> parseAsScientific j where
    parseAsText = withText "LTSBuildStrategy" $ \t -> do
      let configLTS = t
      return LTSBuildStrategy{..}
    -- allows people to not have to quote the lts field
    parseAsScientific = withScientific "LTSBuildStrategy" $ \s -> do
      let configLTS = Text.pack $ show s
      return LTSBuildStrategy{..}

instance FromJSON NightlyBuildStrategy where
  parseJSON = withText "NightlyBuildStrategy" $ \t -> do
    let configNightly = t
    return NightlyBuildStrategy{..}

instance FromJSON SnapshotBuildStrategy where
  parseJSON = withText "SnapshotBuildStrategy" $ \t -> do
    let configSnapshot = t
    return SnapshotBuildStrategy{..}


instance FromJSON a => FromJSON (CustomBuildStrategy a) where
  parseJSON = withObject "CustomBuildStrategy" $ \obj -> do
    customGhcBinLocation <- obj .: "ghc"
    customCabalBinLocation <- obj .: "cabal"
    customSandboxLocation <- obj .: "sandbox"
    return CustomBuildStrategy{..}

instance FromJSON StackageConfig where
  parseJSON = withObject "StackageConfig" $ \obj -> do
    configStackageRoot <- obj .:? "stackage-root" 
    configStackageHost <- obj .:? "stackage-host"
    configBuildStrategy <-
      (Docker <$> obj .: "docker") <|>
      (CabalSandbox <$> obj .: "cabal-sandbox")
    return StackageConfig{..}

instance FromJSON DockerBuildStrategy where
  parseJSON = withBool "DockerBuildStrategy" $ \b ->
    if b
      then return DockerBuildStrategy
      else fail "docker: false"

instance FromJSON CabalSandboxBuildStrategy where
  parseJSON = withObject "CabalSandboxBuildStrategy" $ \obj ->
    (BuildAgainstLTS <$> obj .: "lts") <|>
    (BuildAgainstNightly <$> obj .: "nightly") <|>
    (BuildAgainstSnapshot <$> obj .: "snapshot") <|>
    (BuildAgainstCustom <$> obj .: "custom")

parseBuildStrategy :: (MonadLogger m, MonadIO m, MonadThrow m)
  => CabalSandboxBuildStrategy -> m (CustomBuildStrategy (Loc Absolute Dir))
parseBuildStrategy (BuildAgainstLTS lts) =
  resolveLTSSnapshot lts >>= parseBuildStrategy . BuildAgainstSnapshot
parseBuildStrategy (BuildAgainstNightly nightly) =
  resolveNightlySnapshot nightly >>= parseBuildStrategy . BuildAgainstSnapshot
parseBuildStrategy (BuildAgainstSnapshot snapshot) = do
  customGhcBinLocation <- resolveSnapshotGhcLoc snapshot
  customCabalBinLocation <- resolveSnapshotCabalLoc snapshot
  customSandboxLocation <- resolveSnapshotSandboxLoc snapshot
  return CustomBuildStrategy{..}
parseBuildStrategy (BuildAgainstCustom custom) = do
  ghcBinLoc <- resolveCustomGhcLoc (customGhcBinLocation custom)
  cabalBinLoc <- resolveCustomCabalLoc (customCabalBinLocation custom)
  sandboxLocation <- resolveCustomSandboxLoc (customSandboxLocation custom)
  return CustomBuildStrategy
    { customGhcBinLocation = ghcBinLoc
    , customCabalBinLocation = cabalBinLoc
    , customSandboxLocation = sandboxLocation
    }

-- TODO
-- Build strategy based on whatever ghc, cabal, and sandbox are visible.
--defaultBuildStrategy :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => m (CustomBuildStrategy (Loc Absolute Dir))
--defaultBuildStrategy = undefined

getStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m StackageConfig
getStackageConfig = do
  mconf <- liftIO $ Yaml.decodeFile "stackage.config"
  maybe (throwM $ NotYetImplemented "getStackageConfig") return mconf


configFromStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => StackageConfig -> m Config
configFromStackageConfig StackageConfig{..} = do
  CustomBuildStrategy{..} <- case configBuildStrategy of
    CabalSandbox strategy -> parseBuildStrategy strategy
    Docker _ -> throwM $ NotYetImplemented "configFromStackageConfig"
  let configPkgDbLocation = undefined
      configSandboxLocation = undefined
      configGhcBinLocation = undefined
      configCabalBinLocation = undefined
      configInDocker = undefined
  return Config{..}

-- TODO: handle more settings
-- TODO: handle failure to retrieve StacakgeConfig
getConfig :: (MonadLogger m,MonadIO m,MonadThrow m)
          => Settings -> m Config
getConfig Settings = do
  stackageConfig <- getStackageConfig
  configFromStackageConfig stackageConfig


resolveLTSSnapshot :: (MonadLogger m, MonadIO m, MonadThrow m)
  => LTSBuildStrategy -> m SnapshotBuildStrategy
resolveLTSSnapshot _ = throwM $ NotYetImplemented "resolveLTSSnapshot"

resolveNightlySnapshot :: (MonadLogger m, MonadIO m, MonadThrow m)
  => NightlyBuildStrategy -> m SnapshotBuildStrategy
resolveNightlySnapshot _ = throwM $ NotYetImplemented "resolveNightlySnapshot"


resolveSnapshotSandboxLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
  => SnapshotBuildStrategy -> m (Loc Absolute Dir)
resolveSnapshotSandboxLoc _ = throwM $ NotYetImplemented "resolveSnapshotSandboxLoc"

resolveSnapshotGhcLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
  => SnapshotBuildStrategy -> m (Loc Absolute Dir)
resolveSnapshotGhcLoc _ = throwM $ NotYetImplemented "resolveSnapshotGhcLoc"

resolveSnapshotCabalLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
  => SnapshotBuildStrategy -> m (Loc Absolute Dir)
resolveSnapshotCabalLoc _ = throwM $ NotYetImplemented "resolveSnapshotCabalLoc"

resolveCustomSandboxLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Text -> m (Loc Absolute Dir)
resolveCustomSandboxLoc _ = throwM $ NotYetImplemented "resolveCustomSandboxLoc"

resolveCustomGhcLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Text -> m (Loc Absolute Dir)
resolveCustomGhcLoc _ = throwM $ NotYetImplemented "resolveCustomGhcLoc"

resolveCustomCabalLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Text -> m (Loc Absolute Dir)
resolveCustomCabalLoc _ = throwM $ NotYetImplemented "resolveCustomCabalLoc"
