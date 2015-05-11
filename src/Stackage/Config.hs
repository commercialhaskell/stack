{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
  , Settings(..)
  , getConfig
  , NotYetImplemented(..)
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Catch
import Control.Monad.Logger hiding (Loc)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import qualified Data.Yaml as Yaml
import Path
import System.Environment

-- | The top-level Stackage configuration.
data Config =
  Config {configPkgDbLocation :: Path Abs Dir
         ,configSandboxLocation :: Path Abs Dir
         ,configGhcBinLocation :: Path Abs Dir
         ,configCabalBinLocation :: Path Abs Dir
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

type StackageRoot = Path Abs Dir
type StackageHost = String

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
  => StackageRoot -> CabalSandboxBuildStrategy -> m (CustomBuildStrategy (Path Abs Dir))
parseBuildStrategy stackageRoot (BuildAgainstLTS lts) =
  resolveLTSSnapshot stackageRoot lts >>= parseBuildStrategy stackageRoot . BuildAgainstSnapshot
parseBuildStrategy stackageRoot (BuildAgainstNightly nightly) =
  resolveNightlySnapshot nightly >>= parseBuildStrategy stackageRoot . BuildAgainstSnapshot
parseBuildStrategy stackageRoot (BuildAgainstSnapshot snapshot) = do
  customGhcBinLocation <- resolveSnapshotGhcLoc stackageRoot snapshot
  customCabalBinLocation <- resolveSnapshotCabalLoc stackageRoot snapshot
  customSandboxLocation <- resolveSnapshotSandboxLoc stackageRoot snapshot
  return CustomBuildStrategy{..}
parseBuildStrategy stackageRoot (BuildAgainstCustom custom) = do
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
--  => m (CustomBuildStrategy (Path Abs Dir))
--defaultBuildStrategy = undefined

getStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m StackageConfig
getStackageConfig = do
  mconf <- liftIO $ Yaml.decodeFile "stackage.config"
  maybe (throwM $ NotYetImplemented "getStackageConfig") return mconf


configFromStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => StackageConfig -> m Config
configFromStackageConfig StackageConfig{..} = do
  stackageHost <- resolveStackageHost configStackageHost
  stackageRoot <- resolveStackageRoot configStackageRoot
  CustomBuildStrategy{..} <- case configBuildStrategy of
    CabalSandbox strategy -> parseBuildStrategy stackageRoot strategy
    Docker _ -> throwM $ NotYetImplemented "configFromStackageConfig"
  let configPkgDbLocation = undefined
      configSandboxLocation = undefined
      configGhcBinLocation = undefined
      configCabalBinLocation = undefined
      configInDocker = undefined
  _ <- return Config{..}
  -- TODO: fill in undefineds above
  throwM $ NotYetImplemented "configFromStackageConfig: return"

-- TODO: handle Settings
-- TODO: handle failure to retrieve StacakgeConfig
getConfig :: (MonadLogger m,MonadIO m,MonadThrow m)
          => Settings -> m Config
getConfig Settings = do
  stackageConfig <- getStackageConfig
  configFromStackageConfig stackageConfig


resolveLTSSnapshot :: (MonadLogger m, MonadIO m, MonadThrow m)
  => StackageRoot -> LTSBuildStrategy -> m SnapshotBuildStrategy
resolveLTSSnapshot stackageRoot LTSBuildStrategy{..} = do
  lts <- case Text.stripPrefix "lts-" configLTS of
    Nothing -> return $ "lts-" <> configLTS
    Just{} -> return configLTS
  mSnapshots <- liftIO $ Yaml.decodeFile (toFilePath stackageRoot)
  configSnapshot <- case Map.lookup lts =<< (mSnapshots :: Maybe (Map Text Text)) of
    Nothing -> return lts
    Just lts' -> return lts'
  return SnapshotBuildStrategy{..}

resolveNightlySnapshot :: (MonadLogger m, MonadIO m, MonadThrow m)
  => NightlyBuildStrategy -> m SnapshotBuildStrategy
resolveNightlySnapshot _ = throwM $ NotYetImplemented "resolveNightlySnapshot"


resolveSnapshotSandboxLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
  => StackageRoot -> SnapshotBuildStrategy -> m (Path Abs Dir)
resolveSnapshotSandboxLoc _ _ = throwM $ NotYetImplemented "resolveSnapshotSandboxLoc"

resolveSnapshotGhcLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
  => StackageRoot -> SnapshotBuildStrategy -> m (Path Abs Dir)
resolveSnapshotGhcLoc _ _ = throwM $ NotYetImplemented "resolveSnapshotGhcLoc"

resolveSnapshotCabalLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
  => StackageRoot -> SnapshotBuildStrategy -> m (Path Abs Dir)
resolveSnapshotCabalLoc _ _ = throwM $ NotYetImplemented "resolveSnapshotCabalLoc"

resolveCustomSandboxLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Text -> m (Path Abs Dir)
resolveCustomSandboxLoc _ = throwM $ NotYetImplemented "resolveCustomSandboxLoc"

resolveCustomGhcLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Text -> m (Path Abs Dir)
resolveCustomGhcLoc _ = throwM $ NotYetImplemented "resolveCustomGhcLoc"

resolveCustomCabalLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Text -> m (Path Abs Dir)
resolveCustomCabalLoc _ = throwM $ NotYetImplemented "resolveCustomCabalLoc"

-- TODO: copy from stackage-sandbox or wherver this has been well defined
resolveStackageRoot :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Maybe Text -> m StackageRoot
resolveStackageRoot (Just t) = parseAbsDir (fromString $ Text.unpack t)
resolveStackageRoot Nothing = do
  home <- liftIO $ getEnv "HOME"
  homeLoc <- parseAbsDir (fromString home)
  stackageLoc <- parseRelDir ".stackage"
  return $ homeLoc </> stackageLoc

-- TODO: copy from stackage-sandbox or wherver this has been well defined
resolveStackageHost :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Maybe Text -> m StackageHost
resolveStackageHost (Just t) = return $ Text.unpack t -- TODO: parse
resolveStackageHost Nothing = do
  mHost <- liftIO $ lookupEnv "STACKAGE_HOST"
  return $ fromMaybe "https://www.stackage.org" mHost
