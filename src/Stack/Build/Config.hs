{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Configuration parsing the stackage-build.yaml.

module Stack.Build.Config where

-- import           Stackage.Build.Defaults
-- import           Stackage.Build.Types

-- import           Control.Exception
-- import qualified Data.ByteString as SB
-- import qualified Data.Yaml as YAML
-- import           Path as FL
-- import           Path.Find
import           Prelude hiding (FilePath)
-- import           System.Directory

-- -- | Find the config file.
-- findConfig :: IO (Maybe (Path Abs File))
-- findConfig =
--   do cur <- getCurrentDirectory >>= parseAbsDir
--      findFileUp cur
--                 ((== configFileName) . FL.filename)
--                 Nothing

-- -- | Tries to find and read the config file. Throws exception if it
-- -- can't be found.
-- readConfig :: IO Config
-- readConfig = readConfig' id

-- -- | Tries to find and read only the docker-related section of the config file.
-- -- Throws exception if it can't be found.
-- readDockerOnlyConfig :: IO DockerOnlyConfig
-- readDockerOnlyConfig =
--   fmap DockerOnlyConfig
--        (readConfig'
--           (\conf -> \cwd -> unDockerOnlyConfig (conf cwd)))

-- -- | Tries to find and read the config file. Throws exception if it
-- -- can't be found.
-- readConfig' :: YAML.FromJSON a => (a -> (Path Abs Dir -> Config)) -> IO Config
-- readConfig' unwrapConfig =
--   do mfp <- findConfig
--      cwd <- getCurrentDirectory >>= parseAbsDir
--      case mfp of
--        Nothing -> throw FPNoConfigFile
--        Just fp ->
--          do bytes <- SB.readFile (FL.toFilePath fp)
--             case YAML.decodeEither' bytes of
--               Left err ->
--                 throwIO (FPConfigError err)
--               Right wrappedConfig ->
--                 do let config = unwrapConfig wrappedConfig cwd
--                        !dir = FL.parent fp
--                    return config {configPackages = configPackages config
--                                  ,configMaybeDir = Just dir}
