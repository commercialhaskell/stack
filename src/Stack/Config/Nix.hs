{-# LANGUAGE CPP, DeriveDataTypeable, RecordWildCards, TemplateHaskell #-}

-- | Nix configuration
module Stack.Config.Nix where

import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Catch (throwM, MonadThrow)
import Data.List (find)
import Data.Maybe
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Path
import Stack.Types

-- | Interprets DockerOptsMonoid options.
execEnvOptsFromMonoid
    :: MonadThrow m
    => Maybe Project -> Path Abs Dir -> ExecEnvOptsMonoid -> m ExecEnvOpts
execEnvOptsFromMonoid mproject stackRoot ExecEnvOptsMonoid{..} = do
    let execEnvType =
            if fromMaybe execEnvMonoidDefaultEnable execEnvMonoidEnable
            then Just NixShellExecEnv
            else Nothing
        execEnvPackages = execEnvMonoidPackages
        execEnvInitFile = execEnvMonoidInitFile
    return ExecEnvOpts{..}
