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
nixOptsFromMonoid
    :: MonadThrow m
    => Maybe Project -> Path Abs Dir -> NixOptsMonoid -> m NixOpts
nixOptsFromMonoid mproject stackRoot NixOptsMonoid{..} = do
    let nixEnable = fromMaybe nixMonoidDefaultEnable nixMonoidEnable
        nixPackages = nixMonoidPackages
        nixInitFile = nixMonoidInitFile
    return NixOpts{..}
