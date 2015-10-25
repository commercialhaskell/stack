{-# LANGUAGE CPP, DeriveDataTypeable, RecordWildCards, TemplateHaskell #-}

-- | Nix configuration
module Stack.Config.Nix where

import Data.Maybe
import Path
import Stack.Types

-- | Interprets DockerOptsMonoid options.
nixOptsFromMonoid :: Monad m => Path Abs Dir -> NixOptsMonoid -> m NixOpts
nixOptsFromMonoid _stackRoot NixOptsMonoid{..} = do
    let nixEnable = fromMaybe nixMonoidDefaultEnable nixMonoidEnable
        nixPackages = nixMonoidPackages
        nixInitFile = nixMonoidInitFile
    return NixOpts{..}
