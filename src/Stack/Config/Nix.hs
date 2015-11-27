{-# LANGUAGE RecordWildCards #-}

-- | Nix configuration
module Stack.Config.Nix where

import Data.Text (pack)
import Data.Maybe
import Path
import Stack.Types

-- | Interprets DockerOptsMonoid options.
nixOptsFromMonoid :: Monad m => Maybe Project -> Path Abs Dir -> NixOptsMonoid -> m NixOpts
nixOptsFromMonoid mproject _stackRoot NixOptsMonoid{..} = do
    let nixEnable = fromMaybe nixMonoidDefaultEnable nixMonoidEnable
        nixPackages = case mproject of
           Nothing -> nixMonoidPackages
           Just p -> nixMonoidPackages ++ [case projectResolver p of
              ResolverSnapshot (LTS x y) ->
                pack ("haskell.packages.lts-" ++ show x ++ "_" ++ show y ++ ".ghc")
              _ -> pack "ghc"]
        nixInitFile = nixMonoidInitFile
        nixShellOptions = nixMonoidShellOptions
    return NixOpts{..}
