{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

-- | Nix configuration
module Stack.Config.Nix
       (nixOptsFromMonoid
       ,StackNixException(..)
       ) where

import Data.Text (pack)
import Data.Maybe
import Data.Typeable
import Path
import Stack.Types
import Control.Exception.Lifted
import Control.Monad.Catch (throwM,MonadCatch)


-- | Interprets NixOptsMonoid options.
nixOptsFromMonoid :: (Monad m, MonadCatch m) => Maybe Project -> Path Abs Dir -> NixOptsMonoid -> m NixOpts
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
    if not (null nixMonoidPackages) && isJust nixInitFile then
       throwM NixCannotUseShellFileAndPackagesException
       else return ()
    return NixOpts{..}

-- Exceptions thown specifically by Stack.Nix
data StackNixException
  = NixCannotUseShellFileAndPackagesException
    -- ^ Nix can't be given packages and a shell file at the same time
    deriving (Typeable)

instance Exception StackNixException

instance Show StackNixException where
  show NixCannotUseShellFileAndPackagesException =
    "You cannot have packages and a shell-file filled at the same time in your nix-shell configuration."
