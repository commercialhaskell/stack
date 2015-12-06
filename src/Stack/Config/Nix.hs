{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

-- | Nix configuration
module Stack.Config.Nix
       (nixOptsFromMonoid
       ,StackNixException(..)
       ) where

import Data.Text (pack)
import Data.Maybe
import Data.Typeable
import Stack.Types
import Control.Exception.Lifted
import Control.Monad.Catch (throwM,MonadCatch)


-- | Interprets NixOptsMonoid options.
nixOptsFromMonoid
    :: (Monad m, MonadCatch m)
    => Maybe Project
    -> Maybe AbstractResolver
    -> NixOptsMonoid
    -> m NixOpts
nixOptsFromMonoid mproject maresolver NixOptsMonoid{..} = do
    let nixEnable = fromMaybe nixMonoidDefaultEnable nixMonoidEnable
        mresolver = case maresolver of
          Just (ARResolver resolver) -> Just resolver
          Just _ -> Nothing
          Nothing -> fmap projectResolver mproject
        nixPackages = case mproject of
           Nothing -> nixMonoidPackages
           Just _ -> nixMonoidPackages ++ [case mresolver of
              Just (ResolverSnapshot (LTS x y)) ->
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
