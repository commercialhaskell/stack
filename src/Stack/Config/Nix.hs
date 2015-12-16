{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

-- | Nix configuration
module Stack.Config.Nix
       (nixOptsFromMonoid
       ,StackNixException(..)
       ) where

import Control.Monad (when)
import qualified Data.Text as T
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
        nixPureShell = fromMaybe False nixMonoidPureShell
        mresolver = case maresolver of
          Just (ARResolver resolver) -> Just resolver
          Just _ -> Nothing
          Nothing -> fmap projectResolver mproject
        pkgs = fromMaybe [] nixMonoidPackages
        nixPackages = case mproject of
           Nothing -> pkgs
           Just _ -> pkgs ++ [case mresolver of
              Just (ResolverSnapshot (LTS x y)) ->
                T.pack ("haskell.packages.lts-" ++ show x ++ "_" ++ show y ++ ".ghc")
              _ -> T.pack "ghc"]
        nixInitFile = nixMonoidInitFile
        nixShellOptions = fromMaybe [] nixMonoidShellOptions
                          ++ prefixAll (T.pack "-I") (fromMaybe [] nixMonoidPath)
    when (not (null pkgs) && isJust nixInitFile) $
       throwM NixCannotUseShellFileAndPackagesException
    return NixOpts{..}
  where prefixAll p (x:xs) = p : x : prefixAll p xs
        prefixAll _ _      = []

-- Exceptions thown specifically by Stack.Nix
data StackNixException
  = NixCannotUseShellFileAndPackagesException
    -- ^ Nix can't be given packages and a shell file at the same time
    deriving (Typeable)

instance Exception StackNixException

instance Show StackNixException where
  show NixCannotUseShellFileAndPackagesException =
    "You cannot have packages and a shell-file filled at the same time in your nix-shell configuration."
