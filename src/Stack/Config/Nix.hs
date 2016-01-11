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
import Distribution.System (OS (..))
import Stack.Types
import Control.Exception.Lifted
import Control.Monad.Catch (throwM,MonadCatch)


-- | Interprets NixOptsMonoid options.
nixOptsFromMonoid
    :: (Monad m, MonadCatch m)
    => NixOptsMonoid
    -> OS
    -> m NixOpts
nixOptsFromMonoid NixOptsMonoid{..} os = do
    let nixEnable = fromMaybe nixMonoidDefaultEnable nixMonoidEnable
        defaultPure = case os of
          OSX -> False
          _ -> True
        nixPureShell = fromMaybe defaultPure nixMonoidPureShell
        nixPackages = fromMaybe [] nixMonoidPackages
        nixInitFile = nixMonoidInitFile
        nixShellOptions = fromMaybe [] nixMonoidShellOptions
                          ++ prefixAll (T.pack "-I") (fromMaybe [] nixMonoidPath)
    when (not (null nixPackages) && isJust nixInitFile) $
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
