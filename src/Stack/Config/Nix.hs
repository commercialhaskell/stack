{-# LANGUAGE RecordWildCards, DeriveDataTypeable, OverloadedStrings #-}

-- | Nix configuration
module Stack.Config.Nix
       (nixOptsFromMonoid
       ,nixCompiler
       ,StackNixException(..)
       ) where

import Control.Monad (when)
import Data.Maybe
import Data.Monoid.Extra
import qualified Data.Text as T
import Data.Typeable
import Distribution.System (OS (..))
import Stack.Types.Version
import Stack.Types.Nix
import Stack.Types.Compiler
import Control.Exception.Lifted
import Control.Monad.Catch (throwM,MonadCatch)
import Prelude

-- | Interprets NixOptsMonoid options.
nixOptsFromMonoid
    :: (Monad m, MonadCatch m)
    => NixOptsMonoid
    -> OS
    -> m NixOpts
nixOptsFromMonoid NixOptsMonoid{..} os = do
    let nixEnable = fromFirst (getAny nixMonoidDefaultEnable) nixMonoidEnable
        defaultPure = case os of
          OSX -> False
          _ -> True
        nixPureShell = fromFirst defaultPure nixMonoidPureShell
        nixPackages = fromFirst [] nixMonoidPackages
        nixInitFile = getFirst nixMonoidInitFile
        nixShellOptions = fromFirst [] nixMonoidShellOptions
                          ++ prefixAll (T.pack "-I") (fromFirst [] nixMonoidPath)
        nixAddGCRoots   = fromFirst False nixMonoidAddGCRoots
    when (not (null nixPackages) && isJust nixInitFile) $
       throwM NixCannotUseShellFileAndPackagesException
    return NixOpts{..}
  where prefixAll p (x:xs) = p : x : prefixAll p xs
        prefixAll _ _      = []

nixCompiler :: CompilerVersion -> T.Text
nixCompiler compilerVersion =
  let -- These are the latest minor versions for each respective major version available in nixpkgs
      fixMinor "8.0" = "8.0.1"
      fixMinor "7.10" = "7.10.3"
      fixMinor "7.8" = "7.8.4"
      fixMinor "7.6" = "7.6.3"
      fixMinor "7.4" = "7.4.2"
      fixMinor "7.2" = "7.2.2"
      fixMinor "6.12" = "6.12.3"
      fixMinor "6.10" = "6.10.4"
      fixMinor v = v
      nixCompilerFromVersion v = T.append (T.pack "haskell.compiler.ghc")
                                          (T.filter (/= '.')
                                             (fixMinor (versionText v)))
  in case compilerVersion of
       GhcVersion v -> nixCompilerFromVersion v
       _ -> error "Only GHC is supported by now by stack --nix"

-- Exceptions thown specifically by Stack.Nix
data StackNixException
  = NixCannotUseShellFileAndPackagesException
    -- ^ Nix can't be given packages and a shell file at the same time
    deriving (Typeable)

instance Exception StackNixException

instance Show StackNixException where
  show NixCannotUseShellFileAndPackagesException =
    "You cannot have packages and a shell-file filled at the same time in your nix-shell configuration."
