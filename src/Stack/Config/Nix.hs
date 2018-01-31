{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards, DeriveDataTypeable, OverloadedStrings #-}

-- | Nix configuration
module Stack.Config.Nix
       (nixOptsFromMonoid
       ,nixCompiler
       ,StackNixException(..)
       ) where

import Stack.Prelude
import qualified Data.Text as T
import Distribution.System (OS (..))
import Stack.Constants
import Stack.Types.Version
import Stack.Types.Nix
import Stack.Types.Compiler
import Stack.Types.Runner

-- | Interprets NixOptsMonoid options.
nixOptsFromMonoid
    :: HasRunner env
    => NixOptsMonoid
    -> OS
    -> RIO env NixOpts
nixOptsFromMonoid NixOptsMonoid{..} os = do
    let nixEnable0 = fromFirst False nixMonoidEnable
        defaultPure = case os of
          OSX -> False
          _ -> True
        nixPureShell = fromFirst defaultPure nixMonoidPureShell
        nixPackages = fromFirst [] nixMonoidPackages
        nixInitFile = getFirst nixMonoidInitFile
        nixShellOptions = fromFirst [] nixMonoidShellOptions
                          ++ prefixAll (T.pack "-I") (fromFirst [] nixMonoidPath)
        nixAddGCRoots   = fromFirst False nixMonoidAddGCRoots
    nixEnable <-
      if nixEnable0 && osIsWindows
        then do
          logInfo "Note: Disabling nix integration, since this is being run in Windows"
          return False
        else return nixEnable0
    when (not (null nixPackages) && isJust nixInitFile) $
       throwIO NixCannotUseShellFileAndPackagesException
    return NixOpts{..}
  where prefixAll p (x:xs) = p : x : prefixAll p xs
        prefixAll _ _      = []

nixCompiler :: CompilerVersion a -> Either StringException T.Text
nixCompiler compilerVersion =
  let -- These are the latest minor versions for each respective major version available in nixpkgs
      fixMinor "8.2" = "8.2.1"
      fixMinor "8.0" = "8.0.2"
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
       GhcVersion v -> Right $ nixCompilerFromVersion v
       _ -> Left $ stringException "Only GHC is supported by stack --nix"

-- Exceptions thown specifically by Stack.Nix
data StackNixException
  = NixCannotUseShellFileAndPackagesException
    -- ^ Nix can't be given packages and a shell file at the same time
    deriving (Typeable)

instance Exception StackNixException

instance Show StackNixException where
  show NixCannotUseShellFileAndPackagesException =
    "You cannot have packages and a shell-file filled at the same time in your nix-shell configuration."
