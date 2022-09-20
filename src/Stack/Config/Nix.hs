{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Nix configuration
module Stack.Config.Nix
       (nixOptsFromMonoid
       ,nixCompiler
       ,nixCompilerVersion
       ,StackNixException(..)
       ) where

import Stack.Prelude
import Control.Monad.Extra (ifM)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Distribution.System (OS (..))
import Stack.Constants
import Stack.Types.Config
import Stack.Types.Nix
import System.Directory (doesFileExist)

-- | Interprets NixOptsMonoid options.
nixOptsFromMonoid
    :: HasRunner env
    => NixOptsMonoid
    -> OS
    -> RIO env NixOpts
nixOptsFromMonoid NixOptsMonoid{..} os = do
    let defaultPure = case os of
          OSX -> False
          _ -> True
        nixPureShell = fromFirst defaultPure nixMonoidPureShell
        nixPackages = fromFirst [] nixMonoidPackages
        nixInitFile = getFirst nixMonoidInitFile
        nixShellOptions = fromFirst [] nixMonoidShellOptions
                          ++ prefixAll (T.pack "-I") (fromFirst [] nixMonoidPath)
        nixAddGCRoots   = fromFirstFalse nixMonoidAddGCRoots

    -- Enable Nix-mode by default on NixOS, unless Docker-mode was specified
    osIsNixOS <- isNixOS
    let nixEnable0 = fromFirst osIsNixOS nixMonoidEnable

    nixEnable <- case () of _
                                | nixEnable0 && osIsWindows -> do
                                      logInfo "Note: Disabling nix integration, since this is being run in Windows"
                                      pure False
                                | otherwise                 -> pure nixEnable0

    when (not (null nixPackages) && isJust nixInitFile) $
       throwIO NixCannotUseShellFileAndPackagesException
    pure NixOpts{..}
  where prefixAll p (x:xs) = p : x : prefixAll p xs
        prefixAll _ _      = []

nixCompiler :: WantedCompiler -> Either StringException T.Text
nixCompiler compilerVersion =
  case compilerVersion of
    WCGhc version ->
      case T.split (== '.') (fromString $ versionString version) of
        x : y : minor ->
          Right $
          case minor of
            [] ->
              -- The minor version is not specified. Select the latest minor
              -- version in Nixpkgs corresponding to the requested major
              -- version.
              let major = T.concat [x, y] in
              "(let compilers = builtins.filter \
              \(name: builtins.match \
              \\"ghc" <> major <> "[[:digit:]]*\" name != null) \
              \(lib.attrNames haskell.compiler); in \
              \if compilers == [] \
              \then abort \"No compiler found for GHC "
              <> T.pack (versionString version) <> "\"\
              \else haskell.compiler.${builtins.head compilers})"
            _ -> "haskell.compiler.ghc" <> T.concat (x : y : minor)
        _ -> Left $ stringException "GHC major version not specified"
    WCGhcjs{} -> Left $ stringException "Only GHC is supported by stack --nix"
    WCGhcGit{} -> Left $ stringException "Only GHC is supported by stack --nix"

nixCompilerVersion :: WantedCompiler -> Either StringException T.Text
nixCompilerVersion compilerVersion =
  case compilerVersion of
    WCGhc version ->
      case T.split (== '.') (fromString $ versionString version) of
        x : y : minor -> Right $ "ghc" <> T.concat (x : y : minor)
        _ -> Left $ stringException "GHC major version not specified"
    WCGhcjs{} -> Left $ stringException "Only GHC is supported by stack --nix"
    WCGhcGit{} -> Left $ stringException "Only GHC is supported by stack --nix"

-- Exceptions thrown specifically by Stack.Nix
data StackNixException
  = NixCannotUseShellFileAndPackagesException
    -- ^ Nix can't be given packages and a shell file at the same time
    deriving (Typeable)

instance Exception StackNixException

instance Show StackNixException where
  show NixCannotUseShellFileAndPackagesException =
    "You cannot have packages and a shell-file filled at the same time in your nix-shell configuration."

isNixOS :: MonadIO m => m Bool
isNixOS = liftIO $ do
    let fp = "/etc/os-release"
    ifM (doesFileExist fp)
        (T.isInfixOf "ID=nixos" <$> TIO.readFile fp)
        (pure False)
