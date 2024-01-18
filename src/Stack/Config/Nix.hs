{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Nix configuration
module Stack.Config.Nix
  ( nixCompiler
  , nixCompilerVersion
  , nixOptsFromMonoid
  ) where

import           Control.Monad.Extra ( ifM )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Distribution.System ( OS (..) )
import           Stack.Constants ( osIsWindows )
import           Stack.Prelude
import           Stack.Types.Runner ( HasRunner )
import           Stack.Types.Nix ( NixOpts (..), NixOptsMonoid (..) )
import           System.Directory ( doesFileExist )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Config.Nix" module.
data ConfigNixException
  = NixCannotUseShellFileAndPackagesException
    -- ^ Nix can't be given packages and a shell file at the same time
  | GHCMajorVersionUnspecified
  | OnlyGHCSupported
  deriving (Show, Typeable)

instance Exception ConfigNixException where
  displayException NixCannotUseShellFileAndPackagesException =
    "Error: [S-2726]\n"
    ++ "You cannot have packages and a shell-file filled at the same time \
       \in your nix-shell configuration."
  displayException GHCMajorVersionUnspecified =
    "Error: [S-9317]\n"
    ++ "GHC major version not specified."
  displayException OnlyGHCSupported =
    "Error: [S-8605]\n"
    ++ "Only GHC is supported by 'stack --nix'."

-- | Interprets NixOptsMonoid options.
nixOptsFromMonoid ::
     (HasRunner env, HasTerm env)
  => NixOptsMonoid
  -> OS
  -> RIO env NixOpts
nixOptsFromMonoid nixMonoid os = do
  let defaultPure = case os of
        OSX -> False
        _ -> True
      pureShell = fromFirst defaultPure nixMonoid.pureShell
      packages = fromFirst [] nixMonoid.packages
      initFile = getFirst nixMonoid.initFile
      shellOptions =
           fromFirst [] nixMonoid.shellOptions
        ++ prefixAll (T.pack "-I") (fromFirst [] nixMonoid.path)
      addGCRoots   = fromFirstFalse nixMonoid.addGCRoots

  -- Enable Nix-mode by default on NixOS, unless Docker-mode was specified
  osIsNixOS <- isNixOS
  let nixEnable0 = fromFirst osIsNixOS nixMonoid.enable

  enable <-
    if nixEnable0 && osIsWindows
      then do
        prettyNoteS
          "Disabling Nix integration, since this is being run in Windows."
        pure False
      else pure nixEnable0

  when (not (null packages) && isJust initFile) $
    throwIO NixCannotUseShellFileAndPackagesException
  pure NixOpts
    { enable
    , pureShell
    , packages
    , initFile
    , shellOptions
    , addGCRoots
    }
 where
  prefixAll p (x:xs) = p : x : prefixAll p xs
  prefixAll _ _      = []

nixCompiler :: WantedCompiler -> Either ConfigNixException T.Text
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
        _ -> Left GHCMajorVersionUnspecified
    WCGhcjs{} -> Left OnlyGHCSupported
    WCGhcGit{} -> Left OnlyGHCSupported

nixCompilerVersion :: WantedCompiler -> Either ConfigNixException T.Text
nixCompilerVersion compilerVersion =
  case compilerVersion of
    WCGhc version ->
      case T.split (== '.') (fromString $ versionString version) of
        x : y : minor -> Right $ "ghc" <> T.concat (x : y : minor)
        _ -> Left GHCMajorVersionUnspecified
    WCGhcjs{} -> Left OnlyGHCSupported
    WCGhcGit{} -> Left OnlyGHCSupported

isNixOS :: MonadIO m => m Bool
isNixOS = liftIO $ do
  let fp = "/etc/os-release"
  ifM (doesFileExist fp)
      (T.isInfixOf "ID=nixos" <$> TIO.readFile fp)
      (pure False)
