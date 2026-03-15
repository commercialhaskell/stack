{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Stack.Config.Nix
Description : Nix configuration.
License     : BSD-3-Clause

Nix configuration.
-}

module Stack.Config.Nix
  ( ConfigNixPrettyException
  , nixCompiler
  , nixCompilerVersion
  , nixOptsFromMonoid
  ) where

import           Control.Monad.Extra ( ifM, whenJust )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Distribution.System ( OS (..) )
import           Stack.Constants ( osIsWindows )
import           Stack.Prelude
import           Stack.Types.Runner ( HasRunner )
import           Stack.Types.Nix ( NixOpts (..), NixOptsMonoid (..) )
import           System.Directory ( doesFileExist )

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.Config.Nix" module.
data ConfigNixPrettyException
  = NixCannotUseShellFileAndPackagesException !FilePath ![Text]
    -- ^ Nix can't be given packages and a shell file at the same time
  | GHCMajorVersionUnspecified
  | OnlyGHCSupported
  deriving Show

instance Pretty ConfigNixPrettyException where
  pretty (NixCannotUseShellFileAndPackagesException initFile packages) =
    "[S-2726]"
    <> line
    <> flow "The configuration of Stack's Nix integration cannot specify both \
            \a Nix shell file and Nix packages. You have specified:"
    <> blankLine
    <> spacedBulletedList
         [ fillSep
             [ flow "Shell file:"
             , style File (fromString initFile) <> ";"
             , "and"
             ]
         , fillSep $
               flow "Nix packages:"
             : mkNarrativeList (Just Shell) False prettyPackages
         ]
   where
    prettyPackages :: [StyleDoc]
    prettyPackages = map (fromString . T.unpack) packages
  pretty GHCMajorVersionUnspecified =
    "[S-9317]"
    <> line
    <> flow "Stack's Nix integration requires at least a major version of GHC \
            \to be specified. No major version is specified."
  pretty OnlyGHCSupported =
    "[S-8605]"
    <> line
    <> flow "Stack's Nix integration supports only GHC binary distributions as \
            \compiler."

instance Exception ConfigNixPrettyException

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

  unless (null packages) $ whenJust initFile $ \fp ->
    prettyThrowIO $ NixCannotUseShellFileAndPackagesException fp packages

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

nixCompiler :: WantedCompiler -> Either ConfigNixPrettyException T.Text
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

nixCompilerVersion :: WantedCompiler -> Either ConfigNixPrettyException T.Text
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
