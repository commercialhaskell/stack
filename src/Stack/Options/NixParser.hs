module Stack.Options.NixParser where

import           Data.Monoid.Extra
import qualified Data.Text                         as T
import           Options.Applicative
import           Options.Applicative.Args
import           Options.Applicative.Builder.Extra
import           Stack.Nix
import           Stack.Options.Utils
import           Stack.Types.Nix

nixOptsParser :: Bool -> Parser NixOptsMonoid
nixOptsParser hide0 = overrideActivation <$>
  (NixOptsMonoid
  <$> pure (Any False)
  <*> firstBoolFlags nixCmdName
                     "use of a Nix-shell. Implies 'system-ghc: true'"
                     hide
  <*> firstBoolFlags "nix-pure"
                     "use of a pure Nix-shell. Implies 'system-ghc: true'"
                     hide
  <*> optionalFirst
          (textArgsOption
              (long "nix-packages" <>
               metavar "NAMES" <>
               help "List of packages that should be available in the nix-shell (space separated)" <>
               hide))
  <*> optionalFirst
          (option
              str
              (long "nix-shell-file" <>
               metavar "FILEPATH" <>
               help "Nix file to be used to launch a nix-shell (for regular Nix users)" <>
               hide))
  <*> optionalFirst
          (textArgsOption
              (long "nix-shell-options" <>
               metavar "OPTIONS" <>
               help "Additional options passed to nix-shell" <>
               hide))
  <*> optionalFirst
          (textArgsOption
              (long "nix-path" <>
               metavar "PATH_OPTIONS" <>
               help "Additional options to override NIX_PATH parts (notably 'nixpkgs')" <>
               hide))
  <*> firstBoolFlags "nix-add-gc-roots"
                     "addition of packages to the nix GC roots so nix-collect-garbage doesn't remove them"
                     hide
  )
  where
    hide = hideMods hide0
    overrideActivation m =
      if m /= mempty then m { nixMonoidEnable = (First . Just . fromFirst True) (nixMonoidEnable m) }
      else m
    textArgsOption = fmap (map T.pack) . argsOption
