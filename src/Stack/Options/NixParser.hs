{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Stack.Options.NixParser
License     : BSD-3-Clause
-}

module Stack.Options.NixParser
  ( nixOptsParser
  ) where

import qualified Data.Text as T
import           Options.Applicative
                   ( Parser, completer, help, long, metavar, option, str )
import           Options.Applicative.Args ( argsOption )
import           Options.Applicative.Builder.Extra
                   ( fileExtCompleter, firstBoolFlagsFalse
                   , firstBoolFlagsNoDefault, optionalFirst
                   )
import           Stack.Nix ( nixCmdName )
import           Stack.Options.Utils ( hideMods )
import           Stack.Prelude
import           Stack.Types.Nix ( NixOptsMonoid (..) )

nixOptsParser :: Bool -> Parser NixOptsMonoid
nixOptsParser hide0 = overrideActivation <$>
  (   NixOptsMonoid
  <$> firstBoolFlagsNoDefault
        nixCmdName
        "use of a Nix-shell. Implies 'system-ghc: true'."
        hide
  <*> firstBoolFlagsNoDefault
        "nix-pure"
        "use of a pure Nix-shell. Implies '--nix' and 'system-ghc: true'."
        hide
  <*> optionalFirst (textArgsOption
        (  long "nix-packages"
        <> metavar "NAMES"
        <> help "List of packages that should be available in the nix-shell \
                \(space separated)."
        <> hide
        ))
  <*> optionalFirst (option str
        (  long "nix-shell-file"
        <> metavar "FILE"
        <> completer (fileExtCompleter [".nix"])
        <> help "Nix file to be used to launch a nix-shell (for regular Nix \
                \users)."
        <> hide
        ))
  <*> optionalFirst (textArgsOption
        (  long "nix-shell-options"
        <> metavar "OPTIONS"
        <> help "Additional options passed to nix-shell."
        <> hide
        ))
  <*> optionalFirst (textArgsOption
        (  long "nix-path"
        <> metavar "PATH_OPTIONS"
        <> help "Additional options to override NIX_PATH parts (notably \
                \'nixpkgs')."
        <> hide
        ))
  <*> firstBoolFlagsFalse
        "nix-add-gc-roots"
        "addition of packages to the nix GC roots so nix-collect-garbage does \
        \not remove them."
        hide
  )
 where
  hide = hideMods hide0
  overrideActivation m =
    if fromFirst False m.pureShell
      then m { enable = (First . Just . fromFirst True) m.enable }
      else m
  textArgsOption = fmap (map T.pack) . argsOption
