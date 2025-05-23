{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Options.GhciParser
Description : Parse arguments for Stack's @ghci@ and @repl@ commands.
License     : BSD-3-Clause

Function to parse arguments for Stack's @ghci@ and @repl@ commands.
-}

module Stack.Options.GhciParser
  ( ghciOptsParser
  ) where

import           Options.Applicative
                   ( Parser, completer, flag, help, idm, internal, long, metavar
                   , strOption, switch
                   )
import           Options.Applicative.Args ( argsOption )
import           Options.Applicative.Builder.Extra
                   ( boolFlags, boolFlagsNoDefault, fileExtCompleter
                   , textArgument, textOption
                   )
import           Stack.Options.Completion ( ghcOptsCompleter, targetCompleter )
import           Stack.Options.FlagsParser ( flagsParser )
import           Stack.Options.PackagesParser ( packagesParser )
import           Stack.Prelude
import           Stack.Types.GhciOpts ( GhciOpts (..) )

-- | Parse command line arguments for Stack's @ghci@ and @repl@ commands.
ghciOptsParser :: Parser GhciOpts
ghciOptsParser = GhciOpts
  <$> many (textArgument
        (  metavar "TARGET/FILE"
        <> completer (targetCompleter <> fileExtCompleter [".hs", ".lhs"])
        <> help "If none specified, use all project packages. See \
                \https://docs.haskellstack.org/en/stable/commands/build_command/#target-syntax \
                \for details. If a path to a .hs or .lhs file is specified, it \
                \will be loaded."
        ))
  <*> (     (\x y -> x ++ concat y)
        <$> flag
              []
              ["-Wall", "-Werror"]
              (  long "pedantic"
              <> help "Turn on -Wall and -Werror."
              )
        <*> many (argsOption
              (  long "ghci-options"
              <> metavar "OPTIONS"
              <> completer ghcOptsCompleter
              <> help "Additional options passed to GHCi (can be specified \
                      \multiple times)."
              ))
      )
  <*> (     concat
        <$> many (argsOption
              (  long "ghc-options"
              <> metavar "OPTIONS"
              <> completer ghcOptsCompleter
              <> help "Additional options passed to both GHC and GHCi (can be \
                      \specified multiple times)."
              ))
      )
  <*> flagsParser
  <*> optional (strOption
        (  long "with-ghc"
        <> metavar "GHC"
        <> help "Use this GHC to run GHCi."
        ))
  <*> (     not
        <$> boolFlags True
              "load"
              "load modules on start-up."
              idm
      )
  <*> packagesParser
  <*> optional (textOption
        (  long "main-is"
        <> metavar "TARGET"
        <> completer targetCompleter
        <> help "Specify which target should contain the main module to load, \
                \such as for an executable for test suite or benchmark."
        ))
  <*> switch
        (  long "load-local-deps"
        <> help "Load all local dependencies of your targets."
        )
  <*> optional (boolFlagsNoDefault
        "package-hiding"
        "package hiding"
        idm)
  <*> switch
        (  long "no-build"
        <> help "Don't build before launching GHCi."
        <> internal
        )
  <*> switch
        (  long "only-main"
        <> help "Only load and import the main module. If no main module, no \
                \modules will be loaded."
        )
