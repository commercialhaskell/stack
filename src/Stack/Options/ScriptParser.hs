{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Options.ScriptParser
Description : Parse arguments for Stack's @script@ command.
License     : BSD-3-Clause

Functions to parse command line arguments for Stack's @script@ command.
-}

module Stack.Options.ScriptParser
  ( scriptOptsParser
  ) where

import           Options.Applicative
                   ( Parser, completer, eitherReader, flag', help, long, metavar
                   , option, strArgument, strOption
                   )
import           Options.Applicative.Builder.Extra
                   ( boolFlags, fileExtCompleter )
import           Stack.Options.Completion ( ghcOptsCompleter )
import           Stack.Options.PackagesParser ( packagesParser )
import           Stack.Prelude
import           Stack.Script
                   ( ScriptExecute (..), ScriptOpts (..), ShouldRun (..) )

-- | Parse command line arguments for Stack's @script@ command.
scriptOptsParser :: Parser ScriptOpts
scriptOptsParser = ScriptOpts
  <$> packagesParser
  <*> strArgument
        (  metavar "FILE"
        <> completer (fileExtCompleter [".hs", ".lhs"])
        )
  <*> many (strArgument
        (  metavar "-- ARGUMENT(S) (e.g. stack script X.hs -- argument(s) to \
                   \program)."
        ))
  <*> (   flag' SECompile
            (  long "compile"
            <> help "Compile the script without optimization and run the \
                    \executable."
            )
      <|> flag' SEOptimize
            (  long "optimize"
            <> help "Compile the script with optimization and run the \
                    \executable."
            )
      <|> pure SEInterpret
      )
  <*> boolFlags False
        "use-root"
        "writing of all compilation outputs to a script-specific location in \
        \the scripts directory of the Stack root."
        mempty
  <*> many (strOption
        (  long "ghc-options"
        <> metavar "OPTIONS"
        <> completer ghcOptsCompleter
        <> help "Additional options passed to GHC (can be specified multiple \
                \times)."
        ))
  <*> many (option extraDepRead
        (  long "extra-dep"
        <> metavar "EXTRA-DEP"
        <> help "An immutable extra dependency to be added to the snapshot \
                \(can be specified multiple times)."
        ))
  <*> (   flag' NoRun
            (  long "no-run"
            <> help "Do not run, just compile."
            )
      <|> pure YesRun
      )
 where
  extraDepRead = eitherReader $
                   mapLeft show . parseRawPackageLocationImmutables . fromString
