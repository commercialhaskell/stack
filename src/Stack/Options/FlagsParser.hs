{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Options.FlagsParser
Description : Parser for one or more Cabal flags.
License     : BSD-3-Clause

Parser for one or more Cabal flags.
-}

module Stack.Options.FlagsParser
  ( flagsParser
  ) where

import qualified Data.Map as Map
import           Options.Applicative
                   ( Parser, completer, help, long, metavar, option )
import           Stack.Options.Completion ( flagCompleter )
import           Stack.Options.PackageParser ( readFlag )
import           Stack.Prelude
import           Stack.Types.BuildOptsCLI ( ApplyCLIFlag )

-- | Parser for one or more @--flag@ options, each for a Cabal flag.
flagsParser :: Parser (Map.Map ApplyCLIFlag (Map.Map FlagName Bool))
flagsParser = Map.unionsWith Map.union
  <$> many (option readFlag
       (  long "flag"
       <> completer flagCompleter
       <> metavar "PACKAGE:[-]FLAG"
       <> help "Set (or unset) the Cabal flag for the package (or use '*' for \
               \all packages) (can be specified multiple times). Applies to \
               \project packages, packages included directly in the snapshot, \
               \and extra-deps. Takes precedence over any Cabal flags \
               \specified for the package in the snapshot or in the \
               \project-level configuration file (stack.yaml)."
       ))
