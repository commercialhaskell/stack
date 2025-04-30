{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Options.DotParser
License     : BSD-3-Clause

Function to parse command line arguments for Stack's @dot@ command and certain
command line arguments for Stack's @ls dependencies@ command.
-}

module Stack.Options.DotParser
  ( dotOptsParser
  ) where

import           Data.Char ( isSpace )
import           Data.List.Split ( splitOn )
import qualified Data.Set as Set
import           Distribution.Types.PackageName ( mkPackageName )
import           Options.Applicative
                   ( Parser, auto, completer, help, idm, long, metavar, option
                   , strOption, switch
                   )
import           Options.Applicative.Builder.Extra ( boolFlags, textArgument )
import           Stack.Options.Completion ( targetCompleter )
import           Stack.Options.FlagsParser ( flagsParser )
import           Stack.Prelude
import           Stack.Types.DotOpts ( DotOpts (..) )

-- | Parser for arguments to `stack dot`
dotOptsParser :: Bool -> Parser DotOpts
dotOptsParser externalDefault = DotOpts
  <$> includeExternal
  <*> includeBase
  <*> depthLimit
  <*> fmap (maybe Set.empty $ Set.fromList . splitNames) prunedPkgs
  <*> targetsParser
  <*> flagsParser
  <*> testTargets
  <*> benchTargets
  <*> globalHints
 where
  includeExternal = boolFlags externalDefault
    "external"
    "inclusion of external dependencies."
    idm
  includeBase = boolFlags True
    "include-base"
    "inclusion of dependencies on base."
    idm
  depthLimit = optional (option auto
    (  long "depth"
    <> metavar "DEPTH"
    <> help "Limit the depth of dependency resolution. (default: no limit)"
    ))
  prunedPkgs = optional (strOption
    (  long "prune"
    <> metavar "PACKAGES"
    <> help "Prune specified package(s). PACKAGES is a comma-separated list of \
            \package names."
    ))

  targetsParser :: Parser [Text]
  targetsParser =
    many (textArgument
      (  metavar "TARGET"
      <> completer targetCompleter
      <> help "Can be specified multiple times. If none specified, use all \
              \project packages. Ignores project package components and \
              \non-project packages."
      ))

  testTargets = switch
    (  long "test"
    <> help "Consider dependencies of test components."
    )
  benchTargets = switch
    (  long "bench"
    <> help "Consider dependencies of benchmark components."
    )

  splitNames :: String -> [PackageName]
  splitNames = map
      ( mkPackageName
      . takeWhile (not . isSpace)
      . dropWhile isSpace
      )
    . splitOn ","

  globalHints = switch
    (  long "global-hints"
    <> help "Do not require an installed GHC; instead, use a hints file for \
            \global packages."
    )
