{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Options.SDistParser
Description : Parse arguments for Stack's @sdist@ command.
License     : BSD-3-Clause

Functions to parse command line arguments for Stack's @sdist@ command.
-}

module Stack.Options.SDistParser
 ( sdistOptsParser
 ) where

import           Options.Applicative
                   ( Parser, completer, help, idm, long, metavar, strArgument
                   , strOption, switch
                   )
import           Options.Applicative.Builder.Extra ( boolFlags, dirCompleter )
import           Stack.Prelude
import           Stack.Types.SDistOpts ( SDistOpts (..) )
import           Stack.Options.PvpBoundsParser ( pvpBoundsParser )

-- | Parse command line arguments for Stack's @sdist@ command.
sdistOptsParser :: Parser SDistOpts
sdistOptsParser = SDistOpts
  <$> many (strArgument
        (  metavar "DIR"
        <> completer dirCompleter
        <> help "A relative path to a package directory. Can be specified \
                \multiple times. If none specified, use all project packages."
        ))
  <*> optional (pvpBoundsParser Nothing)
  <*> ignoreCheckSwitch
  <*> buildPackageOption
  <*> optional (strOption
        (  long "tar-dir"
        <> help "If specified, copy all the generated files to this directory."
        ))
 where
  ignoreCheckSwitch = switch
    (  long "ignore-check"
    <> help "Do not check packages for common mistakes."
    )
  buildPackageOption = boolFlags False
    "test-tarball"
    "building of the resulting generated files."
    idm
