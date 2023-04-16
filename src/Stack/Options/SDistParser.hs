{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions to parse command line arguments for Stack's @sdist@ and @upload@
-- commands.
module Stack.Options.SDistParser
 ( sdistOptsParser
 ) where

import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Stack.Prelude
import           Stack.SDist
import           Stack.Options.HpcReportParser ( pvpBoundsOption )

-- | Parse command line arguments for Stack's @sdist@ and @upload@ commands.
sdistOptsParser :: Parser SDistOpts
sdistOptsParser = SDistOpts
  <$> many (strArgument
        (  metavar "DIR"
        <> completer dirCompleter
        ))
  <*> optional pvpBoundsOption
  <*> ignoreCheckSwitch
  <*> buildPackageOption
  <*> optional (strOption
        (  long "tar-dir"
        <> help "If specified, copy all the tar to this dir"
        ))
 where
  ignoreCheckSwitch = switch
    (  long "ignore-check"
    <> help "Do not check package for common mistakes"
    )
  buildPackageOption = boolFlags False
    "test-tarball"
    "building of the resulting tarball"
    idm
