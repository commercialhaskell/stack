{-# LANGUAGE NoImplicitPrelude #-}
module Stack.Options.SDistParser where

import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Stack.Prelude
import           Stack.SDist
import           Stack.Options.HpcReportParser (pvpBoundsOption)

-- | Parser for arguments to `stack sdist` and `stack upload`
sdistOptsParser :: Bool -- ^ Whether to sign by default `stack upload` does, `stack sdist` doesn't
                -> Parser SDistOpts
sdistOptsParser signDefault = SDistOpts <$>
  many (strArgument $ metavar "DIR" <> completer dirCompleter) <*>
  optional pvpBoundsOption <*>
  ignoreCheckSwitch <*>
  (if signDefault
    then switch (long "no-signature" <> help "Do not sign & upload signatures")
    else switch (long "sign" <> help "Sign & upload signatures")) <*>
  strOption
  (long "sig-server" <> metavar "URL" <> showDefault <>
    value "https://sig.commercialhaskell.org" <>
    help "URL") <*>
  buildPackageOption
  where
    ignoreCheckSwitch =
      switch (long "ignore-check"
               <> help "Do not check package for common mistakes")
    buildPackageOption =
      boolFlags False "test-tarball" "building of the resulting tarball" idm
