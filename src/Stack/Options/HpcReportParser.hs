{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Options.HpcReportParser
Description : Parser for @stack hpc report@.
License     : BSD-3-Clause

Parser for @stack hpc report@.
-}

module Stack.Options.HpcReportParser
  ( hpcReportOptsParser
  ) where

import           Options.Applicative
                   ( Parser, completer, help, long, metavar, strOption, switch )
import           Options.Applicative.Builder.Extra
                   ( dirCompleter, fileExtCompleter, textArgument )
import           Stack.Options.Completion ( targetCompleter )
import           Stack.Prelude
import           Stack.Types.HpcReportOpts ( HpcReportOpts (..) )

-- | Parser for @stack hpc report@.
hpcReportOptsParser :: Parser HpcReportOpts
hpcReportOptsParser = HpcReportOpts
  <$> many (textArgument
        (  metavar "TARGET_OR_TIX"
        <> completer (targetCompleter <> fileExtCompleter [".tix"])
        ))
  <*> switch
        (  long "all"
        <> help "Use results from all packages and components involved in \
                \previous --coverage run."
        )
  <*> optional (strOption
        (  long "destdir"
        <> metavar "DIR"
        <> completer dirCompleter
        <> help "Output directory for HTML report."
        ))
  <*> switch
        (  long "open"
        <> help "Open the report in the browser."
        )
