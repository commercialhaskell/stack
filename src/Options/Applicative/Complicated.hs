{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Options.Applicative.Complicated
Description : Simple interface to complicated program arguments.
License     : BSD-3-Clause

Simple interface to complicated program arguments.

This is a "fork" of the @optparse-simple@ package that has some workarounds for
optparse-applicative issues that become problematic with programs that have many
options and subcommands. Because it makes the interface more complex, these
workarounds are not suitable for pushing upstream to optparse-applicative.
-}

module Options.Applicative.Complicated
  ( addCommand
  , addSubCommands
  , complicatedOptions
  , complicatedParser
  ) where

import           Control.Monad.Trans.Except ( runExceptT )
import           Control.Monad.Trans.Writer ( runWriter, tell )
import           Options.Applicative
                   ( Parser, ParserFailure, ParserHelp, ParserResult (..)
                   , abortOption, command, execParserPure, footer, fullDesc
                   , handleParseResult, header, help, hsubparser, info
                   , infoOption, long, metavar, noBacktrack, prefs, progDesc
                   , short, showHelpOnEmpty
                   )
import           Options.Applicative.Builder.Extra ( showHelpText )
import           Stack.Prelude
import           Stack.Types.AddCommand ( AddCommand )
import           Stack.Types.GlobalOptsMonoid ( GlobalOptsMonoid )
import           Stack.Types.Runner ( Runner )
import           System.Environment ( getArgs )

-- | Generate and execute a complicated options parser.
complicatedOptions ::
     Version
     -- ^ numeric version
  -> Maybe String
     -- ^ version string
  -> String
     -- ^ Hpack numeric version, as string
  -> String
     -- ^ header
  -> String
     -- ^ program description (displayed between usage and options listing in
     -- the help output)
  -> String
     -- ^ footer
  -> Parser GlobalOptsMonoid
     -- ^ common settings
  -> Maybe (  ParserFailure ParserHelp
           -> [String]
           -> IO (GlobalOptsMonoid, (RIO Runner (), GlobalOptsMonoid))
           )
     -- ^ optional handler for parser failure; 'handleParseResult' is called by
     -- default
  -> AddCommand
     -- ^ commands (use 'addCommand')
  -> IO (GlobalOptsMonoid, RIO Runner ())
complicatedOptions
    numericVersion
    stringVersion
    numericHpackVersion
    h
    pd
    footerStr
    commonParser
    mOnFailure
    commandParser
  = do
    args <- getArgs
    (a, (b, c)) <- let parserPrefs = prefs $ noBacktrack <> showHelpOnEmpty
                   in  case execParserPure parserPrefs parser args of
       -- call onFailure handler if it's present and parsing options failed
      Failure f | Just onFailure <- mOnFailure -> onFailure f args
      parseResult -> handleParseResult parseResult
    pure (mappend c a, b)
 where
  parser = info
    (   helpOption
    <*> versionOptions
    <*> complicatedParser "COMMAND|FILE" commonParser commandParser
    )
    desc
  desc = fullDesc <> header h <> progDesc pd <> footer footerStr
  versionOptions =
    case stringVersion of
      Nothing -> versionOption (versionString numericVersion)
      Just s ->
            versionOption s
        <*> numericVersionOption
        <*> numericHpackVersionOption
  versionOption s =
    infoOption
      s
      (  long "version"
      <> help "Show version."
      )
  numericVersionOption =
    infoOption
      (versionString numericVersion)
      (  long "numeric-version"
      <> help "Show only version number."
      )
  numericHpackVersionOption =
    infoOption
      numericHpackVersion
      (  long "hpack-numeric-version"
      <> help "Show only Hpack's version number."
      )

-- | Add a command to the options dispatcher.
addCommand ::
     String   -- ^ command string
  -> String   -- ^ title of command
  -> String   -- ^ footer of command help
  -> (opts -> RIO Runner ())
     -- ^ constructor to wrap up command in common data type
  -> (opts -> GlobalOptsMonoid -> GlobalOptsMonoid)
     -- ^ extend common settings from local settings
  -> Parser GlobalOptsMonoid -- ^ common parser
  -> Parser opts -- ^ command parser
  -> AddCommand
addCommand cmd title footerStr constr extendCommon =
  addCommand' cmd title footerStr (\a c -> (constr a, extendCommon a c))

-- | Add a command that takes sub-commands to the options dispatcher.
addSubCommands ::
     String
     -- ^ command string
  -> String
     -- ^ title of command
  -> String
     -- ^ footer of command help
  -> Parser GlobalOptsMonoid
     -- ^ common parser
  -> AddCommand
     -- ^ sub-commands (use 'addCommand')
  -> AddCommand
addSubCommands cmd title footerStr commonParser commandParser =
  addCommand'
    cmd
    title
    footerStr
    (\(c1, (a, c2)) c3 -> (a, mconcat [c3, c2, c1]))
    commonParser
    (complicatedParser "COMMAND" commonParser commandParser)

-- | Add a command to the options dispatcher.
addCommand' ::
     String   -- ^ command string
  -> String   -- ^ title of command
  -> String   -- ^ footer of command help
  -> (opts -> GlobalOptsMonoid -> (RIO Runner (),GlobalOptsMonoid))
     -- ^ constructor to wrap up command in common data type
  -> Parser GlobalOptsMonoid -- ^ common parser
  -> Parser opts -- ^ command parser
  -> AddCommand
addCommand' cmd title footerStr constr commonParser inner =
  lift $ tell $
    command
      cmd
      ( info
          (constr <$> inner <*> commonParser)
          (progDesc title <> footer footerStr)
      )

-- | Generate a complicated options parser.
complicatedParser ::
     String
     -- ^ metavar for the sub-command
  -> Parser GlobalOptsMonoid
     -- ^ common settings
  -> AddCommand
     -- ^ commands (use 'addCommand')
  -> Parser (GlobalOptsMonoid, (RIO Runner (), GlobalOptsMonoid))
complicatedParser commandMetavar commonParser commandParser =
  (,)
    <$> commonParser
    <*> case runWriter (runExceptT commandParser) of
          (Right (), m) -> hsubparser (m <> metavar commandMetavar)
          (Left b, _) -> pure (b, mempty)

-- | Non-hidden help option.
helpOption :: Parser (a -> a)
helpOption =
  abortOption showHelpText $
       long "help"
    <> short 'h'
    <> help "Show this help text."
