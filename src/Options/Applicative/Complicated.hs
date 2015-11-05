{-# LANGUAGE TemplateHaskell #-}

-- | Simple interface to complicated program arguments.
--
-- This is a "fork" of the @optparse-simple@ package that has some workarounds for
-- optparse-applicative issues that become problematic with programs that have many options and
-- subcommands. Because it makes the interface more complex, these workarounds are not suitable for
-- pushing upstream to optparse-applicative.

module Options.Applicative.Complicated where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Writer
import           Data.Monoid
import           Data.Version
import           Options.Applicative
import           Options.Applicative.Types
import           Options.Applicative.Builder.Internal
import           System.Environment

-- | Generate and execute a complicated options parser.
complicatedOptions
  :: Monoid a
  => Version
  -- ^ numeric version
  -> Maybe String
  -- ^ version string
  -> String
  -- ^ header
  -> String
  -- ^ program description
  -> (Bool -> Parser a)
  -- ^ global settings
  -> EitherT b (Writer (Parser a -> Mod CommandFields (b,a))) ()
  -- ^ commands (use 'addCommand')
  -> IO (a,b)
complicatedOptions numericVersion versionString h pd globalParser commandParser =
  do args <- getArgs
     (a,(b,c)) <- case execParserPure (prefs noBacktrack) parser args of
       Failure _ | null args -> withArgs ["--help"] (execParser parser)
       parseResult -> handleParseResult parseResult
     return (mappend c a,b)
  where parser = info (helpOption <*> versionOptions <*> complicatedParser globalParser commandParser) desc
        desc = fullDesc <> header h <> progDesc pd
        versionOptions =
          case versionString of
            Nothing -> versionOption (showVersion numericVersion)
            Just s -> versionOption s <*> numericVersionOption
        versionOption s =
          infoOption
            s
            (long "version" <>
             help "Show version")
        numericVersionOption =
          infoOption
            (showVersion numericVersion)
            (long "numeric-version" <>
             help "Show only version number")

-- | Add a command to the options dispatcher.
addCommand :: String   -- ^ command string
           -> String   -- ^ title of command
           -> String   -- ^ footer of command help
           -> (a -> b) -- ^ constructor to wrap up command in common data type
           -> Parser a -- ^ command parser
           -> EitherT b (Writer (Parser c -> Mod CommandFields (b,c))) ()
addCommand cmd title footerStr constr inner =
  addCommand' cmd title footerStr constr (const inner)

-- | Add a command that takes sub-commands to the options dispatcher.
addSubCommands
  :: Monoid a
  => String
  -- ^ command string
  -> String
  -- ^ title of command
  -> String
  -- ^ footer of command help
  -> EitherT b (Writer (Parser a -> Mod CommandFields (b,a))) ()
  -- ^ sub-commands (use 'addCommand')
  -> EitherT b (Writer (Parser a -> Mod CommandFields (b,a))) ()
addSubCommands cmd title footerStr commandParser =
  addCommand' cmd
              title
              footerStr
              (\(_, (a, _)) -> a)
              (\commonParser -> complicatedParser (const commonParser) commandParser)

-- | Add a command to the options dispatcher.
addCommand' :: String   -- ^ command string
            -> String   -- ^ title of command
            -> String   -- ^ footer of command help
            -> (a -> b) -- ^ constructor to wrap up command in common data type
            -> (Parser c -> Parser a) -- ^ command parser
            -> EitherT b (Writer (Parser c -> Mod CommandFields (b,c))) ()
addCommand' cmd title footerStr constr inner =
  lift (tell (\commonParser -> command cmd
                      (info ((,) <$> (constr <$> inner commonParser) <*> commonParser)
                            (progDesc title <> footer footerStr))))

-- | Generate a complicated options parser.
complicatedParser
  :: Monoid a
  => (Bool -> Parser a)
  -- ^ common settings
  -> EitherT b (Writer (Parser a -> Mod CommandFields (b,a))) ()
  -- ^ commands (use 'addCommand')
  -> Parser (a,(b,a))
complicatedParser commonParser commandParser =
   (,) <$>
   commonParser False <*>
   case runWriter (runEitherT commandParser) of
     (Right (),d) -> hsubparser' (d (commonParser True))
     (Left b,_) -> pure (b,mempty)

-- way to do in 'addCommand' | Subparser with @--help@ argument. Borrowed with slight modification
-- from Options.Applicative.Extra.
hsubparser' :: Mod CommandFields a -> Parser a
hsubparser' m = mkParser d g rdr
  where
    Mod _ d g = m `mappend` metavar "COMMAND"
    (cmds, subs) = mkCommand m
    rdr = CmdReader cmds (fmap add_helper . subs)
    add_helper pinfo = pinfo
      { infoParser = infoParser pinfo <**> helpOption }

-- | Non-hidden help option.
helpOption :: Parser (a -> a)
helpOption =
    abortOption ShowHelpText $
    long "help" <>
    help "Show this help text"
