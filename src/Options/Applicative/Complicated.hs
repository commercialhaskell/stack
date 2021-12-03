{-# LANGUAGE NoImplicitPrelude #-}
-- | Simple interface to complicated program arguments.
--
-- This is a "fork" of the @optparse-simple@ package that has some workarounds for
-- optparse-applicative issues that become problematic with programs that have many options and
-- subcommands. Because it makes the interface more complex, these workarounds are not suitable for
-- pushing upstream to optparse-applicative.

module Options.Applicative.Complicated
  ( AddCommand
  , addCommand
  , addSubCommands
  , complicatedOptions
  , complicatedParser
  ) where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Writer
import           Options.Applicative
import           Options.Applicative.Types
import           Options.Applicative.Builder.Extra
import           Options.Applicative.Builder.Internal
import           Stack.Prelude
import           Stack.Types.Config
import           System.Environment

type AddCommand env =
    ExceptT (RIO env ()) (Writer (Mod CommandFields (RIO env (), GlobalOptsMonoid))) ()

-- | Generate and execute a complicated options parser.
complicatedOptions
  :: Version
  -- ^ numeric version
  -> Maybe String
  -- ^ version string
  -> String
  -- ^ hpack numeric version, as string
  -> String
  -- ^ header
  -> String
  -- ^ program description (displayed between usage and options listing in the help output)
  -> String
  -- ^ footer
  -> Parser GlobalOptsMonoid
  -- ^ common settings
  -> Maybe (ParserFailure ParserHelp -> [String] -> IO (GlobalOptsMonoid,(RIO env (),GlobalOptsMonoid)))
  -- ^ optional handler for parser failure; 'handleParseResult' is called by
  -- default
  -> AddCommand env
  -- ^ commands (use 'addCommand')
  -> IO (GlobalOptsMonoid, RIO env ())
complicatedOptions numericVersion stringVersion numericHpackVersion h pd footerStr commonParser mOnFailure commandParser =
  do args <- getArgs
     (a,(b,c)) <- case execParserPure (prefs noBacktrack) parser args of
       Failure _ | null args -> withArgs ["--help"] (execParser parser)
       -- call onFailure handler if it's present and parsing options failed
       Failure f | Just onFailure <- mOnFailure -> onFailure f args
       parseResult -> handleParseResult parseResult
     return (mappend c a,b)
  where parser = info (helpOption <*> versionOptions <*> complicatedParser "COMMAND|FILE" commonParser commandParser) desc
        desc = fullDesc <> header h <> progDesc pd <> footer footerStr
        versionOptions =
          case stringVersion of
            Nothing -> versionOption (versionString numericVersion)
            Just s -> versionOption s <*> numericVersionOption <*> numericHpackVersionOption
        versionOption s =
          infoOption
            s
            (long "version" <>
             help "Show version")
        numericVersionOption =
          infoOption
            (versionString numericVersion)
            (long "numeric-version" <>
             help "Show only version number")
        numericHpackVersionOption =
          infoOption
            numericHpackVersion
            (long "hpack-numeric-version" <>
             help "Show only hpack's version number")

-- | Add a command to the options dispatcher.
addCommand :: String   -- ^ command string
           -> String   -- ^ title of command
           -> String   -- ^ footer of command help
           -> (opts -> RIO env ()) -- ^ constructor to wrap up command in common data type
           -> (opts -> GlobalOptsMonoid -> GlobalOptsMonoid) -- ^ extend common settings from local settings
           -> Parser GlobalOptsMonoid -- ^ common parser
           -> Parser opts -- ^ command parser
           -> AddCommand env
addCommand cmd title footerStr constr extendCommon =
  addCommand' cmd title footerStr (\a c -> (constr a,extendCommon a c))

-- | Add a command that takes sub-commands to the options dispatcher.
addSubCommands
  :: String
  -- ^ command string
  -> String
  -- ^ title of command
  -> String
  -- ^ footer of command help
  -> Parser GlobalOptsMonoid
  -- ^ common parser
  -> AddCommand env
  -- ^ sub-commands (use 'addCommand')
  -> AddCommand env
addSubCommands cmd title footerStr commonParser commandParser =
  addCommand' cmd
              title
              footerStr
              (\(c1,(a,c2)) c3 -> (a,mconcat [c3, c2, c1]))
              commonParser
              (complicatedParser "COMMAND" commonParser commandParser)

-- | Add a command to the options dispatcher.
addCommand' :: String   -- ^ command string
            -> String   -- ^ title of command
            -> String   -- ^ footer of command help
            -> (opts -> GlobalOptsMonoid -> (RIO env (),GlobalOptsMonoid)) -- ^ constructor to wrap up command in common data type
            -> Parser GlobalOptsMonoid -- ^ common parser
            -> Parser opts -- ^ command parser
            -> AddCommand env
addCommand' cmd title footerStr constr commonParser inner =
  lift (tell (command cmd
                      (info (constr <$> inner <*> commonParser)
                            (progDesc title <> footer footerStr))))

-- | Generate a complicated options parser.
complicatedParser
  :: String
  -- ^ metavar for the sub-command
  -> Parser GlobalOptsMonoid
  -- ^ common settings
  -> AddCommand env
  -- ^ commands (use 'addCommand')
  -> Parser (GlobalOptsMonoid,(RIO env (),GlobalOptsMonoid))
complicatedParser commandMetavar commonParser commandParser =
   (,) <$>
   commonParser <*>
   case runWriter (runExceptT commandParser) of
     (Right (),d) -> hsubparser' commandMetavar d
     (Left b,_) -> pure (b,mempty)

-- | Subparser with @--help@ argument. Borrowed with slight modification
-- from Options.Applicative.Extra.
hsubparser' :: String -> Mod CommandFields a -> Parser a
hsubparser' commandMetavar m = mkParser d g rdr
  where
    Mod _ d g = metavar commandMetavar `mappend` m
    (groupName, cmds, subs) = mkCommand m
    rdr = CmdReader groupName cmds (fmap add_helper . subs)
    add_helper pinfo = pinfo
      { infoParser = infoParser pinfo <**> helpOption }

-- | Non-hidden help option.
helpOption :: Parser (a -> a)
helpOption =
    abortOption showHelpText $
    long "help" <>
    help "Show this help text"
