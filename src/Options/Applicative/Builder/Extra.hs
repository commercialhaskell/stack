-- | Extra functions for optparse-applicative.

module Options.Applicative.Builder.Extra
  (boolFlags
  ,maybeBoolFlags
  ,enableDisableFlags
  ,extraHelpOption
  ,execExtraHelp)
  where

import Control.Monad (when)
import Options.Applicative
import System.Environment (withArgs)
import System.FilePath (takeBaseName)

-- | Enable/disable flags for a @Bool@.
boolFlags :: Bool -> String -> String -> Mod FlagFields Bool -> Parser Bool
boolFlags defaultValue = enableDisableFlags defaultValue True False

-- | Enable/disable flags for a @(Maybe Bool)@.
maybeBoolFlags :: String -> String -> Mod FlagFields (Maybe Bool) -> Parser (Maybe Bool)
maybeBoolFlags = enableDisableFlags Nothing (Just True) (Just False)

-- | Enable/disable flags for any type.
enableDisableFlags :: a -> a -> a -> String -> String -> Mod FlagFields a -> Parser a
enableDisableFlags defaultValue enabledValue disabledValue name helpSuffix mods =
  flag' enabledValue
        (long name <>
         help ("Enable " ++ helpSuffix) <>
         mods) <|>
  flag' enabledValue
        (internal <>
         long ("enable-" ++ name) <>
         help ("Enable " ++ helpSuffix) <>
         mods) <|>
  flag' disabledValue
        (long ("no-" ++ name) <>
         help ("Disable " ++ helpSuffix) <>
         mods) <|>
  flag' disabledValue
        (internal <>
         long ("disable-" ++ name) <>
         help ("Disable " ++ helpSuffix) <>
         mods) <|>
  pure defaultValue

-- | Show an extra help option (e.g. @--docker-help@ shows help for all @--docker*@ args).
-- To actually show have that help appear, use 'execExtraHelp' before executing the main parser.
extraHelpOption :: String -> String -> String -> Parser (a -> a)
extraHelpOption progName fakeName helpName =
    infoOption (optDesc ++ ".") (long helpName <> hidden <> internal) <*>
    infoOption (optDesc ++ ".") (long fakeName <> help optDesc)
  where optDesc = concat ["Run '", takeBaseName progName, " --", helpName, "' for details"]

-- | Display extra help if extea help option passed in arguments.
-- Since optparse-applicative doesn't allow an arbirary IO action for an 'abortOption', this
-- was the best way I found that doesn't require manually formatting the help.
execExtraHelp :: [String] -> String -> Parser a -> String -> IO ()
execExtraHelp args helpOpt parser pd = do
    when (args == ["--" ++ helpOpt]) $
      withArgs ["--help"] $ do
        _ <- execParser (info (hiddenHelper <*>
                               ((,) <$>
                                parser <*>
                                some (strArgument (metavar "OTHER ARGUMENTS"))))
                        (fullDesc <> progDesc pd))
        return ()
  where hiddenHelper = abortOption ShowHelpText (long "help" <> hidden <> internal)
