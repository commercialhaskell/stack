-- | Extra functions for optparse-applicative.

module Options.Applicative.Builder.Extra
  (boolFlags
  ,boolFlagsNoDefault
  ,maybeBoolFlags
  ,enableDisableFlags
  ,enableDisableFlagsNoDefault
  ,extraHelpOption
  ,execExtraHelp
  ,textOption
  ,textArgument)
  where

import Control.Monad (when)
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import System.Environment (withArgs)
import System.FilePath (takeBaseName)
import Data.Text (Text)
import qualified Data.Text as T

-- | Enable/disable flags for a 'Bool'.
boolFlags :: Bool                 -- ^ Default value
          -> String               -- ^ Flag name
          -> String               -- ^ Help suffix
          -> Mod FlagFields Bool
          -> Parser Bool
boolFlags defaultValue = enableDisableFlags defaultValue True False

-- | Enable/disable flags for a 'Bool', without a default case (to allow chaining with '<|>').
boolFlagsNoDefault :: String               -- ^ Flag name
                   -> String               -- ^ Help suffix
                   -> Mod FlagFields Bool
                   -> Parser Bool
boolFlagsNoDefault = enableDisableFlagsNoDefault True False

-- | Enable/disable flags for a @('Maybe' 'Bool')@.
maybeBoolFlags :: String                       -- ^ Flag name
               -> String                       -- ^ Help suffix
               -> Mod FlagFields (Maybe Bool)
               -> Parser (Maybe Bool)
maybeBoolFlags = enableDisableFlags Nothing (Just True) (Just False)

-- | Enable/disable flags for any type.
enableDisableFlags :: (Eq a)
                   => a                 -- ^ Default value
                   -> a                 -- ^ Enabled value
                   -> a                 -- ^ Disabled value
                   -> String            -- ^ Name
                   -> String            -- ^ Help suffix
                   -> Mod FlagFields a
                   -> Parser a
enableDisableFlags defaultValue enabledValue disabledValue name helpSuffix mods =
  enableDisableFlagsNoDefault enabledValue disabledValue name helpSuffix mods <|>
  pure defaultValue

-- | Enable/disable flags for any type, without a default (to allow chaining with '<|>')
enableDisableFlagsNoDefault :: (Eq a)
                            => a                 -- ^ Enabled value
                            -> a                 -- ^ Disabled value
                            -> String            -- ^ Name
                            -> String            -- ^ Help suffix
                            -> Mod FlagFields a
                            -> Parser a
enableDisableFlagsNoDefault enabledValue disabledValue name helpSuffix mods =
  last <$> some
      ((flag'
           enabledValue
           (hidden <>
            internal <>
            long name <>
            help helpSuffix <>
            mods) <|>
       flag'
           disabledValue
           (hidden <>
            internal <>
            long ("no-" ++ name) <>
            help helpSuffix <>
            mods)) <|>
       flag'
           disabledValue
           (long (concat ["[no-]", name]) <>
            help (concat ["Enable/disable ", helpSuffix]) <>
            mods))

-- | Show an extra help option (e.g. @--docker-help@ shows help for all @--docker*@ args).
--
-- To actually have that help appear, use 'execExtraHelp' before executing the main parser.
extraHelpOption :: Bool             -- ^ Hide from the brief description?
                -> String           -- ^ Program name, e.g. @"stack"@
                -> String           -- ^ Option glob expression, e.g. @"docker*"@
                -> String           -- ^ Help option name, e.g. @"docker-help"@
                -> Parser (a -> a)
extraHelpOption hide progName fakeName helpName =
    infoOption (optDesc' ++ ".") (long helpName <> hidden <> internal) <*>
    infoOption (optDesc' ++ ".") (long fakeName <>
                                  help optDesc' <>
                                  (if hide then hidden <> internal else idm))
  where optDesc' = concat ["Run '", takeBaseName progName, " --", helpName, "' for details"]

-- | Display extra help if extra help option passed in arguments.
--
-- Since optparse-applicative doesn't allow an arbitrary IO action for an 'abortOption', this
-- was the best way I found that doesn't require manually formatting the help.
execExtraHelp :: [String]  -- ^ Command line arguments
              -> String    -- ^ Extra help option name, e.g. @"docker-help"@
              -> Parser a  -- ^ Option parser for the relevant command
              -> String    -- ^ Option description
              -> IO ()
execExtraHelp args helpOpt parser pd =
    when (args == ["--" ++ helpOpt]) $
      withArgs ["--help"] $ do
        _ <- execParser (info (hiddenHelper <*>
                               ((,) <$>
                                parser <*>
                                some (strArgument (metavar "OTHER ARGUMENTS"))))
                        (fullDesc <> progDesc pd))
        return ()
  where hiddenHelper = abortOption ShowHelpText (long "help" <> hidden <> internal)

-- | 'option', specialized to 'Text'.
textOption :: Mod OptionFields Text -> Parser Text
textOption = option (T.pack <$> readerAsk)

-- | 'argument', specialized to 'Text'.
textArgument :: Mod ArgumentFields Text -> Parser Text
textArgument = argument (T.pack <$> readerAsk)
