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

-- | Enable/disable flags for a @Bool@.
boolFlags :: Bool -> String -> String -> Mod FlagFields Bool -> Parser Bool
boolFlags defaultValue = enableDisableFlags defaultValue True False

-- | Enable/disable flags for a @Bool@, without a default case (to allow chaining @<|>@s).
boolFlagsNoDefault :: Maybe Bool -> String -> String -> Mod FlagFields Bool -> Parser Bool
boolFlagsNoDefault = enableDisableFlagsNoDefault True False

-- | Enable/disable flags for a @(Maybe Bool)@.
maybeBoolFlags :: String -> String -> Mod FlagFields (Maybe Bool) -> Parser (Maybe Bool)
maybeBoolFlags = enableDisableFlags Nothing (Just True) (Just False)

-- | Enable/disable flags for any type.
enableDisableFlags :: (Eq a) => a -> a -> a -> String -> String -> Mod FlagFields a -> Parser a
enableDisableFlags defaultValue enabledValue disabledValue name helpSuffix mods =
  enableDisableFlagsNoDefault enabledValue disabledValue (Just defaultValue) name helpSuffix mods <|>
  pure defaultValue

-- | Enable/disable flags for any type, without a default (to allow chaining @<|>@s)
enableDisableFlagsNoDefault :: (Eq a) => a -> a -> Maybe a -> String -> String -> Mod FlagFields a -> Parser a
enableDisableFlagsNoDefault enabledValue disabledValue maybeHideValue name helpSuffix mods =
  last <$> some (enableDisableFlagsNoDefault' enabledValue disabledValue maybeHideValue name helpSuffix mods)

enableDisableFlagsNoDefault' :: (Eq a) => a -> a -> Maybe a -> String -> String -> Mod FlagFields a -> Parser a
enableDisableFlagsNoDefault' enabledValue disabledValue maybeHideValue name helpSuffix mods =
    let hideEnabled = Just enabledValue == maybeHideValue
        hideDisabled = Just disabledValue == maybeHideValue
    in flag'
           enabledValue
           ((if hideEnabled
                 then hidden <> internal
                 else idm) <>
            long name <>
            help
                (concat $ concat
                     [ ["Enable ", helpSuffix]
                     , [" (--no-" ++ name ++ " to disable)" | hideDisabled]]) <>
            mods) <|>
       flag'
           enabledValue
           (hidden <> internal <> long ("enable-" ++ name) <> mods) <|>
       flag'
           disabledValue
           ((if hideDisabled
                 then hidden <> internal
                 else idm) <>
            long ("no-" ++ name) <>
            help
                (concat $ concat
                     [ ["Disable ", helpSuffix]
                     , [" (--" ++ name ++ " to enable)" | hideEnabled]]) <>
            mods) <|>
       flag'
           disabledValue
           (hidden <> internal <> long ("disable-" ++ name) <> mods)

-- | Show an extra help option (e.g. @--docker-help@ shows help for all @--docker*@ args).
-- To actually show have that help appear, use 'execExtraHelp' before executing the main parser.
extraHelpOption :: String -> String -> String -> Parser (a -> a)
extraHelpOption progName fakeName helpName =
    infoOption (optDesc' ++ ".") (long helpName <> hidden <> internal) <*>
    infoOption (optDesc' ++ ".") (long fakeName <> help optDesc')
  where optDesc' = concat ["Run '", takeBaseName progName, " --", helpName, "' for details"]

-- | Display extra help if extea help option passed in arguments.
-- Since optparse-applicative doesn't allow an arbirary IO action for an 'abortOption', this
-- was the best way I found that doesn't require manually formatting the help.
execExtraHelp :: [String] -> String -> Parser a -> String -> IO ()
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

textOption :: Mod OptionFields Text -> Parser Text
textOption = option (T.pack <$> readerAsk)

textArgument :: Mod ArgumentFields Text -> Parser Text
textArgument = argument (T.pack <$> readerAsk)
