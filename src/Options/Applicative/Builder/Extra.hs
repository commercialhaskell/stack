-- | Extra functions for optparse-applicative.

module Options.Applicative.Builder.Extra
  (boolFlags
  ,maybeBoolFlags
  ,enableDisableFlags)
  where

import Options.Applicative

-- | Enable/disable flags for a @Bool@.
boolFlags :: Bool -> String -> String -> Parser Bool
boolFlags defaultValue = enableDisableFlags defaultValue True False

-- | Enable/disable flags for a @(Maybe Bool)@.
maybeBoolFlags :: String -> String -> Parser (Maybe Bool)
maybeBoolFlags = enableDisableFlags Nothing (Just True) (Just False)

-- | Enable/disable flags for any type.
enableDisableFlags :: a -> a -> a -> String -> String -> Parser a
enableDisableFlags defaultValue enabledValue disabledValue name helpSuffix =
  flag' enabledValue
        (long name <>
         help ("Enable " ++ helpSuffix)) <|>
  flag' enabledValue
        (internal <>
         long ("enable-" ++ name) <>
         help ("Enable " ++ helpSuffix)) <|>
  flag' disabledValue
        (long ("no-" ++ name) <>
         help ("Disable " ++ helpSuffix)) <|>
  flag' disabledValue
        (internal <>
         long ("disable-" ++ name) <>
         help ("Disable " ++ helpSuffix)) <|>
  pure defaultValue
