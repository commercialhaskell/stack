{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Options.Applicative.Builder.Extra
Description : Extra functions for optparse-applicative.
License     : BSD-3-Clause

Extra functions for optparse-applicative.
-}

module Options.Applicative.Builder.Extra
  ( boolFlags
  , boolFlagsNoDefault
  , firstBoolFlagsNoDefault
  , firstBoolFlagsTrue
  , firstBoolFlagsFalse
  , enableDisableFlags
  , enableDisableFlagsNoDefault
  , extraHelpOption
  , execExtraHelp
  , textOption
  , textArgument
  , optionalFirst
  , optionalFirstTrue
  , optionalFirstFalse
  , absFileOption
  , relFileOption
  , absDirOption
  , relDirOption
  , eitherReader'
  , fileCompleter
  , fileExtCompleter
  , dirCompleter
  , PathCompleterOpts (..)
  , defaultPathCompleterOpts
  , pathCompleterWith
  , unescapeBashArg
  , showHelpText
  ) where

import           Data.List ( isPrefixOf )
import qualified Data.Text as T
import           Options.Applicative
                   ( ArgumentFields, Completer, FlagFields, Mod, OptionFields
                   , ParseError (..), Parser, ReadM, abortOption, argument
                   , completer, eitherReader, execParser, flag', fullDesc, help
                   , hidden, idm, info, infoOption, internal, long, metavar
                   , mkCompleter, option, progDesc, strArgument
                   )
import           Options.Applicative.Types ( readerAsk )
import           Path ( parseAbsDir, parseAbsFile, parseRelDir, parseRelFile )
import           Stack.Prelude
import           System.Directory
                   ( doesDirectoryExist, getCurrentDirectory
                   , getDirectoryContents
                   )
import           System.Environment ( withArgs )
import           System.FilePath
                   ( (</>), isRelative, splitFileName, takeBaseName
                   , takeExtension
                   )

-- | Type representing exceptions thrown by functions exported by the
-- "Options.Applicative.Builder.Extra" module.
data OptionsApplicativeExtraException
  = FlagNotFoundBug
  deriving (Show, Typeable)

instance Exception OptionsApplicativeExtraException where
  displayException FlagNotFoundBug =
    "Error: [S-2797]\n"
    ++ "The impossible happened! No valid flags found in \
       \enableDisableFlagsNoDefault. Please report this bug at Stack's \
       \repository."

-- | Enable/disable flags for a 'Bool'.
boolFlags ::
     Bool                 -- ^ Default value
  -> String               -- ^ Flag name
  -> String               -- ^ Help suffix
  -> Mod FlagFields Bool
  -> Parser Bool
boolFlags defaultValue name helpSuffix =
  enableDisableFlags defaultValue True False name $ concat
    [ helpSuffix
    , " (default: "
    , if defaultValue then "enabled" else "disabled"
    , ")"
    ]

-- | Enable/disable flags for a 'Bool', without a default case (to allow
-- chaining with '<|>').
boolFlagsNoDefault ::
     String               -- ^ Flag name
  -> String               -- ^ Help suffix
  -> Mod FlagFields Bool
  -> Parser Bool
boolFlagsNoDefault = enableDisableFlagsNoDefault True False

-- | Flag with no default of True or False
firstBoolFlagsNoDefault ::
     String
  -> String
  -> Mod FlagFields (Maybe Bool)
  -> Parser (First Bool)
firstBoolFlagsNoDefault name helpSuffix mod' =
  First <$>
  enableDisableFlags Nothing (Just True) (Just False)
  name helpSuffix mod'

-- | Flag with a Semigroup instance and a default of True
firstBoolFlagsTrue ::
     String
  -> String
  -> Mod FlagFields FirstTrue
  -> Parser FirstTrue
firstBoolFlagsTrue name helpSuffix =
  enableDisableFlags mempty (FirstTrue (Just True)) (FirstTrue (Just False))
  name $ helpSuffix ++ " (default: enabled)"

-- | Flag with a Semigroup instance and a default of False
firstBoolFlagsFalse ::
     String
  -> String
  -> Mod FlagFields FirstFalse
  -> Parser FirstFalse
firstBoolFlagsFalse name helpSuffix =
  enableDisableFlags mempty (FirstFalse (Just True)) (FirstFalse (Just False))
  name $ helpSuffix ++ " (default: disabled)"

-- | Enable/disable flags for any type.
enableDisableFlags ::
     a                 -- ^ Default value
  -> a                 -- ^ Enabled value
  -> a                 -- ^ Disabled value
  -> String            -- ^ Name
  -> String            -- ^ Help suffix
  -> Mod FlagFields a
  -> Parser a
enableDisableFlags defaultValue enabledValue disabledValue name helpSuffix
  mods =
    enableDisableFlagsNoDefault
      enabledValue
      disabledValue
      name
      helpSuffix
      mods <|> pure defaultValue

-- | Enable/disable flags for any type, without a default (to allow chaining with '<|>')
enableDisableFlagsNoDefault ::
     a                 -- ^ Enabled value
  -> a                 -- ^ Disabled value
  -> String            -- ^ Name
  -> String            -- ^ Help suffix
  -> Mod FlagFields a
  -> Parser a
enableDisableFlagsNoDefault enabledValue disabledValue name helpSuffix mods =
  last <$> some
    (   flag'
          enabledValue
          (  hidden
          <> internal
          <> long name
          <> help helpSuffix
          <> mods
          )
    <|> flag'
          disabledValue
          (  hidden
          <> internal
          <> long ("no-" ++ name)
          <> help helpSuffix
          <> mods
          )
    <|> flag'
          disabledValue
          (  long ("[no-]" ++ name)
          <> help ("Enable/disable " ++ helpSuffix)
          <> mods
          )
    )
 where
  last xs =
    case reverse xs of
      [] -> impureThrow FlagNotFoundBug
      x:_ -> x

-- | Show an extra help option (e.g. @--docker-help@ shows help for all
-- @--docker*@ args).
--
-- To actually have that help appear, use 'execExtraHelp' before executing the
-- main parser.
extraHelpOption ::
     Bool             -- ^ Hide from the brief description?
  -> String           -- ^ Program name, e.g. @"stack"@
  -> String           -- ^ Option glob expression, e.g. @"docker*"@
  -> String           -- ^ Help option name, e.g. @"docker-help"@
  -> Parser (a -> a)
extraHelpOption hide progName fakeName helpName =
      infoOption
        (optDesc' ++ ".")
        (long helpName <> hidden <> internal)
  <*> infoOption
        (optDesc' ++ ".")
        (  long fakeName
        <> help optDesc'
        <> (if hide then hidden <> internal else idm)
        )
 where
  optDesc' = concat
    [ "Run '"
    , takeBaseName progName
    , " --"
    , helpName
    , "' for details."
    ]

-- | Display extra help if extra help option passed in arguments.
--
-- Since optparse-applicative doesn't allow an arbitrary IO action for an
-- 'abortOption', this was the best way I found that doesn't require manually
-- formatting the help.
execExtraHelp ::
     [String]  -- ^ Command line arguments
  -> String    -- ^ Extra help option name, e.g. @"docker-help"@
  -> Parser a  -- ^ Option parser for the relevant command
  -> String    -- ^ Option description
  -> IO ()
execExtraHelp args helpOpt parser pd =
  when (args == ["--" ++ helpOpt]) $
    withArgs ["--help"] $
      void $ execParser (info
             (   hiddenHelper
             <*> ( (,)
                     <$> parser
                     <*> some (strArgument
                           (metavar "OTHER ARGUMENTS") :: Parser String)
                 )
             )
            (fullDesc <> progDesc pd))
 where
  hiddenHelper = abortOption showHelpText (long "help" <> hidden <> internal)

-- | 'option', specialized to 'Text'.
textOption :: Mod OptionFields Text -> Parser Text
textOption = option (T.pack <$> readerAsk)

-- | 'argument', specialized to 'Text'.
textArgument :: Mod ArgumentFields Text -> Parser Text
textArgument = argument (T.pack <$> readerAsk)

-- | Like 'optional', but returning a t'First'.
optionalFirst :: Alternative f => f a -> f (First a)
optionalFirst = fmap First . optional

-- | Like 'optional', but returning a t'FirstTrue'.
optionalFirstTrue :: Alternative f => f Bool -> f FirstTrue
optionalFirstTrue = fmap FirstTrue . optional

-- | Like 'optional', but returning a t'FirstFalse'.
optionalFirstFalse :: Alternative f => f Bool -> f FirstFalse
optionalFirstFalse = fmap FirstFalse . optional

absFileOption :: Mod OptionFields (Path Abs File) -> Parser (Path Abs File)
absFileOption mods = option (eitherReader' parseAbsFile) $
     completer
       (pathCompleterWith defaultPathCompleterOpts { relative = False })
  <> mods

relFileOption :: Mod OptionFields (Path Rel File) -> Parser (Path Rel File)
relFileOption mods = option (eitherReader' parseRelFile) $
     completer
       (pathCompleterWith defaultPathCompleterOpts { absolute = False })
  <> mods

absDirOption :: Mod OptionFields (Path Abs Dir) -> Parser (Path Abs Dir)
absDirOption mods = option (eitherReader' parseAbsDir) $
     completer
       ( pathCompleterWith
          defaultPathCompleterOpts
            { relative = False
            , fileFilter = const False
            }
       )
  <> mods

relDirOption :: Mod OptionFields (Path Rel Dir) -> Parser (Path Rel Dir)
relDirOption mods = option (eitherReader' parseRelDir) $
     completer
       ( pathCompleterWith
           defaultPathCompleterOpts
             { absolute = False
             , fileFilter = const False
             }
       )
  <> mods

-- | Like 'eitherReader', but accepting any @'Show' e@ on the 'Left'.
eitherReader' :: Show e => (String -> Either e a) -> ReadM a
eitherReader' f = eitherReader (mapLeft show . f)

data PathCompleterOpts = PathCompleterOpts
  { absolute :: Bool
  , relative :: Bool
  , rootDir :: Maybe FilePath
  , fileFilter :: FilePath -> Bool
  , dirFilter :: FilePath -> Bool
  }

defaultPathCompleterOpts :: PathCompleterOpts
defaultPathCompleterOpts = PathCompleterOpts
  { absolute = True
  , relative = True
  , rootDir = Nothing
  , fileFilter = const True
  , dirFilter = const True
  }

fileCompleter :: Completer
fileCompleter = pathCompleterWith defaultPathCompleterOpts

fileExtCompleter :: [String] -> Completer
fileExtCompleter exts =
  pathCompleterWith
    defaultPathCompleterOpts { fileFilter = (`elem` exts) . takeExtension }

dirCompleter :: Completer
dirCompleter =
  pathCompleterWith defaultPathCompleterOpts { fileFilter = const False }

pathCompleterWith :: PathCompleterOpts -> Completer
pathCompleterWith pco = mkCompleter $ \inputRaw -> do
  -- Unescape input, to handle single and double quotes. Note that the
  -- results do not need to be re-escaped, due to some fiddly bash
  -- magic.
  let input = unescapeBashArg inputRaw
  let (inputSearchDir0, searchPrefix) = splitFileName input
      inputSearchDir = if inputSearchDir0 == "./" then "" else inputSearchDir0
  msearchDir <-
    case (isRelative inputSearchDir, pco.absolute, pco.relative) of
      (True, _, True) -> do
        rootDir <- maybe getCurrentDirectory pure pco.rootDir
        pure $ Just (rootDir </> inputSearchDir)
      (False, True, _) -> pure $ Just inputSearchDir
      _ -> pure Nothing
  case msearchDir of
    Nothing
      | input == "" && pco.absolute -> pure ["/"]
      | otherwise -> pure []
    Just searchDir -> do
      entries <-
        getDirectoryContents searchDir `catch` \(_ :: IOException) -> pure []
      fmap catMaybes $ forM entries $ \entry ->
        -- Skip . and .. unless user is typing . or ..
        if entry `elem` ["..", "."] && searchPrefix `notElem` ["..", "."]
          then pure Nothing
          else
            if searchPrefix `isPrefixOf` entry
              then do
                let path = searchDir </> entry
                case (pco.fileFilter path, pco.dirFilter path) of
                  (True, True) -> pure $ Just (inputSearchDir </> entry)
                  (fileAllowed, dirAllowed) -> do
                    isDir <- doesDirectoryExist path
                    if (if isDir then dirAllowed else fileAllowed)
                      then pure $ Just (inputSearchDir </> entry)
                      else pure Nothing
              else pure Nothing

unescapeBashArg :: String -> String
unescapeBashArg ('\'' : rest) = rest
unescapeBashArg ('\"' : rest) = go rest
 where
  special = "$`\"\\\n" :: String
  go [] = []
  go ('\\' : x : xs)
    | x `elem` special = x : xs
    | otherwise = '\\' : x : go xs
  go (x : xs) = x : go xs
unescapeBashArg input = go input
 where
  go [] = []
  go ('\\' : x : xs) = x : go xs
  go (x : xs) = x : go xs

showHelpText :: ParseError
showHelpText = ShowHelpText Nothing
