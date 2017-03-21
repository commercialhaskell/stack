{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Extra functions for optparse-applicative.

module Options.Applicative.Builder.Extra
  (boolFlags
  ,boolFlagsNoDefault
  ,maybeBoolFlags
  ,firstBoolFlags
  ,enableDisableFlags
  ,enableDisableFlagsNoDefault
  ,extraHelpOption
  ,execExtraHelp
  ,textOption
  ,textArgument
  ,optionalFirst
  ,absFileOption
  ,relFileOption
  ,absDirOption
  ,relDirOption
  ,eitherReader'
  ,fileCompleter
  ,fileExtCompleter
  ,dirCompleter
  ,PathCompleterOpts(..)
  ,defaultPathCompleterOpts
  ,pathCompleterWith
  ) where

import Control.Exception (IOException, catch)
import Control.Monad (when, forM)
import Data.Either.Combinators
import Data.List (isPrefixOf)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import Options.Applicative.Types (readerAsk, Completer(..))
import Path hiding ((</>))
import System.Directory (getCurrentDirectory, getDirectoryContents, doesDirectoryExist)
import System.Environment (withArgs)
import System.FilePath (takeBaseName, (</>), splitFileName, isRelative, takeExtension)

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

-- | Like 'maybeBoolFlags', but parsing a 'First'.
firstBoolFlags :: String -> String -> Mod FlagFields (Maybe Bool) -> Parser (First Bool)
firstBoolFlags long0 help0 mod0 = First <$> maybeBoolFlags long0 help0 mod0

-- | Enable/disable flags for any type.
enableDisableFlags :: a                 -- ^ Default value
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
enableDisableFlagsNoDefault :: a                 -- ^ Enabled value
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
           (long ("[no-]" ++ name) <>
            help ("Enable/disable " ++ helpSuffix) <>
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

-- | Like 'optional', but returning a 'First'.
optionalFirst :: Alternative f => f a -> f (First a)
optionalFirst = fmap First . optional

absFileOption :: Mod OptionFields (Path Abs File) -> Parser (Path Abs File)
absFileOption mods = option (eitherReader' parseAbsFile) $
  completer (pathCompleterWith defaultPathCompleterOpts { pcoRelative = False }) <> mods

relFileOption :: Mod OptionFields (Path Rel File) -> Parser (Path Rel File)
relFileOption mods = option (eitherReader' parseRelFile) $
  completer (pathCompleterWith defaultPathCompleterOpts { pcoAbsolute = False }) <> mods

absDirOption :: Mod OptionFields (Path Abs Dir) -> Parser (Path Abs Dir)
absDirOption mods = option (eitherReader' parseAbsDir) $
  completer (pathCompleterWith defaultPathCompleterOpts { pcoRelative = False, pcoFileFilter = const False }) <> mods

relDirOption :: Mod OptionFields (Path Rel Dir) -> Parser (Path Rel Dir)
relDirOption mods = option (eitherReader' parseRelDir) $
  completer (pathCompleterWith defaultPathCompleterOpts { pcoAbsolute = False, pcoFileFilter = const False }) <> mods

-- | Like 'eitherReader', but accepting any @'Show' e@ on the 'Left'.
eitherReader' :: Show e => (String -> Either e a) -> ReadM a
eitherReader' f = eitherReader (mapLeft show . f)

data PathCompleterOpts = PathCompleterOpts
    { pcoAbsolute :: Bool
    , pcoRelative :: Bool
    , pcoRootDir :: Maybe FilePath
    , pcoFileFilter :: FilePath -> Bool
    , pcoDirFilter :: FilePath -> Bool
    }

defaultPathCompleterOpts :: PathCompleterOpts
defaultPathCompleterOpts = PathCompleterOpts
    { pcoAbsolute = True
    , pcoRelative = True
    , pcoRootDir = Nothing
    , pcoFileFilter = const True
    , pcoDirFilter = const True
    }

fileCompleter :: Completer
fileCompleter = pathCompleterWith defaultPathCompleterOpts

fileExtCompleter :: [String] -> Completer
fileExtCompleter exts = pathCompleterWith defaultPathCompleterOpts { pcoFileFilter = (`elem` exts) . takeExtension }

dirCompleter :: Completer
dirCompleter = pathCompleterWith defaultPathCompleterOpts { pcoFileFilter = const False }

pathCompleterWith :: PathCompleterOpts -> Completer
pathCompleterWith PathCompleterOpts {..} = mkCompleter $ \inputRaw -> do
    -- Unescape input, to handle single and double quotes. Note that the
    -- results do not need to be re-escaped, due to some fiddly bash
    -- magic.
    let input = unescapeBashArg inputRaw
    let (inputSearchDir0, searchPrefix) = splitFileName input
        inputSearchDir = if inputSearchDir0 == "./" then "" else inputSearchDir0
    msearchDir <-
        case (isRelative inputSearchDir, pcoAbsolute, pcoRelative) of
            (True, _, True) -> do
                rootDir <- maybe getCurrentDirectory return pcoRootDir
                return $ Just (rootDir </> inputSearchDir)
            (False, True, _) -> return $ Just inputSearchDir
            _ -> return Nothing
    case msearchDir of
        Nothing
            | input == "" && pcoAbsolute -> return ["/"]
            | otherwise -> return []
        Just searchDir -> do
            entries <- getDirectoryContents searchDir `catch` \(_ :: IOException) -> return []
            results <- fmap catMaybes $ forM entries $ \entry ->
                -- Skip . and .. unless user is typing . or ..
                if entry `elem` ["..", "."] && searchPrefix `notElem` ["..", "."] then return Nothing else
                    if searchPrefix `isPrefixOf` entry
                        then do
                            let path = searchDir </> entry
                            case (pcoFileFilter path, pcoDirFilter path) of
                                (True, True) -> return $ Just (inputSearchDir </> entry)
                                (fileAllowed, dirAllowed) -> do
                                    isDir <- doesDirectoryExist path
                                    if (if isDir then dirAllowed else fileAllowed)
                                        then return $ Just (inputSearchDir </> entry)
                                        else return Nothing
                        else return Nothing
            return results

unescapeBashArg :: String -> String
unescapeBashArg ('\'' : rest) = rest
unescapeBashArg ('\"' : rest) = go rest
  where
    go [] = []
    go ('\\' : x : xs)
        | x `elem` "$`\"\\\n" = x : xs
        | otherwise = '\\' : x : go xs
    go (x : xs) = x : go xs
unescapeBashArg input = go input
  where
    go [] = []
    go ('\\' : x : xs) = x : go xs
    go (x : xs) = x : go xs
