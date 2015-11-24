{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Template name handling.

module Stack.Types.TemplateName where

import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.Haskell.TH
import qualified Options.Applicative as O
import           Path
import           Path.Internal

-- | A template name.
data TemplateName = TemplateName !Text !(Either (Path Abs File) (Path Rel File))
  deriving (Ord,Eq,Show)

-- | An argument which accepts a template name of the format
-- @foo.hsfiles@ or @foo@, ultimately normalized to @foo@.
templateNameArgument :: O.Mod O.ArgumentFields TemplateName
                     -> O.Parser TemplateName
templateNameArgument =
    O.argument
        (do string <- O.str
            either O.readerError return (parseTemplateNameFromString string))

-- | An argument which accepts a @key:value@ pair for specifying parameters.
templateParamArgument :: O.Mod O.OptionFields (Text,Text)
                      -> O.Parser (Text,Text)
templateParamArgument =
    O.option
        (do string <- O.str
            either O.readerError return (parsePair string))
  where
    parsePair :: String -> Either String (Text, Text)
    parsePair s =
        case break (==':') s of
            (key,':':value@(_:_)) -> Right (T.pack key, T.pack value)
            _ -> Left ("Expected key:value format for argument: " <> s)

-- | Parse a template name from a string.
parseTemplateNameFromString :: String -> Either String TemplateName
parseTemplateNameFromString fname =
    case T.stripSuffix ".hsfiles" (T.pack fname) of
        Nothing -> parseValidFile (T.pack fname) (fname <> ".hsfiles")
        Just prefix -> parseValidFile prefix fname
  where
    parseValidFile prefix str =
        case parseRelFile str of
            Nothing ->
                case parseAbsFile str of
                    Nothing -> Left expected
                    Just fp -> return (TemplateName prefix (Left fp))
            Just fp -> return (TemplateName prefix (Right fp))
    expected = "Expected a template filename like: foo or foo.hsfiles"

-- | Make a template name.
mkTemplateName :: String -> Q Exp
mkTemplateName s =
    case parseTemplateNameFromString s of
        Left{} -> error ("Invalid template name: " ++ show s)
        Right (TemplateName (T.unpack -> prefix) p) ->
            [|TemplateName (T.pack prefix) $(pn)|]
            where pn =
                      case p of
                          Left (Path fp) -> [|Left (Path fp)|]
                          Right (Path fp) -> [|Right (Path fp)|]

-- | Get a text representation of the template name.
templateName :: TemplateName -> Text
templateName (TemplateName prefix _) = prefix

-- | Get the path of the template.
templatePath :: TemplateName -> Either (Path Abs File) (Path Rel File)
templatePath (TemplateName _ fp) = fp
