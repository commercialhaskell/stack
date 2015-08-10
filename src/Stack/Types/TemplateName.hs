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

-- | A template name of the format @foo.hsfiles@.
data TemplateName = TemplateName !Text !(Path Rel File)

-- | An argument which accepts a template name of the format
-- @foo.hsfiles@ or @foo@, ultimately normalized to @foo.hsfiles@.
templateNameArgument :: O.Mod O.ArgumentFields TemplateName
                     -> O.Parser TemplateName
templateNameArgument =
    O.argument
        (do string <- O.str
            either O.readerError return (parseTemplateNameFromString string))

parseTemplateNameFromString :: String -> Either String TemplateName
parseTemplateNameFromString fname =
    case T.stripSuffix ".hsfiles" (T.pack fname) of
        Nothing -> parseValidFile (T.pack fname) (fname <> ".hsfiles")
        Just prefix -> parseValidFile prefix fname
  where
    parseValidFile prefix str =
        case parseRelFile str of
            Nothing -> Left expected
            Just fp -> return (TemplateName prefix fp)
    expected = "Expected a template filename like: foo or foo.hsfiles"


-- | Make a template name.
mkTemplateName :: String -> Q Exp
mkTemplateName s =
    case parseTemplateNameFromString s of
        Left{} -> error ("Invalid template name: " ++ show s)
        Right (TemplateName (T.unpack -> prefix) (Path pn)) ->
            [|TemplateName (T.pack prefix) (Path pn)|]

-- | Get a text representation of the template name.
templateName :: TemplateName -> Text
templateName (TemplateName prefix _) = prefix

-- | Get the path of the template.
templatePath :: TemplateName -> Path Rel File
templatePath (TemplateName _ fp) = fp
