{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Template name handling.

module Stack.Types.TemplateName
  ( TemplateName
  , RepoTemplatePath (..)
  , RepoService (..)
  , TemplatePath (..)
  , templateName
  , templatePath
  , parseTemplateNameFromString
  , parseRepoPathWithService
  , templateNameArgument
  , templateParamArgument
  , defaultTemplateName
  ) where

import           Data.Aeson ( FromJSON (..), withText )
import qualified Data.Text as T
import           Network.HTTP.StackClient ( parseRequest )
import qualified Options.Applicative as O
import           Path
import           Stack.Prelude

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Types.TemplateName" module.
newtype TypeTemplateNameException
    = DefaultTemplateNameNotParsedBug String
    deriving (Show, Typeable)

instance Exception TypeTemplateNameException where
    displayException (DefaultTemplateNameNotParsedBug s) = bugReport "[S-7410]" $
        "The impossible happened! Cannot parse default template name: "
        ++ s

-- | A template name.
data TemplateName = TemplateName !Text !TemplatePath
  deriving (Ord, Eq, Show)

data TemplatePath = AbsPath (Path Abs File)
                  -- ^ an absolute path on the filesystem
                  | RelPath String (Path Rel File)
                  -- ^ a relative path on the filesystem, or relative to
                  -- the template repository. To avoid path separator conversion
                  -- on Windows, the raw command-line parameter passed is also
                  -- given as the first field (possibly with @.hsfiles@ appended).
                  | UrlPath String
                  -- ^ a full URL
                  | RepoPath RepoTemplatePath
  deriving (Eq, Ord, Show)

-- | Details for how to access a template from a remote repo.
data RepoTemplatePath = RepoTemplatePath
    { rtpService  :: RepoService
    , rtpUser     :: Text
    , rtpTemplate :: Text
    }
    deriving (Eq, Ord, Show)

-- | Services from which templates can be retrieved from a repository.
data RepoService = GitHub | GitLab | Bitbucket
    deriving (Eq, Ord, Show)

instance FromJSON TemplateName where
    parseJSON = withText "TemplateName" $
        either fail pure . parseTemplateNameFromString . T.unpack

-- | An argument which accepts a template name of the format
-- @foo.hsfiles@ or @foo@, ultimately normalized to @foo@.
templateNameArgument :: O.Mod O.ArgumentFields TemplateName
                     -> O.Parser TemplateName
templateNameArgument =
    O.argument
        (do s <- O.str
            either O.readerError pure (parseTemplateNameFromString s))

-- | An argument which accepts a @key:value@ pair for specifying parameters.
templateParamArgument :: O.Mod O.OptionFields (Text,Text)
                      -> O.Parser (Text,Text)
templateParamArgument =
    O.option
        (do s <- O.str
            either O.readerError pure (parsePair s))
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
        Nothing -> parseValidFile (T.pack fname) (fname <> ".hsfiles") fname
        Just prefix -> parseValidFile prefix fname fname
  where
    parseValidFile prefix hsf orig = maybe (Left expected) Right
                                           $ asum (validParses prefix hsf orig)
    validParses prefix hsf orig =
        -- NOTE: order is important
        [ TemplateName prefix        . RepoPath <$> parseRepoPath hsf
        , TemplateName (T.pack orig) . UrlPath <$> (parseRequest orig *> Just orig)
        , TemplateName prefix        . AbsPath <$> parseAbsFile hsf
        , TemplateName prefix        . RelPath hsf <$> parseRelFile hsf
        ]
    expected = "Expected a template like: foo or foo.hsfiles or\
               \ https://example.com/foo.hsfiles or github:user/foo"

-- | The default template name you can use if you don't have one.
defaultTemplateName :: TemplateName
defaultTemplateName =
  case parseTemplateNameFromString "new-template" of
    Left s -> impureThrow $ DefaultTemplateNameNotParsedBug s
    Right x -> x

-- | Get a text representation of the template name.
templateName :: TemplateName -> Text
templateName (TemplateName prefix _) = prefix

-- | Get the path of the template.
templatePath :: TemplateName -> TemplatePath
templatePath (TemplateName _ fp) = fp

defaultRepoUserForService :: RepoService -> Maybe Text
defaultRepoUserForService GitHub = Just "commercialhaskell"
defaultRepoUserForService _      = Nothing

-- | Parses a template path of the form @github:user/template@.
parseRepoPath :: String -> Maybe RepoTemplatePath
parseRepoPath s =
  case T.splitOn ":" (T.pack s) of
    ["github"    , rest] -> parseRepoPathWithService GitHub rest
    ["gitlab"    , rest] -> parseRepoPathWithService GitLab rest
    ["bitbucket" , rest] -> parseRepoPathWithService Bitbucket rest
    _                    -> Nothing

-- | Parses a template path of the form @user/template@, given a service
parseRepoPathWithService :: RepoService -> Text -> Maybe RepoTemplatePath
parseRepoPathWithService service path =
  case T.splitOn "/" path of
    [user, name] -> Just $ RepoTemplatePath service user name
    [name]       -> do
        repoUser <- defaultRepoUserForService service
        Just $ RepoTemplatePath service repoUser name
    _            -> Nothing
