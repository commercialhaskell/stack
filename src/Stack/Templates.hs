{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Templates
Description : Functions related to Stack's @templates@ command.
License     : BSD-3-Clause

Functions related to Stack's @templates@ command.
-}

module Stack.Templates
  ( templatesCmd
  , templatesHelp
  ) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.IO as T
import           Network.HTTP.StackClient
                   ( HttpException (..), getResponseBody, httpLbs, parseUrlThrow
                   , setGitHubHeaders
                   )
import           Stack.Prelude
import           Stack.Runners ( ShouldReexec (..), withConfig )
import           Stack.Types.Runner ( Runner )

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.Templates" module.
data TemplatesPrettyException
  = DownloadTemplatesHelpFailed !HttpException
  | TemplatesHelpEncodingInvalid !String !UnicodeException

deriving instance Show TemplatesPrettyException

instance Pretty TemplatesPrettyException where
  pretty (DownloadTemplatesHelpFailed err) =
    "[S-8143]"
    <> line
    <> fillSep
         [ flow "Stack failed to download the help for"
         , style Shell "stack templates" <> "."
         ]
    <> blankLine
    <> flow "While downloading, Stack encountered the following error:"
    <> blankLine
    <> string (displayException err)
  pretty (TemplatesHelpEncodingInvalid url err) =
    "[S-6670]"
    <> line
    <> fillSep
         [ flow "Stack failed to decode the help for"
         , style Shell "stack templates"
         , flow "downloaded from"
         , style Url (fromString url) <> "."
         ]
    <> blankLine
    <> flow "While decoding, Stack encountered the following error:"
    <> blankLine
    <> string (displayException err)

instance Exception TemplatesPrettyException

-- | Function underlying the @stack templates@ command. Display instructions for
-- how to use templates.
templatesCmd :: () -> RIO Runner ()
templatesCmd () = withConfig NoReexec templatesHelp

-- | Display help for the templates command.
templatesHelp :: HasTerm env => RIO env ()
templatesHelp = do
  let url = defaultTemplatesHelpUrl
  req <- fmap setGitHubHeaders (parseUrlThrow url)
  resp <- catch
    (httpLbs req)
    (prettyThrowM . DownloadTemplatesHelpFailed)
  case decodeUtf8' $ LB.toStrict $ getResponseBody resp of
    Left err -> prettyThrowM $ TemplatesHelpEncodingInvalid url err
    Right txt -> liftIO $ T.putStrLn txt

-- | Default web URL to get the `stack templates` help output.
defaultTemplatesHelpUrl :: String
defaultTemplatesHelpUrl =
  "https://raw.githubusercontent.com/commercialhaskell/stack-templates/master/STACK_HELP.md"
