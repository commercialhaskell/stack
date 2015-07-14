{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Stack.New
    ( newProject
    , NewOpts(..)
    ) where

import           Control.Monad          (filterM, forM_, forM, unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger   (MonadLogger, logInfo, logDebug, logError)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as S
import           Data.FileEmbed         (embedDir)
import qualified Data.List              as List
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.Lazy         as LT
import           System.Directory       (createDirectoryIfMissing,
                                         doesFileExist,
                                         getCurrentDirectory)
import           System.FilePath        (takeDirectory,
                                         takeFileName,
                                         dropTrailingPathSeparator)
import           System.Exit            (exitFailure)
import           Text.Hastache
import           Text.Hastache.Context

import           Stack.Init (InitOpts(forceOverwrite))

data NewOpts = NewOpts
    { newOptsTemplateRepository :: String
    , newOptsTemplate :: Maybe Template
    , newOptsTemplateArgs :: [String]
    , newOptsInitOpts :: InitOpts
    }

type Template = String

defaultTemplate :: Template
defaultTemplate = "new-template"

-- TODO(DanBurton): support multiple templates
-- Get the files associated with a given template
getFiles :: (MonadIO m, MonadLogger m)
         => String -> Template -> m (Map FilePath ByteString)
getFiles _repo "new-template" = return $  Map.fromList $(embedDir "new-template")
getFiles repo template = do
    $logError $
        "Error fetching template: " <> T.pack template <> "\n"
        <> "     from repository: " <> T.pack repo <> "\n"
        <> "\n"
        <> "Sorry, only new-template is supported right now.\n"
        <> "Support for more templates soon to come.\n"
        <> "See: https://github.com/commercialhaskell/stack/issues/137"
    liftIO exitFailure -- the end

-- Detect default key:value pairs for mustache template.
getDefaultArgs :: (MonadIO m, MonadLogger m) => m [(String, String)]
getDefaultArgs = do
    currentDirectory <- liftIO getCurrentDirectory
    let name = takeFileName $ dropTrailingPathSeparator currentDirectory
    return [("name", name)]

-- Take a list of strings of the form "key:val" and turn into a list of tuples.
toArgs :: [String] -> [(String, String)]
toArgs = map toArg
  where
    toArg s = case break (== ':') s of
        (key, ':':val) -> (key, val)
        _-> (s, "") -- TODO(DanBurton): Handle this error case better.

newProject :: (MonadIO m, MonadLogger m)
           => NewOpts
           -> m ()
newProject newOpts = do
    let NewOpts repo templateMay args0 initOpts = newOpts

    $logDebug "Calculating template arguments"
    defaultArgs <- getDefaultArgs
    -- TODO(DanBurton): Do this logic in the arg parser instead.
    let (template, args1) = case templateMay of
            Nothing -> (defaultTemplate, args0)
            -- If the "template" arg has a colon, treat as arg instead.
            Just template0 -> case break (== ':') template0 of
               (_, []) -> (template0, args0)
               (_key, _colonVal) -> (defaultTemplate, template0:args0)
    let args = toArgs args1 ++ defaultArgs

    $logDebug "Loading template files"
    files <- getFiles repo template

    let contextLookup key = case List.lookup key args of
            Just val -> MuVariable val
            Nothing  -> MuNothing

    let runHastache text =
            hastacheStr defaultConfig text (mkStrContext contextLookup)

    -- Render file paths and file contents via mustache.
    -- There is some unsafety in `unMustache` on file names,
    -- because file names could collide.
    -- I believe the correct way to handle this is to tell template creators
    -- to be careful to avoid this if they use mustache in file names.
    -- ~ Dan Burton
    $logDebug "Rendering templates"
    files' <- liftIO $ forM (Map.toList files) $ \(fp, bs) -> do
        let fpText = T.pack fp
        fpLText' <- runHastache fpText
        let fp' = LT.unpack fpLText'

        let bsText = T.decodeUtf8 bs
        bsLText' <- runHastache bsText
        let bs' = T.encodeUtf8 $ LT.toStrict bsLText'

        return (fp', bs')

    $logDebug "Checking presence of template files"
    exist <- filterM (liftIO . doesFileExist) (map fst files')
    unless (forceOverwrite initOpts || null exist) $
       error $ unlines
           $ "The following files already exist, refusing to overwrite (no --force):"
           : map ("- " ++) exist

    $logDebug "Writing template files"
    $logInfo ""

    forM_ files' $ \(fp, bs) -> do
        $logInfo $ T.pack $ "Writing: " ++ fp
        liftIO $ do
            createDirectoryIfMissing True $ takeDirectory fp
            S.writeFile fp bs
