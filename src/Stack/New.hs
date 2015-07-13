{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Stack.New
    ( newProject
    ) where

import           Control.Monad          (filterM, forM_, forM, unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger   (MonadLogger, logInfo)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as S
import           Data.FileEmbed         (embedDir)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.Lazy         as LT
import           System.Directory       (createDirectoryIfMissing,
                                         doesFileExist,
                                         getCurrentDirectory)
import           System.FilePath        (takeDirectory,
                                         takeFileName,
                                         dropTrailingPathSeparator)
import           Text.Hastache
import           Text.Hastache.Context

import           Stack.Init (InitOpts(forceOverwrite))

newProject :: (MonadIO m, MonadLogger m)
           => InitOpts
           -> m ()
newProject initOpts = do
    $logInfo "NOTE: Currently stack new functionality is very rudimentary"
    $logInfo "There are plans to make this feature more useful in the future"
    $logInfo "For more information, see: https://github.com/commercialhaskell/stack/issues/137"
    $logInfo "For now, we'll just be generating a basic project structure in your current directory"

    exist <- filterM (liftIO . doesFileExist) (Map.keys files)
    unless (forceOverwrite initOpts || null exist) $
       error $ unlines
           $ "The following files already exist, refusing to overwrite (no --force):"
           : map ("- " ++) exist

    $logInfo ""

    -- Detect settings for mustache template.
    currentDirectory <- liftIO getCurrentDirectory
    let name = takeFileName $ dropTrailingPathSeparator currentDirectory

    let contextLookup "name" = MuVariable name
        contextLookup _ = MuNothing

    let runHastache template =
            hastacheStr defaultConfig template (mkStrContext contextLookup)

    -- Render file paths and file contents via mustache.
    -- There is some unsafety in `unMustache` on file names,
    -- because file names could collide.
    -- I believe the correct way to handle this is to tell template creators
    -- to be careful to avoid this if they use mustache in file names.
    -- ~ Dan Burton
    files' <- liftIO $ forM (Map.toList files) $ \(fp, bs) -> do
        let fpText = T.pack fp
        fpLText' <- runHastache fpText
        let fp' = LT.unpack fpLText'

        let bsText = T.decodeUtf8 bs
        bsLText' <- runHastache bsText
        let bs' = T.encodeUtf8 $ LT.toStrict bsLText'

        return (fp', bs')

    forM_ files' $ \(fp, bs) -> do
        $logInfo $ T.pack $ "Writing: " ++ fp
        liftIO $ do
            createDirectoryIfMissing True $ takeDirectory fp
            S.writeFile fp bs

files :: Map FilePath ByteString
files = Map.fromList $(embedDir "new-template")
