{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Stack.New
    ( newProject
    ) where

import           Control.Monad          (filterM, forM_, unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger   (MonadLogger, logInfo)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as S
import           Data.FileEmbed         (embedDir)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import qualified Data.Text              as T
import           System.Directory       (createDirectoryIfMissing,
                                         doesFileExist)
import           System.FilePath        (takeDirectory)

newProject :: (MonadIO m, MonadLogger m)
           => m ()
newProject = do
    $logInfo "NOTE: Currently stack new functionality is very rudimentary"
    $logInfo "There are plans to make this feature more useful in the future"
    $logInfo "For more information, see: https://github.com/commercialhaskell/stack/issues/137"
    $logInfo "For now, we'll just be generating a basic project structure in your current directory"

    exist <- filterM (liftIO . doesFileExist) (Map.keys files)
    unless (null exist) $
        error $ unlines
            $ "The following files already exist, refusing to overwrite:"
            : map ("- " ++) exist

    $logInfo ""
    forM_ (Map.toList files) $ \(fp, bs) -> do
        $logInfo $ T.pack $ "Writing: " ++ fp
        liftIO $ do
            createDirectoryIfMissing True $ takeDirectory fp
            S.writeFile fp bs

files :: Map FilePath ByteString
files = Map.fromList $(embedDir "new-template")
