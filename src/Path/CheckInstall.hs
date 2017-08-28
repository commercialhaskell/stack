{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Path.CheckInstall where

import           Control.Monad.Extra (anyM, (&&^))
import qualified Data.Text as T
import           Stack.Prelude
import qualified System.Directory as D
import qualified System.FilePath as FP

-- | Checks if the installed executable will be available on the user's
-- PATH. This doesn't use @envSearchPath menv@ because it includes paths
-- only visible when running in the stack environment.
warnInstallSearchPathIssues :: (MonadIO m, MonadLogger m) => FilePath -> [Text] -> m ()
warnInstallSearchPathIssues destDir installed = do
    searchPath <- liftIO FP.getSearchPath
    destDirIsInPATH <- liftIO $
        anyM (\dir -> D.doesDirectoryExist dir &&^ fmap (FP.equalFilePath destDir) (D.canonicalizePath dir)) searchPath
    if destDirIsInPATH
        then forM_ installed $ \exe -> do
            mexePath <- (liftIO . D.findExecutable . T.unpack) exe
            case mexePath of
                Just exePath -> do
                    exeDir <- (liftIO . fmap FP.takeDirectory . D.canonicalizePath) exePath
                    unless (exeDir `FP.equalFilePath` destDir) $ do
                        logWarn ""
                        logWarn $ T.concat
                            [ "WARNING: The \""
                            , exe
                            , "\" executable found on the PATH environment variable is "
                            , T.pack exePath
                            , ", and not the version that was just installed."
                            ]
                        logWarn $ T.concat
                            [ "This means that \""
                            , exe
                            , "\" calls on the command line will not use this version."
                            ]
                Nothing -> do
                    logWarn ""
                    logWarn $ T.concat
                        [ "WARNING: Installation path "
                        , T.pack destDir
                        , " is on the PATH but the \""
                        , exe
                        , "\" executable that was just installed could not be found on the PATH."
                        ]
        else do
            logWarn ""
            logWarn $ T.concat
                [ "WARNING: Installation path "
                , T.pack destDir
                , " not found on the PATH environment variable"
                ]
