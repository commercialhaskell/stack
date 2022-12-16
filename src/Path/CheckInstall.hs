{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Path.CheckInstall
  ( warnInstallSearchPathIssues
  ) where

import           Control.Monad.Extra ( anyM, (&&^) )
import qualified Data.Text as T
import           Stack.Prelude
import           Stack.Types.Config
import qualified System.Directory as D
import qualified System.FilePath as FP

-- | Checks if the installed executable will be available on the user's
-- PATH. This doesn't use @envSearchPath menv@ because it includes paths
-- only visible when running in the Stack environment.
warnInstallSearchPathIssues :: HasConfig env => FilePath -> [Text] -> RIO env ()
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
            prettyWarnL
              [ flow "The"
              , style File . fromString . T.unpack $ exe
              , flow "executable found on the PATH environment variable is"
              , style File . fromString $ exePath
              , flow "and not the version that was just installed."
              , flow "This means that"
              , style File . fromString . T.unpack $ exe
              , "calls on the command line will not use this version."
              ]
        Nothing -> do
          prettyWarnL
            [ flow "Installation path"
            , style Dir . fromString $ destDir
            , flow "is on the PATH but the"
            , style File . fromString . T.unpack $ exe
            , flow "executable that was just installed could not be found on the PATH."
            ]
    else do
      prettyWarnL
        [ flow "Installation path "
        , style Dir . fromString $ destDir
        , "not found on the PATH environment variable."
        ]
