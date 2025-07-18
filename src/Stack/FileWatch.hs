{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack.FileWatch
License     : BSD-3-Clause
-}

module Stack.FileWatch
  ( WatchMode (WatchModePoll)
  , fileWatch
  , fileWatchPoll
  ) where

import           Control.Concurrent.STM ( check )
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           GHC.IO.Exception
                   ( IOErrorType (InvalidArgument), IOException (..) )
import           Path ( fileExtension, parent )
import           Path.IO ( doesFileExist, executable, getPermissions )
import           RIO.Process
                   ( EnvVars, HasProcessContext (..), proc, runProcess
                   , withModifyEnvVars
                   )
import           System.Permissions ( osIsWindows )
import           Stack.Prelude
import           Stack.Types.Config ( Config (..), HasConfig (..) )
import           Stack.Types.Runner ( HasRunner (..), Runner (..) )
import           System.FSNotify
                   ( WatchConfig, WatchMode (..), confWatchMode, defaultConfig
                   , eventPath, watchDir, withManagerConf
                   )
import           System.IO ( getLine )

fileWatch ::
     (HasConfig env, HasTerm env)
  => ((Set (Path Abs File) -> IO ()) -> RIO Runner ())
  -> RIO env ()
fileWatch = fileWatchConf defaultConfig

fileWatchPoll ::
     (HasConfig env, HasTerm env)
  => ((Set (Path Abs File) -> IO ()) -> RIO Runner ())
  -> RIO env ()
fileWatchPoll =
  fileWatchConf $ defaultConfig { confWatchMode = WatchModePoll 1000000 }

-- | Run an action, watching for file changes
--
-- The action provided takes a callback that is used to set the files to be
-- watched. When any of those files are changed, we rerun the action again.
fileWatchConf ::
     (HasConfig env, HasTerm env)
  => WatchConfig
  -> ((Set (Path Abs File) -> IO ()) -> RIO Runner ())
  -> RIO env ()
fileWatchConf cfg inner = do
  runner <- view runnerL
  mHook <- view $ configL . to (.fileWatchHook)
  withRunInIO $ \run -> withManagerConf cfg $ \manager -> do
    allFiles <- newTVarIO Set.empty
    dirtyVar <- newTVarIO True
    watchVar <- newTVarIO Map.empty

    let onChange event = atomically $ do
          files <- readTVar allFiles
          when (eventPath event `Set.member` files) (writeTVar dirtyVar True)

        setWatched :: Set (Path Abs File) -> IO ()
        setWatched files = do
          atomically $ writeTVar allFiles $ Set.map toFilePath files
          watch0 <- readTVarIO watchVar
          let actions = Map.merge
                (Map.mapMissing stopListening)
                (Map.mapMissing startListening)
                (Map.zipWithMatched keepListening)
                watch0
                newDirs
          watch1 <- forM (Map.toList actions) $ \(k, mmv) ->
            mmv <&> \case
              Nothing -> Map.empty
              Just v -> Map.singleton k v
          atomically $ writeTVar watchVar $ Map.unions watch1
         where
          newDirs = Map.fromList $ map (, ())
                  $ Set.toList
                  $ Set.map parent files

          keepListening _dir listen () = pure $ Just listen
          stopListening _ f = do
            () <- f `catch` \ioe ->
              -- Ignore invalid argument error - it can happen if
              -- the directory is removed.
              case ioe_type ioe of
                InvalidArgument -> pure ()
                _ -> throwIO ioe
            pure Nothing
          startListening dir () = do
            let dir' = fromString $ toFilePath dir
            listen <- watchDir manager dir' (const True) onChange
            pure $ Just listen

    let watchInput = do
          l <- getLine
          unless (l == "quit") $ do
            run $ case l of
              "help" -> do
                prettyInfo $
                     line
                  <> fillSep
                       [ style Shell "help" <> ":"
                       , flow "display this help."
                       ]
                  <> line
                  <> fillSep
                       [ style Shell "quit" <> ":"
                       , "exit."
                       ]
                  <> line
                  <> fillSep
                       [ style Shell "build" <> ":"
                       , flow "force a rebuild."
                       ]
                  <> line
                  <> fillSep
                       [ style Shell "watched" <> ":"
                       , flow "display watched files."
                       ]
              "build" -> atomically $ writeTVar dirtyVar True
              "watched" -> do
                watch <- readTVarIO allFiles
                mapM_ (prettyInfo . style File . fromString) (Set.toList watch)
              "" -> atomically $ writeTVar dirtyVar True
              _ -> prettyInfoL
                     [ flow "Unknown command:"
                     , style Shell (fromString l) <> "."
                     , "Try"
                     , style Shell "help" <> "."
                     ]

            watchInput

    race_ watchInput $ run $ forever $ do
      atomically $ do
        dirty <- readTVar dirtyVar
        check dirty

      eres <- tryAny $ runRIO runner (inner setWatched)

      -- Clear dirtiness flag after the build to avoid an infinite loop caused
      -- by the build itself triggering dirtiness. This could be viewed as a
      -- bug, since files changed during the build will not trigger an extra
      -- rebuild, but overall seems like better behavior. See
      -- https://github.com/commercialhaskell/stack/issues/822
      atomically $ writeTVar dirtyVar False

      let defaultAction = case eres of
            Left e ->
              case fromException e of
                Just ExitSuccess ->
                  prettyInfo $ style Good $ fromString $ displayException e
                _ -> case fromException e :: Maybe PrettyException of
                  Just pe -> prettyError $ pretty pe
                  _ -> prettyInfo $ style Error $ fromString $ displayException e
            _ -> prettyInfo $
                   style Good (flow "Success! Waiting for next file change.")

      case mHook of
        Nothing -> defaultAction
        Just hook -> do
          hookIsExecutable <- handleIO (\_ -> pure False) $ if osIsWindows
            then
              -- can't really detect executable on windows, only file extension
              doesFileExist hook
            else executable <$> getPermissions hook
          if hookIsExecutable
            then runFileWatchHook eres hook
            else do
              prettyWarn $
                flow "File watch hook not executable. Falling back on default."
              defaultAction

      prettyInfoL
        [ "Type"
        , style Shell "help"
        , flow "for the available commands. Press enter to force a rebuild."
        ]

runFileWatchHook ::
     (HasProcessContext env, HasTerm env)
  => Either SomeException ()
  -> Path Abs File
  -> RIO env ()
runFileWatchHook buildResult hook =
  withModifyEnvVars insertBuildResultInEnv $ do
    let (cmd, args) = if osIsWindows && isShFile
          then ("sh", [toFilePath hook])
          else (toFilePath hook, [])
    menv <- view processContextL
    withProcessContext menv $ proc cmd args runProcess >>= \case
      ExitSuccess -> pure ()
      ExitFailure i -> do
        prettyWarnL
          [ flow "File watch hook exited with code:"
          , style Error (fromString $ show i) <> "."
          ]
        pure ()
 where
  insertBuildResultInEnv :: EnvVars -> EnvVars
  insertBuildResultInEnv = Map.insert "HOOK_FW_RESULT" $ case buildResult of
    Left e -> T.pack $ displayException e
    Right _ -> ""
  isShFile = case fileExtension hook of
    Just ".sh" -> True
    _ -> False
