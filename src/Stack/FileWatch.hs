{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.FileWatch
  ( WatchMode (WatchModePoll)
  , fileWatch
  , fileWatchPoll
  ) where

import           Control.Concurrent.STM ( check )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           GHC.IO.Exception
                   ( IOErrorType (InvalidArgument), IOException (..) )
import           Path ( parent )
import           Stack.Prelude
import           System.FSNotify
                   ( WatchConfig, WatchMode (..), confWatchMode, defaultConfig
                   , eventPath, watchDir, withManagerConf
                   )
import           System.IO ( getLine )

fileWatch ::
     HasTerm env
  => ((Set (Path Abs File) -> IO ()) -> RIO env ())
  -> RIO env ()
fileWatch = fileWatchConf defaultConfig

fileWatchPoll ::
     HasTerm env
  => ((Set (Path Abs File) -> IO ()) -> RIO env ())
  -> RIO env ()
fileWatchPoll =
  fileWatchConf $ defaultConfig { confWatchMode = WatchModePoll 1000000 }

-- | Run an action, watching for file changes
--
-- The action provided takes a callback that is used to set the files to be
-- watched. When any of those files are changed, we rerun the action again.
fileWatchConf ::
     HasTerm env
  => WatchConfig
  -> ((Set (Path Abs File) -> IO ()) -> RIO env ())
  -> RIO env ()
fileWatchConf cfg inner =
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
          let actions = Map.mergeWithKey
                keepListening
                stopListening
                startListening
                watch0
                newDirs
          watch1 <- forM (Map.toList actions) $ \(k, mmv) -> do
            mv <- mmv
            pure $
              case mv of
                Nothing -> Map.empty
                Just v -> Map.singleton k v
          atomically $ writeTVar watchVar $ Map.unions watch1
         where
          newDirs = Map.fromList $ map (, ())
                  $ Set.toList
                  $ Set.map parent files

          keepListening _dir listen () = Just $ pure $ Just listen
          stopListening = Map.map $ \f -> do
            () <- f `catch` \ioe ->
              -- Ignore invalid argument error - it can happen if
              -- the directory is removed.
              case ioe_type ioe of
                InvalidArgument -> pure ()
                _ -> throwIO ioe
            pure Nothing
          startListening = Map.mapWithKey $ \dir () -> do
            let dir' = fromString $ toFilePath dir
            listen <- watchDir manager dir' (const True) onChange
            pure $ Just listen

    let watchInput = do
          l <- getLine
          unless (l == "quit") $ do
            run $ case l of
              "help" -> do
                logInfo ""
                logInfo "help: display this help"
                logInfo "quit: exit"
                logInfo "build: force a rebuild"
                logInfo "watched: display watched files"
              "build" -> atomically $ writeTVar dirtyVar True
              "watched" -> do
                watch <- readTVarIO allFiles
                mapM_ (logInfo . fromString) (Set.toList watch)
              "" -> atomically $ writeTVar dirtyVar True
              _ -> logInfo $
                "Unknown command: " <>
                displayShow l <>
                ". Try 'help'"

            watchInput

    race_ watchInput $ run $ forever $ do
      atomically $ do
        dirty <- readTVar dirtyVar
        check dirty

      eres <- tryAny $ inner setWatched

      -- Clear dirtiness flag after the build to avoid an infinite loop caused
      -- by the build itself triggering dirtiness. This could be viewed as a
      -- bug, since files changed during the build will not trigger an extra
      -- rebuild, but overall seems like better behavior. See
      -- https://github.com/commercialhaskell/stack/issues/822
      atomically $ writeTVar dirtyVar False

      case eres of
        Left e ->
          case fromException e of
            Just ExitSuccess ->
              prettyInfo $ style Good $ fromString $ displayException e
            _ -> case fromException e :: Maybe PrettyException of
              Just pe -> prettyError $ pretty pe
              _ -> prettyInfo $ style Error $ fromString $ displayException e
        _ -> prettyInfo $ style Good "Success! Waiting for next file change."

      logInfo "Type help for available commands. Press enter to force a rebuild."
