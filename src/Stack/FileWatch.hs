{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Stack.FileWatch
    ( fileWatch
    , fileWatchPoll
    , printExceptionStderr
    ) where

import Blaze.ByteString.Builder (toLazyByteString, copyByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception (Exception)
import Control.Exception.Enclosed (tryAny)
import Control.Monad (forever, unless, when)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Traversable (forM)
import Ignore
import Path
import System.FSNotify
import System.IO (stderr)

-- | Print an exception to stderr
printExceptionStderr :: Exception e => e -> IO ()
printExceptionStderr e =
    L.hPut stderr $ toLazyByteString $ fromShow e <> copyByteString "\n"

fileWatch :: IO (Path Abs Dir)
          -> ((Set (Path Abs File) -> IO ()) -> IO ())
          -> IO ()
fileWatch = fileWatchConf defaultConfig

fileWatchPoll :: IO (Path Abs Dir)
               -> ((Set (Path Abs File) -> IO ()) -> IO ())
               -> IO ()
fileWatchPoll = fileWatchConf $ defaultConfig { confUsePolling = True }

-- | Run an action, watching for file changes
--
-- The action provided takes a callback that is used to set the files to be
-- watched. When any of those files are changed, we rerun the action again.
fileWatchConf :: WatchConfig
              -> IO (Path Abs Dir)
              -> ((Set (Path Abs File) -> IO ()) -> IO ())
              -> IO ()
fileWatchConf cfg getProjectRoot inner = withManagerConf cfg $ \manager -> do
    allFiles <- newTVarIO Set.empty
    dirtyVar <- newTVarIO True
    watchVar <- newTVarIO Map.empty
    projRoot <- getProjectRoot
    mChecker <- findIgnoreFiles [VCSGit, VCSMercurial, VCSDarcs] projRoot >>= buildChecker
    (FileIgnoredChecker isFileIgnored) <-
        case mChecker of
          Left err ->
              do putStrLn $ "Failed to parse VCS's ignore file: " ++ err
                 return $ FileIgnoredChecker (const False)
          Right chk -> return chk

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
                return $
                    case mv of
                        Nothing -> Map.empty
                        Just v -> Map.singleton k v
            atomically $ writeTVar watchVar $ Map.unions watch1
          where
            newDirs = Map.fromList $ map (, ())
                    $ Set.toList
                    $ Set.map parent files

            keepListening _dir listen () = Just $ return $ Just listen
            stopListening = Map.map $ \f -> do
                () <- f
                return Nothing
            startListening = Map.mapWithKey $ \dir () -> do
                let dir' = fromString $ toFilePath dir
                listen <- watchDir manager dir' (not . isFileIgnored . eventPath) onChange
                return $ Just listen

    let watchInput = do
            line <- getLine
            unless (line == "quit") $ do
                case line of
                    "help" -> do
                        putStrLn ""
                        putStrLn "help: display this help"
                        putStrLn "quit: exit"
                        putStrLn "build: force a rebuild"
                        putStrLn "watched: display watched directories"
                    "build" -> atomically $ writeTVar dirtyVar True
                    "watched" -> do
                        watch <- readTVarIO watchVar
                        mapM_ (putStrLn . toFilePath) (Map.keys watch)
                    "" -> atomically $ writeTVar dirtyVar True
                    _ -> putStrLn $ concat
                        [ "Unknown command: "
                        , show line
                        , ". Try 'help'"
                        ]

                watchInput

    race_ watchInput $ forever $ do
        atomically $ do
            dirty <- readTVar dirtyVar
            check dirty

        eres <- tryAny $ inner setWatched

        -- Clear dirtiness flag after the build to avoid an infinite
        -- loop caused by the build itself triggering dirtiness. This
        -- could be viewed as a bug, since files changed during the
        -- build will not trigger an extra rebuild, but overall seems
        -- like better behavior. See
        -- https://github.com/commercialhaskell/stack/issues/822
        atomically $ writeTVar dirtyVar False

        case eres of
            Left e -> printExceptionStderr e
            Right () -> putStrLn "Success! Waiting for next file change. " ++
              "Press enter to force a rebuild."

        putStrLn "Type help for available commands. Press enter to force a rebuild."
