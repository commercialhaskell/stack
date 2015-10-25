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
import Control.Exception (Exception, fromException)
import Control.Exception.Enclosed (tryAny)
import Control.Monad (forever, unless, when)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Traversable (forM)
import GHC.IO.Handle (hIsTerminalDevice)
import Path
import System.Console.ANSI
import System.Exit
import System.FSNotify
import System.IO (stdout, stderr)

-- | Print an exception to stderr
printExceptionStderr :: Exception e => e -> IO ()
printExceptionStderr e =
    L.hPut stderr $ toLazyByteString $ fromShow e <> copyByteString "\n"

fileWatch :: ((Set (Path Abs File) -> IO ()) -> IO ())
          -> IO ()
fileWatch = fileWatchConf defaultConfig

fileWatchPoll :: ((Set (Path Abs File) -> IO ()) -> IO ())
               -> IO ()
fileWatchPoll = fileWatchConf $ defaultConfig { confUsePolling = True }

-- | Run an action, watching for file changes
--
-- The action provided takes a callback that is used to set the files to be
-- watched. When any of those files are changed, we rerun the action again.
fileWatchConf :: WatchConfig
              -> ((Set (Path Abs File) -> IO ()) -> IO ())
              -> IO ()
fileWatchConf cfg inner = withManagerConf cfg $ \manager -> do
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
                listen <- watchDir manager dir' (const True) onChange
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
                        putStrLn "watched: display watched files"
                    "build" -> atomically $ writeTVar dirtyVar True
                    "watched" -> do
                        watch <- readTVarIO allFiles
                        mapM_ putStrLn (Set.toList watch)
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

        let withColor color action = do
                outputIsTerminal <- hIsTerminalDevice stdout
                if outputIsTerminal
                then do
                    setSGR [SetColor Foreground Dull color]
                    action
                    setSGR [Reset]
                else action

        case eres of
            Left e -> do
                let color = case fromException e of
                        Just ExitSuccess -> Green
                        _ -> Red
                withColor color $ printExceptionStderr e
            _ -> withColor Green $
                putStrLn "Success! Waiting for next file change."

        putStrLn "Type help for available commands. Press enter to force a rebuild."
