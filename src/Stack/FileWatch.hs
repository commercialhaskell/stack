{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Stack.FileWatch
    ( fileWatch
    , displayException
    ) where

import Blaze.ByteString.Builder (toLazyByteString, copyByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception (Exception)
import Control.Exception.Enclosed (tryAny)
import Control.Monad (forever, unless)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Traversable (forM)
import Path
import System.FSNotify
import System.IO (stderr)

-- | Display an exception to stderr
displayException :: Exception e => e -> IO ()
displayException e =
    L.hPut stderr $ toLazyByteString $ fromShow e <> copyByteString "\n"

-- | Run an action, watching for file changes
--
-- The action provided takes a callback that is used to set the files to be
-- watched. When any of those files are changed, we rerun the action again.
fileWatch :: ((Set (Path Abs File) -> IO ()) -> IO ())
          -> IO ()
fileWatch inner = withManager $ \manager -> do
    dirtyVar <- newTVarIO True
    watchVar <- newTVarIO Map.empty

    let onChange = atomically $ writeTVar dirtyVar True

        setWatched :: Set (Path Abs File) -> IO ()
        setWatched files = do
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
                listen <- watchDir manager dir' (const True) (const onChange)
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
                    "build" -> onChange
                    "watched" -> do
                        watch <- readTVarIO watchVar
                        mapM_ (putStrLn . toFilePath) (Map.keys watch)
                    _ -> putStrLn $ "Unknown command: " ++ show line

                watchInput

    race_ watchInput $ forever $ do
        atomically $ do
            dirty <- readTVar dirtyVar
            check dirty
            writeTVar dirtyVar False

        eres <- tryAny $ inner setWatched
        case eres of
            Left e -> displayException e
            Right () -> putStrLn "Success! Waiting for next file change."

        putStrLn "Type help for available commands"
