{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import System.Exit
import System.IO

-- | Fake server that always responds with HTTP OK
main =
    withSocketsDo $ do
        _ <- forkIO serve
        -- Exit after a delay to ensure the process doesn't linger around
        threadDelay 10000000
        exitSuccess

serve :: IO ()
serve = do
    let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
    (addr:_) <- getAddrInfo (Just hints) Nothing (Just "12415")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 10
    forever $ do
        (conn, _) <- accept sock
        _ <- recv conn 1024
        sendAll
            conn
            "HTTP/1.1 200 OK\r\nContent-Length: 1\r\nContent-Type: text/plain\r\n\r\na"
        shutdown conn ShutdownSend
