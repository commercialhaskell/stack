{-# LANGUAGE OverloadedStrings #-}

import Network.Simple.TCP

-- | Fake server that always responds with HTTP OK
main =
    serve (Host "127.0.0.1") "12415" $ \(socket, _) ->
        send socket "HTTP/1.1 200 OK\r\n\r\n"
