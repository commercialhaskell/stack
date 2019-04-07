{-# LANGUAGE OverloadedStrings #-}
module Curator.HackageDistro
    ( uploadHackageDistro
    ) where

import Curator.Types
import Data.ByteString.Builder (toLazyByteString)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Pantry
import RIO
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.Text as T

uploadHackageDistro ::
       (HasLogFunc env) => Target -> Map PackageName Version -> RIO env ()
uploadHackageDistro target packages = do
    man <- liftIO $ newManager tlsManagerSettings
    ecreds <- tryIO $ readFileBinary "/hackage-creds"
    case T.words $ decodeUtf8Lenient $ either (const mempty) id ecreds of
        [username, password] -> do
            logInfo $ "Uploading as Hackage distro: " <> display distroName
            res2 <- liftIO $
              uploadDistro distroName packages (encodeUtf8 username) (encodeUtf8 password) man
            logInfo $ "Distro upload response: " <> displayShow res2
        _ -> error "No Hackage creds found at /hackage-creds"
  where
    distroName :: Text
    distroName =
        case target of
            TargetNightly _ -> "Stackage"
            TargetLts _ _ -> "LTSHaskell"

uploadDistro
    :: Text -- ^ distro name
    -> Map PackageName Version
    -> ByteString -- ^ Hackage username
    -> ByteString -- ^ Hackage password
    -> Manager
    -> IO (Response LByteString)
uploadDistro name packages username password manager = do
    req1 <- parseRequest $ concat
        [ "https://hackage.haskell.org/distro/"
        , T.unpack name
        , "/packages.csv"
        ]
    let req2 = req1
                { requestHeaders = [("Content-Type", "text/csv")]
                , requestBody = RequestBodyLBS csv
                , method = "PUT"
                }
    httpLbs (applyBasicAuth username password req2) manager
  where
    csv = toLazyByteString . getUtf8Builder
        $ mconcat
        $ L.intersperse "\n"
        $ map go
        $ Map.toList packages
    go (name', version) =
        "\"" <>
        displayShow name' <>
        "\",\"" <>
        displayShow version <>
        "\",\"https://www.stackage.org/package/" <>
        displayShow name' <>
        "\""
