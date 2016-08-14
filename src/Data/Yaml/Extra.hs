{-# LANGUAGE ScopedTypeVariables #-}
-- | Wrappers for Yaml functions to workaround
-- https://github.com/commercialhaskell/stack/issues/2491.
-- Import Data.Yaml.Extra in place of Data.Yaml to use this workaround.
-- Beware these functions construct/deconstruct the entire file at once!
module Data.Yaml.Extra (decodeFileEither, encodeFile, module Data.Yaml) where

import           Control.Exception
import           Data.Yaml hiding (decodeFileEither, encodeFile)
import qualified Data.ByteString as B
import           System.IO

-- Note: we refrain from using 'B.readFile' and 'B.writeFile', as they open
-- the file in binary mode rather than text mode.
decodeFileEither :: FromJSON a => FilePath -> IO (Either ParseException a)
decodeFileEither path =
  handle (\(e :: IOException) -> return . Left . OtherParseException . SomeException $ e) $
  withFile path ReadMode $
  \hnd -> do
    fileContent <- B.hGetContents hnd
    return $ decodeEither' fileContent

encodeFile :: ToJSON a => FilePath -> a -> IO ()
encodeFile path v = withFile path WriteMode $
  \hnd -> do
    let fileContent = encode v
    B.hPut hnd fileContent
