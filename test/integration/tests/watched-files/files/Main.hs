{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.FileEmbed
import qualified Data.ByteString as B
import System.IO (stdout)

main :: IO ()
main = B.hPut stdout $(embedFile "some-text-file.txt")
