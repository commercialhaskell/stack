{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad ( when )
import qualified Data.ByteString.Char8 as C8
import           Data.FileEmbed ( embedFile )
import           Unlisted ( unlistedFunc )

main :: IO ()
main = do
  unlistedFunc
  when ("FAIL" `C8.isPrefixOf` embedded) $ error "embedded contains FAIL"

embedded = $(embedFile "embed.txt")
