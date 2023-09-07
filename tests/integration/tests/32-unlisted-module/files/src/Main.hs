{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import qualified Data.ByteString.Char8 as C8
import           Data.FileEmbed
import           Data.List
import           Unlisted

main :: IO ()
main = do
    putStrLn ("main " ++ show foo ++ " " ++ show embedded)
    when ("FAIL" `C8.isPrefixOf` embedded) $ error "embedded contains FAIL"

embedded = $(embedFile "embed.txt")
