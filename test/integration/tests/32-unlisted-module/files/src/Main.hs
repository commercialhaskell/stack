{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as C8
import Data.FileEmbed
import Data.List
import Unlisted

main :: IO ()
main = do
    putStrLn ("main " ++ show foo ++ " " ++ show embedded)
    if "FAIL" `C8.isPrefixOf` embedded
        then error "embedded contains FAIL"
        else return ()

embedded = $(embedFile "embed.txt")
