{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.FileEmbed
import Unlisted

main :: IO ()
main = do
    putStrLn ("main " ++ show foo ++ " " ++ show embedded)
    if embedded == "FAIL\n"
        then error "embedded contains FAIL"
        else return ()

embedded  = $(embedFile "embed.txt")
