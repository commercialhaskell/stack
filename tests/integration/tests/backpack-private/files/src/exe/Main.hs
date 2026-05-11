module Main where

import PrivateBackpack (greetString)

main :: IO ()
main = putStrLn (greetString "world")
