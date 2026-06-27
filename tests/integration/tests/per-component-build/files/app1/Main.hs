module Main where

import Lib (greeting)

main :: IO ()
main = putStrLn ("app1: " ++ greeting)
