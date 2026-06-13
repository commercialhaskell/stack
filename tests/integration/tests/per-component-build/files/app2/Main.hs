module Main where

import Lib (greeting)

main :: IO ()
main = putStrLn ("app2: " ++ greeting)
