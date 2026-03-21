module Main where

import SublibDeps (greetLoud)

main :: IO ()
main = putStrLn (greetLoud "world")
