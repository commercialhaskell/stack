module Main where

import Consumer (shout)

main :: IO ()
main = putStrLn (shout "hello world")
