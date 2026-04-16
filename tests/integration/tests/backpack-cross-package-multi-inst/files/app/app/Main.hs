module Main where

import ConsumerA (helloA)
import ConsumerB (helloB)

main :: IO ()
main = do
  putStrLn helloA
  putStrLn helloB
