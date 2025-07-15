module Main (main) where

import qualified OsPathSpec
import Test.Tasty

main :: IO ()
main = defaultMain OsPathSpec.tests
