module Main where

import TestGen (tests)
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testProperties "doctests" tests

