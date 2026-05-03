{-# LANGUAGE CPP #-}

module Main
  ( main
  ) where

main :: IO ()
main =
#if TEST_FLAG
  putStrLn "TEST_FLAG was set"
#else
  putStrLn "TEST_FLAG was not set"
#endif
