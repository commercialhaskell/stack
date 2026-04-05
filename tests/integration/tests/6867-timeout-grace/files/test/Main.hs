{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Concurrent (threadDelay)

#ifndef mingw32_HOST_OS
import System.Posix.Signals (Handler (Ignore), installHandler, sigTERM)
#endif

main :: IO ()
main = do
#ifndef mingw32_HOST_OS
  _ <- installHandler sigTERM Ignore Nothing
#endif
  threadDelay 6000000
