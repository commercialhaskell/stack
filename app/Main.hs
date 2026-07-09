{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main
  ) where

import           GHC.IO.Encoding ( getLocaleEncoding )
import           RIO
                   ( IO, String, Show (..), (.), (<>), (>>=), ($), elem, filter
                   , fst, stderr, stdout
                   )
import qualified Stack
import           System.Environment ( getEnvironment )
import           System.IO ( hGetEncoding, print, putStrLn )

-- | The entry point for the Stack executable.
main :: IO ()
main = do
  -- Add temporary diagnostics:
  getLocaleEncoding >>= report "locale"
  hGetEncoding stdout >>= report "stdout"
  hGetEncoding stderr >>= report "stderr"
  env <- getEnvironment
  print $ filter ((`elem` ["LANG", "LC_ALL", "LC_CTYPE", "LOCALE_ARCHIVE"]) . fst) env
  Stack.main
 where
  report :: Show a => String -> (a -> IO ())
  report msg = putStrLn . ((msg <> ": ") <>) . show
