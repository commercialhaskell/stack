{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Separate module because TH.

module System.Process.Log
    (logCreateProcess
    ,withProcessTimeLog
    ,showProcessArgDebug)
    where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.Haskell.TH
import qualified System.Clock as Clock
import           System.Process (CreateProcess(..), CmdSpec(..))

-- | Log running a process with its arguments, for debugging (-v).
logCreateProcess :: Q Exp
logCreateProcess =
    [|let f :: MonadLogger m => CreateProcess -> m ()
          f CreateProcess { cmdspec = ShellCommand shellCmd } =
              $logDebug ("Creating shell process: " <> T.pack shellCmd)
          f CreateProcess { cmdspec = RawCommand name args } =
              $logDebug
                  ("Creating process: " <> T.pack name <> " " <>
                   T.intercalate
                       " "
                       (map showProcessArgDebug args))
      in f|]

-- | Log running a process with its arguments, for debugging (-v).
--
-- This logs one message before running the process and one message after.
withProcessTimeLog :: Q Exp
withProcessTimeLog =
    [|let f :: (MonadIO m, MonadLogger m) => String -> [String] -> m a -> m a
          f name args proc = do
              let cmdText =
                      T.intercalate
                          " "
                          (T.pack name : map showProcessArgDebug args)
              $logDebug ("Run process: " <> cmdText)
              start <- liftIO $ Clock.getTime Clock.Monotonic
              x <- proc
              end <- liftIO $ Clock.getTime Clock.Monotonic
              let diff = Clock.diffTimeSpec start end
              -- useAnsi <- asks getAnsiTerminal
              let useAnsi = True
              $logDebug
                  ("Process finished in " <>
                  (if useAnsi then "\ESC[92m" else "") <> -- green
                  timeSpecMilliSecondText diff <>
                  (if useAnsi then "\ESC[0m" else "") <> -- reset
                   ": " <> cmdText)
              return x
      in f|]

timeSpecMilliSecondText :: Clock.TimeSpec -> Text
timeSpecMilliSecondText t =
    (T.pack . show . (`div` 10^(6 :: Int)) . Clock.toNanoSecs) t <> "ms"

-- | Show a process arg including speechmarks when necessary. Just for
-- debugging purposes, not functionally important.
showProcessArgDebug :: String -> Text
showProcessArgDebug x
    | any special x = T.pack (show x)
    | otherwise = T.pack x
  where special '"' = True
        special ' ' = True
        special _ = False
