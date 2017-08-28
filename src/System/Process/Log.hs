{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Separate module because TH.

module System.Process.Log
    (logCreateProcess
    ,withProcessTimeLog
    ,showProcessArgDebug)
    where

import qualified Data.Text as T
import           Stack.Prelude
import qualified System.Clock as Clock
import           System.Process (CreateProcess(..), CmdSpec(..))

-- | Log running a process with its arguments, for debugging (-v).
logCreateProcess :: MonadLogger m => CreateProcess -> m ()
logCreateProcess CreateProcess { cmdspec = ShellCommand shellCmd } =
  logDebug ("Creating shell process: " <> T.pack shellCmd)
logCreateProcess CreateProcess { cmdspec = RawCommand name args } =
  logDebug
      ("Creating process: " <> T.pack name <> " " <>
       T.intercalate
           " "
           (map showProcessArgDebug args))

-- | Log running a process with its arguments, for debugging (-v).
--
-- This logs one message before running the process and one message after.
withProcessTimeLog :: (MonadIO m, MonadLogger m) => String -> [String] -> m a -> m a
withProcessTimeLog name args proc = do
  let cmdText =
          T.intercalate
              " "
              (T.pack name : map showProcessArgDebug args)
  logDebug ("Run process: " <> cmdText)
  start <- liftIO $ Clock.getTime Clock.Monotonic
  x <- proc
  end <- liftIO $ Clock.getTime Clock.Monotonic
  let diff = Clock.diffTimeSpec start end
  -- useAnsi <- asks getAnsiTerminal
  let useAnsi = True
  logDebug
      ("Process finished in " <>
      (if useAnsi then "\ESC[92m" else "") <> -- green
      timeSpecMilliSecondText diff <>
      (if useAnsi then "\ESC[0m" else "") <> -- reset
       ": " <> cmdText)
  return x

timeSpecMilliSecondText :: Clock.TimeSpec -> Text
timeSpecMilliSecondText t =
    (T.pack . show . (`div` 10^(6 :: Int)) . Clock.toNanoSecs) t <> "ms"

-- | Show a process arg including speechmarks when necessary. Just for
-- debugging purposes, not functionally important.
showProcessArgDebug :: String -> Text
showProcessArgDebug x
    | any special x || null x = T.pack (show x)
    | otherwise = T.pack x
  where special '"' = True
        special ' ' = True
        special _ = False
