{-# LANGUAGE TemplateHaskell #-}

-- | Separate module because TH.

module System.Process.Log
    (logCreateProcess
    ,logProcessRun
    ,showProcessArgDebug)
    where

import           Control.Monad.Logger
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.Haskell.TH
import           System.Process (CreateProcess(..), CmdSpec(..))

-- | Log running a process with its arguments, for debugging (-v).
logCreateProcess :: Q Exp
logCreateProcess =
    [|let f :: MonadLogger m => CreateProcess -> m ()
          f (CreateProcess { cmdspec = ShellCommand shellCmd }) =
              $logDebug ("Creating shell process: " <> T.pack shellCmd)
          f (CreateProcess { cmdspec = RawCommand name args }) =
              $logDebug
                  ("Creating process: " <> T.pack name <> " " <>
                   T.intercalate
                       " "
                       (map showProcessArgDebug args))
      in f|]

-- | Log running a process with its arguments, for debugging (-v).
logProcessRun :: Q Exp
logProcessRun =
    [|let f :: MonadLogger m => String -> [String] -> m ()
          f name args =
              $logDebug
                  ("Run process: " <> T.pack name <> " " <>
                   T.intercalate
                       " "
                       (map showProcessArgDebug args))
      in f|]

-- | Show a process arg including speechmarks when necessary. Just for
-- debugging purposes, not functionally important.
showProcessArgDebug :: String -> Text
showProcessArgDebug x
    | any special x = T.pack (show x)
    | otherwise = T.pack x
  where special '"' = True
        special ' ' = True
        special _ = False
