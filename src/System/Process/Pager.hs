{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, DeriveDataTypeable #-}

-- | Run external pagers (@$PAGER@, @less@, @more@).
module System.Process.Pager
  ( pageWriter
  , pageText
  , PagerException (..)
  ) where

import Stack.Prelude
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Process ( createProcess, cmdspec, shell, proc, waitForProcess
                      , CmdSpec (ShellCommand, RawCommand)
                      , StdStream (CreatePipe)
                      , CreateProcess (std_in, close_fds, delegate_ctlc)
                      )
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT, MaybeT))
import qualified Data.Text.IO as T

-- | Run pager, providing a function that writes to the pager's input.
pageWriter :: (Handle -> IO ()) -> IO ()
pageWriter writer =
  do mpager <- runMaybeT $ cmdspecFromEnvVar
                       <|> cmdspecFromExeName "less"
                       <|> cmdspecFromExeName "more"
     case mpager of
       Just pager ->
         do (Just h,_,_,procHandle) <- createProcess pager
                                         { std_in = CreatePipe
                                         , close_fds = True
                                         , delegate_ctlc = True
                                         }
            (_ :: Either IOException ()) <- try (do writer h
                                                    hClose h)
            exit <- waitForProcess procHandle
            case exit of
              ExitSuccess -> return ()
              ExitFailure n -> throwIO (PagerExitFailure (cmdspec pager) n)
            return ()
       Nothing -> writer stdout
  where
    cmdspecFromEnvVar = shell <$> MaybeT (lookupEnv "PAGER")
    cmdspecFromExeName =
      fmap (\path -> proc path []) . MaybeT . findExecutable

-- | Run pager to display a 'Text'
pageText :: Text -> IO ()
pageText = pageWriter . flip T.hPutStr

-- | Exception running pager.
data PagerException = PagerExitFailure CmdSpec Int
  deriving Typeable
instance Show PagerException where
  show (PagerExitFailure cmd n) =
    let
      getStr (ShellCommand c) = c
      getStr (RawCommand exePath _) = exePath
    in
      "Pager (`" ++ getStr cmd ++ "') exited with non-zero status: " ++ show n

instance Exception PagerException
