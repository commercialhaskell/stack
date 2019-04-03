{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, DeriveDataTypeable #-}

-- | Run external pagers (@$PAGER@, @less@, @more@).
module System.Process.Pager
  (pageWriter
  ,pageText
  ,PagerException(..))
  where

import Stack.Prelude
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.Process (createProcess,shell,waitForProcess,StdStream (CreatePipe)
                      ,CreateProcess(std_in, close_fds, delegate_ctlc))
import System.IO (stdout)
import qualified Data.Text.IO as T

-- | Run pager, providing a function that writes to the pager's input.
pageWriter :: (Handle -> IO ()) -> IO ()
pageWriter writer =
  do mpager <- lookupEnv "PAGER" `orElse`
               findExecutable "less" `orElse`
               findExecutable "more"
     case mpager of
       Just pager ->
         do (Just h,_,_,procHandle) <- createProcess (shell pager)
                                                       {std_in = CreatePipe
                                                       ,close_fds = True
                                                       ,delegate_ctlc = True}
            (_::Either IOException ()) <- try (do writer h
                                                  hClose h)
            exit <- waitForProcess procHandle
            case exit of
              ExitSuccess -> return ()
              ExitFailure n -> throwIO (PagerExitFailure pager n)
            return ()
       Nothing -> writer stdout
  where
    orElse a b = maybe b (return . Just) =<< a

-- | Run pager to display a 'Text'
pageText :: Text -> IO ()
pageText = pageWriter . flip T.hPutStr

-- | Exception running pager.
data PagerException = PagerExitFailure FilePath Int
  deriving Typeable
instance Show PagerException where
  show (PagerExitFailure p n) = "Pager (`" ++ p ++ "') exited with non-zero status: " ++ show n
instance Exception PagerException
