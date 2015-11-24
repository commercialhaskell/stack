{-# LANGUAGE ScopedTypeVariables, RankNTypes, DeriveDataTypeable #-}

-- | Run external pagers (@$PAGER@, @less@, @more@) and editors (@$VISUAL@,
-- @$EDITOR@, @nano@, @pico@, @vi@).
module System.Process.PagerEditor
  (-- * Pager
   pageWriter
  ,pageByteString
  ,pageBuilder
  ,pageFile
  ,pageString
  ,PagerException(..)
   -- * Editor
  ,editFile
  ,editReaderWriter
  ,editByteString
  ,editString
  ,EditorException(..))
  where

import Control.Exception (try,IOException,throwIO,Exception)
import Data.ByteString.Lazy (ByteString,hPut,readFile)
import Data.ByteString.Builder (Builder,stringUtf8,hPutBuilder)
import Data.Typeable (Typeable)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (createProcess,shell,proc,waitForProcess,StdStream (CreatePipe)
                      ,CreateProcess(std_in, close_fds, delegate_ctlc))
import System.IO (hClose,Handle,hPutStr,readFile,withFile,IOMode(WriteMode),stdout)
import System.IO.Temp (withSystemTempDirectory)

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

-- | Run pager to display a lazy ByteString.
pageByteString :: ByteString -> IO ()
pageByteString = pageWriter . flip hPut

-- | Run pager to display a ByteString-Builder.
pageBuilder :: Builder -> IO ()
pageBuilder = pageWriter . flip hPutBuilder

-- | Run pager to display contents of a file.
pageFile :: FilePath -> IO ()
pageFile p = pageByteString =<< Data.ByteString.Lazy.readFile p

-- | Run pager to display a string.
pageString :: String -> IO ()
pageString = pageBuilder . stringUtf8

-- | Run editor to edit a file.
editFile :: FilePath -> IO ()
editFile path =
  do meditor <- lookupEnv "VISUAL" `orElse`
                lookupEnv "EDITOR" `orElse`
                findExecutable "nano" `orElse`
                findExecutable "pico" `orElse`
                findExecutable "vi"
     case meditor of
       Just editor ->
         do (_,_,_,procHandle) <- createProcess (proc "sh" ["-c", editor ++ " \"$1\"", "sh", path])
                                                  {close_fds = True,delegate_ctlc = True}
            exitCode <- waitForProcess procHandle
            case exitCode of
               ExitSuccess -> return ()
               ExitFailure n -> throwIO (EditorExitFailure editor n)
       Nothing -> throwIO EditorNotFound

-- | Run editor, providing functions to write and read the file contents.
editReaderWriter :: forall a. String -> (Handle -> IO ()) -> (FilePath -> IO a) -> IO a
editReaderWriter filename writer reader =
  withSystemTempDirectory ""
                          (\p -> do let p' = p </> filename
                                    withFile p' WriteMode writer
                                    editFile p'
                                    reader p')

-- | Run editor on a ByteString.
editByteString :: String -> ByteString -> IO ByteString
editByteString f s = editReaderWriter f (`hPut` s) Data.ByteString.Lazy.readFile

-- | Run editor on a String.
editString :: String -> String -> IO String
editString f s = editReaderWriter f (`hPutStr` s) System.IO.readFile

-- | Short-circuit first Just.
orElse :: (Monad m) => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
orElse a b = do m <- a
                case m of
                  Just _ -> return m
                  Nothing -> b

-- | Exception running pager.
data PagerException = PagerNotFound
                    | PagerExitFailure FilePath Int
  deriving Typeable
instance Show PagerException where
  show PagerNotFound = "No pager found (tried $PAGER, `less`, and `more`.)"
  show (PagerExitFailure p n) = "Pager (`" ++ p ++ "') exited with non-zero status: " ++ show n
instance Exception PagerException

-- | Exception running editor.
data EditorException = EditorNotFound
                     | EditorExitFailure FilePath Int
  deriving Typeable
instance Show EditorException where
  show EditorNotFound = "No editor found (tried $VISUAL, $PAGER, `nano`, `pico`, and `vi`.)"
  show (EditorExitFailure p n) = "Editor (`" ++ p ++ "') exited with non-zero status: " ++ show n
instance Exception EditorException
