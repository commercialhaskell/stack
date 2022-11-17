{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Stack.Prelude
  ( PrettyException (..)
  , Pretty (..)
  , withSystemTempDir
  , withKeepSystemTempDir
  , sinkProcessStderrStdout
  , sinkProcessStdout
  , logProcessStderrStdout
  , readProcessNull
  , withProcessContext
  , stripCR
  , prompt
  , promptPassword
  , promptBool
  , stackProgName
  , FirstTrue (..)
  , fromFirstTrue
  , defaultFirstTrue
  , FirstFalse (..)
  , fromFirstFalse
  , defaultFirstFalse
  , writeBinaryFileAtomic
  , bugReport
  , bugPrettyReport
  , blankLine
  , module X
  ) where

import           Data.Monoid as X
                   ( First (..), Any (..), Sum (..), Endo (..) )

import           Data.Conduit as X ( ConduitM, runConduit, (.|) )
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process.Typed
                   ( withLoggedProcess_, createSource, byteStringInput)
import qualified Data.Text.IO as T
import           Pantry as X hiding ( Package (..), loadSnapshot )
import           Path as X
                   ( Abs, Dir, File, Path, Rel, toFilePath )
import qualified Path.IO
import           RIO as X
import           RIO.File as X hiding ( writeBinaryFileAtomic )
import           RIO.PrettyPrint ( Pretty (..), StyleDoc, (<+>), flow, line )
import           RIO.PrettyPrint.PrettyException ( PrettyException (..) )
import           RIO.Process
                   ( HasProcessContext (..), ProcessContext, setStdin, closed
                   , getStderr, getStdout, proc, withProcessWait_, setStdout
                   , setStderr, ProcessConfig, readProcess_, workingDirL
                   , waitExitCode
                   )
import qualified RIO.Text as T
import           System.IO.Echo ( withoutInputEcho )

-- | Path version
withSystemTempDir :: MonadUnliftIO m => String -> (Path Abs Dir -> m a) -> m a
withSystemTempDir str inner = withRunInIO $ \run ->
  Path.IO.withSystemTempDir str $ run . inner

-- | Like `withSystemTempDir`, but the temporary directory is not deleted.
withKeepSystemTempDir :: MonadUnliftIO m
                      => String
                      -> (Path Abs Dir -> m a)
                      -> m a
withKeepSystemTempDir str inner = withRunInIO $ \run -> do
  path <- Path.IO.getTempDir
  dir <- Path.IO.createTempDir path str
  run $ inner dir

-- | Consume the stdout and stderr of a process feeding strict 'ByteString's to
-- the consumers.
--
-- Throws a 'ReadProcessException' if unsuccessful in launching, or
-- 'ExitCodeException' if the process itself fails.
sinkProcessStderrStdout
  :: forall e o env. (HasProcessContext env, HasLogFunc env, HasCallStack)
  => String -- ^ Command
  -> [String] -- ^ Command line arguments
  -> ConduitM ByteString Void (RIO env) e -- ^ Sink for stderr
  -> ConduitM ByteString Void (RIO env) o -- ^ Sink for stdout
  -> RIO env (e,o)
sinkProcessStderrStdout name args sinkStderr sinkStdout =
  proc name args $ \pc0 -> do
    let pc = setStdout createSource
           $ setStderr createSource
           -- Don't use closed, since that can break ./configure scripts
           -- See https://github.com/commercialhaskell/stack/pull/4722
           $ setStdin (byteStringInput "")
             pc0
    withProcessWait_ pc $ \p ->
      (runConduit (getStderr p .| sinkStderr) `concurrently`
      runConduit (getStdout p .| sinkStdout)) <* waitExitCode p

-- | Consume the stdout of a process feeding strict 'ByteString's to a consumer.
-- If the process fails, spits out stdout and stderr as error log
-- level. Should not be used for long-running processes or ones with
-- lots of output; for that use 'sinkProcessStderrStdout'.
--
-- Throws a 'ReadProcessException' if unsuccessful.
sinkProcessStdout
    :: (HasProcessContext env, HasLogFunc env, HasCallStack)
    => String -- ^ Command
    -> [String] -- ^ Command line arguments
    -> ConduitM ByteString Void (RIO env) a -- ^ Sink for stdout
    -> RIO env a
sinkProcessStdout name args sinkStdout =
  proc name args $ \pc ->
  withLoggedProcess_ (setStdin closed pc) $ \p -> runConcurrently
    $ Concurrently (runConduit $ getStderr p .| CL.sinkNull)
   *> Concurrently (runConduit $ getStdout p .| sinkStdout)

logProcessStderrStdout
    :: (HasCallStack, HasProcessContext env, HasLogFunc env)
    => ProcessConfig stdin stdoutIgnored stderrIgnored
    -> RIO env ()
logProcessStderrStdout pc = withLoggedProcess_ pc $ \p ->
    let logLines = CB.lines .| CL.mapM_ (logInfo . displayBytesUtf8)
     in runConcurrently
            $ Concurrently (runConduit $ getStdout p .| logLines)
           *> Concurrently (runConduit $ getStderr p .| logLines)

-- | Read from the process, ignoring any output.
--
-- Throws a 'ReadProcessException' exception if the process fails.
readProcessNull :: (HasProcessContext env, HasLogFunc env, HasCallStack)
                => String -- ^ Command
                -> [String] -- ^ Command line arguments
                -> RIO env ()
readProcessNull name args =
  -- We want the output to appear in any exceptions, so we capture and drop it
  void $ proc name args readProcess_

-- | Use the new 'ProcessContext', but retain the working directory
-- from the parent environment.
withProcessContext :: HasProcessContext env
                   => ProcessContext
                   -> RIO env a
                   -> RIO env a
withProcessContext pcNew inner = do
  pcOld <- view processContextL
  let pcNew' = set workingDirL (view workingDirL pcOld) pcNew
  local (set processContextL pcNew') inner

-- | Remove a trailing carriage pure if present
stripCR :: Text -> Text
stripCR = T.dropSuffix "\r"

-- | Prompt the user by sending text to stdout, and taking a line of
-- input from stdin.
prompt :: MonadIO m => Text -> m Text
prompt txt = liftIO $ do
  T.putStr txt
  hFlush stdout
  T.getLine

-- | Prompt the user by sending text to stdout, and collecting a line
-- of input from stdin. While taking input from stdin, input echoing is
-- disabled, to hide passwords.
--
-- Based on code from cabal-install, Distribution.Client.Upload
promptPassword :: MonadIO m => Text -> m Text
promptPassword txt = liftIO $ do
  T.putStr txt
  hFlush stdout
  -- Save/restore the terminal echoing status (no echoing for entering
  -- the password).
  password <- withoutInputEcho T.getLine
  -- Since the user's newline is not echoed, one needs to be inserted.
  T.putStrLn ""
  pure password

-- | Prompt the user by sending text to stdout, and collecting a line of
-- input from stdin. If something other than "y" or "n" is entered, then
-- print a message indicating that "y" or "n" is expected, and ask
-- again.
promptBool :: MonadIO m => Text -> m Bool
promptBool txt = liftIO $ do
  input <- prompt txt
  case input of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      T.putStrLn "Please press either 'y' or 'n', and then enter."
      promptBool txt

-- | Name of the 'stack' program.
--
-- NOTE: Should be defined in "Stack.Constants", but not doing so due to the
-- GHC stage restrictions.
stackProgName :: String
stackProgName = "stack"

-- | Like @First Bool@, but the default is @True@.
newtype FirstTrue = FirstTrue { getFirstTrue :: Maybe Bool }
  deriving (Show, Eq, Ord)
instance Semigroup FirstTrue where
  FirstTrue (Just x) <> _ = FirstTrue (Just x)
  FirstTrue Nothing <> x = x
instance Monoid FirstTrue where
  mempty = FirstTrue Nothing
  mappend = (<>)

-- | Get the 'Bool', defaulting to 'True'
fromFirstTrue :: FirstTrue -> Bool
fromFirstTrue = fromMaybe True . getFirstTrue

-- | Helper for filling in default values
defaultFirstTrue :: (a -> FirstTrue) -> Bool
defaultFirstTrue _ = True

-- | Like @First Bool@, but the default is @False@.
newtype FirstFalse = FirstFalse { getFirstFalse :: Maybe Bool }
  deriving (Show, Eq, Ord)
instance Semigroup FirstFalse where
  FirstFalse (Just x) <> _ = FirstFalse (Just x)
  FirstFalse Nothing <> x = x
instance Monoid FirstFalse where
  mempty = FirstFalse Nothing
  mappend = (<>)

-- | Get the 'Bool', defaulting to 'False'
fromFirstFalse :: FirstFalse -> Bool
fromFirstFalse = fromMaybe False . getFirstFalse

-- | Helper for filling in default values
defaultFirstFalse :: (a -> FirstFalse) -> Bool
defaultFirstFalse _ = False

-- | Write a @Builder@ to a file and atomically rename.
writeBinaryFileAtomic :: MonadIO m => Path absrel File -> Builder -> m ()
writeBinaryFileAtomic fp builder =
    liftIO $
    withBinaryFileAtomic (toFilePath fp) WriteMode (`hPutBuilder` builder)

-- | Report a bug in Stack.
bugReport :: String -> String -> String
bugReport code msg =
    "Error: " ++ code ++ "\n" ++
    bugDeclaration ++ " " ++ msg ++ " " ++ bugRequest

-- | Report a pretty bug in Stack.
bugPrettyReport :: String -> StyleDoc -> StyleDoc
bugPrettyReport code msg =
       "Error:" <+> fromString code
    <> line
    <> flow bugDeclaration <+> msg <+> flow bugRequest

-- | Bug declaration message.
bugDeclaration :: String
bugDeclaration = "The impossible happened!"

-- | Bug report message.
bugRequest :: String
bugRequest =  "Please report this bug at Stack's repository."

-- | A 'pretty' blank line.
blankLine :: StyleDoc
blankLine = line <> line
