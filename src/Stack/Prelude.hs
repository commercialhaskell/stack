{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}

module Stack.Prelude
  ( withSystemTempDir
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
  , ppException
  , prettyThrowIO
  , prettyThrowM
  , mcons
  , MungedPackageId (..)
  , MungedPackageName (..)
  , LibraryName (..)
  , module X
  -- * Re-exports from the rio-pretty print package
  , HasStylesUpdate (..)
  , HasTerm (..)
  , Pretty (..)
  , PrettyException (..)
  , PrettyRawSnapshotLocation (..)
  , StyleDoc
  , Style (..)
  , StyleSpec
  , StylesUpdate (..)
  , (<+>)
  , align
  , bulletedList
  , debugBracket
  , defaultStyles
  , encloseSep
  , fill
  , fillSep
  , flow
  , hang
  , hcat
  , hsep
  , indent
  , line
  , logLevelToStyle
  , mkNarrativeList
  , parens
  , parseStylesUpdateFromString
  , prettyDebug
  , prettyDebugL
  , prettyError
  , prettyErrorL
  , prettyGeneric
  , prettyInfo
  , prettyInfoL
  , prettyInfoS
  , prettyNote
  , prettyNoteL
  , prettyNoteS
  , prettyWarn
  , prettyWarnL
  , prettyWarnNoIndent
  , prettyWarnS
  , punctuate
  , sep
  , softbreak
  , softline
  , spacedBulletedList
  , string
  , style
  , vsep
  ) where

import           Data.Monoid as X
                   ( Any (..), Endo (..), First (..), Sum (..) )
import           Data.Conduit as X ( ConduitM, runConduit, (.|) )
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process.Typed
                   ( byteStringInput, createSource, withLoggedProcess_ )
import qualified Data.Text.IO as T
import           Distribution.Types.LibraryName ( LibraryName (..) )
import           Distribution.Types.MungedPackageId ( MungedPackageId (..) )
import           Distribution.Types.MungedPackageName ( MungedPackageName (..) )
import           Pantry as X hiding ( Package (..), loadSnapshot )
import           Path as X
                   ( Abs, Dir, File, Path, Rel, toFilePath )
import qualified Path.IO
import           RIO as X
import           RIO.File as X hiding ( writeBinaryFileAtomic )
import           RIO.PrettyPrint
                   ( HasStylesUpdate (..), HasTerm (..), Pretty (..), Style (..)
                   , StyleDoc, (<+>), align, blankLine, bulletedList
                   , debugBracket, encloseSep, fill, fillSep, flow, hang, hcat
                   , hsep, indent, line, logLevelToStyle, mkNarrativeList
                   , parens, prettyDebug, prettyDebugL, prettyError
                   , prettyErrorL, prettyGeneric, prettyInfo, prettyInfoL
                   , prettyInfoS, prettyNote, prettyNoteL, prettyNoteS
                   , prettyWarn, prettyWarnL, prettyWarnNoIndent, prettyWarnS
                   , punctuate, sep, softbreak, softline, spacedBulletedList
                   , string, style, stylesUpdateL, useColorL, vsep
                   )
import           RIO.PrettyPrint.DefaultStyles (defaultStyles)
import           RIO.PrettyPrint.PrettyException
                   ( PrettyException (..), ppException, prettyThrowIO
                   , prettyThrowM
                   )
import           RIO.PrettyPrint.StylesUpdate
                   ( StylesUpdate (..), parseStylesUpdateFromString )
import           RIO.PrettyPrint.Types ( StyleSpec )
import           RIO.Process
                   ( HasProcessContext (..), ProcessConfig, ProcessContext
                   , closed, getStderr, getStdout, proc, readProcess_, setStderr
                   , setStdin, setStdout, waitExitCode, withProcessWait_
                   , workingDirL
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
sinkProcessStderrStdout ::
     forall e o env. (HasProcessContext env, HasLogFunc env, HasCallStack)
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
sinkProcessStdout ::
     (HasProcessContext env, HasLogFunc env, HasCallStack)
  => String -- ^ Command
  -> [String] -- ^ Command line arguments
  -> ConduitM ByteString Void (RIO env) a -- ^ Sink for stdout
  -> RIO env a
sinkProcessStdout name args sinkStdout =
  proc name args $ \pc ->
  withLoggedProcess_ (setStdin closed pc) $ \p -> runConcurrently
    $ Concurrently (runConduit $ getStderr p .| CL.sinkNull)
   *> Concurrently (runConduit $ getStdout p .| sinkStdout)

logProcessStderrStdout ::
     (HasCallStack, HasProcessContext env, HasLogFunc env)
  => ProcessConfig stdin stdoutIgnored stderrIgnored
  -> RIO env ()
logProcessStderrStdout pc = withLoggedProcess_ pc $ \p ->
  let logLines = CB.lines .| CL.mapM_ (logInfo . displayBytesUtf8)
  in  runConcurrently
        $  Concurrently (runConduit $ getStdout p .| logLines)
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

-- | Like @First Bool@, but the default is @True@.
newtype FirstTrue
  = FirstTrue { getFirstTrue :: Maybe Bool }
  deriving (Eq, Ord, Show)

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
newtype FirstFalse
  = FirstFalse { getFirstFalse :: Maybe Bool }
  deriving (Eq, Ord, Show)

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

newtype PrettyRawSnapshotLocation
  = PrettyRawSnapshotLocation RawSnapshotLocation

instance Pretty PrettyRawSnapshotLocation where
  pretty (PrettyRawSnapshotLocation (RSLCompiler compiler)) =
    fromString $ T.unpack $ utf8BuilderToText $ display compiler
  pretty (PrettyRawSnapshotLocation (RSLUrl url Nothing)) =
    style Url (fromString $ T.unpack url)
  pretty (PrettyRawSnapshotLocation (RSLUrl url (Just blob))) =
    fillSep
    [ style Url (fromString $ T.unpack url)
    , parens $ fromString $ T.unpack $ utf8BuilderToText $ display blob
    ]
  pretty (PrettyRawSnapshotLocation (RSLFilePath resolved)) =
    style File (fromString $ show $ resolvedRelative resolved)
  pretty (PrettyRawSnapshotLocation (RSLSynonym syn)) = fromString $ show syn

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

-- | Maybe cons.
mcons :: Maybe a -> [a] -> [a]
mcons ma as = maybe as (:as) ma
