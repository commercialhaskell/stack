{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack
Description : Main Stack tool entry point.
License     : BSD-3-Clause

Main Stack tool entry point.
-}

module Stack
  ( main
  ) where

import           Control.Monad.Extra ( whenJust )
import           GHC.IO.Encoding ( mkTextEncoding, textEncodingName )
import           Options.Applicative.Builder.Extra ( execExtraHelp )
import           Path ( parseAbsFile )
import           Stack.BuildInfo ( versionString' )
import           Stack.CLI ( commandLineHandler )
import           Stack.Constants ( stackProgName )
import           Stack.Docker ( dockerCmdName, dockerHelpOptName )
import           Stack.Nix ( nixCmdName, nixHelpOptName )
import           Stack.Options.DockerParser ( dockerOptsParser )
import           Stack.Options.GlobalParser ( globalOptsFromMonoid )
import           Stack.Options.NixParser ( nixOptsParser )
import           Stack.Prelude
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withRunnerGlobal )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.Runner ( Runner )
import           Stack.Types.Version
                   ( VersionCheck (..), checkVersion, showStackVersion
                   , stackVersion
                   )
import           System.Directory ( getCurrentDirectory )
import           System.Environment ( executablePath, getArgs, getProgName )
import           System.IO ( hGetEncoding, hPutStrLn, hSetEncoding )
import           System.Terminal ( hIsTerminalDeviceOrMinTTY )

-- | Type representing exceptions thrown by functions in the "Stack" module.
data StackException
  = InvalidReExecVersion String String
  deriving (Show, Typeable)

instance Exception StackException where
  displayException (InvalidReExecVersion expected actual) = concat
    [ "Error: [S-2186]\n"
    , "When re-executing '"
    , stackProgName
    , "' in a container, the incorrect version was found\nExpected: "
    , expected
    , "; found: "
    , actual
    ]

-- | Main Stack tool entry point.
main :: IO ()
main = do
  -- Line buffer the output by default, particularly for non-terminal runs.
  -- See https://github.com/commercialhaskell/stack/pull/360
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr LineBuffering
  hSetTranslit stdout
  hSetTranslit stderr
  args <- getArgs
  progName <- getProgName
  mExecutableFilePath <- fromMaybe (pure Nothing) executablePath
  let mExecutablePath = mExecutableFilePath >>= parseAbsFile
  isTerminal <- hIsTerminalDeviceOrMinTTY stdout
  execExtraHelp
    args
    dockerHelpOptName
    (dockerOptsParser False)
    ("Only showing --" ++ dockerCmdName ++ "* options.")
  execExtraHelp
    args
    nixHelpOptName
    (nixOptsParser False)
    ("Only showing --" ++ nixCmdName ++ "* options.")
  currentDir <- getCurrentDirectory
  try (commandLineHandler currentDir progName mExecutablePath False) >>= \case
    Left (exitCode :: ExitCode) ->
      throwIO exitCode
    Right (globalMonoid, run) -> do
      global <-
        globalOptsFromMonoid progName mExecutablePath isTerminal globalMonoid
      when (global.logLevel == LevelDebug) $
        hPutStrLn stderr versionString'
      whenJust global.reExecVersion $ \expectVersion -> do
        expectVersion' <- parseVersionThrowing expectVersion
        unless (checkVersion MatchMinor expectVersion' stackVersion) $
          throwIO $ InvalidReExecVersion expectVersion showStackVersion
      withRunnerGlobal global $ run `catches`
        [ Handler handleExitCode
        , Handler handlePrettyException
        , Handler handlePantryException
        , Handler handleSomeException
        ]

-- | Change the character encoding of the given Handle to transliterate on
-- unsupported characters instead of throwing an exception
hSetTranslit :: Handle -> IO ()
hSetTranslit h = do
  menc <- hGetEncoding h
  whenJust (fmap textEncodingName menc) $ \name ->
    unless ('/' `elem` name) $ do
      enc' <- mkTextEncoding $ name ++ "//TRANSLIT"
      hSetEncoding h enc'

-- | Handle ExitCode exceptions.
handleExitCode :: ExitCode -> RIO Runner a
handleExitCode = exitWith

-- | Handle PrettyException exceptions.
handlePrettyException :: PrettyException -> RIO Runner a
handlePrettyException = handleAnyPrettyException

-- | Handle (pretty) PantryException exceptions.
handlePantryException :: PantryException -> RIO Runner a
handlePantryException = handleAnyPrettyException

-- | Handle any pretty exception.
handleAnyPrettyException :: (Exception e, Pretty e) => e -> RIO Runner a
handleAnyPrettyException e = do
  -- The code below loads the entire Stack configuration, when all that is
  -- needed are the Stack colours. A tailored approach may be better.
  tryAny (withConfig NoReexec $ prettyError $ pretty e) >>= \case
    -- Falls back to the command line's Stack colours if there is any error in
    -- loading the entire Stack configuration.
    Left _ -> prettyError $ pretty e
    Right _ -> pure ()
  exitFailure

-- | Handle SomeException exceptions. This special handler stops "stack: " from
-- being printed before the exception.
handleSomeException :: SomeException -> RIO Runner a
handleSomeException (SomeException e) = do
  logError $ fromString $ displayException e
  exitFailure
