{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

-- | Execute commands within the properly configured Stack
-- environment.

module Stack.Exec where

import           Stack.Prelude
import           Stack.Types.Config
import           System.Process.Log
import           Data.Conduit.Process.Typed (readProcess_, runProcess, setStdin, inherit)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)

import           System.Exit
import           System.Process.Read (withProc, HasEnvOverride (..))
import qualified System.Process.PID1 as PID1
import           System.Process.Read (envHelper, preProcess)

-- | Default @EnvSettings@ which includes locals and GHC_PACKAGE_PATH.
--
-- Note that this also passes through the GHCRTS environment variable.
-- See https://github.com/commercialhaskell/stack/issues/3444
defaultEnvSettings :: EnvSettings
defaultEnvSettings = EnvSettings
    { esIncludeLocals = True
    , esIncludeGhcPackagePath = True
    , esStackExe = True
    , esLocaleUtf8 = False
    , esKeepGhcRts = True
    }

-- | Environment settings which do not embellish the environment
--
-- Note that this also passes through the GHCRTS environment variable.
-- See https://github.com/commercialhaskell/stack/issues/3444
plainEnvSettings :: EnvSettings
plainEnvSettings = EnvSettings
    { esIncludeLocals = False
    , esIncludeGhcPackagePath = False
    , esStackExe = False
    , esLocaleUtf8 = False
    , esKeepGhcRts = True
    }

-- | Execute a process within the Stack configured environment.
--
-- Execution will not return, because either:
--
-- 1) On non-windows, execution is taken over by execv of the
-- sub-process. This allows signals to be propagated (#527)
--
-- 2) On windows, an 'ExitCode' exception will be thrown.
exec :: HasEnvOverride env => String -> [String] -> RIO env b
#ifdef WINDOWS
exec = execSpawn
#else
exec cmd0 args = do
    menv <- view envOverrideL
    cmd <- preProcess cmd0
    withProcessTimeLog Nothing cmd args $
        liftIO $ PID1.run cmd args $ Just $ envHelper menv
#endif

-- | Like 'exec', but does not use 'execv' on non-windows. This way, there
-- is a sub-process, which is helpful in some cases (#1306)
--
-- This function only exits by throwing 'ExitCode'.
execSpawn :: HasEnvOverride env => String -> [String] -> RIO env a
execSpawn cmd args = withProc cmd args (runProcess . setStdin inherit) >>= liftIO . exitWith

execObserve :: HasEnvOverride env => String -> [String] -> RIO env String
execObserve cmd0 args =
  withProc cmd0 args $ \pc -> do
    (out, _err) <- readProcess_ pc
    return
      $ TL.unpack
      $ TL.filter (/= '\r')
      $ TL.concat
      $ take 1
      $ TL.lines
      $ decodeUtf8With lenientDecode out
