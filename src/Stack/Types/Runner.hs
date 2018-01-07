{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Run environment

module Stack.Types.Runner
    ( Runner (..)
    , HasRunner (..)
    , terminalL
    , reExecL
    , logOptionsL
    , LogOptions (..)
    , ColorWhen (..)
    , withRunner
    ) where

import           Distribution.PackageDescription (GenericPackageDescription)
import           GHC.Foreign                (peekCString, withCString)
import           Lens.Micro
import           Stack.Prelude              hiding (lift)
import           Stack.Constants
import           Stack.Types.PackageIdentifier (PackageIdentifierRevision)
import           System.Console.ANSI
import           System.IO                  (localeEncoding)
import           RIO.Process (HasEnvOverride (..), EnvOverride, getEnvOverride)
import           System.Terminal

-- | Monadic environment.
data Runner = Runner
  { runnerReExec     :: !Bool
  , runnerLogFunc    :: !LogFunc
  , runnerLogOptions :: !LogOptions
  , runnerTermWidth  :: !Int
  , runnerEnvOverride :: !EnvOverride
  , runnerParsedCabalFiles :: !(IORef
      ( Map PackageIdentifierRevision GenericPackageDescription
      , Map (Path Abs Dir)            (GenericPackageDescription, Path Abs File)
      ))
  -- ^ Cache of previously parsed cabal files.
  --
  -- TODO: This is really an ugly hack to avoid spamming the user with
  -- warnings when we parse cabal files multiple times and bypass
  -- performance issues. Ideally: we would just design the system such
  -- that it only ever parses a cabal file once. But for now, this is
  -- a decent workaround. See:
  -- <https://github.com/commercialhaskell/stack/issues/3591>.
  }

class HasEnvOverride env => HasRunner env where
  runnerL :: Lens' env Runner
instance HasEnvOverride Runner where
  envOverrideL = lens runnerEnvOverride (\x y -> x { runnerEnvOverride = y })
instance HasRunner Runner where
  runnerL = id

terminalL :: HasRunner env => Lens' env Bool
terminalL = logOptionsL.lens logTerminal (\x y -> x { logTerminal = y })

reExecL :: HasRunner env => Lens' env Bool
reExecL = runnerL.lens runnerReExec (\x y -> x { runnerReExec = y })

logOptionsL :: HasRunner env => Lens' env LogOptions
logOptionsL = runnerL.lens runnerLogOptions (\x y -> x { runnerLogOptions = y })

--------------------------------------------------------------------------------
-- Logging functionality

instance HasLogFunc Runner where
  logFuncL = lens runnerLogFunc (\x y -> x { runnerLogFunc = y })

-- | With a 'Runner', do the thing
withRunner :: MonadIO m
           => LogLevel
           -> Bool -- ^ use time?
           -> Bool -- ^ terminal?
           -> ColorWhen
           -> Maybe Int -- ^ terminal width override
           -> Bool -- ^ reexec?
           -> (Runner -> m a)
           -> m a
withRunner logLevel useTime terminal colorWhen widthOverride reExec inner = do
  useColor <- case colorWhen of
    ColorNever -> return False
    ColorAlways -> return True
    ColorAuto -> liftIO $ hSupportsANSI stderr
  termWidth <- clipWidth <$> maybe (fromMaybe defaultTerminalWidth
                                    <$> liftIO getTerminalWidth)
                                   pure widthOverride
  canUseUnicode <- liftIO getCanUseUnicode
  ref <- newIORef mempty
  menv <- getEnvOverride
  let logOptions = LogOptions
        { logUseColor = useColor
        , logUseUnicode = canUseUnicode
        , logUseTime = useTime
        , logMinLevel = logLevel
        , logVerboseFormat = logLevel <= LevelDebug
        , logTerminal = terminal
        }
  withStickyLogger logOptions $ \logFunc -> inner Runner
    { runnerReExec = reExec
    , runnerLogFunc = logFunc
    , runnerLogOptions = logOptions
    , runnerTermWidth = termWidth
    , runnerParsedCabalFiles = ref
    , runnerEnvOverride = menv
    }
  where clipWidth w
          | w < minTerminalWidth = minTerminalWidth
          | w > maxTerminalWidth = maxTerminalWidth
          | otherwise = w

-- | Taken from GHC: determine if we should use Unicode syntax
getCanUseUnicode :: IO Bool
getCanUseUnicode = do
    let enc = localeEncoding
        str = "\x2018\x2019"
        test = withCString enc str $ \cstr -> do
            str' <- peekCString enc cstr
            return (str == str')
    test `catchIO` \_ -> return False

data ColorWhen = ColorNever | ColorAlways | ColorAuto
    deriving (Show, Generic)
