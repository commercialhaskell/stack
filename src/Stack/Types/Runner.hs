{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Run environment

module Stack.Types.Runner
    ( Runner (..)
    , HasRunner (..)
    , terminalL
    , useColorL
    , reExecL
    , ColorWhen (..)
    , withRunner
    ) where

import           Distribution.PackageDescription (GenericPackageDescription)
import           Lens.Micro
import           Stack.Prelude              hiding (lift)
import           Stack.Constants
import           Stack.Types.PackageIdentifier (PackageIdentifierRevision)
import           System.Console.ANSI
import           RIO.Process (HasProcessContext (..), ProcessContext, mkDefaultProcessContext)
import           System.Terminal

-- | Monadic environment.
data Runner = Runner
  { runnerReExec     :: !Bool
  , runnerTerminal   :: !Bool
  , runnerUseColor   :: !Bool
  , runnerLogFunc    :: !LogFunc
  , runnerTermWidth  :: !Int
  , runnerProcessContext :: !ProcessContext
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

class (HasProcessContext env, HasLogFunc env) => HasRunner env where
  runnerL :: Lens' env Runner
instance HasProcessContext Runner where
  processContextL = lens runnerProcessContext (\x y -> x { runnerProcessContext = y })
instance HasRunner Runner where
  runnerL = id

terminalL :: HasRunner env => Lens' env Bool
terminalL = runnerL.lens runnerTerminal (\x y -> x { runnerTerminal = y })

useColorL :: HasRunner env => Lens' env Bool
useColorL = runnerL.lens runnerUseColor (\x y -> x { runnerUseColor = y })

reExecL :: HasRunner env => Lens' env Bool
reExecL = runnerL.lens runnerReExec (\x y -> x { runnerReExec = y })

--------------------------------------------------------------------------------
-- Logging functionality

instance HasLogFunc Runner where
  logFuncL = lens runnerLogFunc (\x y -> x { runnerLogFunc = y })

-- | With a 'Runner', do the thing
withRunner :: MonadUnliftIO m
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
  ref <- newIORef mempty
  menv <- mkDefaultProcessContext
  logOptions0 <- logOptionsHandle stderr False
  let logOptions
        = setLogUseColor useColor
        $ setLogUseTime useTime
        $ setLogMinLevel logLevel
        $ setLogVerboseFormat (logLevel <= LevelDebug)
        $ setLogTerminal terminal
          logOptions0
  withLogFunc logOptions $ \logFunc -> inner Runner
    { runnerReExec = reExec
    , runnerTerminal = terminal
    , runnerUseColor = useColor
    , runnerLogFunc = logFunc
    , runnerTermWidth = termWidth
    , runnerParsedCabalFiles = ref
    , runnerProcessContext = menv
    }
  where clipWidth w
          | w < minTerminalWidth = minTerminalWidth
          | w > maxTerminalWidth = maxTerminalWidth
          | otherwise = w

data ColorWhen = ColorNever | ColorAlways | ColorAuto
    deriving (Show, Generic)
