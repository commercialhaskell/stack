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
    , stylesUpdateL
    , reExecL
    , ColorWhen (..)
    , withRunner
    ) where

import           Data.Aeson (FromJSON (parseJSON))
import           Lens.Micro
import           Stack.Prelude              hiding (lift)
import           Stack.Constants
import           RIO.PrettyPrint
import           RIO.PrettyPrint.StylesUpdate (HasStylesUpdate (..), StylesUpdate)
import           RIO.Process (HasProcessContext (..), ProcessContext, mkDefaultProcessContext)
import           System.Console.ANSI
import           System.Terminal

-- | Monadic environment.
data Runner = Runner
  { runnerReExec     :: !Bool
  , runnerTerminal   :: !Bool
  , runnerUseColor   :: !Bool
  , runnerStylesUpdate :: !StylesUpdate
  , runnerLogFunc    :: !LogFunc
  , runnerTermWidth  :: !Int
  , runnerProcessContext :: !ProcessContext
  }

class (HasProcessContext env, HasLogFunc env) => HasRunner env where
  runnerL :: Lens' env Runner
instance HasProcessContext Runner where
  processContextL = lens runnerProcessContext (\x y -> x { runnerProcessContext = y })
instance HasRunner Runner where
  runnerL = id
instance HasStylesUpdate Runner where
  stylesUpdateL = lens runnerStylesUpdate (\x y -> x { runnerStylesUpdate = y })
instance HasTerm Runner where
  useColorL = lens runnerUseColor (\x y -> x { runnerUseColor = y })
  termWidthL = lens runnerTermWidth (\x y -> x { runnerTermWidth = y })

terminalL :: HasRunner env => Lens' env Bool
terminalL = runnerL.lens runnerTerminal (\x y -> x { runnerTerminal = y })

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
           -> StylesUpdate
           -> Maybe Int -- ^ terminal width override
           -> Bool -- ^ reexec?
           -> (Runner -> m a)
           -> m a
withRunner logLevel useTime terminal colorWhen stylesUpdate widthOverride reExec inner = do
  useColor <- case colorWhen of
    ColorNever -> return False
    ColorAlways -> return True
    ColorAuto -> liftIO $ fromMaybe True <$>
                          hSupportsANSIWithoutEmulation stderr
  termWidth <- clipWidth <$> maybe (fromMaybe defaultTerminalWidth
                                    <$> liftIO getTerminalWidth)
                                   pure widthOverride
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
    , runnerStylesUpdate = stylesUpdate
    , runnerLogFunc = logFunc
    , runnerTermWidth = termWidth
    , runnerProcessContext = menv
    }
  where clipWidth w
          | w < minTerminalWidth = minTerminalWidth
          | w > maxTerminalWidth = maxTerminalWidth
          | otherwise = w

data ColorWhen = ColorNever | ColorAlways | ColorAuto
    deriving (Eq, Show, Generic)

instance FromJSON ColorWhen where
    parseJSON v = do
        s <- parseJSON v
        case s of
            "never"  -> return ColorNever
            "always" -> return ColorAlways
            "auto"   -> return ColorAuto
            _ -> fail ("Unknown color use: " <> s <> ". Expected values of " <>
                       "option are 'never', 'always', or 'auto'.")
