{-# LANGUAGE CPP #-}
#ifndef WINDOWS
{-# LANGUAGE ForeignFunctionInterface #-}
#endif

module System.Terminal
( getTerminalWidth
) where

-- | Get the width, in columns, of the terminal if we can.
getTerminalWidth :: IO (Maybe Int)
#ifndef WINDOWS
getTerminalWidth = pure Nothing
#else
getTerminalWidth = pure Nothing
#endif
