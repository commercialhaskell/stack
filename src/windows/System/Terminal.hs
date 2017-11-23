module System.Terminal
( getTerminalWidth
) where
-- | Get the width, in columns, of the terminal if we can.
getTerminalWidth :: IO (Maybe Int)
getTerminalWidth = return Nothing
