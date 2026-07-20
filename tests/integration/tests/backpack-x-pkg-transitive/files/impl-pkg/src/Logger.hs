module Logger where

logMessage :: String -> String
logMessage msg = "[LOG] " ++ msg

loggerName :: String
loggerName = "transitive logger"
