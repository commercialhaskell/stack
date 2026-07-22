module LogHelper where

import Str (greeting)
import Logger (logMessage)

greetWithLog :: String
greetWithLog = logMessage greeting
