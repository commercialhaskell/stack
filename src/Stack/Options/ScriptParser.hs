module Stack.Options.ScriptParser where

import           Data.Monoid ((<>))
import           Options.Applicative
import           Options.Applicative.Builder.Extra

data ScriptOpts = ScriptOpts
  { soPackages :: ![String]
  , soFile :: !FilePath
  , soArgs :: ![String]
  , soCompile :: !ScriptExecute
  }
  deriving Show

data ScriptExecute
  = SEInterpret
  | SECompile
  | SEOptimize
  deriving Show

scriptOptsParser :: Parser ScriptOpts
scriptOptsParser = ScriptOpts
    <$> many (strOption (long "package" <> help "Additional packages that must be installed"))
    <*> strArgument (metavar "FILE" <> completer (fileExtCompleter [".hs", ".lhs"]))
    <*> many (strArgument (metavar "-- ARGS (e.g. stack ghc -- X.hs -o x)"))
    <*> (flag' SECompile
            ( long "compile"
           <> help "Compile the script without optimization and run the executable"
            ) <|>
         flag' SEOptimize
            ( long "optimize"
           <> help "Compile the script with optimization and run the executable"
            ) <|>
         pure SEInterpret)
