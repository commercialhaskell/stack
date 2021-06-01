{-# LANGUAGE NoImplicitPrelude #-}
module Stack.Options.ScriptParser where

import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Stack.Options.Completion
import           Stack.Prelude

data ScriptOpts = ScriptOpts
  { soPackages :: ![String]
  , soFile :: !FilePath
  , soArgs :: ![String]
  , soCompile :: !ScriptExecute
  , soGhcOptions :: ![String]
  , soScriptExtraDeps :: ![PackageIdentifierRevision]
  , soShouldRun :: !ShouldRun
  }
  deriving Show

data ScriptExecute
  = SEInterpret
  | SECompile
  | SEOptimize
  deriving Show

data ShouldRun = YesRun | NoRun
  deriving Show

scriptOptsParser :: Parser ScriptOpts
scriptOptsParser = ScriptOpts
    <$> many (strOption
          (long "package" <>
            metavar "PACKAGE(S)" <>
            help "Additional package(s) that must be installed"))
    <*> strArgument (metavar "FILE" <> completer (fileExtCompleter [".hs", ".lhs"]))
    <*> many (strArgument (metavar "-- ARGUMENT(S) (e.g. stack script X.hs -- argument(s) to program)"))
    <*> (flag' SECompile
            ( long "compile"
           <> help "Compile the script without optimization and run the executable"
            ) <|>
         flag' SEOptimize
            ( long "optimize"
           <> help "Compile the script with optimization and run the executable"
            ) <|>
         pure SEInterpret)
    <*> many (strOption
          (long "ghc-options" <>
            metavar "OPTIONS" <>
            completer ghcOptsCompleter <>
            help "Additional options passed to GHC"))
    <*> many (option extraDepRead
          (long "extra-dep" <>
            metavar "PACKAGE-VERSION" <>
            help "Extra dependencies to be added to the snapshot"))
    <*> (flag' NoRun (long "no-run" <> help "Don't run, just compile.") <|> pure YesRun)
  where
    extraDepRead = eitherReader $ mapLeft show . parsePackageIdentifierRevision . fromString
