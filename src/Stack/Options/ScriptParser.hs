{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Options.ScriptParser
  ( ScriptExecute (..)
  , ScriptOpts (..)
  , ShouldRun (..)
  , scriptOptsParser
  ) where

import           Options.Applicative
                   ( Parser, completer, eitherReader, flag', help, long
                   , metavar, option, strArgument, strOption
                   )
import           Options.Applicative.Builder.Extra ( fileExtCompleter )
import           Stack.Options.Completion ( ghcOptsCompleter )
import           Stack.Prelude hiding ( Path )
import           Data.ByteString.Lazy.Char8 ( pack )
import           Data.Aeson ( eitherDecode, FromJSON (parseJSON) )
import           Pantry.Internal.AesonExtended ( WithJSONWarnings )
import           Data.Aeson.Types ( parseEither )
import qualified Data.Text as T
import qualified Data.Aeson.Types as A

data ScriptOpts = ScriptOpts
  { soPackages :: ![String]
  , soFile :: !FilePath
  , soArgs :: ![String]
  , soCompile :: !ScriptExecute
  , soGhcOptions :: ![String]
  , soScriptExtraDeps :: ![WithJSONWarnings (Unresolved (NonEmpty RawPackageLocation))]
  , soShouldRun :: !ShouldRun
  }

data ScriptExecute
  = SEInterpret
  | SECompile
  | SEOptimize
  deriving Show

data ShouldRun
  = YesRun
  | NoRun
  deriving Show

scriptOptsParser :: Parser ScriptOpts
scriptOptsParser = ScriptOpts
  <$> many (strOption
        (  long "package"
        <> metavar "PACKAGE"
        <> help "Add a package (can be specified multiple times)"
        ))
  <*> strArgument
        (  metavar "FILE"
        <> completer (fileExtCompleter [".hs", ".lhs"])
        )
  <*> many (strArgument
        (  metavar "-- ARGUMENT(S) (e.g. stack script X.hs -- argument(s) to \
                   \program)"
        ))
  <*> (   flag' SECompile
            (  long "compile"
            <> help "Compile the script without optimization and run the executable"
            )
      <|> flag' SEOptimize
            (  long "optimize"
            <> help "Compile the script with optimization and run the executable"
            )
      <|> pure SEInterpret
      )
  <*> many (strOption
        (  long "ghc-options"
        <> metavar "OPTIONS"
        <> completer ghcOptsCompleter
        <> help "Additional options passed to GHC (can be specified multiple \
                \times)"
        ))
  <*> many (option extraDepRead
        (  long "extra-dep"
        <> metavar "PACKAGE-VERSION"
        <> help "Extra dependencies to be added to the snapshot"
        ))
  <*> (   flag' NoRun
            (  long "no-run"
            <> help "Don't run, just compile."
            )
      <|> pure YesRun
      )
 where
  extraDepRead =
      eitherReader (parseEither parseJSON . A.String . T.pack)
      <|> eitherReader (eitherDecode . pack)
