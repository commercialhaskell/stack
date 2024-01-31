{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds         #-}

module Stack.Options.ResolverParser
  ( abstractResolverOptsParser
  , compilerOptsParser
  , readCompilerVersion
  ) where

import qualified Data.Text as T
import           Options.Applicative
                   ( Parser, ReadM, help, long, metavar, option, readerError )
import           Options.Applicative.Types ( readerAsk )
import           Stack.Options.Utils ( hideMods )
import           Stack.Prelude
import           Stack.Types.Resolver ( AbstractResolver, readAbstractResolver )

-- | Parser for the snapshot
abstractResolverOptsParser :: Bool -> Parser (Unresolved AbstractResolver)
abstractResolverOptsParser hide = option readAbstractResolver
  (  long "snapshot"
  <> long "resolver"
  <> metavar "SNAPSHOT"
  <> help "Override snapshot in the project configuration file."
  <> hideMods hide
  )

compilerOptsParser :: Bool -> Parser WantedCompiler
compilerOptsParser hide = option readCompilerVersion
  (  long "compiler"
  <> metavar "COMPILER"
  <> help "Use the specified compiler."
  <> hideMods hide
  )

readCompilerVersion :: ReadM WantedCompiler
readCompilerVersion = do
  s <- readerAsk
  case parseWantedCompiler (T.pack s) of
    Left{} -> readerError $ "Failed to parse compiler: " ++ s
    Right x -> pure x
