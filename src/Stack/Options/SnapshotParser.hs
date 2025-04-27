{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds         #-}

{-|
Module      : Stack.Options.SnapshotParser
License     : BSD-3-Clause
-}

module Stack.Options.SnapshotParser
  ( abstractSnapshotOptsParser
  , compilerOptsParser
  , readCompilerVersion
  ) where

import qualified Data.Text as T
import           Options.Applicative
                   ( Parser, ReadM, help, long, metavar, option, readerError )
import           Options.Applicative.Types ( readerAsk )
import           Stack.Options.Utils ( hideMods )
import           Stack.Prelude
import           Stack.Types.Snapshot ( AbstractSnapshot, readAbstractSnapshot )

-- | Parser for the snapshot
abstractSnapshotOptsParser :: Bool -> Parser (Unresolved AbstractSnapshot)
abstractSnapshotOptsParser hide = option readAbstractSnapshot
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
