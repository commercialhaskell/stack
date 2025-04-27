{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Options.PathParser
Description : Parse arguments for Stack's @path@ command.
License     : BSD-3-Clause

Functions to parse command line arguments for Stack's @path@ command.
-}

module Stack.Options.PathParser
  ( pathParser
  ) where

import qualified Data.Text as T
import           Options.Applicative ( Parser, flag, help, long )
import           Stack.Path
                   ( pathsFromConfig, pathsFromEnvConfig, pathsFromRunner )
import           Stack.Prelude

-- | Parse command line arguments for Stack's @path@ command.
pathParser :: Parser [Text]
pathParser = mapMaybeA
  ( \(desc, name) -> flag Nothing (Just name)
      (  long (T.unpack name)
      <> help desc
      )
  )
  paths
 where
  toDescName (desc, name, _) = (desc, name)
  paths =
       pathsFromRunner
    :  map toDescName pathsFromConfig
    <> map toDescName pathsFromEnvConfig
