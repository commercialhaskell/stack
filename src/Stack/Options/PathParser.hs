{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions to parse command line arguments for Stack's @path@ command.
module Stack.Options.PathParser
  ( pathParser
  ) where

import qualified Data.Text as T
import           Options.Applicative ( Parser, flag, help, long )
import           Stack.Path ( paths )
import           Stack.Prelude

-- | Parse command line arguments for Stack's @path@ command.
pathParser :: Parser [Text]
pathParser = mapMaybeA
  ( \(desc, name, _) -> flag Nothing (Just name)
      (  long (T.unpack name)
      <> help desc
      )
  )
  paths
