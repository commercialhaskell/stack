{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Options.FreezeParser where

import           Data.Semigroup ((<>))
import           Options.Applicative
import           Stack.Freeze


-- | Parser for arguments to `stack freeze`
freezeOptsParser :: Parser FreezeOpts
freezeOptsParser =
  FreezeOpts <$> flag FreezeProject FreezeSnapshot
                 ( long "snapshot"
                   <> short 's'
                   <> help "Freeze snapshot definition instead of project's stack.yaml" )
