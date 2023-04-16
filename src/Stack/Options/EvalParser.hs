{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions to parse command line arguments for Stack's @eval@ command.
module Stack.Options.EvalParser
  ( evalOptsParser
  ) where

import           Options.Applicative ( Parser, metavar, strArgument )
import           Stack.Eval ( EvalOpts (..) )
import           Stack.Options.ExecParser ( execOptsExtraParser )
import           Stack.Prelude

-- | Parse command line arguments for Stack's @eval@ command.
evalOptsParser ::
     String -- ^ metavar
  -> Parser EvalOpts
evalOptsParser meta = EvalOpts
  <$> eoArgsParser
  <*> execOptsExtraParser
 where
  eoArgsParser :: Parser String
  eoArgsParser = strArgument (metavar meta)
