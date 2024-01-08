{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Types and functions related to Stack's @eval@ command.
module Stack.Eval
  ( EvalOpts (..)
  , evalCmd
  ) where

import           Stack.Exec
                   ( ExecOpts (..), ExecOptsExtra, SpecialExecCmd (..)
                   , execCmd
                   )
import           Stack.Prelude
import           Stack.Types.Runner ( Runner )

-- Type representing command line options for the @stack eval@ command.
data EvalOpts = EvalOpts
  { evalArg :: !String
  , evalExtra :: !ExecOptsExtra
  }
  deriving Show

-- | Function underlying the @stack eval@ command. Evaluate some Haskell code
-- inline.
evalCmd :: EvalOpts -> RIO Runner ()
evalCmd eval = execCmd execOpts
 where
  execOpts = ExecOpts
    { eoCmd = ExecGhc
    , eoArgs = ["-e", eval.evalArg]
    , eoExtra = eval.evalExtra
    }
