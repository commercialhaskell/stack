{-# LANGUAGE NoImplicitPrelude #-}
module Stack.Options.SolverParser where

import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Stack.Prelude

-- | Parser for @solverCmd@
solverOptsParser :: Parser Bool
solverOptsParser = boolFlags False
    "update-config"
    "Automatically update stack.yaml with the solver's recommendations"
    idm
