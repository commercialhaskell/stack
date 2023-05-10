{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Constants.StackProgName
  ( stackProgName
  ) where

import           Stack.Prelude ( String )

-- | Name of the Stack program.

-- NOTE: Defined in this module rather than in "Stack.Constants", due to
-- GHC stage restrictions and the use of Template Haskell.
stackProgName :: String
stackProgName = "stack"
