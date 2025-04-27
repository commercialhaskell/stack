{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Constants.StackProgName
License     : BSD-3-Clause

'stackProgName' is defined in this module rather than in "Stack.Constants", due
to GHC stage restrictions and the use of Template Haskell.
-}

module Stack.Constants.StackProgName
  ( stackProgName
  ) where

import           Stack.Prelude ( String )

-- | Name of the Stack program.
stackProgName :: String
stackProgName = "stack"
