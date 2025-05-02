{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NoFieldSelectors      #-}

{-|
Module      : Stack.Types.HpcReportOpts
Description : Types related to Stack's @hpc report@ command.
License     : BSD-3-Clause

Types related to Stack's @hpc report@ command.
-}

module Stack.Types.HpcReportOpts
  ( HpcReportOpts (..)
  ) where

import           Stack.Prelude

-- | Type representing command line options for the @stack hpc report@ command.
data HpcReportOpts = HpcReportOpts
  { inputs :: [Text]
  , all :: Bool
  , destDir :: Maybe String
  , openBrowser :: Bool
  }
  deriving Show
