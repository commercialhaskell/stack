{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Types.Build.FileTargets
License     : BSD-3-Clause
-}

module Stack.Types.Build.FileTargets
  ( FileTarget (..)
  , unionFileTargets
  , toTarget
  ) where

import qualified Data.Map as Map
import           Stack.Prelude
import           Stack.Types.NamedComponent ( NamedComponent )
import           Stack.Types.SourceMap ( Target (..) )

-- Type representing information about file targets that are associated with a
-- project package.
newtype FileTarget = FileTarget (Map NamedComponent [Path Abs File])

-- | Combine file targets.
unionFileTargets ::
     Ord k
  => Map k FileTarget
  -> Map k FileTarget
  -> Map k FileTarget
unionFileTargets = Map.unionWith $ \(FileTarget l) (FileTarget r) ->
  FileTarget (Map.unionWith (<>) l r)

-- | For the given v'FileTarget', yield the corresponding 'Target'.
toTarget :: FileTarget -> Target
toTarget (FileTarget t) = TargetComps $ Map.keysSet t
