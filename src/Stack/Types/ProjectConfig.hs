{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.ProjectConfig
  ( ProjectConfig (..)
  , isPCGlobalProject
  ) where

import           Stack.Prelude

-- | Project configuration information. Not every run of Stack has a
-- true local project; see constructors below.
data ProjectConfig a
  = PCProject a
    -- ^ Normal run: we want a project, and have one. This comes from
    -- either 'SYLDefault' or 'SYLOverride'.
  | PCGlobalProject
    -- ^ No project was found when using 'SYLDefault'. Instead, use
    -- the implicit global.
  | PCNoProject ![RawPackageLocationImmutable]
    -- ^ Use a no project run. This comes from 'SYLNoProject'.

-- | Yields 'True' only if the project configuration information is for the
-- implicit global project.
isPCGlobalProject :: ProjectConfig a -> Bool
isPCGlobalProject PCGlobalProject = True
isPCGlobalProject _ = False
