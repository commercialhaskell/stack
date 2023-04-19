{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.ProjectConfig
  ( ProjectConfig (..)
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
  | PCNoProject ![PackageIdentifierRevision]
    -- ^ Use a no project run. This comes from 'SYLNoProject'.
