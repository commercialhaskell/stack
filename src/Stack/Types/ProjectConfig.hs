{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Types.ProjectConfig
License     : BSD-3-Clause
-}

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
    -- either 'Stack.Types.StackYamlLoc.SYLDefault' or
    -- 'Stack.Types.StackYamlLoc.SYLOverride'.
  | PCGlobalProject
    -- ^ No project was found when using 'Stack.Types.StackYamlLoc.SYLDefault'.
    -- Instead, use the implicit global.
  | PCNoProject ![RawPackageLocationImmutable]
    -- ^ Use a no project run. This comes from
    -- 'Stack.Types.StackYamlLocSYLNoProject'.

-- | Yields 'True' only if the project configuration information is for the
-- implicit global project.
isPCGlobalProject :: ProjectConfig a -> Bool
isPCGlobalProject PCGlobalProject = True
isPCGlobalProject _ = False
