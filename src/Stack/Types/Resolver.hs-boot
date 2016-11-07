{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Stack.Types.Resolver where

import           Data.Text (Text)
import           Stack.Types.BuildPlan (SnapName, SnapshotHash)
import           Stack.Types.Compiler

data IsLoaded = Loaded | NotLoaded

data ResolverThat's (l :: IsLoaded) where
    ResolverSnapshot :: !SnapName -> ResolverThat's l
    ResolverCompiler :: !CompilerVersion -> ResolverThat's l
    ResolverCustom :: !Text -> !Text -> ResolverThat's 'NotLoaded
    ResolverCustomLoaded :: !Text -> !Text -> !SnapshotHash -> ResolverThat's 'Loaded

type LoadedResolver = ResolverThat's 'Loaded

type Resolver = ResolverThat's 'NotLoaded
