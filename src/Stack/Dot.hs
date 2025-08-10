{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack.Dot
Description : Functions related to Stack's @dot@ command.
License     : BSD-3-Clause

Functions related to Stack's @dot@ command.
-}

module Stack.Dot
  ( dotCmd
  , printGraph
  ) where

import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Stack.Constants ( wiredInPackages )
import           Stack.DependencyGraph ( createPrunedDependencyGraph )
import           Stack.Prelude
import           Stack.Types.Compiler ( ActualCompiler )
import           Stack.Types.DependencyTree ( DependencyGraph )
import           Stack.Types.DotOpts ( DotOpts (..) )
import           Stack.Types.Runner ( Runner )

-- | Visualize the project's dependencies as a graphviz graph
dotCmd :: DotOpts -> RIO Runner ()
dotCmd dotOpts = do
  (compiler, localNames, prunedGraph) <- createPrunedDependencyGraph dotOpts
  printGraph dotOpts compiler localNames prunedGraph

-- | Print a graphviz graph of the edges in the Map and highlight the given
-- project packages
printGraph ::
     (Applicative m, MonadIO m)
  => DotOpts
  -> ActualCompiler
  -> Set PackageName -- ^ All project packages.
  -> DependencyGraph
  -> m ()
printGraph dotOpts compiler locals graph = do
  liftIO $ Text.putStrLn "strict digraph deps {"
  printLocalNodes dotOpts filteredLocals
  printLeaves compiler graph
  let allNodes = Map.keysSet graph
  void (Map.traverseWithKey (printEdges allNodes) (fst <$> graph))
  liftIO $ Text.putStrLn "}"
 where
  filteredLocals =
    Set.filter (\local' -> local' `Set.notMember` dotOpts.prune) locals

-- | Print the project packages nodes with a different style, depending on
-- options
printLocalNodes ::
     (F.Foldable t, MonadIO m)
  => DotOpts
  -> t PackageName
  -> m ()
printLocalNodes dotOpts locals =
  liftIO $ Text.putStrLn (Text.intercalate "\n" lpNodes)
 where
  applyStyle :: Text -> Text
  applyStyle n = if dotOpts.includeExternal
                   then n <> " [style=dashed];"
                   else n <> " [style=solid];"
  lpNodes :: [Text]
  lpNodes = map (applyStyle . nodeName) (F.toList locals)

-- | Print relevant nodes, based on their relevant attributes.
printLeaves :: MonadIO m => ActualCompiler -> DependencyGraph -> m ()
printLeaves compiler graph =
  F.mapM_ printLeaf (Map.mapWithKey nodeAttributes graph)
 where
  allNodes = Map.keysSet graph
  hasNoNodes = F.all (`Set.notMember` allNodes)
  nodeAttributes package (deps, _) =
    let isWiredInPackage = isWiredIn compiler package
        isBottomRow = hasNoNodes deps
    in  (package, isWiredInPackage, isBottomRow)

-- | @printDedges ps p ps'@ prints an edge from @p@ to every @ps'@, if it is a
-- member of @ps@.
printEdges ::
     MonadIO m
  => Set PackageName
     -- ^ The nodes in the graph.
  -> PackageName
     -- ^ The node in question.
  -> Set PackageName
     -- ^ The dependencies of the node in question.
  -> m ()
printEdges nodes package deps = F.forM_ deps $ \dep ->
  when (dep `elem` nodes) $ printEdge package dep

-- | Print an edge between the two package names
printEdge :: MonadIO m => PackageName -> PackageName -> m ()
printEdge from to' =
  liftIO $ Text.putStrLn (Text.concat [ nodeName from
                                      , " -> "
                                      , nodeName to'
                                      , ";" ])

-- | Convert a package name to a graph node name.
nodeName :: PackageName -> Text
nodeName name = "\"" <> Text.pack (packageNameString name) <> "\""

-- | Print a node if it (a) is a GHC wired-in package or (b) has no dependencies
-- that are also nodes.
printLeaf ::
     MonadIO m
  =>  ( PackageName
      , Bool
        -- Is package a GHC wired-in package?
      , Bool
        -- Does package have no dependencies that are are also nodes in the
        -- graph?
      )
  -> m ()
printLeaf (package, isWiredInPackage, isBottomRow) =
  when (isWiredInPackage || isBottomRow) $
    liftIO . Text.putStrLn . Text.concat $
         [ "{"]
      <> [ "rank=max; " | isBottomRow ]
      <> [ nodeName package ]
      <> [ " [shape=box]" | isWiredInPackage ]
      <> [ "; };" ]

-- | Check if the package is a GHC wired-in package
isWiredIn :: ActualCompiler -> PackageName -> Bool
isWiredIn compiler package =
  package `Set.member` wiredInPackages compiler
