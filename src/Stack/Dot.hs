{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Stack.Dot (dot
                 ,DotOpts(..)
                 ,resolveDependencies
                 ,printGraph
                 ,pruneGraph
                 ) where

import           Control.Applicative
import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger (MonadLogger)
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Foldable as F
import qualified Data.HashSet as HashSet
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Traversable as T
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Stack.Build (withLoadPackage)
import           Stack.Build.Source
import           Stack.Build.Types
import           Stack.Constants
import           Stack.Package
import           Stack.Types

-- | Options record for `stack dot`
data DotOpts = DotOpts
    { dotIncludeExternal :: Bool
    -- ^ Include external dependencies
    , dotIncludeBase :: Bool
    -- ^ Include dependencies on base
    , dotDependencyDepth :: Maybe Int
    -- ^ Limit the depth of dependency resolution to (Just n) or continue until fixpoint
    , dotPrune :: Set String
    -- ^ Package names to prune from the graph
    }

-- | Visualize the project's dependencies as a graphviz graph
dot :: (HasEnvConfig env
       ,HasHttpManager env
       ,MonadBaseControl IO m
       ,MonadCatch m
       ,MonadLogger m
       ,MonadIO m
       ,MonadReader env m
       )
    => DotOpts
    -> m ()
dot dotOpts = do
    (locals,_,_) <- loadLocals defaultBuildOpts Map.empty
    (_,_,_,sourceMap) <- loadSourceMap defaultBuildOpts
    let graph = Map.fromList (localDependencies dotOpts locals)
    menv <- getMinimalEnvOverride
    resultGraph <- withLoadPackage menv (\loader -> do
      let depLoader = createDepLoader sourceMap (fmap3 packageAllDeps loader)
      liftIO $ resolveDependencies (dotDependencyDepth dotOpts) graph depLoader)
    let pkgsToPrune = if dotIncludeBase dotOpts
                         then dotPrune dotOpts
                         else Set.insert "base" (dotPrune dotOpts)
        localNames = Set.fromList (map (packageName . lpPackage) locals)
        prunedGraph = pruneGraph localNames pkgsToPrune resultGraph
    printGraph dotOpts locals prunedGraph
  where -- fmap a function over the result of a function with 3 arguments
        fmap3 :: Functor f => (d -> e) -> (a -> b -> c -> f d) -> a -> b -> c -> f e
        fmap3 f g a b c = f <$> g a b c

-- | `pruneGraph dontPrune toPrune graph` prunes all packages in
-- `graph` with a name in `toPrune` and removes resulting orphans
-- unless they are in `dontPrune`
pruneGraph :: (F.Foldable f, F.Foldable g)
           => f PackageName
           -> g String
           -> Map PackageName (Set PackageName)
           -> Map PackageName (Set PackageName)
pruneGraph dontPrune names =
  pruneUnreachable dontPrune . Map.mapMaybeWithKey (\pkg pkgDeps ->
    if show pkg `F.elem` names
      then Nothing
      else let filtered = Set.filter (\n -> show n `F.notElem` names) pkgDeps
           in if Set.null filtered && not (Set.null pkgDeps)
                then Nothing
                else Just filtered)

-- | Make sure that all unreachable nodes (orphans) are pruned
pruneUnreachable :: F.Foldable f
                 => f PackageName
                 -> Map PackageName (Set PackageName)
                 -> Map PackageName (Set PackageName)
pruneUnreachable dontPrune = fixpoint prune
  where fixpoint :: Eq a => (a -> a) -> a -> a
        fixpoint f v = if f v == v then v else fixpoint f (f v)
        prune graph' = Map.filterWithKey (\k _ -> reachable k) graph'
          where reachable k = k `F.elem` dontPrune || k `Set.member` reachables
                reachables = F.fold graph'


-- | Resolve the dependency graph up to (Just depth) or until fixpoint is reached
resolveDependencies :: (Applicative m, Monad m)
                    => Maybe Int
                    -> Map PackageName (Set PackageName)
                    -> (PackageName -> m (Set PackageName))
                    -> m (Map PackageName (Set PackageName))
resolveDependencies (Just 0) graph _ = return graph
resolveDependencies limit graph loadPackageDeps = do
  let values = Set.unions (Map.elems graph)
      keys = Map.keysSet graph
      next = Set.difference values keys
  if Set.null next
     then return graph
     else do
       x <- T.traverse (\name -> (name,) <$> loadPackageDeps name) (F.toList next)
       resolveDependencies (subtract 1 <$> limit)
                      (Map.unionWith Set.union graph (Map.fromList x))
                      loadPackageDeps

-- | Given a SourceMap and a dependency loader, load the set of dependencies for a package
createDepLoader :: Applicative m
                => Map PackageName PackageSource
                -> (PackageName -> Version -> Map FlagName Bool -> m (Set PackageName))
                -> PackageName
                -> m (Set PackageName)
createDepLoader sourceMap loadPackageDeps pkgName =
  case Map.lookup pkgName sourceMap of
    Just (PSLocal lp) -> pure (packageAllDeps (lpPackage lp))
    Just (PSUpstream version _ flags) -> loadPackageDeps pkgName version flags
    Nothing -> pure Set.empty

-- | Resolve the direct (depth 0) external dependencies of the given local packages
localDependencies :: DotOpts -> [LocalPackage] -> [(PackageName,Set PackageName)]
localDependencies dotOpts locals = map (\lp -> (packageName (lpPackage lp), deps lp)) locals
  where deps lp = if dotIncludeExternal dotOpts
                then Set.delete (lpName lp) (packageAllDeps (lpPackage lp))
                else Set.intersection localNames (packageAllDeps (lpPackage lp))
        lpName lp = packageName (lpPackage lp)
        localNames = Set.fromList $ map (packageName . lpPackage) locals

-- | Print a graphviz graph of the edges in the Map and highlight the given local packages
printGraph :: (Applicative m, MonadIO m)
           => DotOpts
           -> [LocalPackage]
           -> Map PackageName (Set PackageName)
           -> m ()
printGraph dotOpts locals graph = do
  liftIO $ Text.putStrLn "strict digraph deps {"
  printLocalNodes dotOpts filteredLocals
  printLeaves graph
  void (Map.traverseWithKey printEdges graph)
  liftIO $ Text.putStrLn "}"
  where filteredLocals = filter (\local ->
          show (packageName (lpPackage local)) `Set.notMember` dotPrune dotOpts) locals

-- | Print the local nodes with a different style depending on options
printLocalNodes :: (F.Foldable t, MonadIO m)
                => DotOpts
                -> t LocalPackage
                -> m ()
printLocalNodes dotOpts locals = liftIO $ Text.putStrLn (Text.intercalate "\n" lpNodes)
  where applyStyle :: Text -> Text
        applyStyle n = if dotIncludeExternal dotOpts
                         then n <> " [style=dashed];"
                         else n <> " [style=solid];"
        lpNodes :: [Text]
        lpNodes = map (applyStyle . nodeName . packageName . lpPackage) (F.toList locals)

-- | Print nodes without dependencies
printLeaves :: (Applicative m, MonadIO m) => Map PackageName (Set PackageName) -> m ()
printLeaves = F.traverse_ printLeaf . Map.keysSet . Map.filter Set.null

-- | `printDedges p ps` prints an edge from p to every ps
printEdges :: (Applicative m, MonadIO m) => PackageName -> Set PackageName -> m ()
printEdges package deps = F.for_ deps (printEdge package)

-- | Print an edge between the two package names
printEdge :: MonadIO m => PackageName -> PackageName -> m ()
printEdge from to = liftIO $ Text.putStrLn (Text.concat [ nodeName from, " -> ", nodeName to, ";"])

-- | Convert a package name to a graph node name.
nodeName :: PackageName -> Text
nodeName name = "\"" <> Text.pack (packageNameString name) <> "\""

-- | Print a node with no dependencies
printLeaf :: MonadIO m => PackageName -> m ()
printLeaf package = liftIO . Text.putStrLn . Text.concat $
  if isWiredIn package
    then ["{rank=max; ", nodeName package, " [shape=box]; };"]
    else ["{rank=max; ", nodeName package, "; };"]

-- | Check if the package is wired in (shipped with) ghc
isWiredIn :: PackageName -> Bool
isWiredIn = (`HashSet.member` wiredInPackages)
