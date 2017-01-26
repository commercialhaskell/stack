{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Dot (dot
                 ,listDependencies
                 ,DotOpts(..)
                 ,DotPayload(..)
                 ,ListDepsOpts(..)
                 ,resolveDependencies
                 ,printGraph
                 ,pruneGraph
                 ) where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Monad (liftM, void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Unlift (MonadBaseUnlift)
import qualified Data.Foldable as F
import qualified Data.HashSet as HashSet
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Traversable as T
import           Distribution.License (License)
import           Prelude -- Fix redundant import warnings
import           Stack.Build (withLoadPackage)
import           Stack.Build.Installed (getInstalled, GetInstalledOpts(..))
import           Stack.Build.Source
import           Stack.Build.Target
import           Stack.Constants
import           Stack.Package
import           Stack.Types.Build
import           Stack.Types.Config
import           Stack.Types.FlagName
import           Stack.Types.Package
import           Stack.Types.PackageName
import           Stack.Types.StackT
import           Stack.Types.Version

-- | Options record for @stack dot@
data DotOpts = DotOpts
    { dotIncludeExternal :: !Bool
    -- ^ Include external dependencies
    , dotIncludeBase :: !Bool
    -- ^ Include dependencies on base
    , dotDependencyDepth :: !(Maybe Int)
    -- ^ Limit the depth of dependency resolution to (Just n) or continue until fixpoint
    , dotPrune :: !(Set String)
    -- ^ Package names to prune from the graph
    , dotTargets :: [Text]
    -- ^ stack TARGETs to trace dependencies for
    , dotFlags :: !(Map (Maybe PackageName) (Map FlagName Bool))
    -- ^ Flags to apply when calculating dependencies
    , dotTestTargets :: Bool
    -- ^ Like the "--test" flag for build, affects the meaning of 'dotTargets'.
    , dotBenchTargets :: Bool
    -- ^ Like the "--bench" flag for build, affects the meaning of 'dotTargets'.
    }

data ListDepsOpts = ListDepsOpts
    { listDepsDotOpts :: !DotOpts
    -- ^ The normal dot options.
    , listDepsSep :: !Text
    -- ^ Separator between the package name and details.
    , listDepsLicense :: !Bool
    -- ^ Print dependency licenses instead of versions.
    }

-- | Visualize the project's dependencies as a graphviz graph
dot :: (StackM env m, HasEnvConfig env, MonadBaseUnlift IO m)
    => DotOpts
    -> m ()
dot dotOpts = do
  (localNames, prunedGraph) <- createPrunedDependencyGraph dotOpts
  printGraph dotOpts localNames prunedGraph

-- | Information about a package in the dependency graph, when available.
data DotPayload = DotPayload
  { payloadVersion :: Maybe Version
  -- ^ The package version.
  , payloadLicense :: Maybe License
  -- ^ The license the package was released under.
  } deriving (Eq, Show)

-- | Create the dependency graph and also prune it as specified in the dot
-- options. Returns a set of local names and and a map from package names to
-- dependencies.
createPrunedDependencyGraph :: (StackM env m, HasEnvConfig env, MonadBaseUnlift IO m)
                            => DotOpts
                            -> m (Set PackageName,
                                  Map PackageName (Set PackageName, DotPayload))
createPrunedDependencyGraph dotOpts = do
  localNames <- liftM Map.keysSet getLocalPackageViews
  resultGraph <- createDependencyGraph dotOpts
  let pkgsToPrune = if dotIncludeBase dotOpts
                       then dotPrune dotOpts
                       else Set.insert "base" (dotPrune dotOpts)
      prunedGraph = pruneGraph localNames pkgsToPrune resultGraph
  return (localNames, prunedGraph)

-- | Create the dependency graph, the result is a map from a package
-- name to a tuple of dependencies and payload if available. This
-- function mainly gathers the required arguments for
-- @resolveDependencies@.
createDependencyGraph :: (StackM env m, HasEnvConfig env, MonadBaseUnlift IO m)
                       => DotOpts
                       -> m (Map PackageName (Set PackageName, DotPayload))
createDependencyGraph dotOpts = do
  (locals,sourceMap) <- loadSourceMap NeedTargets defaultBuildOptsCLI
      { boptsCLITargets = dotTargets dotOpts
      , boptsCLIFlags = dotFlags dotOpts
      }
  let graph = Map.fromList (localDependencies dotOpts (filter lpWanted locals))
  menv <- getMinimalEnvOverride
  installedMap <- fmap snd . fst4 <$> getInstalled menv
                                                   (GetInstalledOpts False False False)
                                                   sourceMap
  withLoadPackage menv (\loader -> do
    let depLoader =
          createDepLoader sourceMap
                          installedMap
                          (fmap4 (packageAllDeps &&& makePayload) loader)
    liftIO $ resolveDependencies (dotDependencyDepth dotOpts) graph depLoader)
  where -- fmap a function over the result of a function with 3 arguments
        fmap4 :: Functor f => (r -> r') -> (a -> b -> c -> d -> f r) -> a -> b -> c -> d -> f r'
        fmap4 f g a b c d = f <$> g a b c d

        fst4 :: (a,b,c,d) -> a
        fst4 (x,_,_,_) = x

        makePayload pkg = DotPayload (Just $ packageVersion pkg) (Just $ packageLicense pkg)

listDependencies :: (StackM env m, HasEnvConfig env, MonadBaseUnlift IO m)
                  => ListDepsOpts
                  -> m ()
listDependencies opts = do
  let dotOpts = listDepsDotOpts opts
  (_, resultGraph) <- createPrunedDependencyGraph dotOpts
  void (Map.traverseWithKey go (snd <$> resultGraph))
    where go name payload =
            let payloadText =
                    if listDepsLicense opts
                      then maybe "<unknown>" (Text.pack . show) (payloadLicense payload)
                      else maybe "<unknown>" (Text.pack . show) (payloadVersion payload)
                line = packageNameText name <> listDepsSep opts <> payloadText
            in  liftIO $ Text.putStrLn line

-- | @pruneGraph dontPrune toPrune graph@ prunes all packages in
-- @graph@ with a name in @toPrune@ and removes resulting orphans
-- unless they are in @dontPrune@
pruneGraph :: (F.Foldable f, F.Foldable g, Eq a)
           => f PackageName
           -> g String
           -> Map PackageName (Set PackageName, a)
           -> Map PackageName (Set PackageName, a)
pruneGraph dontPrune names =
  pruneUnreachable dontPrune . Map.mapMaybeWithKey (\pkg (pkgDeps,x) ->
    if show pkg `F.elem` names
      then Nothing
      else let filtered = Set.filter (\n -> show n `F.notElem` names) pkgDeps
           in if Set.null filtered && not (Set.null pkgDeps)
                then Nothing
                else Just (filtered,x))

-- | Make sure that all unreachable nodes (orphans) are pruned
pruneUnreachable :: (Eq a, F.Foldable f)
                 => f PackageName
                 -> Map PackageName (Set PackageName, a)
                 -> Map PackageName (Set PackageName, a)
pruneUnreachable dontPrune = fixpoint prune
  where fixpoint :: Eq a => (a -> a) -> a -> a
        fixpoint f v = if f v == v then v else fixpoint f (f v)
        prune graph' = Map.filterWithKey (\k _ -> reachable k) graph'
          where reachable k = k `F.elem` dontPrune || k `Set.member` reachables
                reachables = F.fold (fst <$> graph')


-- | Resolve the dependency graph up to (Just depth) or until fixpoint is reached
resolveDependencies :: (Applicative m, Monad m)
                    => Maybe Int
                    -> Map PackageName (Set PackageName, DotPayload)
                    -> (PackageName -> m (Set PackageName, DotPayload))
                    -> m (Map PackageName (Set PackageName, DotPayload))
resolveDependencies (Just 0) graph _ = return graph
resolveDependencies limit graph loadPackageDeps = do
  let values = Set.unions (fst <$> Map.elems graph)
      keys = Map.keysSet graph
      next = Set.difference values keys
  if Set.null next
     then return graph
     else do
       x <- T.traverse (\name -> (name,) <$> loadPackageDeps name) (F.toList next)
       resolveDependencies (subtract 1 <$> limit)
                      (Map.unionWith unifier graph (Map.fromList x))
                      loadPackageDeps
  where unifier (pkgs1,v1) (pkgs2,_) = (Set.union pkgs1 pkgs2, v1)

-- | Given a SourceMap and a dependency loader, load the set of dependencies for a package
createDepLoader :: Applicative m
                => Map PackageName PackageSource
                -> Map PackageName Installed
                -> (PackageName -> Version -> Map FlagName Bool -> [Text] -> m (Set PackageName, DotPayload))
                -> PackageName
                -> m (Set PackageName, DotPayload)
createDepLoader sourceMap installed loadPackageDeps pkgName =
  case Map.lookup pkgName sourceMap of
    Just (PSLocal lp) -> pure (packageAllDeps pkg, payloadFromLocal pkg)
      where
        pkg = localPackageToPackage lp
    Just (PSUpstream version _ flags ghcOptions _) -> loadPackageDeps pkgName version flags ghcOptions
    Nothing -> pure (Set.empty, payloadFromInstalled (Map.lookup pkgName installed))
  where
    payloadFromLocal pkg = DotPayload (Just $ packageVersion pkg) (Just $ packageLicense pkg)
    payloadFromInstalled maybePkg = DotPayload (fmap installedVersion maybePkg) Nothing

-- | Resolve the direct (depth 0) external dependencies of the given local packages
localDependencies :: DotOpts -> [LocalPackage] -> [(PackageName, (Set PackageName, DotPayload))]
localDependencies dotOpts locals =
    map (\lp -> let pkg = localPackageToPackage lp
                 in (packageName pkg, (deps pkg, lpPayload pkg)))
        locals
  where deps pkg =
          if dotIncludeExternal dotOpts
            then Set.delete (packageName pkg) (packageAllDeps pkg)
            else Set.intersection localNames (packageAllDeps pkg)
        localNames = Set.fromList $ map (packageName . lpPackage) locals
        lpPayload pkg = DotPayload (Just $ packageVersion pkg) (Just $ packageLicense pkg)

-- | Print a graphviz graph of the edges in the Map and highlight the given local packages
printGraph :: (Applicative m, MonadIO m)
           => DotOpts
           -> Set PackageName -- ^ all locals
           -> Map PackageName (Set PackageName, DotPayload)
           -> m ()
printGraph dotOpts locals graph = do
  liftIO $ Text.putStrLn "strict digraph deps {"
  printLocalNodes dotOpts filteredLocals
  printLeaves graph
  void (Map.traverseWithKey printEdges (fst <$> graph))
  liftIO $ Text.putStrLn "}"
  where filteredLocals = Set.filter (\local ->
          packageNameString local `Set.notMember` dotPrune dotOpts) locals

-- | Print the local nodes with a different style depending on options
printLocalNodes :: (F.Foldable t, MonadIO m)
                => DotOpts
                -> t PackageName
                -> m ()
printLocalNodes dotOpts locals = liftIO $ Text.putStrLn (Text.intercalate "\n" lpNodes)
  where applyStyle :: Text -> Text
        applyStyle n = if dotIncludeExternal dotOpts
                         then n <> " [style=dashed];"
                         else n <> " [style=solid];"
        lpNodes :: [Text]
        lpNodes = map (applyStyle . nodeName) (F.toList locals)

-- | Print nodes without dependencies
printLeaves :: MonadIO m
            => Map PackageName (Set PackageName, DotPayload)
            -> m ()
printLeaves = F.mapM_ printLeaf . Map.keysSet . Map.filter Set.null . fmap fst

-- | @printDedges p ps@ prints an edge from p to every ps
printEdges :: MonadIO m => PackageName -> Set PackageName -> m ()
printEdges package deps = F.forM_ deps (printEdge package)

-- | Print an edge between the two package names
printEdge :: MonadIO m => PackageName -> PackageName -> m ()
printEdge from to' = liftIO $ Text.putStrLn (Text.concat [ nodeName from, " -> ", nodeName to', ";"])

-- | Convert a package name to a graph node name.
nodeName :: PackageName -> Text
nodeName name = "\"" <> packageNameText name <> "\""

-- | Print a node with no dependencies
printLeaf :: MonadIO m => PackageName -> m ()
printLeaf package = liftIO . Text.putStrLn . Text.concat $
  if isWiredIn package
    then ["{rank=max; ", nodeName package, " [shape=box]; };"]
    else ["{rank=max; ", nodeName package, "; };"]

-- | Check if the package is wired in (shipped with) ghc
isWiredIn :: PackageName -> Bool
isWiredIn = (`HashSet.member` wiredInPackages)

localPackageToPackage :: LocalPackage -> Package
localPackageToPackage lp =
  fromMaybe (lpPackage lp) (lpTestBench lp)
