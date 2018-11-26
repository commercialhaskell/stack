{-# LANGUAGE NoImplicitPrelude #-}
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

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Traversable as T
import           Distribution.Text (display)
import qualified Distribution.SPDX.License as SPDX
import           Distribution.License (License(BSD3), licenseFromSPDX)
import           Distribution.Types.PackageName (mkPackageName)
import           Stack.Build (loadPackage)
import           Stack.Build.Installed (getInstalled, GetInstalledOpts(..))
import           Stack.Build.Source
import           Stack.Build.Target
import           Stack.Constants
import           Stack.Package
import           Stack.PackageDump (DumpPackage(..))
import           Stack.Prelude hiding (Display (..), pkgName, loadPackage)
import qualified Stack.Prelude (pkgName)
import           Stack.Types.Build
import           Stack.Types.Config
import           Stack.Types.GhcPkgId
import           Stack.Types.Package

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
    , dotFlags :: !(Map ApplyCLIFlag (Map FlagName Bool))
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
    , listDepsTree :: !Bool
    -- ^ Print dependency tree.
    }

-- | Visualize the project's dependencies as a graphviz graph
dot :: HasEnvConfig env => DotOpts -> RIO env ()
dot dotOpts = do
  (localNames, prunedGraph) <- createPrunedDependencyGraph dotOpts
  printGraph dotOpts localNames prunedGraph

-- | Information about a package in the dependency graph, when available.
data DotPayload = DotPayload
  { payloadVersion :: Maybe Version
  -- ^ The package version.
  , payloadLicense :: Maybe (Either SPDX.License License)
  -- ^ The license the package was released under.
  } deriving (Eq, Show)

-- | Create the dependency graph and also prune it as specified in the dot
-- options. Returns a set of local names and and a map from package names to
-- dependencies.
createPrunedDependencyGraph :: HasEnvConfig env
                            => DotOpts
                            -> RIO env
                                 (Set PackageName,
                                  Map PackageName (Set PackageName, DotPayload))
createPrunedDependencyGraph dotOpts = do
  localNames <- view $ buildConfigL.to (Map.keysSet . bcPackages)
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
createDependencyGraph :: HasEnvConfig env
                       => DotOpts
                       -> RIO env (Map PackageName (Set PackageName, DotPayload))
createDependencyGraph dotOpts = do
  (locals, sourceMap) <- loadSourceMap NeedTargets defaultBuildOptsCLI
      { boptsCLITargets = dotTargets dotOpts
      , boptsCLIFlags = dotFlags dotOpts
      }
  let graph = Map.fromList (localDependencies dotOpts (filter lpWanted locals))
  (installedMap, globalDump, _, _) <- getInstalled (GetInstalledOpts False False False)
                                                   sourceMap
  -- TODO: Can there be multiple entries for wired-in-packages? If so,
  -- this will choose one arbitrarily..
  let globalDumpMap = Map.fromList $ map (\dp -> (Stack.Prelude.pkgName (dpPackageIdent dp), dp)) globalDump
      globalIdMap = Map.fromList $ map (\dp -> (dpGhcPkgId dp, dpPackageIdent dp)) globalDump
  let depLoader = createDepLoader sourceMap installedMap globalDumpMap globalIdMap loadPackageDeps
      loadPackageDeps name version loc flags ghcOptions
          -- Skip packages that can't be loaded - see
          -- https://github.com/commercialhaskell/stack/issues/2967
          | name `elem` [mkPackageName "rts", mkPackageName "ghc"] =
              return (Set.empty, DotPayload (Just version) (Just $ Right BSD3))
          | otherwise = fmap (packageAllDeps &&& makePayload) (loadPackage loc flags ghcOptions)
  resolveDependencies (dotDependencyDepth dotOpts) graph depLoader
  where makePayload pkg = DotPayload (Just $ packageVersion pkg) (Just $ packageLicense pkg)

listDependencies :: HasEnvConfig env
                  => ListDepsOpts
                  -> RIO env ()
listDependencies opts = do
  let dotOpts = listDepsDotOpts opts
  (pkgs, resultGraph) <- createPrunedDependencyGraph dotOpts
  if listDepsTree opts then
    do
      liftIO $ Text.putStrLn "Packages"
      liftIO $ printTree opts 0 [] pkgs resultGraph
    else
      void (Map.traverseWithKey go (snd <$> resultGraph))
      where go name payload = liftIO $ Text.putStrLn $ listDepsLine opts name payload

printTree :: ListDepsOpts
          -> Int
          -> [Int]
          -> Set PackageName
          -> Map PackageName (Set PackageName, DotPayload)
          -> IO ()
printTree opts depth remainingDepsCounts packages dependencyMap =
  F.sequence_ $ Seq.mapWithIndex go (toSeq packages)
  where
    toSeq = Seq.fromList . Set.toList
    go index name = let newDepsCounts = remainingDepsCounts ++ [Set.size packages - index - 1]
                        (deps, payload) = (Map.!) dependencyMap name
                     in do
                          printTreeNode opts depth newDepsCounts deps payload name
                          if Just depth == dotDependencyDepth (listDepsDotOpts opts)
                             then return ()
                             else printTree opts (depth + 1) newDepsCounts deps dependencyMap

printTreeNode :: ListDepsOpts
              -> Int
              -> [Int]
              -> Set PackageName
              -> DotPayload
              -> PackageName
              -> IO ()
printTreeNode opts depth remainingDepsCounts deps payload name =
  let remainingDepth = fromMaybe 999 (dotDependencyDepth (listDepsDotOpts opts)) - depth
      hasDeps = not $ null deps
   in Text.putStrLn $ treeNodePrefix "" remainingDepsCounts hasDeps  remainingDepth  <> " " <> listDepsLine opts name payload

treeNodePrefix :: Text -> [Int] -> Bool -> Int -> Text
treeNodePrefix t [] _ _      = t
treeNodePrefix t [0] True  0 = t <> "└──"
treeNodePrefix t [_] True  0 = t <> "├──"
treeNodePrefix t [0] True  _ = t <> "└─┬"
treeNodePrefix t [_] True  _ = t <> "├─┬"
treeNodePrefix t [0] False _ = t <> "└──"
treeNodePrefix t [_] False _ = t <> "├──"
treeNodePrefix t (0:ns) d remainingDepth = treeNodePrefix (t <> "  ") ns d remainingDepth
treeNodePrefix t (_:ns) d remainingDepth = treeNodePrefix (t <> "│ ") ns d remainingDepth

listDepsLine :: ListDepsOpts -> PackageName -> DotPayload -> Text
listDepsLine opts name payload = Text.pack (packageNameString name) <> listDepsSep opts <> payloadText opts payload

payloadText :: ListDepsOpts -> DotPayload -> Text
payloadText opts payload =
  if listDepsLicense opts
    then maybe "<unknown>" (Text.pack . display . either licenseFromSPDX id) (payloadLicense payload)
    else maybe "<unknown>" (Text.pack . display) (payloadVersion payload)

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
                -> Map PackageName (InstallLocation, Installed)
                -> Map PackageName (DumpPackage () () ())
                -> Map GhcPkgId PackageIdentifier
                -> (PackageName -> Version -> PackageLocationImmutable ->
                    Map FlagName Bool -> [Text] -> m (Set PackageName, DotPayload))
                -> PackageName
                -> m (Set PackageName, DotPayload)
createDepLoader sourceMap installed globalDumpMap globalIdMap loadPackageDeps pkgName =
  if not (pkgName `Set.member` wiredInPackages)
      then case Map.lookup pkgName sourceMap of
          Just (PSFilePath lp _) -> pure (packageAllDeps pkg, payloadFromLocal pkg)
            where
              pkg = localPackageToPackage lp
          Just (PSRemote _ flags ghcOptions loc ident) ->
              -- FIXME pretty certain this could be cleaned up a lot by including more info in PackageSource
              let PackageIdentifier name version = ident
               in assert (pkgName == name) (loadPackageDeps pkgName version loc flags ghcOptions)
          Nothing -> pure (Set.empty, payloadFromInstalled (Map.lookup pkgName installed))
      -- For wired-in-packages, use information from ghc-pkg (see #3084)
      else case Map.lookup pkgName globalDumpMap of
          Nothing -> error ("Invariant violated: Expected to find wired-in-package " ++ packageNameString pkgName ++ " in global DB")
          Just dp -> pure (Set.fromList deps, payloadFromDump dp)
            where
              deps = map (\depId -> maybe (error ("Invariant violated: Expected to find " ++ ghcPkgIdString depId ++ " in global DB"))
                                          Stack.Prelude.pkgName
                                          (Map.lookup depId globalIdMap))
                         (dpDepends dp)
  where
    payloadFromLocal pkg = DotPayload (Just $ packageVersion pkg) (Just $ packageLicense pkg)
    payloadFromInstalled maybePkg = DotPayload (fmap (installedVersion . snd) maybePkg) $
        case maybePkg of
            Just (_, Library _ _ mlicense) -> mlicense
            _ -> Nothing
    payloadFromDump dp = DotPayload (Just $ pkgVersion $ dpPackageIdent dp) (Right <$> dpLicense dp)

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
  where filteredLocals = Set.filter (\local' ->
          packageNameString local' `Set.notMember` dotPrune dotOpts) locals

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
nodeName name = "\"" <> Text.pack (packageNameString name) <> "\""

-- | Print a node with no dependencies
printLeaf :: MonadIO m => PackageName -> m ()
printLeaf package = liftIO . Text.putStrLn . Text.concat $
  if isWiredIn package
    then ["{rank=max; ", nodeName package, " [shape=box]; };"]
    else ["{rank=max; ", nodeName package, "; };"]

-- | Check if the package is wired in (shipped with) ghc
isWiredIn :: PackageName -> Bool
isWiredIn = (`Set.member` wiredInPackages)

localPackageToPackage :: LocalPackage -> Package
localPackageToPackage lp =
  fromMaybe (lpPackage lp) (lpTestBench lp)
