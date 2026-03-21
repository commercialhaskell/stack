{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Stack.Build.Backpack
Description : Backpack (cross-package) instantiation support.
License     : BSD-3-Clause

Create CInst instantiation tasks for Backpack packages. Given the build plan's
dependency resolution results, this module scans consumer packages for mixin
references to indefinite (signature-only) packages and creates additional build
tasks that instantiate those packages with concrete implementations.
-}

module Stack.Build.Backpack
  ( addInstantiationTasks
  , upgradeFoundIndefinites
  ) where

import           Crypto.Hash ( hashWith, SHA256 (..) )
import qualified Data.ByteArray.Encoding as Mem
                   ( Base (Base16), convertToBase )
import qualified Data.ByteString.Char8 as S8
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Distribution.ModuleName ( ModuleName )
import qualified Distribution.Text as CabalText
import           Distribution.Types.IncludeRenaming ( IncludeRenaming (..) )
import           Distribution.Types.Mixin
                   ( Mixin (..), mixinIncludeRenaming, mixinPackageName )
import           Distribution.Types.ModuleRenaming
                   ( ModuleRenaming (..) )
import           Path ( parent )
import           Stack.ConfigureOpts ( packageConfigureOptsFromPackage )
import           Stack.Package
                   ( packageIsIndefinite )
import           Stack.Prelude
import           Stack.Types.Build.ConstructPlan
                   ( AddDepRes (..), CombinedMap, PackageInfo (..) )
import           Stack.Types.SourceMap ( CommonPackage (..) )
import           Distribution.Types.BuildType ( BuildType (Configure) )
import           Stack.Types.Cache ( CachePkgSrc (..) )
import           Stack.Types.Component
                   ( StackBuildInfo (..), StackLibrary (..) )
import           Stack.Types.ConfigureOpts ( BaseConfigOpts (..) )
import           Stack.Types.EnvConfig ( EnvConfig (..), HasEnvConfig (..) )
import           Stack.Types.Installed
                   ( InstallLocation (..), Installed (..)
                   , InstalledLibraryInfo (..), installedVersion
                   )
import           Stack.Types.IsMutable ( IsMutable (..) )
import           Stack.Types.NamedComponent ( NamedComponent (..) )
import           Stack.Types.Package
                   ( LocalPackage (..), Package (..), PackageSource (..)
                   , installedMapGhcPkgId, packageIdentifier
                   )
import           Stack.Types.Plan
                   ( ComponentKey (..), Task (..), TaskConfigOpts (..)
                   , TaskType (..), installLocationIsMutable
                   )

-- | Upgrade ADRFound entries to ADRToInstall for indefinite (Backpack
-- signature) packages whose source is available. This is needed because
-- addInstantiationTasks clones the sig-pkg's Task to create CInst tasks,
-- and ADRFound entries don't carry a Task.
--
-- Only packages actually referenced by a consumer's mixin are loaded,
-- to avoid unnecessary Pantry lookups for the common case (no Backpack).
upgradeFoundIndefinites ::
     forall env. HasEnvConfig env
  => (  PackageLocationImmutable
     -> Map FlagName Bool
     -> [Text]
     -> [Text]
     -> RIO EnvConfig Package
     )
     -- ^ Load package from source (loadPackage0)
  -> EnvConfig
  -> CombinedMap
  -> BaseConfigOpts
  -> [(PackageName, AddDepRes)]
  -> RIO env [(PackageName, AddDepRes)]
upgradeFoundIndefinites loadPkg econfig combinedMap bco adrs = do
  let adrMap = Map.fromList adrs
      -- Collect package names referenced by any consumer's mixin.
      mixinTargets :: Set PackageName
      mixinTargets = Set.fromList
        [ mixinPackageName mixin
        | (_, ADRToInstall task) <- adrs
        , let pkg = case task.taskType of
                TTLocalMutable lp -> lp.package
                TTRemotePackage _ p _ -> p
        , mixin <- concatMap (\lib -> lib.buildInfo.mixins)
                     (maybeToList pkg.library ++ toList pkg.subLibraries)
        ]
      -- Filter to mixin targets that are ADRFound.
      foundTargets =
        [ name
        | name <- Set.toList mixinTargets
        , Just (ADRFound _ _) <- [Map.lookup name adrMap]
        ]
  if null foundTargets
    then pure adrs
    else do
      -- For each ADRFound mixin target, try to load its Package and
      -- upgrade to ADRToInstall if it is indefinite.
      upgrades <- fmap Map.fromList $ forM foundTargets $ \name ->
        case Map.lookup name combinedMap of
          Just (PIOnlyInstalled _ installed) ->
            -- Source not available (GHC boot library or similar). Look up
            -- Hackage as a fallback.
            upgradeFromHackage name installed
          Just (PIBoth ps installed) ->
            upgradeFromSource name ps (Just installed)
          Just (PIOnlySource ps) ->
            -- Should not happen (ADRFound implies installed), but handle
            -- gracefully by loading the package to check.
            upgradeFromSource name ps Nothing
          Nothing ->
            -- Package not in combined map — shouldn't happen. Skip.
            pure (name, Nothing)
      -- Apply upgrades.
      pure
        [ case Map.lookup name upgrades of
            Just (Just adr') -> (name, adr')
            _ -> (name, adr)
        | (name, adr) <- adrs
        ]
 where
  loadFromSource :: PackageSource -> RIO env Package
  loadFromSource = \case
    PSRemote pkgLoc _version _fromSnapshot cp ->
      runRIO econfig $ loadPkg pkgLoc cp.flags cp.ghcOptions cp.cabalConfigOpts
    PSFilePath lp -> pure lp.package

  mkTask :: Package -> PackageSource -> Maybe Installed -> Task
  mkTask package ps mInstalled =
    let loc = psLocation ps
        isMutable = installLocationIsMutable loc
        presentMap = case mInstalled of
          Just (Library pid ili) -> installedMapGhcPkgId pid ili
          _ -> Map.empty
    in  Task
          { configOpts = TaskConfigOpts
              { missing = Set.empty
              , envConfig = econfig
              , baseConfigOpts = bco
              , isLocalNonExtraDep = psLocal ps
              , isMutable
              , pkgConfigOpts = packageConfigureOptsFromPackage package
              , instantiationDeps = []
              }
          , buildHaddocks = False
          , present = presentMap
          , taskType = case ps of
              PSFilePath lp -> TTLocalMutable lp
              PSRemote pkgLoc _version _fromSnapshot _cp ->
                TTRemotePackage isMutable package pkgLoc
          , cachePkgSrc = toCachePkgSrc ps
          , buildTypeConfig = packageBuildTypeConfig package
          , backpackInstEntries = []
          }

  upgradeFromSource ::
       PackageName
    -> PackageSource
    -> Maybe Installed
    -> RIO env (PackageName, Maybe AddDepRes)
  upgradeFromSource name ps mInstalled = do
    package <- loadFromSource ps
    if packageIsIndefinite package
      then pure (name, Just $ ADRToInstall $ mkTask package ps mInstalled)
      else pure (name, Nothing)

  upgradeFromHackage ::
       PackageName
    -> Installed
    -> RIO env (PackageName, Maybe AddDepRes)
  upgradeFromHackage name installed = do
    let version = installedVersion installed
    mPkgLoc <- runRIO econfig $
      getLatestHackageRevision YesRequireHackageIndex name version >>= \case
        Nothing -> pure Nothing
        Just (_rev, cfKey, treeKey) ->
          pure $ Just $ PLIHackage (PackageIdentifier name version) cfKey treeKey
    case mPkgLoc of
      Nothing -> pure (name, Nothing)
      Just pkgLoc -> do
        package <- runRIO econfig $ loadPkg pkgLoc Map.empty [] []
        if packageIsIndefinite package
          then do
            let presentMap = case installed of
                  Library pid ili -> installedMapGhcPkgId pid ili
                  _ -> Map.empty
                task = Task
                  { configOpts = TaskConfigOpts
                      { missing = Set.empty
                      , envConfig = econfig
                      , baseConfigOpts = bco
                      , isLocalNonExtraDep = False
                      , isMutable = Immutable
                      , pkgConfigOpts = packageConfigureOptsFromPackage package
                      , instantiationDeps = []
                      }
                  , buildHaddocks = False
                  , present = presentMap
                  , taskType = TTRemotePackage Immutable package pkgLoc
                  , cachePkgSrc = CacheSrcUpstream
                  , buildTypeConfig = packageBuildTypeConfig package
                  , backpackInstEntries = []
                  }
            pure (name, Just $ ADRToInstall task)
          else pure (name, Nothing)

-- | Post-pass: scan consumer tasks for Backpack mixins referencing indefinite
-- packages and create CInst instantiation tasks. Returns the augmented list
-- plus any warnings about unsupported Backpack patterns.
addInstantiationTasks ::
     Map PackageName (Set ModuleName)
     -- ^ Installed package modules (from ghc-pkg dump). Used for module
     -- resolution when the implementing package is ADRFound (already
     -- installed, no Task/Package metadata available).
  -> [(PackageName, AddDepRes)]     -- ^ Original per-package ADRs
  -> [(ComponentKey, AddDepRes)]    -- ^ Expanded component-keyed ADRs
  -> ([(ComponentKey, AddDepRes)], [StyleDoc])
     -- ^ (Augmented with CInst tasks, warnings)
addInstantiationTasks installedModules origAdrs expandedAdrs =
  let adrMap = Map.fromList origAdrs
      -- Process each entry, collecting new CInst tasks and modified consumers.
      (newInstTasks, modifiedEntries, warns) =
        foldr (processEntry adrMap) ([], [], []) expandedAdrs
      -- Deduplicate CInst tasks: multiple components of the same consumer
      -- may reference the same mixin, producing identical CInst entries.
      -- Map.fromList keeps the last, Map.toList restores the list.
      dedupedInstTasks = Map.toList $ Map.fromList newInstTasks
  in  (modifiedEntries ++ dedupedInstTasks, warns)
 where
  processEntry ::
       Map PackageName AddDepRes
    -> (ComponentKey, AddDepRes)
    -> ([(ComponentKey, AddDepRes)], [(ComponentKey, AddDepRes)], [StyleDoc])
    -> ([(ComponentKey, AddDepRes)], [(ComponentKey, AddDepRes)], [StyleDoc])
  processEntry adrMap (ck, ADRToInstall task) (instAcc, entryAcc, warnAcc) =
    let pkg = taskPackage task
        -- Get mixins from the main library and all sub-libraries.
        mainMixins = case pkg.library of
          Just lib -> lib.buildInfo.mixins
          Nothing  -> []
        subLibMixins = concatMap (\lib -> lib.buildInfo.mixins)
                                 (toList pkg.subLibraries)
        allMixins = mainMixins ++ subLibMixins
        -- Collect the consumer's build-depends to scope module resolution.
        -- When multiple packages expose the same module, only the consumer's
        -- direct dependencies are considered (matching Cabal's behavior).
        consumerDeps = Set.fromList $ concatMap
          (\lib -> Map.keys lib.buildInfo.dependency)
          (maybeToList pkg.library ++ toList pkg.subLibraries)
        -- Process each mixin that references an indefinite dep.
        (instTasks, instKeys, mixinWarns) =
          processAllMixins ck pkg allMixins adrMap consumerDeps
        -- Add CInst keys to the consumer's instantiationDeps.
        modifiedTask
          | null instKeys = task
          | otherwise = task
              { configOpts = task.configOpts
                  { instantiationDeps =
                      task.configOpts.instantiationDeps ++ instKeys
                  }
              }
    in  ( instTasks ++ instAcc
        , (ck, ADRToInstall modifiedTask) : entryAcc
        , mixinWarns ++ warnAcc
        )
  processEntry _ entry (instAcc, entryAcc, warnAcc) =
    (instAcc, entry : entryAcc, warnAcc)

  -- Extract the Package from a Task's TaskType.
  taskPackage :: Task -> Package
  taskPackage t = case t.taskType of
    TTLocalMutable lp       -> lp.package
    TTRemotePackage _ p _   -> p

  -- Process all mixins for a consumer, returning new CInst entries, their
  -- ComponentKeys (to add as deps on the consumer), and any warnings.
  processAllMixins ::
       ComponentKey           -- ^ Consumer's key
    -> Package                -- ^ Consumer's package
    -> [Mixin]                -- ^ Consumer's library mixins
    -> Map PackageName AddDepRes
    -> Set PackageName         -- ^ Consumer's build-depends
    -> ([(ComponentKey, AddDepRes)], [ComponentKey], [StyleDoc])
  processAllMixins _ck _pkg mixins adrMap' consumerDeps =
    foldr (processMixin adrMap' consumerDeps) ([], [], []) mixins

  processMixin ::
       Map PackageName AddDepRes
    -> Set PackageName         -- ^ Consumer's build-depends
    -> Mixin
    -> ([(ComponentKey, AddDepRes)], [ComponentKey], [StyleDoc])
    -> ([(ComponentKey, AddDepRes)], [ComponentKey], [StyleDoc])
  processMixin adrMap' consumerDeps mixin (instAcc, keyAcc, warnAcc) =
    let depPkgName = mixinPackageName mixin
    in  case Map.lookup depPkgName adrMap' of
      Just (ADRToInstall sigTask)
        | let sigPkg = taskPackage sigTask
        , packageIsIndefinite sigPkg ->
            let -- Get signatures from the sig-pkg's main library.
                ownSigs :: [ModuleName]
                ownSigs = case sigPkg.library of
                  Just lib -> lib.signatures
                  Nothing  -> []
                -- Collect inherited signatures from transitive indefinite
                -- deps.  When a sig-pkg depends on another indefinite package,
                -- its holes propagate upward.  The CInst must fill ALL holes
                -- (own + inherited) or Cabal reports "non-closing
                -- substitution".
                inheritedSigs :: [ModuleName]
                inheritedSigs =
                  -- Deduplicate: diamond deps can yield the same sig
                  -- from multiple paths.
                  L.nub $ collectInheritedSigs sigPkg adrMap' Set.empty
                -- Determine the module mapping from the mixin's requiresRn.
                renaming = includeRequiresRn (mixinIncludeRenaming mixin)
                -- Resolve the sig-pkg's own signatures using the consumer's
                -- mixin renaming.  Module lookup is scoped to the consumer's
                -- build-depends so that multiple instantiations with different
                -- implementations don't conflict.
                (ownEntries, ownWarns) =
                  resolveAllEntries depPkgName renaming adrMap'
                    consumerDeps ownSigs
                -- Resolve inherited signatures using DefaultRenaming (the
                -- consumer's mixin only renames the direct sig-pkg's
                -- signatures, not inherited ones).
                (inheritedEntries, inheritedWarns) =
                  resolveAllEntries depPkgName DefaultRenaming adrMap'
                    consumerDeps inheritedSigs
                entries = ownEntries ++ inheritedEntries
                resolveWarns = ownWarns ++ inheritedWarns
            in  if null entries
                  then (instAcc, keyAcc, resolveWarns ++ warnAcc)
                  else
                    let hashSuffix = instHashSuffix entries
                        instCk = ComponentKey depPkgName (CInst hashSuffix)
                        -- The CInst task's missing includes the sig-pkg's
                        -- original missing plus implementing packages that
                        -- are ADRToInstall.
                        implPidsMissing = Set.fromList
                          [ packageIdentifier (taskPackage implTask)
                          | (_, implPkgName', _) <- entries
                          , Just (ADRToInstall implTask) <-
                              [Map.lookup implPkgName' adrMap']
                          ]
                        -- For ADRFound implementing packages, add their
                        -- PID→GhcPkgId to the CInst task's present map so
                        -- that mkInstantiateWithOpts can generate
                        -- --instantiate-with flags.
                        implPresent = Map.fromList
                          [ (pid, gid)
                          | (_, implPkgName', _) <- entries
                          , Just (ADRFound _ (Library pid (InstalledLibraryInfo gid _ _))) <-
                              [Map.lookup implPkgName' adrMap']
                          ]
                        instTask = sigTask
                          { backpackInstEntries = entries
                          , configOpts = sigTask.configOpts
                              { missing =
                                  sigTask.configOpts.missing <> implPidsMissing
                              , instantiationDeps = []
                              }
                          , present =
                              sigTask.present <> implPresent
                          }
                    in  ( (instCk, ADRToInstall instTask) : instAcc
                        , instCk : keyAcc
                        , resolveWarns ++ warnAcc
                        )
      Just (ADRFound _ _) ->
        -- Sig-pkg is installed and its source could not be resolved
        -- (upgradeFoundIndefinites already tried). This typically means
        -- a GHC boot library or a package missing from Hackage.
        let w = fillSep
              [ flow "Backpack: mixin referencing"
              , style Current (fromPackageName depPkgName)
              , flow "is skipped because that package is installed and its"
              , flow "source could not be found for instantiation."
              , flow "Consider adding it as a local package in stack.yaml."
              ]
        in  (instAcc, keyAcc, w : warnAcc)
      _ -> (instAcc, keyAcc, warnAcc)

  -- Collect signatures inherited from a package's transitive indefinite deps.
  -- When pkg-A depends on indefinite pkg-B, pkg-B's signatures propagate up
  -- as holes in pkg-A.  This recursively walks the dep graph, collecting
  -- signatures from all reachable indefinite packages.
  collectInheritedSigs ::
       Package
    -> Map PackageName AddDepRes
    -> Set PackageName           -- ^ Already visited (cycle prevention)
    -> [ModuleName]
  collectInheritedSigs pkg adrMap' visited =
    concatMap collectFromDep depNames
   where
    depNames :: [PackageName]
    depNames = case pkg.library of
      Just lib -> Map.keys lib.buildInfo.dependency
      Nothing  -> []
    collectFromDep :: PackageName -> [ModuleName]
    collectFromDep depName
      | depName `Set.member` visited = []
      | otherwise =
          case Map.lookup depName adrMap' of
            Just (ADRToInstall depTask)
              | let depPkg = taskPackage depTask
              , packageIsIndefinite depPkg ->
                  let depSigs = case depPkg.library of
                        Just lib -> lib.signatures
                        Nothing  -> []
                      transitive = collectInheritedSigs depPkg adrMap'
                                     (Set.insert depName visited)
                  in  depSigs ++ transitive
            _ -> []

  -- Resolve all signature entries for a mixin, collecting both successful
  -- resolutions and warnings for signatures that could not be resolved.
  resolveAllEntries ::
       PackageName              -- ^ Sig-pkg name (for warning messages)
    -> ModuleRenaming
    -> Map PackageName AddDepRes
    -> Set PackageName          -- ^ Consumer's build-depends (scope)
    -> [ModuleName]             -- ^ Signatures to resolve
    -> ([(ModuleName, PackageName, ModuleName)], [StyleDoc])
  resolveAllEntries sigPkgName renaming adrMap' scope sigs =
    case renaming of
      HidingRenaming hiddenSigs ->
        -- HidingRenaming: hide specified sigs from mixin linking.  Hidden
        -- sigs remain as unfilled holes (the consumer becomes indefinite for
        -- them).  Since Cabal requires a closing substitution, we can only
        -- create a CInst when nothing is actually hidden — otherwise the
        -- partial --instantiate-with would fail.
        let visibleSigs = filter (`notElem` hiddenSigs) sigs
        in  if length visibleSigs == length sigs
              -- Nothing actually hidden: equivalent to DefaultRenaming.
              then foldr (resolveOne sigPkgName DefaultRenaming adrMap' scope)
                         ([], []) visibleSigs
              -- Some sigs hidden: can't fully instantiate this copy.
              else ([], [])
      _ -> foldr (resolveOne sigPkgName renaming adrMap' scope) ([], []) sigs

  -- Resolve a single signature entry, producing a warning if it fails.
  resolveOne ::
       PackageName              -- ^ Sig-pkg name (for warning messages)
    -> ModuleRenaming
    -> Map PackageName AddDepRes
    -> Set PackageName          -- ^ Consumer's build-depends (scope)
    -> ModuleName              -- ^ Signature module name
    -> ([(ModuleName, PackageName, ModuleName)], [StyleDoc])
    -> ([(ModuleName, PackageName, ModuleName)], [StyleDoc])
  resolveOne sigPkgName renaming adrMap' scope sigName (entryAcc, warnAcc) =
    case renaming of
      DefaultRenaming ->
        -- Identity mapping: sigName is filled by a module with the same name.
        case findDepExposingModule sigName adrMap' scope of
          Right implPkgName ->
            ((sigName, implPkgName, sigName) : entryAcc, warnAcc)
          Left w ->
            (entryAcc, mkModuleWarn sigPkgName sigName w : warnAcc)
      ModuleRenaming mappings ->
        -- Explicit: look for (sigName, implModuleName) in the mapping.
        case lookup sigName mappings of
          Just implModName ->
            case findDepExposingModule implModName adrMap' scope of
              Right implPkgName ->
                ((sigName, implPkgName, implModName) : entryAcc, warnAcc)
              Left w ->
                (entryAcc, mkModuleWarn sigPkgName implModName w : warnAcc)
          Nothing ->
            -- Signature not mentioned in the explicit mapping — this is
            -- normal (the mixin doesn't remap this sig), skip silently.
            (entryAcc, warnAcc)
      HidingRenaming _ ->
        -- Converted to DefaultRenaming in resolveAllEntries; should not
        -- reach here.
        (entryAcc, warnAcc)

  -- Build a warning message for a failed module resolution.
  mkModuleWarn :: PackageName -> ModuleName -> [PackageName] -> StyleDoc
  mkModuleWarn sigPkgName modName candidates = fillSep $ case candidates of
    [] ->
      [ flow "Backpack: no dependency exposes module"
      , style Current (fromString (CabalText.display modName))
      , flow "needed to instantiate"
      , style Current (fromPackageName sigPkgName) <> "."
      , flow "Ensure the implementing package is listed in build-depends."
      ]
    _ ->
      [ flow "Backpack: multiple dependencies expose module"
      , style Current (fromString (CabalText.display modName))
      , flow "needed to instantiate"
      , style Current (fromPackageName sigPkgName) <> ":"
      ]
      ++ L.intersperse "," (map (style Current . fromPackageName) candidates)
      ++ ["—", flow "cannot determine which to use. Skipping instantiation."]

  -- Find which dep in the ADR map exposes a given module name. Only packages
  -- that are in the consumer's build-depends (scope) are considered, so that
  -- multiple consumers can instantiate the same sig-pkg with different
  -- implementations without ambiguity.  Returns @Right pkgName@ on unique
  -- match, or @Left candidates@ (empty = no match, multiple = ambiguous) on
  -- failure.
  --
  -- Checks both ADRToInstall entries (via Package metadata) and ADRFound
  -- entries (via the installed modules map from ghc-pkg dump). This allows
  -- module resolution to work when the implementing package is already
  -- installed (e.g., from a snapshot or a previous build).
  findDepExposingModule ::
       ModuleName
    -> Map PackageName AddDepRes
    -> Set PackageName          -- ^ Consumer's build-depends (scope)
    -> Either [PackageName] PackageName
  findDepExposingModule modName adrMap' scope =
    let -- Check ADRToInstall entries (have full Package metadata).
        fromTasks =
          [ pn
          | (pn, ADRToInstall t) <- Map.toList adrMap'
          , pn `Set.member` scope
          , let p = taskPackage t
          , not (packageIsIndefinite p)
          , exposesModule modName p
          ]
        -- Check ADRFound entries (use installed modules map from dump).
        fromInstalled =
          [ pn
          | (pn, ADRFound _ _) <- Map.toList adrMap'
          , pn `Set.member` scope
          , case Map.lookup pn installedModules of
              Just mods -> modName `Set.member` mods
              Nothing -> False
          ]
        candidates = fromTasks ++ fromInstalled
    in  case candidates of
          [pn] -> Right pn
          _    -> Left candidates

  exposesModule :: ModuleName -> Package -> Bool
  exposesModule modName p =
    mainExposes || subLibExposes
   where
    mainExposes = case p.library of
      Just lib -> modName `L.elem` (lib.exposedModules :: [ModuleName])
      Nothing  -> False
    subLibExposes = any
      (\lib -> modName `L.elem` (lib.exposedModules :: [ModuleName]))
      (toList p.subLibraries)

  -- Produce a deterministic hash suffix from instantiation entries.
  instHashSuffix :: [(ModuleName, PackageName, ModuleName)] -> Text
  instHashSuffix entries =
    let sorted = L.sort
          [ CabalText.display sig ++ "=" ++ packageNameString implPkg
              ++ ":" ++ CabalText.display implMod
          | (sig, implPkg, implMod) <- entries
          ]
        input = S8.pack (L.intercalate "," sorted)
        digest = hashWith SHA256 input
        -- Take first 16 hex chars for a short but unique-enough suffix.
        hexStr = Mem.convertToBase Mem.Base16 digest :: ByteString
    in  T.take 16 (decodeUtf8Lenient hexStr)

-- Local helpers (duplicated from ConstructPlan to avoid import cycle)

packageBuildTypeConfig :: Package -> Bool
packageBuildTypeConfig pkg = pkg.buildType == Configure

psLocal :: PackageSource -> Bool
psLocal (PSFilePath _) = True
psLocal PSRemote{}     = False

psLocation :: PackageSource -> InstallLocation
psLocation (PSFilePath _) = Local
psLocation PSRemote{}     = Snap

toCachePkgSrc :: PackageSource -> CachePkgSrc
toCachePkgSrc (PSFilePath lp) =
  CacheSrcLocal (toFilePath (parent lp.cabalFP))
toCachePkgSrc PSRemote{} = CacheSrcUpstream
