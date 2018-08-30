{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Reading in @SnapshotDef@s and converting them into
-- @LoadedSnapshot@s.
module Stack.Snapshot
  ( loadResolver
  , loadSnapshot
  , calculatePackagePromotion
  , loadGlobalHints
  ) where

import           Stack.Prelude hiding (Display (..))
import           Control.Monad.State.Strict      (get, put, StateT, execStateT)
import qualified Data.Conduit.List as CL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Yaml (ParseException (AesonException), decodeFileThrow)
import           Distribution.InstalledPackageInfo (PError)
import           Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.PackageDescription as C
import           Distribution.System (Platform)
import           Distribution.Text (display)
import qualified Distribution.Version as C
import           Network.HTTP.Download (download, redownload)
import           Network.HTTP.StackClient (Request, parseRequest)
import qualified RIO
import           Data.ByteString.Builder (toLazyByteString)
import qualified Pantry
import qualified Pantry.SHA256 as SHA256
import           Stack.Package
import           Stack.PackageDump
import           Stack.StoreTH
import           Stack.Types.BuildPlan
import           Stack.Types.GhcPkgId
import           Stack.Types.VersionIntervals
import           Stack.Types.Config
import           Stack.Types.Compiler
import           Stack.Types.Resolver
import           Stack.Types.Runner (HasRunner)

data SnapshotException
  = InvalidCabalFileInSnapshot !PackageLocation !PError
  | PackageDefinedTwice !PackageName !PackageLocation !PackageLocation
  | UnmetDeps !(Map PackageName (Map PackageName (VersionIntervals, Maybe Version)))
  | FilepathInCustomSnapshot !Text
  | NeedResolverOrCompiler !Text
  | MissingPackages !(Set PackageName)
  | CustomResolverException !Text !(Either Request FilePath) !ParseException
  | InvalidStackageException !SnapName !String
  deriving Typeable
instance Exception SnapshotException
instance Show SnapshotException where
  show (InvalidCabalFileInSnapshot loc err) = concat
    [ "Invalid cabal file at "
    , show loc
    , ": "
    , show err
    ]
  show (PackageDefinedTwice name loc1 loc2) = concat
    [ "Package "
    , packageNameString name
    , " is defined twice, at "
    , show loc1
    , " and "
    , show loc2
    ]
  show (UnmetDeps m) =
      concat $ "Some dependencies in the snapshot are unmet.\n" : map go (Map.toList m)
    where
      go (name, deps) = concat
        $ "\n"
        : packageNameString name
        : " is missing:\n"
        : map goDep (Map.toList deps)

      goDep (dep, (intervals, mversion)) = concat
        [ "- "
        , packageNameString dep
        , ". Requires: "
        , display $ toVersionRange intervals
        , ", "
        , case mversion of
            Nothing -> "none present"
            Just version -> versionString version ++ " found"
        , "\n"
        ]
  show (FilepathInCustomSnapshot url) =
    "Custom snapshots do not support filepaths, as the contents may change over time. Found in: " ++
    T.unpack url
  show (NeedResolverOrCompiler url) =
    "You must specify either a resolver or compiler value in " ++
    T.unpack url
  show (MissingPackages names) =
    "The following packages specified by flags or options are not found: " ++
    unwords (map packageNameString (Set.toList names))
  show (CustomResolverException url loc e) = concat
    [ "Unable to load custom resolver "
    , T.unpack url
    , " from "
    , case loc of
        Left _req -> "HTTP request"
        Right fp -> "local file:\n  " ++ fp
    , "\nException: "
    , case e of
        AesonException s -> s
        _ -> show e
    ]
  show (InvalidStackageException snapName e) = concat
    [ "Unable to parse Stackage snapshot "
    , T.unpack (renderSnapName snapName)
    , ": "
    , e
    ]

-- | Convert a 'Resolver' into a 'SnapshotDef'
loadResolver
  :: forall env. HasConfig env
  => SnapshotLocation
  -> Maybe WantedCompiler
  -> RIO env SnapshotDef
loadResolver (SLCompiler c1) (Just c2) = throwIO $ InvalidOverrideCompiler c1 c2
loadResolver sl mcompiler = do
  esnap <- Pantry.loadSnapshot sl
  (compiler, msnap, uniqueHash) <-
    case esnap of
      Left compiler -> pure (compiler, Nothing, mkUniqueHash compiler)
      Right (snap, sha) -> do
        sd <- loadResolver (snapshotParent snap) (snapshotCompiler snap)
        pure
          ( sdWantedCompilerVersion sd
          , Just (snap, sd)
          , combineHashes sha $ sdUniqueHash sd
          )
  pure SnapshotDef
    { sdResolver = sl
    , sdSnapshot = msnap
    , sdWantedCompilerVersion = fromMaybe compiler mcompiler
    , sdUniqueHash = uniqueHash
    }

  where

    mkUniqueHash :: WantedCompiler -> SHA256
    mkUniqueHash = SHA256.hashLazyBytes . toLazyByteString . getUtf8Builder . RIO.display

    combineHashes :: SHA256 -> SHA256 -> SHA256
    combineHashes x y = SHA256.hashBytes (SHA256.toRaw x <> SHA256.toRaw y)

-- | Fully load up a 'SnapshotDef' into a 'LoadedSnapshot'
loadSnapshot
  :: forall env.
     (HasConfig env, HasGHCVariant env)
  => Maybe ActualCompiler -- ^ installed GHC we should query; if none provided, use the global hints
  -> SnapshotDef
  -> RIO env LoadedSnapshot
loadSnapshot mcompiler =
    start
  where
    start sd = do
      path <- configLoadedSnapshotCache
        sd
        (maybe GISSnapshotHints GISCompiler mcompiler)
      decodeOrLoadLoadedSnapshot path (inner sd)

    inner :: SnapshotDef -> RIO env LoadedSnapshot
    inner sd = do
      logInfo $ "Loading a snapshot from a SnapshotDef: " <> RIO.display (sdResolverName sd)
      case sdSnapshot sd of
        Nothing ->
          case mcompiler of
            Nothing -> do
              ghfp <- globalHintsFile
              mglobalHints <- loadGlobalHints ghfp $ sdWantedCompilerVersion sd
              globalHints <-
                case mglobalHints of
                  Just x -> pure x
                  Nothing -> do
                    logWarn $ "Unable to load global hints for " <> RIO.display (sdWantedCompilerVersion sd)
                    pure mempty
              return LoadedSnapshot
                { lsCompilerVersion = wantedToActual $ sdWantedCompilerVersion sd
                , lsGlobals = fromGlobalHints globalHints
                , lsPackages = Map.empty
                }
            Just cv' -> loadCompiler cv'
        Just (snapshot, sd') -> start sd' >>= inner2 snapshot

    inner2 snap ls0 = do
      gpds <-
        forM (snapshotLocations snap) $ \loc -> (, PLImmutable loc) <$> loadCabalFileImmutable loc

      (globals, snapshot, locals) <-
        calculatePackagePromotion ls0
        (map (\(x, y) -> (x, y, ())) gpds)
        (snapshotFlags snap)
        (snapshotHidden snap)
        (snapshotGhcOptions snap)
        (snapshotDropPackages snap)

      return LoadedSnapshot
        { lsCompilerVersion = lsCompilerVersion ls0
        , lsGlobals = globals
        -- When applying a snapshot on top of another one, we merge
        -- the two snapshots' packages together.
        , lsPackages = Map.union snapshot (Map.map (fmap fst) locals)
        }

-- | Given information on a 'LoadedSnapshot' and a given set of
-- additional packages and configuration values, calculates the new
-- global and snapshot packages, as well as the new local packages.
--
-- The new globals and snapshots must be a subset of the initial
-- values.
calculatePackagePromotion
  :: forall env localLocation.
     (HasConfig env, HasGHCVariant env)
  => LoadedSnapshot
  -> [(GenericPackageDescription, PackageLocation, localLocation)] -- ^ packages we want to add on top of this snapshot
  -> Map PackageName (Map FlagName Bool) -- ^ flags
  -> Map PackageName Bool -- ^ overrides whether a package should be registered hidden
  -> Map PackageName [Text] -- ^ GHC options
  -> Set PackageName -- ^ packages in the snapshot to drop
  -> RIO env
       ( Map PackageName (LoadedPackageInfo GhcPkgId) -- new globals
       , Map PackageName (LoadedPackageInfo PackageLocation) -- new snapshot
       , Map PackageName (LoadedPackageInfo (PackageLocation, Maybe localLocation)) -- new locals
       )
calculatePackagePromotion
  (LoadedSnapshot compilerVersion globals0 parentPackages0)
  gpds flags0 hides0 options0 drops0 = do

      platform <- view platformL

      -- Hand out flags, hide, and GHC options to the newly added
      -- packages
      (packages1, flags, hide, ghcOptions) <- execStateT
        (mapM_ (findPackage platform compilerVersion) gpds)
        (Map.empty, flags0, hides0, options0)

      let
          -- We need to drop all packages from globals and parent
          -- packages that are either marked to be dropped, or
          -- included in the new packages.
          toDrop = Map.union (void packages1) (Map.fromSet (const ()) drops0)
          globals1 = Map.difference globals0 toDrop
          parentPackages1 = Map.difference parentPackages0 toDrop

          -- The set of all packages that need to be upgraded based on
          -- newly set flags, hide values, or GHC options
          toUpgrade = Set.unions [Map.keysSet flags, Map.keysSet hide, Map.keysSet ghcOptions]

          -- Perform a sanity check: ensure that all of the packages
          -- that need to be upgraded actually exist in the global or
          -- parent packages
          oldNames = Set.union (Map.keysSet globals1) (Map.keysSet parentPackages1)
          extraToUpgrade = Set.difference toUpgrade oldNames
      unless (Set.null extraToUpgrade) $ throwM $ MissingPackages extraToUpgrade

      let
          -- Split up the globals into those that are to be upgraded
          -- (no longer globals) and those that remain globals, based
          -- solely on the toUpgrade value
          (noLongerGlobals1, globals2) = Map.partitionWithKey
            (\name _ -> name `Set.member` toUpgrade)
            globals1
          -- Further: now that we've removed a bunch of packages from
          -- globals, split out any packages whose dependencies are no
          -- longer met
          (globals3, noLongerGlobals2) = splitUnmetDeps Map.empty globals2

          -- Put together the two split out groups of packages
          noLongerGlobals3 :: Map PackageName (LoadedPackageInfo PackageLocation)
          noLongerGlobals3 = Map.mapWithKey globalToSnapshot (Map.union noLongerGlobals1 noLongerGlobals2)

          -- Now do the same thing with parent packages: take out the
          -- packages to be upgraded and then split out unmet
          -- dependencies.
          (noLongerParent1, parentPackages2) = Map.partitionWithKey
            (\name _ -> name `Set.member` toUpgrade)
            parentPackages1
          (parentPackages3, noLongerParent2) = splitUnmetDeps
            (Map.map lpiVersion globals3)
            parentPackages2
          noLongerParent3 = Map.union noLongerParent1 noLongerParent2

          -- Everything split off from globals and parents will be upgraded...
          allToUpgrade = Map.union noLongerGlobals3 noLongerParent3

      -- ... so recalculate based on new values
      upgraded <- fmap Map.fromList
                $ mapM (recalculate compilerVersion flags hide ghcOptions)
                $ Map.toList allToUpgrade

      -- Could be nice to check snapshot early... but disabling
      -- because ConstructPlan gives much nicer error messages
      let packages2 = Map.unions [Map.map void upgraded, Map.map void packages1, Map.map void parentPackages3]
          allAvailable = Map.union
            (lpiVersion <$> globals3)
            (lpiVersion <$> packages2)
      when False $ checkDepsMet allAvailable packages2

      unless (Map.null (globals3 `Map.difference` globals0))
        (error "calculatePackagePromotion: subset invariant violated for globals")
      unless (Map.null (parentPackages3 `Map.difference` parentPackages0))
        (error "calculatePackagePromotion: subset invariant violated for parents")

      return
        ( globals3
        , parentPackages3
        , Map.union (Map.map (fmap (, Nothing)) upgraded) (Map.map (fmap (second Just)) packages1)
        )

-- | Recalculate a 'LoadedPackageInfo' based on updates to flags,
-- hide values, and GHC options.
recalculate :: forall env.
               (HasConfig env, HasGHCVariant env)
            => ActualCompiler
            -> Map PackageName (Map FlagName Bool)
            -> Map PackageName Bool -- ^ hide?
            -> Map PackageName [Text] -- ^ GHC options
            -> (PackageName, LoadedPackageInfo PackageLocation)
            -> RIO env (PackageName, LoadedPackageInfo PackageLocation)
recalculate compilerVersion allFlags allHide allOptions (name, lpi0) = do
  let hide = fromMaybe (lpiHide lpi0) (Map.lookup name allHide)
      options = fromMaybe (lpiGhcOptions lpi0) (Map.lookup name allOptions)
  case Map.lookup name allFlags of
    Nothing -> return (name, lpi0 { lpiHide = hide, lpiGhcOptions = options }) -- optimization
    Just flags -> do
      let loc = lpiLocation lpi0
      gpd <- loadCabalFile loc
      platform <- view platformL
      let res@(name', lpi) = calculate gpd platform compilerVersion loc flags hide options
      unless (name == name' && lpiVersion lpi0 == lpiVersion lpi) $ error "recalculate invariant violated"
      return res

fromGlobalHints
  :: Map PackageName Version
  -> Map PackageName (LoadedPackageInfo GhcPkgId)
fromGlobalHints =
    Map.unions . map go . Map.toList
  where
    go (name, ver) = Map.singleton name LoadedPackageInfo
      { lpiVersion = ver
      -- For global hint purposes, we only care about the
      -- version. All other fields are ignored when checking
      -- project compatibility.
      , lpiLocation = either impureThrow id
                    $ parseGhcPkgId
                    $ fromString
                    $ packageIdentifierString
                    $ PackageIdentifier name ver
      , lpiFlags = Map.empty
      , lpiGhcOptions = []
      , lpiPackageDeps = Map.empty
      , lpiExposedModules = Set.empty
      , lpiHide = False
      }

-- | Ensure that all of the dependencies needed by this package
-- are available in the given Map of packages.
checkDepsMet :: MonadThrow m
             => Map PackageName Version -- ^ all available packages
             -> Map PackageName (LoadedPackageInfo localLocation)
             -> m ()
checkDepsMet available m
  | Map.null errs = return ()
  | otherwise = throwM $ UnmetDeps errs
  where
    errs = foldMap (uncurry go) (Map.toList m)

    go :: PackageName
       -> LoadedPackageInfo loc
       -> Map PackageName (Map PackageName (VersionIntervals, Maybe Version))
    go name lpi
      | Map.null errs' = Map.empty
      | otherwise = Map.singleton name errs'
      where
        errs' = foldMap (uncurry goDep) (Map.toList (lpiPackageDeps lpi))

    goDep :: PackageName -> VersionIntervals -> Map PackageName (VersionIntervals, Maybe Version)
    goDep name intervals =
      case Map.lookup name available of
        Nothing -> Map.singleton name (intervals, Nothing)
        Just version
          | version `withinIntervals` intervals -> Map.empty
          | otherwise -> Map.singleton name (intervals, Just version)

-- | Load a snapshot from the given compiler version, using just the
-- information in the global package database.
loadCompiler :: forall env.
                HasConfig env
             => ActualCompiler
             -> RIO env LoadedSnapshot
loadCompiler cv = do
  m <- ghcPkgDump (whichCompiler cv) []
    (conduitDumpPackage .| CL.foldMap (\dp -> Map.singleton (dpGhcPkgId dp) dp))
  return LoadedSnapshot
    { lsCompilerVersion = cv
    , lsGlobals = toGlobals m
    , lsPackages = Map.empty
    }
  where
    toGlobals :: Map GhcPkgId (DumpPackage () () ())
              -> Map PackageName (LoadedPackageInfo GhcPkgId)
    toGlobals m =
        Map.fromList $ map go $ Map.elems m
      where
        identMap = Map.map dpPackageIdent m

        go :: DumpPackage () () () -> (PackageName, LoadedPackageInfo GhcPkgId)
        go dp =
            (name, lpi)
          where
            PackageIdentifier name version = dpPackageIdent dp

            goDep ghcPkgId =
              case Map.lookup ghcPkgId identMap of
                Nothing -> Map.empty
                Just (PackageIdentifier name' _) -> Map.singleton name' (fromVersionRange C.anyVersion)

            lpi :: LoadedPackageInfo GhcPkgId
            lpi = LoadedPackageInfo
                { lpiVersion = version
                , lpiLocation = dpGhcPkgId dp
                , lpiFlags = Map.empty
                , lpiGhcOptions = []
                , lpiPackageDeps = Map.unions $ map goDep $ dpDepends dp
                , lpiExposedModules = dpExposedModules dp
                , lpiHide = not $ dpIsExposed dp
                }

type FindPackageS localLocation =
    ( Map PackageName (LoadedPackageInfo (PackageLocation, localLocation))
    , Map PackageName (Map FlagName Bool) -- flags
    , Map PackageName Bool -- hide
    , Map PackageName [Text] -- ghc options
    )

-- | Find the package at the given 'PackageLocation', grab any flags,
-- hidden state, and GHC options from the 'StateT' (removing them from
-- the 'StateT'), and add the newly found package to the contained
-- 'Map'.
findPackage :: forall m localLocation.
               MonadThrow m
            => Platform
            -> ActualCompiler
            -> (GenericPackageDescription, PackageLocation, localLocation)
            -> StateT (FindPackageS localLocation) m ()
findPackage platform compilerVersion (gpd, loc, localLoc) = do
    (m, allFlags, allHide, allOptions) <- get

    case Map.lookup name m of
      Nothing -> return ()
      Just lpi -> throwM $ PackageDefinedTwice name loc (fst (lpiLocation lpi))

    let flags = fromMaybe Map.empty $ Map.lookup name allFlags
        allFlags' = Map.delete name allFlags

        hide = fromMaybe False $ Map.lookup name allHide
        allHide' = Map.delete name allHide

        options = fromMaybe [] $ Map.lookup name allOptions
        allOptions' = Map.delete name allOptions

        (name', lpi) = calculate gpd platform compilerVersion (loc, localLoc) flags hide options
        m' = Map.insert name lpi m

    assert (name == name') $ put (m', allFlags', allHide', allOptions')
  where
    PackageIdentifier name _version = C.package $ C.packageDescription gpd

-- | Convert a global 'LoadedPackageInfo' to a snapshot one by
-- creating a 'PackageLocation'.
globalToSnapshot :: PackageName -> LoadedPackageInfo loc -> LoadedPackageInfo PackageLocation
globalToSnapshot name lpi = lpi
    { lpiLocation = PLImmutable (PLIHackage (PackageIdentifierRevision name (lpiVersion lpi) CFILatest) Nothing)
    }

-- | Split the packages into those which have their dependencies met,
-- and those that don't. The first argument is packages that are known
-- to be available for use as a dependency. The second argument is the
-- packages to check.
--
-- This works by repeatedly iterating through the list of input
-- packages, adding any that have their dependencies satisfied to a map
-- (eventually this set is the fst of the result tuple). Once an
-- iteration completes without adding anything to this set, it knows it
-- has found everything that has its dependencies met, and exits.
splitUnmetDeps :: Map PackageName Version -- ^ extra dependencies available
               -> Map PackageName (LoadedPackageInfo loc)
               -> ( Map PackageName (LoadedPackageInfo loc)
                  , Map PackageName (LoadedPackageInfo loc)
                  )
splitUnmetDeps extra =
    start Map.empty . Map.toList
  where
    start newGlobals0 toProcess0
      | anyAdded = start newGlobals1 toProcess1
      | otherwise = (newGlobals1, Map.fromList toProcess1)
      where
        (newGlobals1, toProcess1, anyAdded) = loop False newGlobals0 id toProcess0

    loop anyAdded newGlobals front [] = (newGlobals, front [], anyAdded)
    loop anyAdded newGlobals front (x@(k, v):xs)
      | depsMet newGlobals v = loop True (Map.insert k v newGlobals) front xs
      | otherwise = loop anyAdded newGlobals (front . (x:)) xs

    depsMet globals = all (depsMet' globals) . Map.toList . lpiPackageDeps

    -- MSS 2018-01-10. Previously, we would actually perform a version
    -- bounds check at this point. I believe this is a mistake: we
    -- don't want to promote a package from a snapshot to a local just
    -- because the version ranges aren't satisfied. In fact, we
    -- intentionally allow snapshots to specify mismatched versions of
    -- packages, and try building anyway.
    --
    -- With the old behavior: a number of packages would be converted
    -- and treated as local packages. I specifically stumbled on this
    -- while investigating Stackage issues #3185, where a revision to
    -- semigroupoids's tagged dependency caused the builds to
    -- break. Stack should have just ignored this and printed a
    -- warning. Instead, Stack believed that semigroupoids was a local
    -- package, not a snapshot package, and failed.
    --
    -- All that said: I'm pretty certain this is the right behavior,
    -- but all of this is strongly indicating that we need some code
    -- cleanup around this promotion business. I don't think I did a
    -- particularly good job on this code during the extensible
    -- snapshot rewrite.
    depsMet' globals (name, _intervals) =
      case (lpiVersion <$> Map.lookup name globals) <|> Map.lookup name extra of
        -- The dependency doesn't exist at all in the snapshot or
        -- extra, therefore this package must be promoted to local as
        -- well.
        Nothing -> False
        -- It exists. As explained above, don't bother checking the
        -- version bounds, we trust the snapshot.
        Just _version -> True

-- | Calculate a 'LoadedPackageInfo' from the given 'GenericPackageDescription'
calculate :: GenericPackageDescription
          -> Platform
          -> ActualCompiler
          -> loc
          -> Map FlagName Bool
          -> Bool -- ^ hidden?
          -> [Text] -- ^ GHC options
          -> (PackageName, LoadedPackageInfo loc)
calculate gpd platform compilerVersion loc flags hide options =
    (name, lpi)
  where
    pconfig = PackageConfig
      { packageConfigEnableTests = False
      , packageConfigEnableBenchmarks = False
      , packageConfigFlags = flags
      , packageConfigGhcOptions = options
      , packageConfigCompilerVersion = compilerVersion
      , packageConfigPlatform = platform
      }
    -- We want to ignore test suites and benchmarks, therefore choose
    -- the package description which modifies buildable
    pd = pdpModifiedBuildable $ resolvePackageDescription pconfig gpd
    PackageIdentifier name version = C.package pd
    lpi = LoadedPackageInfo
      { lpiVersion = version
      , lpiLocation = loc
      , lpiFlags = flags
      , lpiGhcOptions = options
      , lpiPackageDeps = Map.map fromVersionRange
                       $ Map.filterWithKey (const . (/= name))
                       $ packageDependencies pconfig pd
      , lpiExposedModules = maybe
          Set.empty
          (Set.fromList . C.exposedModules)
          (C.library pd)
      , lpiHide = hide
      }

-- | Load the global hints from Github.
loadGlobalHints
  :: HasRunner env
  => Path Abs File -- ^ local cached file location
  -> WantedCompiler
  -> RIO env (Maybe (Map PackageName Version))
loadGlobalHints dest wc =
    inner False
  where
    inner alreadyDownloaded = do
      req <- parseRequest "https://raw.githubusercontent.com/fpco/stackage-content/master/stack/global-hints.yaml"
      downloaded <- download req dest
      eres <- tryAny inner2
      mres <-
        case eres of
          Left e -> Nothing <$ logError ("Error when parsing global hints: " <> displayShow e)
          Right x -> pure x
      case mres of
        Nothing | not alreadyDownloaded && not downloaded -> do
          logInfo $
            "Could not find local global hints for " <>
            RIO.display wc <>
            ", forcing a redownload"
          x <- redownload req dest
          if x
            then inner True
            else do
              logInfo "Redownload didn't happen"
              pure Nothing
        _ -> pure mres

    inner2 = liftIO
           $ Map.lookup wc . fmap (fmap unCabalString . unCabalStringMap)
         <$> decodeFileThrow (toFilePath dest)
