{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Curator.Snapshot
  ( makeSnapshot
  , checkDependencyGraph
  , snapshotVersion
  ) where

import Curator.GithubPings
import Curator.Types
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.InstalledPackageInfo (InstalledPackageInfo(..))
import qualified Distribution.PackageDescription as C
import Distribution.Simple.Compiler (PackageDB(GlobalPackageDB))
import Distribution.Simple.GHC (hcPkgInfo)
import Distribution.Simple.Program.Builtin (ghcProgram)
import Distribution.Simple.Program.Db
       (configureAllKnownPrograms, defaultProgramDb, lookupProgramVersion)
import Distribution.Simple.Program.HcPkg (dump)
import Distribution.System (Arch(..), OS(..))
import qualified Distribution.Text as DT
import qualified Distribution.Types.CondTree as C
import Distribution.Types.Dependency (depPkgName, depVerRange, Dependency(..))
import Distribution.Types.ExeDependency (ExeDependency(..))
import Distribution.Types.UnitId
import Distribution.Types.UnqualComponentName (unqualComponentNameToPackageName)
import Distribution.Types.VersionRange (thisVersion, withinRange, VersionRange)
import Distribution.Verbosity (silent)
import Pantry
import RIO hiding (display)
import RIO.List (find, partition)
import qualified RIO.Map as Map
import RIO.PrettyPrint
import RIO.Process
import qualified RIO.Set as Set
import RIO.Seq (Seq)
import qualified RIO.Seq as Seq
import qualified RIO.Text as T
import qualified RIO.Text.Partial as TP

makeSnapshot
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Constraints
  -> RIO env RawSnapshotLayer
makeSnapshot cons = do
    locs <-
        traverseValidate (\(pn, pc) -> (pn,) <$> toLoc pn pc) $
        Map.toList $ consPackages cons
    let snapshotPackages = Set.fromList [ pn | (pn, Just _) <- locs ]
        inSnapshot pn = pn `Set.member` snapshotPackages
    pure
        RawSnapshotLayer
        { rslParent = RSLCompiler $ WCGhc $ consGhcVersion cons
        , rslCompiler = Nothing
        , rslLocations = mapMaybe snd locs
        , rslDropPackages = mempty
        , rslFlags = Map.mapMaybeWithKey (\pn pc -> if (inSnapshot pn) then getFlags pc else Nothing)
                     (consPackages cons)
        , rslHidden = Map.filterWithKey (\pn hide -> hide && inSnapshot pn)
                      (pcHide <$> consPackages cons)
        , rslGhcOptions = mempty
        }

getFlags :: PackageConstraints -> Maybe (Map FlagName Bool)
getFlags pc
  | Map.null (pcFlags pc) = Nothing
  | otherwise = Just (pcFlags pc)

toLoc
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> PackageConstraints
  -> RIO env (Maybe RawPackageLocationImmutable)
toLoc name pc =
  case pcSource pc of
    PSHackage (HackageSource mrange mrequiredLatest revisions) -> do
      versions <- getHackagePackageVersions YesRequireHackageIndex IgnorePreferredVersions name -- don't follow the preferred versions on Hackage, give curators more control
      when (Map.null versions) $ error $ "Package not found on Hackage: " ++ packageNameString name
      for_ mrequiredLatest $ \required ->
        case Map.maxViewWithKey versions of
          Nothing -> error $ "No versions found for " ++ packageNameString name
          Just ((version, _), _)
            | version == required -> pure ()
            | otherwise -> error $ concat
                [ "For package "
                , fromString (packageNameString name)
                , ", required latest version to be "
                , fromString (versionString required)
                , ", but actual latest is "
                , fromString (versionString version)
                ]
      let versions' =
            case mrange of
              Nothing -> versions
              Just range -> Map.filterWithKey (\v _ -> v `withinRange` range) versions
      case Map.maxViewWithKey versions' of
        Nothing -> pure Nothing -- argument could be made for erroring out, but currently used by curators to mean "don't include this"...
        Just ((version, revs), _) -> do
          let viewer =
                case revisions of
                  NoRevisions -> Map.minView
                  UseRevisions -> Map.maxView
          cfi <-
            case viewer revs of
              Nothing -> error $ "Impossible! No revisions found for " ++ show (name, version)
              Just (BlobKey sha size, _) -> pure $ CFIHash sha $ Just size
          pure $ Just $ RPLIHackage (PackageIdentifierRevision name version cfi) Nothing

traverseValidate
  :: (MonadUnliftIO m, Traversable t)
  => (a -> m b)
  -> t a
  -> m (t b)
traverseValidate f t = do
  errsRef <- newIORef id
  let f' a = f a `catchAny` \e -> do
        modifyIORef' errsRef $ (. (e:))
        pure $ impureThrow e -- should never be called
  res <- traverse f' t
  errs <- ($ []) <$> readIORef errsRef
  case errs of
    [] -> pure res
    [x] -> throwIO x
    _ -> throwIO $ TraverseValidateExceptions errs

newtype TraverseValidateExceptions = TraverseValidateExceptions [SomeException]
  deriving (Show, Typeable)
instance Exception TraverseValidateExceptions

checkDependencyGraph ::
       (HasTerm env, HasProcessContext env, HasPantryConfig env)
    => Constraints
    -> RawSnapshot
    -> RIO env ()
checkDependencyGraph constraints snapshot = do
    let compiler = rsCompiler snapshot
        compilerVer = case compiler of
          WCGhc v -> v
          WCGhcGit {} -> error "GHC-GIT is not supported"
          WCGhcjs _ _ -> error "GHCJS is not supported"
    let snapshotPackages =
            Map.fromList
                [ (pn, snapshotVersion (rspLocation sp))
                | (pn, sp) <- Map.toList (rsPackages snapshot)
                ]
    ghcBootPackages0 <- liftIO $ getBootPackages compilerVer
    let ghcBootPackages = prunedBootPackages ghcBootPackages0 (Map.keysSet snapshotPackages)
        declared = snapshotPackages <> Map.map (Just . bpVersion) ghcBootPackages
        cabalName = "Cabal"
        cabalError err = pure . Map.singleton cabalName $ [OtherError err]
    pkgErrors <- case Map.lookup cabalName declared of
      Nothing ->
        cabalError "Cabal not found in snapshot"
      Just Nothing ->
        cabalError "Cabal version in snapshot is not defined"
      Just (Just cabalVersion) -> do
        let isWiredIn pn _ = pn `Set.member` wiredInGhcPackages
            (wiredIn, packages) =
              Map.partitionWithKey isWiredIn (rsPackages snapshot)
        if not (Map.null wiredIn)
        then do
          let errMsg = "GHC wired-in package can not be overriden"
          pure $ Map.map (const [OtherError errMsg]) wiredIn
        else do
          pkgInfos <- Map.traverseWithKey (getPkgInfo constraints compilerVer)
                      packages
          let depTree =
                Map.map (piVersion &&& piTreeDeps) pkgInfos
                <> Map.map ((, []) . Just . bpVersion) ghcBootPackages
          return $ Map.mapWithKey (validatePackage constraints depTree cabalVersion) pkgInfos
    let (rangeErrors, otherErrors) = splitErrors pkgErrors
        rangeErrors' =
          Map.mapWithKey (\(pname, _, _) bs -> (Map.member pname ghcBootPackages0, bs)) rangeErrors
    unless (Map.null rangeErrors && Map.null otherErrors) $
      throwM (BrokenDependencyGraph rangeErrors' otherErrors)

data BrokenDependencyGraph = BrokenDependencyGraph
  (Map (PackageName, Set Text, Maybe Version) (Bool, Map DependingPackage DepBounds))
  (Map PackageName (Seq String))

instance Exception BrokenDependencyGraph

instance Show BrokenDependencyGraph where
  show (BrokenDependencyGraph rangeErrors otherErrors) = T.unpack . T.unlines $
    "Snapshot dependency graph contains errors:" :
    shownBoundsErrors <>
    shownOtherErrors
    where
      shownBoundsErrors =
        flip map (Map.toList rangeErrors) $ \((dep, maintainers, mver), (isBoot, users)) ->
          pkgBoundsError dep maintainers mver isBoot users
      shownOtherErrors = flip map (Map.toList otherErrors) $ \(pname, errors) -> T.unlines $
        T.pack (packageNameString pname) :
        flip map (toList errors) (\err -> "    " <> fromString err)

pkgBoundsError ::
       PackageName
    -> Set Text
    -> Maybe Version
    -> Bool
    -> Map DependingPackage DepBounds
    -> Text
pkgBoundsError dep maintainers mdepVer isBoot users =
    T.unlines $ ""
              : showDepVer
              : map showUser (Map.toList users)
  where
    showDepVer | Just version <- mdepVer =
                   T.concat [ display dep , "-" , display version
                            , displayMaintainers maintainers
                            , " is out of bounds for:"
                            ]
               | otherwise =
                   T.concat [ display dep, displayMaintainers maintainers
                            , " (not present"
                            , if isBoot then ", GHC boot library" else ""
                            , ") depended on by:"
                            ]

    displayMaintainers ms | Set.null ms = ""
                          | otherwise = T.concat [" (", T.intercalate ", " (Set.toList ms), ")"]

    showUser :: (DependingPackage, DepBounds) -> Text
    showUser (dp, db) = T.concat
            [ "- [ ] "
            , depPackageShow1 dp
            , " ("
            -- add a space after < to avoid confusing Markdown processors (like
            -- Github's issue tracker)
            , TP.replace "<" "< " $ display (dbRange db)
            , "). "
            , depPackageShow2 dp
            , ". Used by: "
            , T.intercalate ", " $ map compToText $ Set.toList (dbComponents db)
            ]

    depPackageShow1 :: DependingPackage -> Text
    depPackageShow1 DependingPackage {..}
      | Just v <- dpVersion = display dpName <> "-" <> display v
      | otherwise = display dpName

    depPackageShow2 :: DependingPackage -> Text
    depPackageShow2 DependingPackage {..} = T.intercalate ". " $
        ( if null dpMaintainers
          then "No maintainer"
          else T.intercalate ", " (Set.toList dpMaintainers)
        ) :
        if null dpGithubPings
        then []
        else [T.concat (map (T.cons '@') (Set.toList dpGithubPings))]

    compToText :: Component -> Text
    compToText CompLibrary = "library"
    compToText CompExecutable = "executable"
    compToText CompTestSuite = "test-suite"
    compToText CompBenchmark = "benchmark"

    display :: DT.Text a => a -> Text
    display = T.pack . DT.display

snapshotVersion :: RawPackageLocationImmutable -> Maybe Version
snapshotVersion (RPLIHackage (PackageIdentifierRevision _ v _) _) = Just v
snapshotVersion _ = Nothing

data DependencyError =
  RangeError RangeErrorData
  | OtherError String

data RangeErrorData = RangeErrorData
    { redPackageName :: !PackageName
    , redMaintainers :: !(Set Maintainer)
    , redPackageVersion :: !(Maybe Version)
    , redDependingPackage :: !DependingPackage
    , redDependingBounds :: !DepBounds
    }

data DependingPackage = DependingPackage
  { dpName :: !PackageName
  , dpVersion :: !(Maybe Version)
  , dpMaintainers :: !(Set Maintainer)
  , dpGithubPings :: !(Set Text)
  } deriving (Eq, Ord)

data DepBounds = DepBounds
  { dbRange :: !VersionRange
  , dbComponents :: !(Set Component)
  }

data PkgInfo = PkgInfo
  { piVersion :: !(Maybe Version)
  , piAllDeps :: ![(Component, [Dependency])]
  , piTreeDeps :: ![PackageName]
  , piCabalVersion :: !Version
  , piMaintainers :: !(Set Maintainer)
  , piGithubPings :: !(Set Text)
  }

splitErrors ::
       Map PackageName [DependencyError]
    -> ( Map (PackageName, Set Maintainer, Maybe Version) (Map DependingPackage DepBounds)
       , Map PackageName (Seq String))
splitErrors = Map.foldrWithKey go (mempty, mempty)
  where
    go pname errors (res, oes) = foldr (go' pname) (res, oes) errors
    go' pname (OtherError oe) (res, oes) =
      (res, Map.insertWith (<>) pname (Seq.singleton oe) oes)
    go' _pname (RangeError red) (res, oes) =
      ( Map.insertWith (<>)
        (redPackageName red, redMaintainers red, redPackageVersion red)
        (Map.singleton (redDependingPackage red) (redDependingBounds red))
        res
      , oes)

targetOS :: OS
targetOS = Linux

targetArch :: Arch
targetArch = X86_64

targetFlavor :: CompilerFlavor
targetFlavor = GHC

checkConditions ::
       (Monad m)
    => Version
    -> PackageName
    -> Map FlagName Bool
    -> C.ConfVar
    -> m Bool
checkConditions compilerVer pname flags confVar =
    case confVar of
        C.OS os -> return $ os == targetOS
        C.Arch arch -> return $ arch == targetArch
        C.Flag flag ->
            case Map.lookup flag flags of
                Nothing ->
                    error $
                    "Flag " <> show flag <> " for " <> show pname <>
                    " is not defined"
                Just b -> return b
        C.Impl flavor range ->
          return $ (flavor == targetFlavor) && (compilerVer `withinRange` range)

getPkgInfo ::
       (HasProcessContext env, HasLogFunc env, HasPantryConfig env)
    => Constraints
    -> Version
    -> PackageName
    -> RawSnapshotPackage
    -> RIO env PkgInfo
getPkgInfo constraints compilerVer pname rsp = do
    gpd <- loadCabalFileRawImmutable (rspLocation rsp)
    logDebug $ "Extracting deps for " <> displayShow pname
    let mpc = Map.lookup pname (consPackages constraints)
        skipBuild = maybe False pcSkipBuild mpc
        skipTest = skipBuild || maybe False ((== CASkip) . pcTests) mpc
        skipBench = skipBuild || maybe False ((== CASkip) . pcBenchmarks) mpc
        setupDepends = maybe mempty C.setupDepends $
                       C.setupBuildInfo (C.packageDescription gpd)
        -- TODO: we should also check executable names, not only their packages
        buildInfoDeps = map (\(ExeDependency p _ vr) -> Dependency p vr) . C.buildToolDepends
        gpdFlags = Map.fromList $ map (C.flagName &&& C.flagDefault) (C.genPackageFlags gpd)
        checkCond = checkConditions compilerVer pname $ maybe mempty pcFlags mpc <> gpdFlags
        collectDeps0 :: Monoid a
                     => C.CondTree C.ConfVar [Dependency] a
                     -> (a -> C.BuildInfo)
                     -> [Dependency]
        collectDeps0 tree getBuildInfo =
          let (deps, a) = C.simplifyCondTree checkCond tree
          in buildInfoDeps (getBuildInfo a) <> setupDepends <> deps
        sublibraries =
          Map.fromList [ ( unqualComponentNameToPackageName un
                         , collectDeps0 ct C.libBuildInfo
                         )
                       | (un, ct) <- C.condSubLibraries gpd
                       ]
        collectDeps tree getBI =
          let deps0 = collectDeps0 tree getBI
              partitionSublibs = partition (\d -> Map.member (depPkgName d) sublibraries)
              (sublibs, otherDeps) = partitionSublibs deps0
              (_intraDeps, sublibDeps) = partitionSublibs . Map.foldr (<>) mempty $
                Map.restrictKeys sublibraries (Set.fromList $ map depPkgName sublibs)
          in sublibDeps <> otherDeps
        toCheck skip comp getBI condTree = (skip, comp, collectDeps condTree getBI)
        checks =
          maybe [] (\ltree -> [toCheck skipBuild CompLibrary C.libBuildInfo ltree]) (C.condLibrary gpd) ++
          map (toCheck skipBuild CompExecutable C.buildInfo . snd) (C.condExecutables gpd) ++
          map (toCheck skipTest CompTestSuite C.testBuildInfo . snd) (C.condTestSuites gpd) ++
          map (toCheck skipBench CompBenchmark C.benchmarkBuildInfo . snd) (C.condBenchmarks gpd)
        allDeps = [ (component, deps)
                  | (False, component, deps) <- checks]
        treeDeps = [ depPkgName dep
                   | (comp, deps) <- allDeps
                   , comp == CompLibrary || comp == CompExecutable
                   , dep <- deps ]
    return PkgInfo
      { piVersion = snapshotVersion (rspLocation rsp)
      , piAllDeps = allDeps
      , piTreeDeps = treeDeps
      , piCabalVersion = C.specVersion $ C.packageDescription gpd
      , piMaintainers = maybe mempty pcMaintainers mpc
      , piGithubPings = applyGithubMapping constraints $ getGithubPings gpd
      }

validatePackage ::
       Constraints
    -> Map PackageName (Maybe Version, [PackageName])
    -> Version
    -> PackageName
    -> PkgInfo
    -> [DependencyError]
validatePackage constraints depTree cabalVersion pname pkg =
    checkCabalVersion <> checkCycles <>
    catMaybes [ checkDependency component dep
              | (component, deps) <- piAllDeps pkg
              , dep <- deps ]
  where
    checkCabalVersion
        | cabalVersion < piCabalVersion pkg =
            [ OtherError $
              "Cabal version " <> DT.display cabalVersion <>
              " not sufficient for " <>
              DT.display (piCabalVersion pkg)
            ]
        | otherwise = []
    checkCycles =
      occursCheck depTree pname (piTreeDeps pkg) mempty
    checkDependency :: Component -> Dependency -> Maybe DependencyError
    checkDependency component dep =
      let depName = depPkgName dep
          depRange = depVerRange dep
          mdeppc = Map.lookup depName (consPackages constraints)
          rangeError mv = Just $ RangeError RangeErrorData
              { redPackageName = depName
              , redMaintainers = maybe mempty pcMaintainers mdeppc
              , redPackageVersion = mv
              , redDependingPackage =
                  DependingPackage
                  { dpName = pname
                  , dpVersion = piVersion pkg
                  , dpMaintainers = piMaintainers pkg --maybe mempty pcMaintainers mpc
                  , dpGithubPings = piGithubPings pkg --pings
                  }
              , redDependingBounds = DepBounds depRange (Set.singleton component)
              }
      in case Map.lookup depName depTree of
          Nothing ->
            rangeError Nothing
          Just (mversion, _) ->
            case mversion of
              Just version ->
                if version `withinRange` depRange
                then Nothing
                else rangeError (Just version)
              Nothing ->
                Nothing

-- | Check whether the package(s) occurs within its own dependency
-- tree.
occursCheck
    :: Map PackageName (Maybe Version, [PackageName])
    -- ^ All packages.
    -> PackageName
    -- ^ Starting package to check for cycles in.
    -> [PackageName]
    -- ^ Dependencies of the package (only library and executable dependencies
    -- get checked).
    -> Set PackageName
    -- ^ Previously seen packages up the dependency tree.
    -> [DependencyError]
occursCheck allPackages = go
    where
        go pname deps seen =
            case find (flip Set.member seen) deps of
                Just cyclic ->
                    let mv = fst =<< Map.lookup cyclic allPackages
                    in [ OtherError $
                         "Dependency cycle detected with loop closing at " <>
                         DT.display cyclic <> maybe "" (("-" <>) . DT.display) mv
                       ]
                Nothing ->
                    flip concatMap deps $
                    \pname' ->
                         case Map.lookup pname' allPackages of
                             Just (_ver, deps')
                                 | pname' /= pname -> go pname' deps' seen'
                             _ -> []
            where seen' = Set.insert pname seen

data BootPackage = BootPackage
    { bpName :: !PackageName
    , bpVersion :: !Version
    , bpId :: !UnitId
    , bpDepends :: ![UnitId]
    }

getBootPackages :: Version -> IO (Map PackageName BootPackage)
getBootPackages ghcVersion = do
    db <- configureAllKnownPrograms silent defaultProgramDb
    rslt <- lookupProgramVersion silent ghcProgram (thisVersion ghcVersion) db
    case rslt of
      Left err -> error $ "Can't get proper GHC version: " ++ err
      Right _ -> return ()
    let toBootPackage ipi =
          let PackageIdentifier name version = sourcePackageId ipi
          in (name, BootPackage name version (installedUnitId ipi) (depends ipi))
    Map.fromList . map toBootPackage <$> dump (hcPkgInfo db) silent GlobalPackageDB

prunedBootPackages ::
       Map PackageName BootPackage
    -> Set PackageName
    -> Map PackageName BootPackage
prunedBootPackages ghcBootPackages0 overrides =
    snd $
    partitionReplacedDependencies
        ghcBootPackages0
        bpName
        bpId
        bpDepends
        overrides

-- | GHC wired-in packages, list taken from Stack.Constants
-- see also ghc\/compiler\/basicTypes\/Module.hs
wiredInGhcPackages :: Set PackageName
wiredInGhcPackages =
    Set.fromList
        [ "ghc-prim"
        , "integer-gmp"
        , "integer-simple"
        , "base"
        , "rts"
        , "template-haskell"
        , "dph-seq"
        , "dph-par"
        , "ghc"
        , "interactive"
        ]
