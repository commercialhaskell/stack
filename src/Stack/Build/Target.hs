{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}
-- | Parsing command line targets
--
-- There are two relevant data sources for performing this parsing:
-- the project configuration, and command line arguments. Project
-- configurations includes the resolver (defining a LoadedSnapshot of
-- global and snapshot packages), local dependencies, and project
-- packages. It also defines local flag overrides.
--
-- The command line arguments specify both additional local flag
-- overrides and targets in their raw form.
--
-- Flags are simple: we just combine CLI flags with config flags and
-- make one big map of flags, preferring CLI flags when present.
--
-- Raw targets can be a package name, a package name with component,
-- just a component, or a package name and version number. We first
-- must resolve these raw targets into both simple targets and
-- additional dependencies. This works as follows:
--
-- * If a component is specified, find a unique project package which
--   defines that component, and convert it into a name+component
--   target.
--
-- * Ensure that all name+component values refer to valid components
--   in the given project package.
--
-- * For names, check if the name is present in the snapshot, local
--   deps, or project packages. If it is not, then look up the most
--   recent version in the package index and convert to a
--   name+version.
--
-- * For name+version, first ensure that the name is not used by a
--   project package. Next, if that name+version is present in the
--   snapshot or local deps _and_ its location is PLIndex, we have the
--   package. Otherwise, add to local deps with the appropriate
--   PLIndex.
--
-- If in either of the last two bullets we added a package to local
-- deps, print a warning to the user recommending modifying the
-- extra-deps.
--
-- Combine the various 'ResolveResults's together into 'Target'
-- values, by combining various components for a single package and
-- ensuring that no conflicting statements were made about targets.
--
-- At this point, we now have a Map from package name to SimpleTarget,
-- and an updated Map of local dependencies. We still have the
-- aggregated flags, and the snapshot and project packages.
--
-- Finally, we upgrade the snapshot by using
-- calculatePackagePromotion.
module Stack.Build.Target
    ( -- * Types
      Target (..)
    , NeedTargets (..)
    , PackageType (..)
    , parseTargets
      -- * Convenience helpers
    , gpdVersion
      -- * Test suite exports
    , parseRawTarget
    , RawTarget (..)
    , UnresolvedComponent (..)
    ) where

import           Stack.Prelude
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Distribution.PackageDescription (GenericPackageDescription, package, packageDescription)
import           Path
import           Path.Extra (rejectMissingDir)
import           Path.IO
import           Stack.Config (getLocalPackages)
import           Stack.PackageIndex
import           Stack.PackageLocation
import           Stack.Snapshot (calculatePackagePromotion)
import           Stack.Types.Config
import           Stack.Types.NamedComponent
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version
import           Stack.Types.Build
import           Stack.Types.BuildPlan
import           Stack.Types.GhcPkgId

-- | Do we need any targets? For example, `stack build` will fail if
-- no targets are provided.
data NeedTargets = NeedTargets | AllowNoTargets

---------------------------------------------------------------------------------
-- Get the RawInput
---------------------------------------------------------------------------------

-- | Raw target information passed on the command line.
newtype RawInput = RawInput { unRawInput :: Text }

getRawInput :: BuildOptsCLI -> Map PackageName LocalPackageView -> ([Text], [RawInput])
getRawInput boptscli locals =
    let textTargets' = boptsCLITargets boptscli
        textTargets =
            -- Handle the no targets case, which means we pass in the names of all project packages
            if null textTargets'
                then map packageNameText (Map.keys locals)
                else textTargets'
     in (textTargets', map RawInput textTargets)

---------------------------------------------------------------------------------
-- Turn RawInput into RawTarget
---------------------------------------------------------------------------------

-- | The name of a component, which applies to executables, test
-- suites, and benchmarks
type ComponentName = Text

-- | Either a fully resolved component, or a component name that could be
-- either an executable, test, or benchmark
data UnresolvedComponent
    = ResolvedComponent !NamedComponent
    | UnresolvedComponent !ComponentName
    deriving (Show, Eq, Ord)

-- | Raw command line input, without checking against any databases or list of
-- locals. Does not deal with directories
data RawTarget
    = RTPackageComponent !PackageName !UnresolvedComponent
    | RTComponent !ComponentName
    | RTPackage !PackageName
    -- Explicitly _not_ supporting revisions on the command line. If
    -- you want that, you should be modifying your stack.yaml! (In
    -- fact, you should probably do that anyway, we're just letting
    -- people be lazy, since we're Haskeletors.)
    | RTPackageIdentifier !PackageIdentifier
  deriving (Show, Eq)

-- | Same as @parseRawTarget@, but also takes directories into account.
parseRawTargetDirs :: MonadIO m
                   => Path Abs Dir -- ^ current directory
                   -> Map PackageName LocalPackageView
                   -> RawInput -- ^ raw target information from the commandline
                   -> m (Either Text [(RawInput, RawTarget)])
parseRawTargetDirs root locals ri =
    case parseRawTarget t of
        Just rt -> return $ Right [(ri, rt)]
        Nothing -> do
            mdir <- liftIO $ forgivingAbsence (resolveDir root (T.unpack t))
              >>= rejectMissingDir
            case mdir of
                Nothing -> return $ Left $ "Directory not found: " `T.append` t
                Just dir ->
                    case mapMaybe (childOf dir) $ Map.toList locals of
                        [] -> return $ Left $
                            "No local directories found as children of " `T.append`
                            t
                        names -> return $ Right $ map ((ri, ) . RTPackage) names
  where
    childOf dir (name, lpv) =
        if dir == lpvRoot lpv || isProperPrefixOf dir (lpvRoot lpv)
            then Just name
            else Nothing

    RawInput t = ri

-- | If this function returns @Nothing@, the input should be treated as a
-- directory.
parseRawTarget :: Text -> Maybe RawTarget
parseRawTarget t =
        (RTPackageIdentifier <$> parsePackageIdentifier t)
    <|> (RTPackage <$> parsePackageNameFromString s)
    <|> (RTComponent <$> T.stripPrefix ":" t)
    <|> parsePackageComponent
  where
    s = T.unpack t

    parsePackageComponent =
        case T.splitOn ":" t of
            [pname, "lib"]
                | Just pname' <- parsePackageNameFromString (T.unpack pname) ->
                    Just $ RTPackageComponent pname' $ ResolvedComponent CLib
            [pname, cname]
                | Just pname' <- parsePackageNameFromString (T.unpack pname) ->
                    Just $ RTPackageComponent pname' $ UnresolvedComponent cname
            [pname, typ, cname]
                | Just pname' <- parsePackageNameFromString (T.unpack pname)
                , Just wrapper <- parseCompType typ ->
                    Just $ RTPackageComponent pname' $ ResolvedComponent $ wrapper cname
            _ -> Nothing

    parseCompType t' =
        case t' of
            "exe" -> Just CExe
            "test" -> Just CTest
            "bench" -> Just CBench
            _ -> Nothing

---------------------------------------------------------------------------------
-- Resolve the raw targets
---------------------------------------------------------------------------------

data ResolveResult = ResolveResult
  { rrName :: !PackageName
  , rrRaw :: !RawInput
  , rrComponent :: !(Maybe NamedComponent)
  -- ^ Was a concrete component specified?
  , rrAddedDep :: !(Maybe Version)
  -- ^ Only if we're adding this as a dependency
  , rrPackageType :: !PackageType
  }

-- | Convert a 'RawTarget' into a 'ResolveResult' (see description on
-- the module).
resolveRawTarget
  :: forall env. HasConfig env
  => Map PackageName (LoadedPackageInfo GhcPkgId) -- ^ globals
  -> Map PackageName (LoadedPackageInfo (PackageLocationIndex FilePath)) -- ^ snapshot
  -> Map PackageName (GenericPackageDescription, PackageLocationIndex FilePath) -- ^ local deps
  -> Map PackageName LocalPackageView -- ^ project packages
  -> (RawInput, RawTarget)
  -> RIO env (Either Text ResolveResult)
resolveRawTarget globals snap deps locals (ri, rt) =
    go rt
  where
    -- Helper function: check if a 'NamedComponent' matches the given 'ComponentName'
    isCompNamed :: ComponentName -> NamedComponent -> Bool
    isCompNamed _ CLib = False
    isCompNamed t1 (CExe t2) = t1 == t2
    isCompNamed t1 (CTest t2) = t1 == t2
    isCompNamed t1 (CBench t2) = t1 == t2

    go (RTComponent cname) = return $
        -- Associated list from component name to package that defines
        -- it. We use an assoc list and not a Map so we can detect
        -- duplicates.
        let allPairs = concatMap
                (\(name, lpv) -> map (name,) $ Set.toList $ lpvComponents lpv)
                (Map.toList locals)
         in case filter (isCompNamed cname . snd) allPairs of
                [] -> Left $ cname `T.append` " doesn't seem to be a local target. Run 'stack ide targets' for a list of available targets"
                [(name, comp)] -> Right ResolveResult
                  { rrName = name
                  , rrRaw = ri
                  , rrComponent = Just comp
                  , rrAddedDep = Nothing
                  , rrPackageType = ProjectPackage
                  }
                matches -> Left $ T.concat
                    [ "Ambiugous component name "
                    , cname
                    , ", matches: "
                    , T.pack $ show matches
                    ]
    go (RTPackageComponent name ucomp) = return $
        case Map.lookup name locals of
            Nothing -> Left $ T.pack $ "Unknown local package: " ++ packageNameString name
            Just lpv ->
                case ucomp of
                    ResolvedComponent comp
                        | comp `Set.member` lpvComponents lpv -> Right ResolveResult
                            { rrName = name
                            , rrRaw = ri
                            , rrComponent = Just comp
                            , rrAddedDep = Nothing
                            , rrPackageType = ProjectPackage
                            }
                        | otherwise -> Left $ T.pack $ concat
                            [ "Component "
                            , show comp
                            , " does not exist in package "
                            , packageNameString name
                            ]
                    UnresolvedComponent comp ->
                        case filter (isCompNamed comp) $ Set.toList $ lpvComponents lpv of
                            [] -> Left $ T.concat
                                [ "Component "
                                , comp
                                , " does not exist in package "
                                , T.pack $ packageNameString name
                                ]
                            [x] -> Right ResolveResult
                              { rrName = name
                              , rrRaw = ri
                              , rrComponent = Just x
                              , rrAddedDep = Nothing
                              , rrPackageType = ProjectPackage
                              }
                            matches -> Left $ T.concat
                                [ "Ambiguous component name "
                                , comp
                                , " for package "
                                , T.pack $ packageNameString name
                                , ": "
                                , T.pack $ show matches
                                ]

    go (RTPackage name)
      | Map.member name locals = return $ Right ResolveResult
          { rrName = name
          , rrRaw = ri
          , rrComponent = Nothing
          , rrAddedDep = Nothing
          , rrPackageType = ProjectPackage
          }
      | Map.member name deps ||
        Map.member name snap ||
        Map.member name globals = return $ Right ResolveResult
          { rrName = name
          , rrRaw = ri
          , rrComponent = Nothing
          , rrAddedDep = Nothing
          , rrPackageType = Dependency
          }
      | otherwise = do
          mversion <- getLatestVersion name
          return $ case mversion of
            -- This is actually an error case. We _could_ return a
            -- Left value here, but it turns out to be better to defer
            -- this until the ConstructPlan phase, and let it complain
            -- about the missing package so that we get more errors
            -- together, plus the fancy colored output from that
            -- module.
            Nothing -> Right ResolveResult
              { rrName = name
              , rrRaw = ri
              , rrComponent = Nothing
              , rrAddedDep = Nothing
              , rrPackageType = Dependency
              }
            Just version -> Right ResolveResult
              { rrName = name
              , rrRaw = ri
              , rrComponent = Nothing
              , rrAddedDep = Just version
              , rrPackageType = Dependency
              }
      where
        getLatestVersion pn =
            fmap fst . Set.maxView <$> getPackageVersions pn

    go (RTPackageIdentifier ident@(PackageIdentifier name version))
      | Map.member name locals = return $ Left $ T.concat
            [ packageNameText name
            , " target has a specific version number, but it is a local package."
            , "\nTo avoid confusion, we will not install the specified version or build the local one."
            , "\nTo build the local package, specify the target without an explicit version."
            ]
      | otherwise = return $
          case Map.lookup name allLocs of
            -- Installing it from the package index, so we're cool
            -- with overriding it if necessary
            Just (PLIndex (PackageIdentifierRevision (PackageIdentifier _name versionLoc) _mcfi)) -> Right ResolveResult
                  { rrName = name
                  , rrRaw = ri
                  , rrComponent = Nothing
                  , rrAddedDep =
                      if version == versionLoc
                        -- But no need to override anyway, this is already the
                        -- version we have
                        then Nothing
                        -- OK, we'll override it
                        else Just version
                  , rrPackageType = Dependency
                  }
            -- The package was coming from something besides the
            -- index, so refuse to do the override
            Just (PLOther loc') -> Left $ T.concat
              [ "Package with identifier was targeted on the command line: "
              , packageIdentifierText ident
              , ", but it was specified from a non-index location: "
              , T.pack $ show loc'
              , ".\nRecommendation: add the correctly desired version to extra-deps."
              ]
            -- Not present at all, so add it
            Nothing -> Right ResolveResult
              { rrName = name
              , rrRaw = ri
              , rrComponent = Nothing
              , rrAddedDep = Just version
              , rrPackageType = Dependency
              }

      where
        allLocs :: Map PackageName (PackageLocationIndex FilePath)
        allLocs = Map.unions
          [ Map.mapWithKey
              (\name' lpi -> PLIndex $ PackageIdentifierRevision
                  (PackageIdentifier name' (lpiVersion lpi))
                  CFILatest)
              globals
          , Map.map lpiLocation snap
          , Map.map snd deps
          ]

---------------------------------------------------------------------------------
-- Combine the ResolveResults
---------------------------------------------------------------------------------

-- | How a package is intended to be built
data Target
  = TargetAll !PackageType
  -- ^ Build all of the default components.
  | TargetComps !(Set NamedComponent)
  -- ^ Only build specific components

data PackageType = ProjectPackage | Dependency
  deriving (Eq, Show)

combineResolveResults
  :: forall env. HasLogFunc env
  => [ResolveResult]
  -> RIO env ([Text], Map PackageName Target, Map PackageName (PackageLocationIndex FilePath))
combineResolveResults results = do
    addedDeps <- fmap Map.unions $ forM results $ \result ->
      case rrAddedDep result of
        Nothing -> return Map.empty
        Just version -> do
          let ident = PackageIdentifier (rrName result) version
          return $ Map.singleton (rrName result) $ PLIndex $ PackageIdentifierRevision ident CFILatest

    let m0 = Map.unionsWith (++) $ map (\rr -> Map.singleton (rrName rr) [rr]) results
        (errs, ms) = partitionEithers $ flip map (Map.toList m0) $ \(name, rrs) ->
            let mcomps = map rrComponent rrs in
            -- Confirm that there is either exactly 1 with no component, or
            -- that all rrs are components
            case rrs of
                [] -> assert False $ Left "Somehow got no rrComponent values, that can't happen"
                [rr] | isNothing (rrComponent rr) -> Right $ Map.singleton name $ TargetAll $ rrPackageType rr
                _
                  | all isJust mcomps -> Right $ Map.singleton name $ TargetComps $ Set.fromList $ catMaybes mcomps
                  | otherwise -> Left $ T.concat
                      [ "The package "
                      , packageNameText name
                      , " was specified in multiple, incompatible ways: "
                      , T.unwords $ map (unRawInput . rrRaw) rrs
                      ]

    return (errs, Map.unions ms, addedDeps)

---------------------------------------------------------------------------------
-- OK, let's do it!
---------------------------------------------------------------------------------

parseTargets
    :: HasEnvConfig env
    => NeedTargets
    -> BuildOptsCLI
    -> RIO env
         ( LoadedSnapshot -- upgraded snapshot, with some packages possibly moved to local
         , Map PackageName (LoadedPackageInfo (PackageLocationIndex FilePath)) -- all local deps
         , Map PackageName Target
         )
parseTargets needTargets boptscli = do
  logDebug "Parsing the targets"
  bconfig <- view buildConfigL
  ls0 <- view loadedSnapshotL
  workingDir <- getCurrentDir
  lp <- getLocalPackages
  let locals = lpProject lp
      deps = lpDependencies lp
      globals = lsGlobals ls0
      snap = lsPackages ls0
      (textTargets', rawInput) = getRawInput boptscli locals

  (errs1, concat -> rawTargets) <- fmap partitionEithers $ forM rawInput $
    parseRawTargetDirs workingDir (lpProject lp)

  (errs2, resolveResults) <- fmap partitionEithers $ forM rawTargets $
    resolveRawTarget globals snap deps locals

  (errs3, targets, addedDeps) <- combineResolveResults resolveResults

  case concat [errs1, errs2, errs3] of
    [] -> return ()
    errs -> throwIO $ TargetParseException errs

  case (Map.null targets, needTargets) of
    (False, _) -> return ()
    (True, AllowNoTargets) -> return ()
    (True, NeedTargets)
      | null textTargets' && bcImplicitGlobal bconfig -> throwIO $ TargetParseException
          ["The specified targets matched no packages.\nPerhaps you need to run 'stack init'?"]
      | null textTargets' && Map.null locals -> throwIO $ TargetParseException
          ["The project contains no local packages (packages not marked with 'extra-dep')"]
      | otherwise -> throwIO $ TargetParseException
          ["The specified targets matched no packages"]

  root <- view projectRootL

  let dropMaybeKey (Nothing, _) = Map.empty
      dropMaybeKey (Just key, value) = Map.singleton key value
      flags = Map.unionWith Map.union
        (Map.unions (map dropMaybeKey (Map.toList (boptsCLIFlags boptscli))))
        (bcFlags bconfig)
      hides = Map.empty -- not supported to add hidden packages

      -- We promote packages to the local database if the GHC options
      -- are added to them by name. See:
      -- https://github.com/commercialhaskell/stack/issues/849#issuecomment-320892095.
      --
      -- GHC options applied to all packages are handled by getGhcOptions.
      options = configGhcOptionsByName (bcConfig bconfig)

      drops = Set.empty -- not supported to add drops

  (globals', snapshots, locals') <- do
    addedDeps' <- fmap Map.fromList $ forM (Map.toList addedDeps) $ \(name, loc) -> do
      gpd <- parseSingleCabalFileIndex root loc
      return (name, (gpd, loc, Nothing))

    -- Calculate a list of all of the locals, based on the project
    -- packages, local dependencies, and added deps found from the
    -- command line
    let allLocals :: Map PackageName (GenericPackageDescription, PackageLocationIndex FilePath, Maybe LocalPackageView)
        allLocals = Map.unions
          [ -- project packages
            Map.map
              (\lpv -> (lpvGPD lpv, PLOther $ lpvLoc lpv, Just lpv))
              (lpProject lp)
          , -- added deps take precendence over local deps
            addedDeps'
          , -- added deps take precendence over local deps
            Map.map
              (\(gpd, loc) -> (gpd, loc, Nothing))
              (lpDependencies lp)
          ]

    calculatePackagePromotion
      root ls0 (Map.elems allLocals)
      flags hides options drops

  let ls = LoadedSnapshot
        { lsCompilerVersion = lsCompilerVersion ls0
        , lsGlobals = globals'
        , lsPackages = snapshots
        }

      localDeps = Map.fromList $ flip mapMaybe (Map.toList locals') $ \(name, lpi) ->
        -- We want to ignore any project packages, but grab the local
        -- deps and upgraded snapshot deps
        case lpiLocation lpi of
          (_, Just (Just _localPackageView)) -> Nothing -- project package
          (loc, _) -> Just (name, lpi { lpiLocation = loc }) -- upgraded or local dep

  return (ls, localDeps, targets)

gpdVersion :: GenericPackageDescription -> Version
gpdVersion gpd =
    version
  where
    PackageIdentifier _ version = fromCabalPackageIdentifier $ package $ packageDescription gpd
