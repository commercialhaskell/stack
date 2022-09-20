{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

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
import           Path
import           Path.Extra (rejectMissingDir)
import           Path.IO
import           RIO.Process (HasProcessContext)
import           Stack.SourceMap
import           Stack.Types.Config
import           Stack.Types.NamedComponent
import           Stack.Types.Build
import           Stack.Types.SourceMap

-- | Do we need any targets? For example, `stack build` will fail if
-- no targets are provided.
data NeedTargets = NeedTargets | AllowNoTargets

---------------------------------------------------------------------------------
-- Get the RawInput
---------------------------------------------------------------------------------

-- | Raw target information passed on the command line.
newtype RawInput = RawInput { unRawInput :: Text }

getRawInput :: BuildOptsCLI -> Map PackageName ProjectPackage -> ([Text], [RawInput])
getRawInput boptscli locals =
    let textTargets' = boptsCLITargets boptscli
        textTargets =
            -- Handle the no targets case, which means we pass in the names of all project packages
            if null textTargets'
                then map (T.pack . packageNameString) (Map.keys locals)
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
                   -> Map PackageName ProjectPackage
                   -> RawInput -- ^ raw target information from the commandline
                   -> m (Either Text [(RawInput, RawTarget)])
parseRawTargetDirs root locals ri =
    case parseRawTarget t of
        Just rt -> pure $ Right [(ri, rt)]
        Nothing -> do
            mdir <- liftIO $ forgivingAbsence (resolveDir root (T.unpack t))
              >>= rejectMissingDir
            case mdir of
                Nothing -> pure $ Left $ "Directory not found: " `T.append` t
                Just dir ->
                    case mapMaybe (childOf dir) $ Map.toList locals of
                        [] -> pure $ Left $
                            "No local directories found as children of " `T.append`
                            t
                        names -> pure $ Right $ map ((ri, ) . RTPackage) names
  where
    childOf dir (name, pp) =
        if dir == ppRoot pp || isProperPrefixOf dir (ppRoot pp)
            then Just name
            else Nothing

    RawInput t = ri

-- | If this function returns @Nothing@, the input should be treated as a
-- directory.
parseRawTarget :: Text -> Maybe RawTarget
parseRawTarget t =
        (RTPackageIdentifier <$> parsePackageIdentifier s)
    <|> (RTPackage <$> parsePackageName s)
    <|> (RTComponent <$> T.stripPrefix ":" t)
    <|> parsePackageComponent
  where
    s = T.unpack t

    parsePackageComponent =
        case T.splitOn ":" t of
            [pname, "lib"]
                | Just pname' <- parsePackageName (T.unpack pname) ->
                    Just $ RTPackageComponent pname' $ ResolvedComponent CLib
            [pname, cname]
                | Just pname' <- parsePackageName (T.unpack pname) ->
                    Just $ RTPackageComponent pname' $ UnresolvedComponent cname
            [pname, typ, cname]
                | Just pname' <- parsePackageName (T.unpack pname)
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
  , rrAddedDep :: !(Maybe PackageLocationImmutable)
  -- ^ Only if we're adding this as a dependency
  , rrPackageType :: !PackageType
  }

-- | Convert a 'RawTarget' into a 'ResolveResult' (see description on
-- the module).
resolveRawTarget ::
       (HasLogFunc env, HasPantryConfig env, HasProcessContext env)
    => SMActual GlobalPackage
    -> Map PackageName PackageLocation
    -> (RawInput, RawTarget)
    -> RIO env (Either Text ResolveResult)
resolveRawTarget sma allLocs (ri, rt) =
  go rt
  where
    locals = smaProject sma
    deps = smaDeps sma
    globals = smaGlobal sma
    -- Helper function: check if a 'NamedComponent' matches the given 'ComponentName'
    isCompNamed :: ComponentName -> NamedComponent -> Bool
    isCompNamed _ CLib = False
    isCompNamed t1 (CInternalLib t2) = t1 == t2
    isCompNamed t1 (CExe t2) = t1 == t2
    isCompNamed t1 (CTest t2) = t1 == t2
    isCompNamed t1 (CBench t2) = t1 == t2

    go (RTComponent cname) = do
        -- Associated list from component name to package that defines
        -- it. We use an assoc list and not a Map so we can detect
        -- duplicates.
        allPairs <- fmap concat $ flip Map.traverseWithKey locals
          $ \name pp -> do
              comps <- ppComponents pp
              pure $ map (name, ) $ Set.toList comps
        pure $ case filter (isCompNamed cname . snd) allPairs of
                [] -> Left $ cname `T.append` " doesn't seem to be a local target. Run 'stack ide targets' for a list of available targets"
                [(name, comp)] -> Right ResolveResult
                  { rrName = name
                  , rrRaw = ri
                  , rrComponent = Just comp
                  , rrAddedDep = Nothing
                  , rrPackageType = PTProject
                  }
                matches -> Left $ T.concat
                    [ "Ambiguous component name "
                    , cname
                    , ", matches: "
                    , T.pack $ show matches
                    ]
    go (RTPackageComponent name ucomp) =
        case Map.lookup name locals of
            Nothing -> pure $ Left $ T.pack $ "Unknown local package: " ++ packageNameString name
            Just pp -> do
                comps <- ppComponents pp
                pure $ case ucomp of
                    ResolvedComponent comp
                        | comp `Set.member` comps -> Right ResolveResult
                            { rrName = name
                            , rrRaw = ri
                            , rrComponent = Just comp
                            , rrAddedDep = Nothing
                            , rrPackageType = PTProject
                            }
                        | otherwise -> Left $ T.pack $ concat
                            [ "Component "
                            , show comp
                            , " does not exist in package "
                            , packageNameString name
                            ]
                    UnresolvedComponent comp ->
                        case filter (isCompNamed comp) $ Set.toList comps of
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
                              , rrPackageType = PTProject
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
      | Map.member name locals = pure $ Right ResolveResult
          { rrName = name
          , rrRaw = ri
          , rrComponent = Nothing
          , rrAddedDep = Nothing
          , rrPackageType = PTProject
          }
      | Map.member name deps =
          pure $ deferToConstructPlan name
      | Just gp <- Map.lookup name globals =
          case gp of
              GlobalPackage _ -> pure $ deferToConstructPlan name
              ReplacedGlobalPackage _ -> hackageLatest name
      | otherwise = hackageLatest name

    -- Note that we use getLatestHackageRevision below, even though it's
    -- non-reproducible, to avoid user confusion. In any event,
    -- reproducible builds should be done by updating your config
    -- files!

    go (RTPackageIdentifier ident@(PackageIdentifier name version))
      | Map.member name locals = pure $ Left $ T.concat
            [ tshow (packageNameString name)
            , " target has a specific version number, but it is a local package."
            , "\nTo avoid confusion, we will not install the specified version or build the local one."
            , "\nTo build the local package, specify the target without an explicit version."
            ]
      | otherwise =
          case Map.lookup name allLocs of
            -- Installing it from the package index, so we're cool
            -- with overriding it if necessary
            Just (PLImmutable (PLIHackage (PackageIdentifier _name versionLoc) _cfKey _treeKey)) ->
              if version == versionLoc
              then pure $ deferToConstructPlan name
              else hackageLatestRevision name version
            -- The package was coming from something besides the
            -- index, so refuse to do the override
            Just loc' -> pure $ Left $ T.concat
              [ "Package with identifier was targeted on the command line: "
              , T.pack $ packageIdentifierString ident
              , ", but it was specified from a non-index location: "
              , T.pack $ show loc'
              , ".\nRecommendation: add the correctly desired version to extra-deps."
              ]
            -- Not present at all, add it from Hackage
            Nothing -> do
              mrev <- getLatestHackageRevision YesRequireHackageIndex name version
              pure $ case mrev of
                Nothing -> deferToConstructPlan name
                Just (_rev, cfKey, treeKey) -> Right ResolveResult
                  { rrName = name
                  , rrRaw = ri
                  , rrComponent = Nothing
                  , rrAddedDep = Just $ PLIHackage (PackageIdentifier name version) cfKey treeKey
                  , rrPackageType = PTDependency
                  }

    hackageLatest name = do
        mloc <- getLatestHackageLocation YesRequireHackageIndex name UsePreferredVersions
        pure $ case mloc of
          Nothing -> deferToConstructPlan name
          Just loc -> do
            Right ResolveResult
                  { rrName = name
                  , rrRaw = ri
                  , rrComponent = Nothing
                  , rrAddedDep = Just loc
                  , rrPackageType = PTDependency
                  }

    hackageLatestRevision name version = do
        mrev <- getLatestHackageRevision YesRequireHackageIndex name version
        pure $ case mrev of
          Nothing -> deferToConstructPlan name
          Just (_rev, cfKey, treeKey) -> Right ResolveResult
            { rrName = name
            , rrRaw = ri
            , rrComponent = Nothing
            , rrAddedDep = Just $ PLIHackage (PackageIdentifier name version) cfKey treeKey
            , rrPackageType = PTDependency
            }

    -- This is actually an error case. We _could_ pure a
    -- Left value here, but it turns out to be better to defer
    -- this until the ConstructPlan phase, and let it complain
    -- about the missing package so that we get more errors
    -- together, plus the fancy colored output from that
    -- module.
    deferToConstructPlan name = Right ResolveResult
              { rrName = name
              , rrRaw = ri
              , rrComponent = Nothing
              , rrAddedDep = Nothing
              , rrPackageType = PTDependency
              }
---------------------------------------------------------------------------------
-- Combine the ResolveResults
---------------------------------------------------------------------------------

combineResolveResults
  :: forall env. HasLogFunc env
  => [ResolveResult]
  -> RIO env ([Text], Map PackageName Target, Map PackageName PackageLocationImmutable)
combineResolveResults results = do
    addedDeps <- fmap Map.unions $ forM results $ \result ->
      case rrAddedDep result of
        Nothing -> pure Map.empty
        Just pl -> do
          pure $ Map.singleton (rrName result) pl

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
                      , T.pack $ packageNameString name
                      , " was specified in multiple, incompatible ways: "
                      , T.unwords $ map (unRawInput . rrRaw) rrs
                      ]

    pure (errs, Map.unions ms, addedDeps)

---------------------------------------------------------------------------------
-- OK, let's do it!
---------------------------------------------------------------------------------

parseTargets :: HasBuildConfig env
    => NeedTargets
    -> Bool
    -> BuildOptsCLI
    -> SMActual GlobalPackage
    -> RIO env SMTargets
parseTargets needTargets haddockDeps boptscli smActual = do
  logDebug "Parsing the targets"
  bconfig <- view buildConfigL
  workingDir <- getCurrentDir
  locals <- view $ buildConfigL.to (smwProject . bcSMWanted)
  let (textTargets', rawInput) = getRawInput boptscli locals

  (errs1, concat -> rawTargets) <- fmap partitionEithers $ forM rawInput $
    parseRawTargetDirs workingDir locals

  let depLocs = Map.map dpLocation $ smaDeps smActual

  (errs2, resolveResults) <- fmap partitionEithers $ forM rawTargets $
    resolveRawTarget smActual depLocs

  (errs3, targets, addedDeps) <- combineResolveResults resolveResults

  case concat [errs1, errs2, errs3] of
    [] -> pure ()
    errs -> throwIO $ TargetParseException errs

  case (Map.null targets, needTargets) of
    (False, _) -> pure ()
    (True, AllowNoTargets) -> pure ()
    (True, NeedTargets)
      | null textTargets' && bcImplicitGlobal bconfig -> throwIO $ TargetParseException
          ["The specified targets matched no packages.\nPerhaps you need to run 'stack init'?"]
      | null textTargets' && Map.null locals -> throwIO $ TargetParseException
          ["The project contains no local packages (packages not marked with 'extra-dep')"]
      | otherwise -> throwIO $ TargetParseException
          ["The specified targets matched no packages"]

  addedDeps' <- mapM (additionalDepPackage haddockDeps . PLImmutable) addedDeps

  pure SMTargets
    { smtTargets = targets
    , smtDeps = addedDeps'
    }
  where
    bcImplicitGlobal bconfig =
      case configProject $ bcConfig bconfig of
        PCProject _ -> False
        PCGlobalProject -> True
        PCNoProject _ -> False
