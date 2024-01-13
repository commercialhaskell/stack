{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Parsing command line targets
--
-- There are two relevant data sources for performing this parsing: the project
-- configuration, and command line arguments. Project configurations includes
-- the resolver (defining a LoadedSnapshot of global and snapshot packages),
-- local dependencies, and project packages. It also defines local flag
-- overrides.
--
-- The command line arguments specify both additional local flag overrides and
-- targets in their raw form.
--
-- Flags are simple: we just combine CLI flags with config flags and make one
-- big map of flags, preferring CLI flags when present.
--
-- Raw targets can be a package name, a package name with component, just a
-- component, or a package name and version number. We first must resolve these
-- raw targets into both simple targets and additional dependencies. This works
-- as follows:
--
-- * If a component is specified, find a unique project package which defines
--   that component, and convert it into a name+component target.
--
-- * Ensure that all name+component values refer to valid components in the
--   given project package.
--
-- * For names, check if the name is present in the snapshot, local deps, or
--   project packages. If it is not, then look up the most recent version in the
--   package index and convert to a name+version.
--
-- * For name+version, first ensure that the name is not used by a project
--   package. Next, if that name+version is present in the snapshot or local
--   deps _and_ its location is PLIndex, we have the package. Otherwise, add to
--   local deps with the appropriate PLIndex.
--
-- If in either of the last two bullets we added a package to local deps, print
-- a warning to the user recommending modifying the extra-deps.
--
-- Combine the various 'ResolveResults's together into 'Target' values, by
-- combining various components for a single package and ensuring that no
-- conflicting statements were made about targets.
--
-- At this point, we now have a Map from package name to SimpleTarget, and an
-- updated Map of local dependencies. We still have the aggregated flags, and
-- the snapshot and project packages.
--
-- Finally, we upgrade the snapshot by using calculatePackagePromotion.
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

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Path ( isProperPrefixOf )
import           Path.Extra ( forgivingResolveDir, rejectMissingDir )
import           Path.IO ( getCurrentDir )
import           RIO.Process ( HasProcessContext )
import           Stack.SourceMap ( additionalDepPackage )
import           Stack.Prelude
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..) )
import           Stack.Types.BuildOpts ( BuildOptsCLI (..) )
import           Stack.Types.Config ( Config (..) )
import           Stack.Types.NamedComponent
                   ( NamedComponent (..), renderComponent )
import           Stack.Types.Build.Exception ( BuildPrettyException (..) )
import           Stack.Types.ProjectConfig ( ProjectConfig (..) )
import           Stack.Types.SourceMap
                   ( DepPackage (..), GlobalPackage (..), PackageType (..)
                   , ProjectPackage, SMActual (..), SMTargets (..)
                   , SMWanted (..), Target (..), ppComponents, ppRoot
                   )

-- | Do we need any targets? For example, `stack build` will fail if
-- no targets are provided.
data NeedTargets
  = NeedTargets
  | AllowNoTargets

--------------------------------------------------------------------------------
-- Get the RawInput
--------------------------------------------------------------------------------

-- | Raw target information passed on the command line.
newtype RawInput = RawInput { unRawInput :: Text }

getRawInput ::
     BuildOptsCLI
  -> Map PackageName ProjectPackage
  -> ([Text], [RawInput])
getRawInput boptscli locals =
  let textTargets' = boptscli.targets
      textTargets =
        -- Handle the no targets case, which means we pass in the names of all
        -- project packages
        if null textTargets'
          then map (T.pack . packageNameString) (Map.keys locals)
          else textTargets'
  in  (textTargets', map RawInput textTargets)

--------------------------------------------------------------------------------
-- Turn RawInput into RawTarget
--------------------------------------------------------------------------------

-- | The name of a component, which applies to executables, test
-- suites, and benchmarks
type ComponentName = Text

-- | Either a fully resolved component, or a component name that could be
-- either an executable, test, or benchmark
data UnresolvedComponent
  = ResolvedComponent !NamedComponent
  | UnresolvedComponent !ComponentName
  deriving (Eq, Ord, Show)

-- | Raw command line input, without checking against any databases or list of
-- locals. Does not deal with directories
data RawTarget
  = RTPackageComponent !PackageName !UnresolvedComponent
  | RTComponent !ComponentName
  | RTPackage !PackageName
    -- Explicitly _not_ supporting revisions on the command line. If you want
    -- that, you should be modifying your stack.yaml! (In fact, you should
    -- probably do that anyway, we're just letting people be lazy, since we're
    -- Haskeletors.)
  | RTPackageIdentifier !PackageIdentifier
  deriving (Eq, Show)

-- | Same as @parseRawTarget@, but also takes directories into account.
parseRawTargetDirs :: MonadIO m
                   => Path Abs Dir -- ^ current directory
                   -> Map PackageName ProjectPackage
                   -> RawInput -- ^ raw target information from the commandline
                   -> m (Either StyleDoc [(RawInput, RawTarget)])
parseRawTargetDirs root locals ri =
  case parseRawTarget t of
    Just rt -> pure $ Right [(ri, rt)]
    Nothing -> do
      mdir <- forgivingResolveDir root (T.unpack t) >>= rejectMissingDir
      case mdir of
        Nothing -> pure $ Left $
          if | T.isPrefixOf "stack-yaml=" t -> projectOptionTypo
             | T.isSuffixOf ".yaml" t -> projectYamlExtTypo
             | otherwise ->
                fillSep
                  [ flow "Directory not found:"
                  , style Dir (fromString $ T.unpack t) <> "."
                  ]
        Just dir ->
          case mapMaybe (childOf dir) $ Map.toList locals of
            [] -> pure $ Left $
              fillSep
                [ style Dir (fromString $ T.unpack t)
                , flow "is not a local package directory and it is not a \
                       \parent directory of any local package directory."
                ]
            names -> pure $ Right $ map ((ri, ) . RTPackage) names
 where
  childOf dir (name, pp) =
    if dir == ppRoot pp || isProperPrefixOf dir (ppRoot pp)
      then Just name
      else Nothing

  RawInput t = ri

  projectOptionTypo :: StyleDoc
  projectOptionTypo = let o = "stack-yaml=" in projectTypo 2 (length o) o

  projectYamlExtTypo :: StyleDoc
  projectYamlExtTypo = let o = "stack-yaml " in projectTypo (2 + length o) 0 o

  projectTypo :: Int -> Int -> String -> StyleDoc
  projectTypo padLength dropLength option =
    vsep
      [ style Dir (fromString (replicate padLength ' ') <> fromString (T.unpack t))
        <> " is not a directory."
      , style Highlight (fromString $ "--" <> option)
        <> style Dir (fromString . drop dropLength $ T.unpack t)
        <> " might work as a project option."
      ]

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

--------------------------------------------------------------------------------
-- Resolve the raw targets
--------------------------------------------------------------------------------

data ResolveResult = ResolveResult
  { rrName :: !PackageName
  , rrRaw :: !RawInput
  , rrComponent :: !(Maybe NamedComponent)
    -- ^ Was a concrete component specified?
  , rrAddedDep :: !(Maybe PackageLocationImmutable)
    -- ^ Only if we're adding this as a dependency
  , rrPackageType :: !PackageType
  }

-- | Convert a 'RawTarget' into a 'ResolveResult' (see description on the
-- module).
resolveRawTarget ::
     (HasLogFunc env, HasPantryConfig env, HasProcessContext env)
  => SMActual GlobalPackage
  -> Map PackageName PackageLocation
  -> (RawInput, RawTarget)
  -> RIO env (Either StyleDoc ResolveResult)
resolveRawTarget sma allLocs (ri, rt) =
  go rt
 where
  locals = sma.smaProject
  deps = sma.smaDeps
  globals = sma.smaGlobal
  -- Helper function: check if a 'NamedComponent' matches the given
  -- 'ComponentName'
  isCompNamed :: ComponentName -> NamedComponent -> Bool
  isCompNamed _ CLib = False
  isCompNamed t1 (CSubLib t2) = t1 == t2
  isCompNamed t1 (CExe t2) = t1 == t2
  isCompNamed t1 (CFlib t2) = t1 == t2
  isCompNamed t1 (CTest t2) = t1 == t2
  isCompNamed t1 (CBench t2) = t1 == t2

  go (RTComponent cname) = do
    -- Associated list from component name to package that defines it. We use an
    -- assoc list and not a Map so we can detect duplicates.
    allPairs <- fmap concat $ flip Map.traverseWithKey locals
      $ \name pp -> do
          comps <- ppComponents pp
          pure $ map (name, ) $ Set.toList comps
    pure $ case filter (isCompNamed cname . snd) allPairs of
      [] -> Left $
        fillSep
          [ style Target . fromString . T.unpack $ cname
          , flow "doesn't seem to be a local target. Run"
          , style Shell $ flow "stack ide targets"
          , flow "for a list of available targets."
          ]
      [(name, comp)] -> Right ResolveResult
        { rrName = name
        , rrRaw = ri
        , rrComponent = Just comp
        , rrAddedDep = Nothing
        , rrPackageType = PTProject
        }
      matches -> Left $
           fillSep
             [ flow "Ambiguous component name"
             , style Target (fromString $ T.unpack cname) <> ","
             , "matches:"
             ]
        <> line
        <> bulletedList
             ( map
                 ( \(pn, nc) -> fillSep
                     [ "component"
                     , style
                         PkgComponent
                         (fromString $ T.unpack $ renderComponent nc)
                     , flow "of package"
                     , style PkgComponent (fromPackageName pn)
                     ]
                 )
                 matches
             )

  go (RTPackageComponent name ucomp) =
    case Map.lookup name locals of
      Nothing -> pure $ Left $
        fillSep
          [ flow "Unknown local package:"
          , style Target (fromPackageName name) <> "."
          ]
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
            | otherwise -> Left $
                fillSep
                  [ "Component"
                  , style Target (fromString $ T.unpack $ renderComponent comp)
                  , flow "does not exist in package"
                  , style Target (fromPackageName name) <> "."
                  ]
          UnresolvedComponent comp ->
            case filter (isCompNamed comp) $ Set.toList comps of
              [] -> Left $
                fillSep
                  [ "Component"
                  , style Target (fromString $ T.unpack comp)
                  , flow "does not exist in package"
                  , style Target (fromPackageName name) <> "."
                  ]
              [x] -> Right ResolveResult
                { rrName = name
                , rrRaw = ri
                , rrComponent = Just x
                , rrAddedDep = Nothing
                , rrPackageType = PTProject
                }
              matches -> Left $
                fillSep
                  [ flow "Ambiguous component name"
                  , style Target (fromString $ T.unpack comp)
                  , flow "for package"
                  , style Target (fromPackageName name)
                  , flow "matches components:"
                  , fillSep $
                      mkNarrativeList (Just PkgComponent) False
                        (map ncToStyleDoc matches)
                  ]
   where
    ncToStyleDoc :: NamedComponent -> StyleDoc
    ncToStyleDoc = fromString . T.unpack . renderComponent

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
  -- non-reproducible, to avoid user confusion. In any event, reproducible
  -- builds should be done by updating your config files!

  go (RTPackageIdentifier ident@(PackageIdentifier name version))
    | Map.member name locals = pure $ Left $
        fillSep
          [ style Target (fromPackageName name)
          , flow "target has a specific version number, but it is a local \
                 \package. To avoid confusion, we will not install the \
                 \specified version or build the local one. To build the \
                 \local package, specify the target without an explicit \
                 \version."
          ]
    | otherwise =
        case Map.lookup name allLocs of
          -- Installing it from the package index, so we're cool with overriding
          -- it if necessary
          Just
            ( PLImmutable
                ( PLIHackage
                    (PackageIdentifier _name versionLoc) _cfKey _treeKey
                )
            ) ->
              if version == versionLoc
                then pure $ deferToConstructPlan name
                else hackageLatestRevision name version
          -- The package was coming from something besides the index, so refuse
          -- to do the override
          Just loc' -> pure $ Left $
            fillSep
              [ flow "Package with identifier was targeted on the command \
                     \line:"
              , style Target (fromPackageId ident) <> ","
              , flow "but it was specified from a non-index location:"
              , flow $ T.unpack $ textDisplay loc' <> "."
              , flow "Recommendation: add the correctly desired version to \
                     \extra-deps."
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
                , rrAddedDep = Just $
                    PLIHackage (PackageIdentifier name version) cfKey treeKey
                , rrPackageType = PTDependency
                }

  hackageLatest name = do
    mloc <-
      getLatestHackageLocation YesRequireHackageIndex name UsePreferredVersions
    pure $ case mloc of
      Nothing -> deferToConstructPlan name
      Just loc ->
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
        , rrAddedDep =
            Just $ PLIHackage (PackageIdentifier name version) cfKey treeKey
        , rrPackageType = PTDependency
        }

  -- This is actually an error case. We _could_ pure a Left value here, but it
  -- turns out to be better to defer this until the ConstructPlan phase, and let
  -- it complain about the missing package so that we get more errors together,
  -- plus the fancy colored output from that module.
  deferToConstructPlan name = Right ResolveResult
    { rrName = name
    , rrRaw = ri
    , rrComponent = Nothing
    , rrAddedDep = Nothing
    , rrPackageType = PTDependency
    }
--------------------------------------------------------------------------------
-- Combine the ResolveResults
--------------------------------------------------------------------------------

combineResolveResults ::
     forall env. HasLogFunc env
  => [ResolveResult]
  -> RIO
       env
       ( [StyleDoc]
       , Map PackageName Target
       , Map PackageName PackageLocationImmutable
       )
combineResolveResults results = do
  addedDeps <- fmap Map.unions $ forM results $ \result ->
    case result.rrAddedDep of
      Nothing -> pure Map.empty
      Just pl -> pure $ Map.singleton result.rrName pl

  let m0 = Map.unionsWith (++) $
        map (\rr -> Map.singleton rr.rrName [rr]) results
      (errs, ms) = partitionEithers $ flip map (Map.toList m0) $
        \(name, rrs) ->
          let mcomps = map (.rrComponent) rrs in
          -- Confirm that there is either exactly 1 with no component, or that
          -- all rrs are components
          case rrs of
            [] -> assert False $
              Left $
                flow "Somehow got no rrComponent values, that can't happen."
            [rr] | isNothing rr.rrComponent ->
              Right $ Map.singleton name $ TargetAll rr.rrPackageType
            _
              | all isJust mcomps ->
                  Right $ Map.singleton name $ TargetComps $ Set.fromList $
                    catMaybes mcomps
              | otherwise -> Left $ fillSep
                  [ flow "The package"
                  , style Target $ fromPackageName name
                  , flow "was specified in multiple, incompatible ways:"
                  , fillSep $
                      mkNarrativeList (Just Target) False
                        (map rrToStyleDoc rrs)
                  ]
  pure (errs, Map.unions ms, addedDeps)
 where
  rrToStyleDoc :: ResolveResult -> StyleDoc
  rrToStyleDoc = fromString . T.unpack . (.rrRaw.unRawInput)

--------------------------------------------------------------------------------
-- OK, let's do it!
--------------------------------------------------------------------------------

parseTargets ::
     HasBuildConfig env
  => NeedTargets
  -> Bool
  -> BuildOptsCLI
  -> SMActual GlobalPackage
  -> RIO env SMTargets
parseTargets needTargets haddockDeps boptscli smActual = do
  logDebug "Parsing the targets"
  bconfig <- view buildConfigL
  workingDir <- getCurrentDir
  locals <- view $ buildConfigL . to (.smWanted.smwProject)
  let (textTargets', rawInput) = getRawInput boptscli locals

  (errs1, concat -> rawTargets) <- fmap partitionEithers $ forM rawInput $
    parseRawTargetDirs workingDir locals

  let depLocs = Map.map (.dpLocation) smActual.smaDeps

  (errs2, resolveResults) <- fmap partitionEithers $ forM rawTargets $
    resolveRawTarget smActual depLocs

  (errs3, targets, addedDeps) <- combineResolveResults resolveResults

  case concat [errs1, errs2, errs3] of
    [] -> pure ()
    errs -> prettyThrowIO $ TargetParseException errs

  case (Map.null targets, needTargets) of
    (False, _) -> pure ()
    (True, AllowNoTargets) -> pure ()
    (True, NeedTargets)
      | null textTargets' && bcImplicitGlobal bconfig ->
          prettyThrowIO $ TargetParseException
            [ fillSep
                [ flow "The specified targets matched no packages. Perhaps you \
                       \need to run"
                , style Shell (flow "stack init") <> "?"
                ]
            ]
      | null textTargets' && Map.null locals ->
          prettyThrowIO $ TargetParseException
            [ flow "The project contains no local packages (packages not \
                   \marked with 'extra-dep')."
            ]
      | otherwise -> prettyThrowIO $ TargetParseException
          [ flow "The specified targets matched no packages." ]

  addedDeps' <- mapM (additionalDepPackage haddockDeps . PLImmutable) addedDeps

  pure SMTargets
    { smtTargets = targets
    , smtDeps = addedDeps'
    }
 where
  bcImplicitGlobal bconfig =
    case bconfig.config.project of
      PCProject _ -> False
      PCGlobalProject -> True
      PCNoProject _ -> False
