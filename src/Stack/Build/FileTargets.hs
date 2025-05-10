{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Stack.Build.FileTargets
License     : BSD-3-Clause
-}

module Stack.Build.FileTargets
  ( findFileTargets
  , getAllLocalTargets
  , getAllNonLocalTargets
  , loadGhciPkgDescs
  , getGhciPkgInfos
  , optsAndMacros
  , wantedPackageComponents
  ) where

import           Control.Monad.State.Strict ( State, execState, get, modify )
import qualified Data.ByteString.Char8 as S8
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Distribution.PackageDescription as C
import           Path ( (</>), parent, parseRelFile )
import           Path.IO ( doesFileExist )
import           Stack.Build.Installed ( getInstalled )
import           Stack.Constants ( stackProgName' )
import           Stack.Package
                   ( buildableExes, buildableForeignLibs, buildableSubLibs
                   , buildableTestSuites, buildableBenchmarks
                   , hasBuildableMainLibrary
                   , getPackageOpts, listOfPackageDeps
                   , packageFromPackageDescription, readDotBuildinfo
                   , resolvePackageDescription
                   )
import           Stack.PackageFile ( getPackageFile )
import           Stack.Prelude
import           Stack.Types.BuildOpts ( BuildOpts (..) )
import           Stack.Types.BuildOptsCLI ( ApplyCLIFlag (..) )
import           Stack.Types.Config ( Config (..), HasConfig (..), buildOptsL )
import           Stack.Types.EnvConfig
                   ( EnvConfig (..), HasEnvConfig (..), actualCompilerVersionL )
import           Stack.Types.GhciPkg
                   ( GhciPkgDesc (..), GhciPkgInfo (..), unionModuleMaps )
import           Stack.Types.Installed ( InstallMap, InstalledMap )
import           Stack.Types.NamedComponent
                   ( NamedComponent (..), displayPkgComponent, isCLib )
import           Stack.Types.Package
                   ( BuildInfoOpts (..), LocalPackage (..), Package (..)
                   , PackageConfig (..), dotCabalCFilePath, dotCabalGetPath
                   , dotCabalMainPath
                   )
import           Stack.Types.PackageFile ( PackageComponentFile (..) )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.SourceMap
                   ( CommonPackage (..), DepPackage (..), PackageType (..)
                   , ProjectPackage (..), SourceMap (..), Target (..)
                   , unionTargets
                   )

-- | Given a list of project packages and a list of absolute paths to files,
-- seek to identify which component of which project package each file relates
-- to (if any).
findFileTargets ::
     HasEnvConfig env
  => [LocalPackage]
     -- ^ All project packages
  -> [Path Abs File]
     -- ^ File targets to find
  -> RIO
       env
       ( Map PackageName Target
       , Maybe (Map PackageName [Path Abs File], [Path Abs File])
       )
findFileTargets locals fileTargets = do
  filePackages <- forM locals $ \lp -> do
    PackageComponentFile _ compFiles _ _ <- getPackageFile lp.package lp.cabalFP
    pure (lp, M.map (map dotCabalGetPath) compFiles)
  let foundFileTargetComponents :: [(Path Abs File, [(PackageName, NamedComponent)])]
      foundFileTargetComponents =
        map (\fp -> (fp, ) $ L.sort $
                    concatMap (\(lp, files) -> map ((lp.package.name,) . fst)
                                                   (filter (elem fp . snd) (M.toList files))
                              ) filePackages
            ) fileTargets
  results <- forM foundFileTargetComponents $ \(fp, xs) ->
    case xs of
      [] -> do
        prettyWarnL
          [ flow "Couldn't find a component for file target"
          , pretty fp <> "."
          , flow "This means that the correct GHC options might not be used. \
                 \Attempting to load the file anyway."
          ]
        pure $ Left fp
      [x] -> do
        prettyInfoL
          [ flow "Using configuration for"
          , displayPkgComponent x
          , flow "to load"
          , pretty fp
          ]
        pure $ Right (fp, x)
      (x:_) -> do
        prettyWarn $
             fillSep
               [ flow "Multiple components contain file target"
               , pretty fp <> ":"
               , fillSep $ punctuate "," (map displayPkgComponent xs)
               ]
          <> line
          <> fillSep
               [ flow "Guessing the first one,"
               , displayPkgComponent x <> "."
               ]
        pure $ Right (fp, x)
  let (extraFiles, associatedFiles) = partitionEithers results
      targetMap =
          foldl' unionTargets M.empty $
          map (\(_, (name, comp)) -> M.singleton name (TargetComps (S.singleton comp)))
              associatedFiles
      infoMap =
          foldl' (M.unionWith (<>)) M.empty $
          map (\(fp, (name, _)) -> M.singleton name [fp])
              associatedFiles
  pure (targetMap, Just (infoMap, extraFiles))

-- | Yields all of the targets that are local, those that are directly wanted
-- and those that are extra dependencies to load.
getAllLocalTargets ::
     HasEnvConfig env
  => Bool
     -- ^ Load local dependencies?
  -> Map PackageName Target
     -- ^ Targets.
  -> Maybe (Map PackageName Target)
     -- ^ Optional @--main-is@ targets, applicable only to Stack's @ghci@ and
     -- @repl@ commands.
  -> Map PackageName LocalPackage
  -> RIO
       env
       ( [(PackageName, (Path Abs File, Target))]
         -- Directly wanted.
       , [(PackageName, (Path Abs File, Target))]
         -- Extra dependencies to load.
       )
getAllLocalTargets loadLocalDeps targets0 mainIsTargets localMap = do
  -- Use the 'mainIsTargets' as normal targets, for CLI concision. See #1845.
  -- This is a little subtle - we need to do the target parsing independently in
  -- order to handle the case where no targets are specified.
  let targets = maybe targets0 (unionTargets targets0) mainIsTargets
  packages <- view $ envConfigL . to (.sourceMap.project)
  -- Find all of the packages that are directly demanded by the targets.
  let directlyWanted = flip mapMaybe (M.toList packages) $
        \(name, pp) ->
              case M.lookup name targets of
                Just simpleTargets -> Just (name, (pp.cabalFP, simpleTargets))
                Nothing -> Nothing
      -- Figure out
      extraLoadDeps = getExtraLoadDeps loadLocalDeps localMap directlyWanted
  pure (directlyWanted, extraLoadDeps)

-- | Yields the names of all the packages where the target is (a) not a project
-- package and (b) a dependency.
getAllNonLocalTargets :: Map PackageName Target -> RIO env [PackageName]
getAllNonLocalTargets targets = do
  let isNonLocal (TargetAll PTDependency) = True
      isNonLocal _ = False
  pure $ map fst $ filter (isNonLocal . snd) (M.toList targets)

-- | For the given list of local targets, yields the corresponding list of
-- v'GhciPkgDesc'.
loadGhciPkgDescs ::
     HasEnvConfig env
  => Map ApplyCLIFlag (Map FlagName Bool)
     -- ^ Flags specified on the command line.
  -> [(PackageName, (Path Abs File, Target))]
     -- ^ Local targets.
  -> RIO env [GhciPkgDesc]
loadGhciPkgDescs cliFlags localTargets =
  forM localTargets $ \(name, (cabalFP, target)) ->
    loadGhciPkgDesc cliFlags name cabalFP target

-- | Load package description information for a ghci target.
loadGhciPkgDesc ::
     HasEnvConfig env
  => Map ApplyCLIFlag (Map FlagName Bool)
     -- ^ Flags specified on the command line.
  -> PackageName
  -> Path Abs File
  -> Target
  -> RIO env GhciPkgDesc
loadGhciPkgDesc cliFlags name cabalFP target = do
  econfig <- view envConfigL
  compilerVersion <- view actualCompilerVersionL
  let sm = econfig.sourceMap
      -- Currently this source map is being build with
      -- the default targets
      sourceMapGhcOptions = fromMaybe [] $
        ((.projectCommon.ghcOptions) <$> M.lookup name sm.project)
        <|>
        ((.depCommon.ghcOptions) <$> M.lookup name sm.deps)
      sourceMapCabalConfigOpts = fromMaybe [] $
        ( (.projectCommon.cabalConfigOpts) <$> M.lookup name sm.project)
        <|>
        ((.depCommon.cabalConfigOpts) <$> M.lookup name sm.deps)
      sourceMapFlags =
        maybe mempty (.projectCommon.flags) $ M.lookup name sm.project
      config = PackageConfig
        { enableTests = True
        , enableBenchmarks = True
        , flags = getCliFlags <> sourceMapFlags
        , ghcOptions = sourceMapGhcOptions
        , cabalConfigOpts = sourceMapCabalConfigOpts
        , compilerVersion = compilerVersion
        , platform = view platformL econfig
        }
  -- TODO we've already parsed this information, otherwise we wouldn't have
  -- figured out the cabalFP already. In the future: retain that
  -- GenericPackageDescription in the relevant data structures to avoid
  -- reparsing.
  (gpdio, _name, _cabalFP) <-
    loadCabalFilePath (Just stackProgName') (parent cabalFP)
  gpkgdesc <- liftIO $ gpdio YesPrintWarnings

  -- Source the package's *.buildinfo file created by configure if any. See
  -- https://www.haskell.org/cabal/users-guide/developing-packages.html#system-dependent-parameters
  buildinfofp <- parseRelFile (packageNameString name ++ ".buildinfo")
  hasDotBuildinfo <- doesFileExist (parent cabalFP </> buildinfofp)
  let mbuildinfofp
        | hasDotBuildinfo = Just (parent cabalFP </> buildinfofp)
        | otherwise = Nothing
  mbuildinfo <- forM mbuildinfofp readDotBuildinfo
  let pdp = resolvePackageDescription config gpkgdesc
      package =
        packageFromPackageDescription config (C.genPackageFlags gpkgdesc) $
          maybe pdp (`C.updatePackageDescription` pdp) mbuildinfo
  pure GhciPkgDesc
    { package
    , cabalFP
    , target
    }
 where
  -- | All CLI Cabal flags for a package.
  getCliFlags :: Map FlagName Bool
  getCliFlags = Map.unions
    [ Map.findWithDefault Map.empty (ACFByName name) cliFlags
    , Map.findWithDefault Map.empty ACFAllProjectPackages cliFlags
    ]

-- | Yields the GHC options that are incompatible with GHCi, the other GHC
-- options (other than those that add the macros file as an @-include@), and
-- the content of the macros file.
optsAndMacros ::
     HasEnvConfig env
  => Maybe Bool
     -- ^ Should hide package options
  -> [(PackageName, (Path Abs File, Target))]
  -> [GhciPkgInfo]
  -> [PackageName]
  -> Map PackageName (Seq NamedComponent)
  -> RIO
       env
       ( [String]
         -- GHC options that are incompatible with GHCi.
       , [String]
         -- Other GHC options, other than those that add the macros file as an
         -- @-include@.
       , ByteString
         -- The content of the macros file.
       )
optsAndMacros
    hidePackages
    targets
    pkgs
    exposePackages
    exposeInternalDep
  = do
      config <- view configL
      let subDepsPackageUnhide pName deps =
            if null deps then [] else ["-package", fromPackageName pName]
          pkgopts = hidePkgOpts ++ genOpts ++ ghcOpts
          shouldHidePackages = fromMaybe
            (not (null pkgs && null exposePackages))
            hidePackages
          hidePkgOpts =
            if shouldHidePackages
              then
                   ["-hide-all-packages"]
                -- This is necessary, because current versions of ghci will
                -- entirely fail to start if base isn't visible. This is because
                -- it tries to use the interpreter to set buffering options on
                -- standard IO.
                ++ (if null targets then ["-package", "base"] else [])
                ++ concatMap
                     (\n -> ["-package", packageNameString n])
                     exposePackages
                ++ M.foldMapWithKey subDepsPackageUnhide exposeInternalDep
              else []
          oneWordOpts bio
            | shouldHidePackages = bio.oneWordOpts ++ bio.packageFlags
            | otherwise = bio.oneWordOpts
          genOpts = nubOrd
            (concatMap (concatMap (oneWordOpts . snd) . (.opts)) pkgs)
          (omittedOpts, ghcOpts) = L.partition badForGhci $
               concatMap (concatMap ((.opts) . snd) . (.opts)) pkgs
            ++ map
                 T.unpack
                 (  fold config.ghcOptionsByCat
                    -- ^ include everything, locals, and targets
                 ++ concatMap (getUserOptions . (.name)) pkgs
                 )
          getUserOptions pkg =
            M.findWithDefault [] pkg config.ghcOptionsByName
          badForGhci x =
               L.isPrefixOf "-O" x
            || elem x (words "-debug -threaded -ticky -static -Werror")
      bs <- macrosFileContents pkgs
      pure
        ( omittedOpts
          -- This initial "-i" resets the include directories to not
          -- include CWD. If there aren't any packages, CWD is included.
        , (if null pkgs then id else ("-i" : )) pkgopts
        , bs
        )

-- Adds in intermediate dependencies between ghci targets. Note that it will
-- return a Lib component for these intermediate dependencies even if they don't
-- have a library (but that's fine for the usage within this module).
--
-- If 'True' is passed for loadAllDeps, this loads all local deps, even if they
-- aren't intermediate.
getExtraLoadDeps ::
     Bool
  -> Map PackageName LocalPackage
  -> [(PackageName, (Path Abs File, Target))]
  -> [(PackageName, (Path Abs File, Target))]
getExtraLoadDeps loadAllDeps localMap targets =
  M.toList $
  (\mp -> foldl' (flip M.delete) mp (map fst targets)) $
  M.mapMaybe id $
  execState (mapM_ (mapM_ go . getDeps . fst) targets)
            (M.fromList (map (second Just) targets))
 where
  getDeps :: PackageName -> [PackageName]
  getDeps name =
    case M.lookup name localMap of
      Just lp -> listOfPackageDeps lp.package -- FIXME just Local?
      _ -> []
  go ::
       PackageName
    -> State (Map PackageName (Maybe (Path Abs File, Target))) Bool
  go name = do
    cache <- get
    case (M.lookup name cache, M.lookup name localMap) of
      (Just (Just _), _) -> pure True
      (Just Nothing, _) | not loadAllDeps -> pure False
      (_, Just lp) -> do
        let deps = listOfPackageDeps lp.package
        shouldLoad <- or <$> mapM go deps
        if shouldLoad
          then do
            modify (M.insert name (Just (lp.cabalFP, TargetComps (S.singleton CLib))))
            pure True
          else do
            modify (M.insert name Nothing)
            pure False
      (_, _) -> pure False

macrosFileContents ::
     HasTerm env
  => [GhciPkgInfo]
  -> RIO env ByteString
macrosFileContents pkgs = do
  fps <- fmap (nubOrd . concatMap catMaybes) $
    forM pkgs $ \pkg -> forM pkg.opts $ \(_, bio) -> do
      let cabalMacros = bio.cabalMacros
      exists <- liftIO $ doesFileExist cabalMacros
      if exists
        then pure $ Just cabalMacros
        else do
          prettyWarnL ["Didn't find expected autogen file:", pretty cabalMacros]
          pure Nothing
  files <- liftIO $ mapM (S8.readFile . toFilePath) fps
  pure $ if null files
    then mempty
    else
      S8.concat $ map
        (<> "\n#undef CURRENT_PACKAGE_KEY\n#undef CURRENT_COMPONENT_ID\n")
        files

getGhciPkgInfos ::
     HasEnvConfig env
  => InstallMap
  -> [PackageName]
  -> Maybe (Map PackageName [Path Abs File])
  -> [GhciPkgDesc]
  -> RIO env [GhciPkgInfo]
getGhciPkgInfos installMap addPkgs mfileTargets localTargets = do
  (installedMap, _, _, _) <- getInstalled installMap
  let localLibs =
        [ desc.package.name
        | desc <- localTargets
        , hasLocalComp isCLib desc.target
        ]
  forM localTargets $ \pkgDesc ->
    makeGhciPkgInfo installMap installedMap localLibs addPkgs mfileTargets pkgDesc

hasLocalComp :: (NamedComponent -> Bool) -> Target -> Bool
hasLocalComp p t = case t of
  TargetComps s -> any p (S.toList s)
  TargetAll PTProject -> True
  _ -> False

-- | Make information necessary to load the given package in GHCi.
makeGhciPkgInfo ::
     HasEnvConfig env
  => InstallMap
  -> InstalledMap
  -> [PackageName]
  -> [PackageName]
  -> Maybe (Map PackageName [Path Abs File])
  -> GhciPkgDesc
  -> RIO env GhciPkgInfo
makeGhciPkgInfo installMap installedMap locals addPkgs mfileTargets pkgDesc = do
  bopts <- view buildOptsL
  let pkg = pkgDesc.package
      cabalFP = pkgDesc.cabalFP
      target = pkgDesc.target
      name = pkg.name
  (mods, files, opts) <-
    getPackageOpts pkg installMap installedMap locals addPkgs cabalFP
  let filteredOpts = filterWanted opts
      filterWanted = M.filterWithKey (\k _ -> k `S.member` allWanted)
      allWanted = wantedPackageComponents bopts target pkg
  pure GhciPkgInfo
    { name
    , opts = M.toList filteredOpts
    , dir = parent cabalFP
    , modules = unionModuleMaps $
        map
          ( \(comp, mp) -> M.map
              (\fp -> M.singleton fp (S.singleton (pkg.name, comp)))
              mp
          )
          (M.toList (filterWanted mods))
    , mainIs = M.map (mapMaybe dotCabalMainPath) files
    , cFiles = mconcat
        (M.elems (filterWanted (M.map (mapMaybe dotCabalCFilePath) files)))
    , targetFiles = mfileTargets >>= M.lookup name
    , package = pkg
    }

-- | For the given build options, target and package, yields a set of the wanted
-- components.
--
-- NOTE: this should make the same choices as the components code in
-- 'Stack.Build.Source.loadLocalPackage'. Unfortunately for now we reiterate
-- this logic (differently).
wantedPackageComponents :: BuildOpts -> Target -> Package -> Set NamedComponent
wantedPackageComponents _ (TargetComps cs) _ = cs
wantedPackageComponents bopts (TargetAll PTProject) pkg =
     ( if hasBuildableMainLibrary pkg
         then S.insert CLib (S.mapMonotonic CSubLib buildableForeignLibs')
         else S.empty
     )
  <> S.mapMonotonic CExe buildableExes'
  <> S.mapMonotonic CSubLib buildableSubLibs'
  <> ( if bopts.tests
         then S.mapMonotonic CTest buildableTestSuites'
         else S.empty
     )
  <> ( if bopts.benchmarks
         then S.mapMonotonic CBench buildableBenchmarks'
         else S.empty
     )
 where
  buildableForeignLibs' = buildableForeignLibs pkg
  buildableSubLibs' = buildableSubLibs pkg
  buildableExes' = buildableExes pkg
  buildableTestSuites' = buildableTestSuites pkg
  buildableBenchmarks' = buildableBenchmarks pkg
wantedPackageComponents _ _ _ = S.empty
