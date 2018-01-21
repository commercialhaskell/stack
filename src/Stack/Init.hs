{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Stack.Init
    ( initProject
    , InitOpts (..)
    ) where

import           Stack.Prelude
import qualified Data.ByteString.Builder         as B
import qualified Data.ByteString.Char8           as BC
import qualified Data.ByteString.Lazy            as L
import qualified Data.Foldable                   as F
import qualified Data.HashMap.Strict             as HM
import qualified Data.IntMap                     as IntMap
import           Data.List                       (intercalate, intersect,
                                                  maximumBy)
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import qualified Data.Yaml                       as Yaml
import qualified Distribution.PackageDescription as C
import qualified Distribution.Text               as C
import qualified Distribution.Version            as C
import           Path
import           Path.Extra                      (toFilePathNoTrailingSep)
import           Path.IO
import qualified Paths_stack                     as Meta
import           Stack.BuildPlan
import           Stack.Config                    (getSnapshots,
                                                  makeConcreteResolver)
import           Stack.Constants
import           Stack.Snapshot                  (loadResolver)
import           Stack.Solver
import           Stack.Types.Build
import           Stack.Types.BuildPlan
import           Stack.Types.Config
import           Stack.Types.FlagName
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Resolver
import           Stack.Types.Version
import qualified System.FilePath                 as FP

-- | Generate stack.yaml
initProject
    :: (HasConfig env, HasGHCVariant env)
    => WhichSolverCmd
    -> Path Abs Dir
    -> InitOpts
    -> Maybe AbstractResolver
    -> RIO env ()
initProject whichCmd currDir initOpts mresolver = do
    let dest = currDir </> stackDotYaml

    reldest <- toFilePath `liftM` makeRelativeToCurrentDir dest

    exists <- doesFileExist dest
    when (not (forceOverwrite initOpts) && exists) $
        throwString
            ("Error: Stack configuration file " <> reldest <>
             " exists, use 'stack solver' to fix the existing config file or \
             \'--force' to overwrite it.")

    dirs <- mapM (resolveDir' . T.unpack) (searchDirs initOpts)
    let noPkgMsg =  "In order to init, you should have an existing .cabal \
                    \file. Please try \"stack new\" instead."
        find  = findCabalDirs (includeSubDirs initOpts)
        dirs' = if null dirs then [currDir] else dirs
    logInfo "Looking for .cabal or package.yaml files to use to init the project."
    cabaldirs <- Set.toList . Set.unions <$> mapM find dirs'
    (bundle, dupPkgs)  <- cabalPackagesCheck cabaldirs noPkgMsg Nothing

    (sd, flags, extraDeps, rbundle) <- getDefaultResolver whichCmd dest initOpts
                                                          mresolver bundle

    -- Kind of inefficient, since we've already parsed this value. But
    -- better to reparse in this one case than carry the unneeded data
    -- around everywhere in the codebase.
    resolver <- parseCustomLocation (Just (parent dest)) (void (sdResolver sd))

    let ignored = Map.difference bundle rbundle
        dupPkgMsg
            | dupPkgs /= [] =
                "Warning (added by new or init): Some packages were found to \
                \have names conflicting with others and have been commented \
                \out in the packages section.\n"
            | otherwise = ""

        missingPkgMsg
            | Map.size ignored > 0 =
                "Warning (added by new or init): Some packages were found to \
                \be incompatible with the resolver and have been left commented \
                \out in the packages section.\n"
            | otherwise = ""

        extraDepMsg
            | Map.size extraDeps > 0 =
                "Warning (added by new or init): Specified resolver could not \
                \satisfy all dependencies. Some external packages have been \
                \added as dependencies.\n"
            | otherwise = ""
        makeUserMsg msgs =
            let msg = concat msgs
            in if msg /= "" then
                  msg <> "You can omit this message by removing it from \
                         \stack.yaml\n"
                 else ""

        userMsg = makeUserMsg [dupPkgMsg, missingPkgMsg, extraDepMsg]

        gpds = Map.elems $ fmap snd rbundle
        p = Project
            { projectUserMsg = if userMsg == "" then Nothing else Just userMsg
            , projectPackages = pkgs
            , projectDependencies = map
                (\(n, v) -> PLIndex $ PackageIdentifierRevision (PackageIdentifier n v) CFILatest)
                (Map.toList extraDeps)
            , projectFlags = removeSrcPkgDefaultFlags gpds flags
            , projectResolver = resolver
            , projectCompiler = Nothing
            , projectExtraPackageDBs = []
            }

        makeRelDir dir =
            case stripProperPrefix currDir dir of
                Nothing
                    | currDir == dir -> "."
                    | otherwise -> assert False $ toFilePathNoTrailingSep dir
                Just rel -> toFilePathNoTrailingSep rel

        makeRel = fmap toFilePath . makeRelativeToCurrentDir

        pkgs = map toPkg $ Map.elems (fmap (parent . fst) rbundle)
        toPkg dir = PLFilePath $ makeRelDir dir
        indent t = T.unlines $ fmap ("    " <>) (T.lines t)

    logInfo $ "Initialising configuration using resolver: " <> display (sdResolverName sd)
    logInfo $ "Total number of user packages considered: "
               <> display (Map.size bundle + length dupPkgs)

    when (dupPkgs /= []) $ do
        logWarn $ "Warning! Ignoring "
                   <> displayShow (length dupPkgs)
                   <> " duplicate packages:"
        rels <- mapM makeRel dupPkgs
        logWarn $ display $ indent $ showItems rels

    when (Map.size ignored > 0) $ do
        logWarn $ "Warning! Ignoring "
                   <> displayShow (Map.size ignored)
                   <> " packages due to dependency conflicts:"
        rels <- mapM makeRel (Map.elems (fmap fst ignored))
        logWarn $ display $ indent $ showItems rels

    when (Map.size extraDeps > 0) $ do
        logWarn $ "Warning! " <> displayShow (Map.size extraDeps)
                   <> " external dependencies were added."
    logInfo $
        (if exists then "Overwriting existing configuration file: "
         else "Writing configuration to file: ")
        <> fromString reldest
    liftIO $ L.writeFile (toFilePath dest)
           $ B.toLazyByteString
           $ renderStackYaml p
               (Map.elems $ fmap (makeRelDir . parent . fst) ignored)
               (map (makeRelDir . parent) dupPkgs)
    logInfo "All done."

-- | Render a stack.yaml file with comments, see:
-- https://github.com/commercialhaskell/stack/issues/226
renderStackYaml :: Project -> [FilePath] -> [FilePath] -> B.Builder
renderStackYaml p ignoredPackages dupPackages =
    case Yaml.toJSON p of
        Yaml.Object o -> renderObject o
        _ -> assert False $ B.byteString $ Yaml.encode p
  where
    renderObject o =
           B.byteString headerHelp
        <> B.byteString "\n\n"
        <> F.foldMap (goComment o) comments
        <> goOthers (o `HM.difference` HM.fromList comments)
        <> B.byteString footerHelp

    goComment o (name, comment) =
        case (convert <$> HM.lookup name o) <|> nonPresentValue name of
            Nothing -> assert (name == "user-message") mempty
            Just v ->
                B.byteString comment <>
                B.byteString "\n" <>
                v <>
                if name == "packages" then commentedPackages else "" <>
                B.byteString "\n"
      where
        convert v = B.byteString (Yaml.encode $ Yaml.object [(name, v)])

        -- Some fields in stack.yaml are optional and may not be
        -- generated. For these, we provided commented out dummy
        -- values to go along with the comments.
        nonPresentValue "extra-deps" = Just "# extra-deps: []\n"
        nonPresentValue "flags" = Just "# flags: {}\n"
        nonPresentValue "extra-package-dbs" = Just "# extra-package-dbs: []\n"
        nonPresentValue _ = Nothing

    commentLine l | null l = "#"
                  | otherwise = "# " ++ l
    commentHelp = BC.pack .  intercalate "\n" . map commentLine
    commentedPackages =
        let ignoredComment = commentHelp
                [ "The following packages have been ignored due to incompatibility with the"
                , "resolver compiler, dependency conflicts with other packages"
                , "or unsatisfied dependencies."
                ]
            dupComment = commentHelp
                [ "The following packages have been ignored due to package name conflict "
                , "with other packages."
                ]
        in commentPackages ignoredComment ignoredPackages
           <> commentPackages dupComment dupPackages

    commentPackages comment pkgs
        | pkgs /= [] =
               B.byteString comment
            <> B.byteString "\n"
            <> B.byteString (BC.pack $ concat
                 $ map (\x -> "#- " ++ x ++ "\n") pkgs ++ ["\n"])
        | otherwise = ""

    goOthers o
        | HM.null o = mempty
        | otherwise = assert False $ B.byteString $ Yaml.encode o

    -- Per Section Help
    comments =
        [ ("user-message"     , userMsgHelp)
        , ("resolver"         , resolverHelp)
        , ("packages"         , packageHelp)
        , ("extra-deps"       , "# Dependency packages to be pulled from upstream that are not in the resolver\n# using the same syntax as the packages field.\n# (e.g., acme-missiles-0.3)")
        , ("flags"            , "# Override default flag values for local packages and extra-deps")
        , ("extra-package-dbs", "# Extra package databases containing global packages")
        ]

    -- Help strings
    headerHelp = commentHelp
        [ "This file was automatically generated by 'stack init'"
        , ""
        , "Some commonly used options have been documented as comments in this file."
        , "For advanced use and comprehensive documentation of the format, please see:"
        , "https://docs.haskellstack.org/en/stable/yaml_configuration/"
        ]

    resolverHelp = commentHelp
        [ "Resolver to choose a 'specific' stackage snapshot or a compiler version."
        , "A snapshot resolver dictates the compiler version and the set of packages"
        , "to be used for project dependencies. For example:"
        , ""
        , "resolver: lts-3.5"
        , "resolver: nightly-2015-09-21"
        , "resolver: ghc-7.10.2"
        , "resolver: ghcjs-0.1.0_ghc-7.10.2"
        , ""
        , "The location of a snapshot can be provided as a file or url. Stack assumes"
        , "a snapshot provided as a file might change, whereas a url resource does not."
        , ""
        , "resolver: ./custom-snapshot.yaml"
        , "resolver: https://example.com/snapshots/2018-01-01.yaml"
        ]

    userMsgHelp = commentHelp
        [ "A warning or info to be displayed to the user on config load." ]

    packageHelp = commentHelp
        [ "User packages to be built."
        , "Various formats can be used as shown in the example below."
        , ""
        , "packages:"
        , "- some-directory"
        , "- https://example.com/foo/bar/baz-0.0.2.tar.gz"
        , "- location:"
        , "   git: https://github.com/commercialhaskell/stack.git"
        , "   commit: e7b331f14bcffb8367cd58fbfc8b40ec7642100a"
        , "- location: https://github.com/commercialhaskell/stack/commit/e7b331f14bcffb8367cd58fbfc8b40ec7642100a"
        , " subdirs:"
        , " - auto-update"
        , " - wai"
        ]

    footerHelp =
        let major = toCabalVersion
                    $ toMajorVersion $ fromCabalVersion $ C.mkVersion' Meta.version
        in commentHelp
        [ "Control whether we use the GHC we find on the path"
        , "system-ghc: true"
        , ""
        , "Require a specific version of stack, using version ranges"
        , "require-stack-version: -any # Default"
        , "require-stack-version: \""
          ++ C.display (C.orLaterVersion major) ++ "\""
        , ""
        , "Override the architecture used by stack, especially useful on Windows"
        , "arch: i386"
        , "arch: x86_64"
        , ""
        , "Extra directories used by stack for building"
        , "extra-include-dirs: [/path/to/dir]"
        , "extra-lib-dirs: [/path/to/dir]"
        , ""
        , "Allow a newer minor version of GHC than the snapshot specifies"
        , "compiler-check: newer-minor"
        ]

getSnapshots' :: HasConfig env => RIO env Snapshots
getSnapshots' = do
    getSnapshots `catchAny` \e -> do
        logError $
            "Unable to download snapshot list, and therefore could " <>
            "not generate a stack.yaml file automatically"
        logError $
            "This sometimes happens due to missing Certificate Authorities " <>
            "on your system. For more information, see:"
        logError ""
        logError "    https://github.com/commercialhaskell/stack/issues/234"
        logError ""
        logError "You can try again, or create your stack.yaml file by hand. See:"
        logError ""
        logError "    http://docs.haskellstack.org/en/stable/yaml_configuration/"
        logError ""
        logError $ "Exception was: " <> displayShow e
        throwString ""

-- | Get the default resolver value
getDefaultResolver
    :: (HasConfig env, HasGHCVariant env)
    => WhichSolverCmd
    -> Path Abs File   -- ^ stack.yaml
    -> InitOpts
    -> Maybe AbstractResolver
    -> Map PackageName (Path Abs File, C.GenericPackageDescription)
       -- ^ Src package name: cabal dir, cabal package description
    -> RIO env
         ( SnapshotDef
         , Map PackageName (Map FlagName Bool)
         , Map PackageName Version
         , Map PackageName (Path Abs File, C.GenericPackageDescription))
       -- ^ ( Resolver
       --   , Flags for src packages and extra deps
       --   , Extra dependencies
       --   , Src packages actually considered)
getDefaultResolver whichCmd stackYaml initOpts mresolver bundle = do
    sd <- maybe selectSnapResolver (makeConcreteResolver (Just root) >=> loadResolver) mresolver
    getWorkingResolverPlan whichCmd stackYaml initOpts bundle sd
    where
        root = parent stackYaml
        -- TODO support selecting best across regular and custom snapshots
        selectSnapResolver = do
            let gpds = Map.elems (fmap snd bundle)
            snaps <- fmap getRecommendedSnapshots getSnapshots'
            (s, r) <- selectBestSnapshot (parent stackYaml) gpds snaps
            case r of
                BuildPlanCheckFail {} | not (omitPackages initOpts)
                        -> throwM (NoMatchingSnapshot whichCmd snaps)
                _ -> return s

getWorkingResolverPlan
    :: (HasConfig env, HasGHCVariant env)
    => WhichSolverCmd
    -> Path Abs File   -- ^ stack.yaml
    -> InitOpts
    -> Map PackageName (Path Abs File, C.GenericPackageDescription)
       -- ^ Src package name: cabal dir, cabal package description
    -> SnapshotDef
    -> RIO env
         ( SnapshotDef
         , Map PackageName (Map FlagName Bool)
         , Map PackageName Version
         , Map PackageName (Path Abs File, C.GenericPackageDescription))
       -- ^ ( SnapshotDef
       --   , Flags for src packages and extra deps
       --   , Extra dependencies
       --   , Src packages actually considered)
getWorkingResolverPlan whichCmd stackYaml initOpts bundle sd = do
    logInfo $ "Selected resolver: " <> display (sdResolverName sd)
    go bundle
    where
        go info = do
            eres <- checkBundleResolver whichCmd stackYaml initOpts info sd
            -- if some packages failed try again using the rest
            case eres of
                Right (f, edeps)-> return (sd, f, edeps, info)
                Left ignored
                    | Map.null available -> do
                        logWarn "*** Could not find a working plan for any of \
                                 \the user packages.\nProceeding to create a \
                                 \config anyway."
                        return (sd, Map.empty, Map.empty, Map.empty)
                    | otherwise -> do
                        when (Map.size available == Map.size info) $
                            error "Bug: No packages to ignore"

                        if length ignored > 1 then do
                          logWarn "*** Ignoring packages:"
                          logWarn $ display $ indent $ showItems ignored
                        else
                          logWarn $ "*** Ignoring package: "
                                 <> display
                                      (case ignored of
                                        [] -> error "getWorkingResolverPlan.head"
                                        x:_ -> x)

                        go available
                    where
                      indent t   = T.unlines $ fmap ("    " <>) (T.lines t)
                      isAvailable k _ = k `notElem` ignored
                      available       = Map.filterWithKey isAvailable info

checkBundleResolver
    :: (HasConfig env, HasGHCVariant env)
    => WhichSolverCmd
    -> Path Abs File   -- ^ stack.yaml
    -> InitOpts
    -> Map PackageName (Path Abs File, C.GenericPackageDescription)
       -- ^ Src package name: cabal dir, cabal package description
    -> SnapshotDef
    -> RIO env
         (Either [PackageName] ( Map PackageName (Map FlagName Bool)
                               , Map PackageName Version))
checkBundleResolver whichCmd stackYaml initOpts bundle sd = do
    result <- checkSnapBuildPlanActual (parent stackYaml) gpds Nothing sd
    case result of
        BuildPlanCheckOk f -> return $ Right (f, Map.empty)
        BuildPlanCheckPartial f e -> do
            shouldUseSolver <- case (resolver, initOpts) of
                (_, InitOpts { useSolver = True }) -> return True
                (ResolverCompiler _, _) -> do
                    logInfo "Using solver because a compiler resolver was specified."
                    return True
                _ -> return False
            if shouldUseSolver
                then do
                    warnPartial result
                    solve f
                else if omitPackages initOpts
                    then do
                        warnPartial result
                        logWarn "*** Omitting packages with unsatisfied dependencies"
                        return $ Left $ failedUserPkgs e
                    else throwM $ ResolverPartial whichCmd (sdResolverName sd) (show result)
        BuildPlanCheckFail _ e _
            | omitPackages initOpts -> do
                logWarn $ "*** Resolver compiler mismatch: "
                           <> display (sdResolverName sd)
                logWarn $ display $ indent $ T.pack $ show result
                return $ Left $ failedUserPkgs e
            | otherwise -> throwM $ ResolverMismatch whichCmd (sdResolverName sd) (show result)
    where
      resolver = sdResolver sd
      indent t  = T.unlines $ fmap ("    " <>) (T.lines t)
      warnPartial res = do
          logWarn $ "*** Resolver " <> display (sdResolverName sd)
                      <> " will need external packages: "
          logWarn $ display $ indent $ T.pack $ show res

      failedUserPkgs e = Map.keys $ Map.unions (Map.elems (fmap deNeededBy e))

      gpds        = Map.elems (fmap snd bundle)
      solve flags = do
          let cabalDirs      = map parent (Map.elems (fmap fst bundle))
              srcConstraints = mergeConstraints (gpdPackages gpds) flags

          eresult <- solveResolverSpec stackYaml cabalDirs
                                       (sd, srcConstraints, Map.empty)
          case eresult of
              Right (src, ext) ->
                  return $ Right (fmap snd (Map.union src ext), fmap fst ext)
              Left packages
                  | omitPackages initOpts, srcpkgs /= []-> do
                      pkg <- findOneIndependent srcpkgs flags
                      return $ Left [pkg]
                  | otherwise -> throwM (SolverGiveUp giveUpMsg)
                  where srcpkgs = Map.keys bundle `intersect` packages

      -- among a list of packages find one on which none among the rest of the
      -- packages depend. This package is a good candidate to be removed from
      -- the list of packages when there is conflict in dependencies among this
      -- set of packages.
      findOneIndependent packages flags = do
          platform <- view platformL
          (compiler, _) <- getResolverConstraints Nothing stackYaml sd
          let getGpd pkg = snd (fromMaybe (error "findOneIndependent: getGpd") (Map.lookup pkg bundle))
              getFlags pkg = fromMaybe (error "fromOneIndependent: getFlags") (Map.lookup pkg flags)
              deps pkg = gpdPackageDeps (getGpd pkg) compiler platform
                                        (getFlags pkg)
              allDeps = concatMap (Map.keys . deps) packages
              isIndependent pkg = pkg `notElem` allDeps

              -- prefer to reject packages in deeper directories
              path pkg = fst (fromMaybe (error "findOneIndependent: path") (Map.lookup pkg bundle))
              pathlen = length . FP.splitPath . toFilePath . path
              maxPathlen = maximumBy (compare `on` pathlen)

          return $ maxPathlen (filter isIndependent packages)

      giveUpMsg = concat
          [ "    - Use '--omit-packages to exclude conflicting package(s).\n"
          , "    - Tweak the generated "
          , toFilePath stackDotYaml <> " and then run 'stack solver':\n"
          , "        - Add any missing remote packages.\n"
          , "        - Add extra dependencies to guide solver.\n"
          , "    - Update external packages with 'stack update' and try again.\n"
          ]

getRecommendedSnapshots :: Snapshots -> NonEmpty SnapName
getRecommendedSnapshots snapshots =
    -- in order - Latest LTS, Latest Nightly, all LTS most recent first
    case NonEmpty.nonEmpty ltss of
        Just (mostRecent :| older)
            -> mostRecent :| (nightly : older)
        Nothing
            -> nightly :| []
  where
    ltss = map (uncurry LTS) (IntMap.toDescList $ snapshotsLts snapshots)
    nightly = Nightly (snapshotsNightly snapshots)

data InitOpts = InitOpts
    { searchDirs     :: ![T.Text]
    -- ^ List of sub directories to search for .cabal files
    , useSolver      :: Bool
    -- ^ Use solver to determine required external dependencies
    , omitPackages   :: Bool
    -- ^ Exclude conflicting or incompatible user packages
    , forceOverwrite :: Bool
    -- ^ Overwrite existing stack.yaml
    , includeSubDirs :: Bool
    -- ^ If True, include all .cabal files found in any sub directories
    }
