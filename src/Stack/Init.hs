{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Stack.Init
    ( initProject
    , InitOpts (..)
    ) where

import           Stack.Prelude
import qualified Data.ByteString.Builder         as B
import qualified Data.ByteString.Char8           as BC
import qualified Data.Foldable                   as F
import qualified Data.HashMap.Strict             as HM
import qualified Data.IntMap                     as IntMap
import           Data.List.Extra                 (groupSortOn)
import qualified Data.List.NonEmpty              as NonEmpty
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import qualified Data.Text.Normalize             as T (normalize , NormalizationMode(NFC))
import qualified Data.Yaml                       as Yaml
import qualified Distribution.PackageDescription as C
import qualified Distribution.Text               as C
import qualified Distribution.Version            as C
import           Path
import           Path.Extra                      (toFilePathNoTrailingSep)
import           Path.Find                       (findFiles)
import           Path.IO                         hiding (findFiles)
import qualified Paths_stack                     as Meta
import qualified RIO.FilePath                    as FP
import           RIO.List                        ((\\), intercalate, intersperse,
                                                  isSuffixOf, isPrefixOf)
import           RIO.List.Partial                (minimumBy)
import           Stack.BuildPlan
import           Stack.Config                    (getSnapshots,
                                                  makeConcreteResolver)
import           Stack.Constants
import           Stack.SourceMap
import           Stack.Types.Config
import           Stack.Types.Resolver
import           Stack.Types.Version

-- | Generate stack.yaml
initProject
    :: (HasConfig env, HasGHCVariant env)
    => Path Abs Dir
    -> InitOpts
    -> Maybe AbstractResolver
    -> RIO env ()
initProject currDir initOpts mresolver = do
    let dest = currDir </> stackDotYaml

    reldest <- toFilePath `liftM` makeRelativeToCurrentDir dest

    exists <- doesFileExist dest
    when (not (forceOverwrite initOpts) && exists) $
        throwString
            ("Error: Stack configuration file " <> reldest <>
             " exists, use '--force' to overwrite it.")

    dirs <- mapM (resolveDir' . T.unpack) (searchDirs initOpts)
    let find  = findCabalDirs (includeSubDirs initOpts)
        dirs' = if null dirs then [currDir] else dirs
    logInfo "Looking for .cabal or package.yaml files to use to init the project."
    cabaldirs <- Set.toList . Set.unions <$> mapM find dirs'
    (bundle, dupPkgs)  <- cabalPackagesCheck cabaldirs Nothing
    let makeRelDir dir =
            case stripProperPrefix currDir dir of
                Nothing
                    | currDir == dir -> "."
                    | otherwise -> assert False $ toFilePathNoTrailingSep dir
                Just rel -> toFilePathNoTrailingSep rel
        fpToPkgDir fp =
            let absDir = parent fp
            in ResolvedPath (RelFilePath $ T.pack $ makeRelDir absDir) absDir
        pkgDirs = Map.map (fpToPkgDir . fst) bundle
    (snapshotLoc, flags, extraDeps, rbundle) <- getDefaultResolver initOpts mresolver pkgDirs

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

        gpdByDir = Map.fromList [ (parent fp, gpd) | (fp, gpd) <- Map.elems bundle]
        gpds = Map.elems $
          Map.mapMaybe (flip Map.lookup gpdByDir . resolvedAbsolute) rbundle

    deps <- for (Map.toList extraDeps) $ \(n, v) ->
      PLImmutable <$> completePackageLocation (RPLIHackage (PackageIdentifierRevision n v CFILatest) Nothing)

    let p = Project
            { projectUserMsg = if userMsg == "" then Nothing else Just userMsg
            , projectPackages = resolvedRelative <$> Map.elems rbundle
            , projectDependencies = map toRawPL deps
            , projectFlags = removeSrcPkgDefaultFlags gpds flags
            , projectResolver = snapshotLoc
            , projectCompiler = Nothing
            , projectExtraPackageDBs = []
            , projectCurator = Nothing
            , projectDropPackages = mempty
            }

        makeRel = fmap toFilePath . makeRelativeToCurrentDir

        indent t = T.unlines $ fmap ("    " <>) (T.lines t)

    logInfo $ "Initialising configuration using resolver: " <> display snapshotLoc
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
    writeBinaryFileAtomic dest
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
        <> "\n"

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
        , ("extra-deps"       , extraDepsHelp)
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
        , "  subdirs:"
        , "  - auto-update"
        , "  - wai"
        ]

    extraDepsHelp = commentHelp
        [ "Dependency packages to be pulled from upstream that are not in the resolver."
        , "These entries can reference officially published versions as well as"
        , "forks / in-progress versions pinned to a git hash. For example:"
        , ""
        , "extra-deps:"
        , "- acme-missiles-0.3"
        , "- git: https://github.com/commercialhaskell/stack.git"
        , "  commit: e7b331f14bcffb8367cd58fbfc8b40ec7642100a"
        , ""
        ]

    footerHelp =
        let major = toMajorVersion $ C.mkVersion' Meta.version
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
    => InitOpts
    -> Maybe AbstractResolver
    -> Map PackageName (ResolvedPath Dir)
    -- ^ Src package name: cabal dir
    -> RIO env
         ( RawSnapshotLocation
         , Map PackageName (Map FlagName Bool)
         , Map PackageName Version
         , Map PackageName (ResolvedPath Dir))
       -- ^ ( Resolver
       --   , Flags for src packages and extra deps
       --   , Extra dependencies
       --   , Src packages actually considered)
getDefaultResolver initOpts mresolver pkgDirs = do
    (candidate, loc) <- case mresolver of
      Nothing -> selectSnapResolver
      Just ar -> do
        sl <- makeConcreteResolver ar
        c <- loadProjectSnapshotCandidate sl NoPrintWarnings False
        return (c, sl)
    getWorkingResolverPlan initOpts pkgDirs candidate loc
    where
        -- TODO support selecting best across regular and custom snapshots
        selectSnapResolver = do
            snaps <- fmap getRecommendedSnapshots getSnapshots'
            (c, l, r) <- selectBestSnapshot (Map.elems pkgDirs) snaps
            case r of
                BuildPlanCheckFail {} | not (omitPackages initOpts)
                        -> throwM (NoMatchingSnapshot snaps)
                _ -> return (c, l)

getWorkingResolverPlan
    :: (HasConfig env, HasGHCVariant env)
    => InitOpts
    -> Map PackageName (ResolvedPath Dir)
    -- ^ Src packages: cabal dir
    -> SnapshotCandidate env
    -> RawSnapshotLocation
    -> RIO env
         ( RawSnapshotLocation
         , Map PackageName (Map FlagName Bool)
         , Map PackageName Version
         , Map PackageName (ResolvedPath Dir))
       -- ^ ( SnapshotDef
       --   , Flags for src packages and extra deps
       --   , Extra dependencies
       --   , Src packages actually considered)
getWorkingResolverPlan initOpts pkgDirs0 snapCandidate snapLoc = do
    logInfo $ "Selected resolver: " <> display snapLoc
    go pkgDirs0
    where
        go pkgDirs = do
            eres <- checkBundleResolver initOpts snapLoc snapCandidate (Map.elems pkgDirs)
            -- if some packages failed try again using the rest
            case eres of
                Right (f, edeps)-> return (snapLoc, f, edeps, pkgDirs)
                Left ignored
                    | Map.null available -> do
                        logWarn "*** Could not find a working plan for any of \
                                 \the user packages.\nProceeding to create a \
                                 \config anyway."
                        return (snapLoc, Map.empty, Map.empty, Map.empty)
                    | otherwise -> do
                        when (Map.size available == Map.size pkgDirs) $
                            error "Bug: No packages to ignore"

                        if length ignored > 1 then do
                          logWarn "*** Ignoring packages:"
                          logWarn $ display $ indent $ showItems $ map packageNameString ignored
                        else
                          logWarn $ "*** Ignoring package: "
                                 <> fromString
                                      (case ignored of
                                        [] -> error "getWorkingResolverPlan.head"
                                        x:_ -> packageNameString x)

                        go available
                    where
                      indent t   = T.unlines $ fmap ("    " <>) (T.lines t)
                      isAvailable k _ = k `notElem` ignored
                      available       = Map.filterWithKey isAvailable pkgDirs

checkBundleResolver
    :: (HasConfig env, HasGHCVariant env)
    => InitOpts
    -> RawSnapshotLocation
    -> SnapshotCandidate env
    -> [ResolvedPath Dir]
    -- ^ Src package dirs
    -> RIO env
         (Either [PackageName] ( Map PackageName (Map FlagName Bool)
                               , Map PackageName Version))
checkBundleResolver initOpts snapshotLoc snapCandidate pkgDirs = do
    result <- checkSnapBuildPlan pkgDirs Nothing snapCandidate
    case result of
        BuildPlanCheckOk f -> return $ Right (f, Map.empty)
        BuildPlanCheckPartial _f e -> do -- FIXME:qrilka unused f
            if omitPackages initOpts
                then do
                    warnPartial result
                    logWarn "*** Omitting packages with unsatisfied dependencies"
                    return $ Left $ failedUserPkgs e
                else throwM $ ResolverPartial snapshotLoc (show result)
        BuildPlanCheckFail _ e _
            | omitPackages initOpts -> do
                logWarn $ "*** Resolver compiler mismatch: "
                           <> display snapshotLoc
                logWarn $ display $ indent $ T.pack $ show result
                return $ Left $ failedUserPkgs e
            | otherwise -> throwM $ ResolverMismatch snapshotLoc (show result)
    where
      indent t  = T.unlines $ fmap ("    " <>) (T.lines t)
      warnPartial res = do
          logWarn $ "*** Resolver " <> display snapshotLoc
                      <> " will need external packages: "
          logWarn $ display $ indent $ T.pack $ show res

      failedUserPkgs e = Map.keys $ Map.unions (Map.elems (fmap deNeededBy e))

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
    , omitPackages   :: Bool
    -- ^ Exclude conflicting or incompatible user packages
    , forceOverwrite :: Bool
    -- ^ Overwrite existing stack.yaml
    , includeSubDirs :: Bool
    -- ^ If True, include all .cabal files found in any sub directories
    }

findCabalDirs
  :: HasConfig env
  => Bool -> Path Abs Dir -> RIO env (Set (Path Abs Dir))
findCabalDirs recurse dir =
    Set.fromList . map parent
    <$> liftIO (findFiles dir isHpackOrCabal subdirFilter)
  where
    subdirFilter subdir = recurse && not (isIgnored subdir)
    isHpack = (== "package.yaml")     . toFilePath . filename
    isCabal = (".cabal" `isSuffixOf`) . toFilePath
    isHpackOrCabal x = isHpack x || isCabal x

    isIgnored path = "." `isPrefixOf` dirName || dirName `Set.member` ignoredDirs
      where
        dirName = FP.dropTrailingPathSeparator (toFilePath (dirname path))

-- | Special directories that we don't want to traverse for .cabal files
ignoredDirs :: Set FilePath
ignoredDirs = Set.fromList
    [ "dist"
    ]

cabalPackagesCheck
    :: (HasConfig env, HasGHCVariant env)
     => [Path Abs Dir]
     -> Maybe String
     -> RIO env
          ( Map PackageName (Path Abs File, C.GenericPackageDescription)
          , [Path Abs File])
cabalPackagesCheck cabaldirs dupErrMsg = do
    when (null cabaldirs) $ do
      logWarn "We didn't find any local package directories"
      logWarn "You may want to create a package with \"stack new\" instead"
      logWarn "Create an empty project for now"
      logWarn "If this isn't what you want, please delete the generated \"stack.yaml\""

    relpaths <- mapM prettyPath cabaldirs
    logInfo "Using cabal packages:"
    logInfo $ formatGroup relpaths

    packages <- for cabaldirs $ \dir -> do
      (gpdio, _name, cabalfp) <- loadCabalFilePath dir
      gpd <- liftIO $ gpdio YesPrintWarnings
      pure (cabalfp, gpd)

    -- package name cannot be empty or missing otherwise
    -- it will result in cabal solver failure.
    -- stack requires packages name to match the cabal file name
    -- Just the latter check is enough to cover both the cases

    let normalizeString = T.unpack . T.normalize T.NFC . T.pack
        getNameMismatchPkg (fp, gpd)
            | (normalizeString . packageNameString . gpdPackageName) gpd /= (normalizeString . FP.takeBaseName . toFilePath) fp
                = Just fp
            | otherwise = Nothing
        nameMismatchPkgs = mapMaybe getNameMismatchPkg packages

    when (nameMismatchPkgs /= []) $ do
        rels <- mapM prettyPath nameMismatchPkgs
        error $ "Package name as defined in the .cabal file must match the \
                \.cabal file name.\n\
                \Please fix the following packages and try again:\n"
                <> T.unpack (utf8BuilderToText (formatGroup rels))

    let dupGroups = filter ((> 1) . length)
                            . groupSortOn (gpdPackageName . snd)
        dupAll    = concat $ dupGroups packages

        -- Among duplicates prefer to include the ones in upper level dirs
        pathlen     = length . FP.splitPath . toFilePath . fst
        getmin      = minimumBy (compare `on` pathlen)
        dupSelected = map getmin (dupGroups packages)
        dupIgnored  = dupAll \\ dupSelected
        unique      = packages \\ dupIgnored

    when (dupIgnored /= []) $ do
        dups <- mapM (mapM (prettyPath. fst)) (dupGroups packages)
        logWarn $
            "Following packages have duplicate package names:\n" <>
            mconcat (intersperse "\n" (map formatGroup dups))
        case dupErrMsg of
          Nothing -> logWarn $
                 "Packages with duplicate names will be ignored.\n"
              <> "Packages in upper level directories will be preferred.\n"
          Just msg -> error msg

    return (Map.fromList
            $ map (\(file, gpd) -> (gpdPackageName gpd,(file, gpd))) unique
           , map fst dupIgnored)

formatGroup :: [String] -> Utf8Builder
formatGroup = foldMap (\path -> "- " <> fromString path <> "\n")

prettyPath ::
       (MonadIO m, RelPath (Path r t) ~ Path Rel t, AnyPath (Path r t))
    => Path r t
    -> m FilePath
prettyPath path = do
    eres <- liftIO $ try $ makeRelativeToCurrentDir path
    return $ case eres of
        Left (_ :: PathException) -> toFilePath path
        Right res -> toFilePath res
