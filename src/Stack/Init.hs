{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Stack.Init
  ( initProject
  , InitOpts (..)
  ) where

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Foldable as F
import qualified Data.IntMap as IntMap
import           Data.List.Extra ( groupSortOn )
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Distribution.PackageDescription as C
import qualified Distribution.Text as C
import qualified Distribution.Version as C
import           Path
                   ( PathException, (</>), dirname, filename, parent
                   , stripProperPrefix
                   )
import           Path.Extra ( toFilePathNoTrailingSep )
import           Path.Find ( findFiles )
import           Path.IO
                   ( AnyPath, RelPath, doesFileExist, makeRelativeToCurrentDir
                   , resolveDir'
                   )
import qualified RIO.FilePath as FP
import           RIO.List ( (\\), intercalate, isSuffixOf, isPrefixOf )
import           RIO.List.Partial ( minimumBy )
import           Stack.BuildPlan
                   ( BuildPlanCheck (..), checkSnapBuildPlan, deNeededBy
                   , removeSrcPkgDefaultFlags, selectBestSnapshot
                   )
import           Stack.Config ( getSnapshots, makeConcreteResolver )
import           Stack.Constants ( stackDotYaml )
import           Stack.Prelude
import           Stack.SourceMap
                   ( SnapshotCandidate, loadProjectSnapshotCandidate )
import           Stack.Types.Config
                   ( ConfigPrettyException (..), HasConfig, HasGHCVariant
                   , Project (..)
                   )
import           Stack.Types.Resolver ( AbstractResolver, Snapshots (..) )
import           Stack.Types.Version ( stackMajorVersion )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Init" module.
data InitException
  = NoPackagesToIgnoreBug
  deriving (Show, Typeable)

instance Exception InitException where
  displayException NoPackagesToIgnoreBug = bugReport "[S-2747]"
    "No packages to ignore."

data InitPrettyException
  = SnapshotDownloadFailure SomeException
  | ConfigFileAlreadyExists FilePath
  | PackageNameInvalid [(Path Abs File, PackageName)]
  deriving (Show, Typeable)

instance Pretty InitPrettyException where
  pretty (ConfigFileAlreadyExists reldest) =
    "[S-8009]"
    <> line
    <> flow "Stack declined to create a project-level YAML configuration file."
    <> blankLine
    <> fillSep
         [ flow "The file"
         , style File (fromString reldest)
         , "already exists. To overwrite it, pass the flag"
         , style Shell "--force" <> "."
         ]
  pretty (PackageNameInvalid rels) =
    "[S-5267]"
    <> line
    <> flow "Stack did not create project-level YAML configuration, as (like \
            \Hackage) it requires a Cabal file name to match the package it \
            \defines."
    <> blankLine
    <> flow "Please rename the following Cabal files:"
    <> line
    <> bulletedList
         ( map
             ( \(fp, name) -> fillSep
                 [ pretty fp
                 , "as"
                 , style
                     File
                     (fromString (packageNameString name) <> ".cabal")
                 ]
             )
             rels
         )
  pretty (SnapshotDownloadFailure e) =
    "[S-8332]"
    <> line
    <> flow "Stack failed to create project-level YAML configuration, as it \
            \was unable to download the index of available snapshots."
    <> blankLine
    <> fillSep
         [ flow "This sometimes happens because Certificate Authorities are \
                \missing on your system. You can try the Stack command again \
                \or manually create the configuration file. For help about the \
                \content of Stack's YAML configuration files, see (for the \
                \most recent release of Stack)"
         , style
             Url
             "http://docs.haskellstack.org/en/stable/yaml_configuration/"
           <> "."
         ]
    <> blankLine
    <> flow "While downloading the snapshot index, Stack encountered the \
            \following error:"
    <> blankLine
    <> string (displayException e)

instance Exception InitPrettyException

-- | Generate stack.yaml
initProject ::
     (HasConfig env, HasGHCVariant env)
  => Path Abs Dir
  -> InitOpts
  -> Maybe AbstractResolver
  -> RIO env ()
initProject currDir initOpts mresolver = do
  let dest = currDir </> stackDotYaml
  reldest <- toFilePath <$> makeRelativeToCurrentDir dest
  exists <- doesFileExist dest
  when (not (forceOverwrite initOpts) && exists) $
    prettyThrowIO $ ConfigFileAlreadyExists reldest
  dirs <- mapM (resolveDir' . T.unpack) (searchDirs initOpts)
  let find  = findCabalDirs (includeSubDirs initOpts)
      dirs' = if null dirs then [currDir] else dirs
  prettyInfo $
       fillSep
         [ flow "Looking for Cabal or"
         , style File "package.yaml"
         , flow "files to use to initialise Stack's project-level YAML \
                \configuration file."
         ]
    <> line
  cabaldirs <- Set.toList . Set.unions <$> mapM find dirs'
  (bundle, dupPkgs)  <- cabalPackagesCheck cabaldirs
  let makeRelDir dir =
        case stripProperPrefix currDir dir of
          Nothing
              | currDir == dir -> "."
              | otherwise -> assert False $ toFilePathNoTrailingSep dir
          Just rel -> toFilePathNoTrailingSep rel
      fpToPkgDir fp =
        let absDir = parent fp
        in  ResolvedPath (RelFilePath $ T.pack $ makeRelDir absDir) absDir
      pkgDirs = Map.map (fpToPkgDir . fst) bundle
  (snapshotLoc, flags, extraDeps, rbundle) <-
    getDefaultResolver initOpts mresolver pkgDirs
  let ignored = Map.difference bundle rbundle
      dupPkgMsg
        | dupPkgs /= [] =
            "Warning (added by new or init): Some packages were found to have \
            \names conflicting with others and have been commented out in the \
            \packages section.\n"
        | otherwise = ""
      missingPkgMsg
        | Map.size ignored > 0 =
            "Warning (added by new or init): Some packages were found to be \
            \incompatible with the resolver and have been left commented out \
            \in the packages section.\n"
        | otherwise = ""
      extraDepMsg
        | Map.size extraDeps > 0 =
            "Warning (added by new or init): Specified resolver could not \
            \satisfy all dependencies. Some external packages have been added \
            \as dependencies.\n"
        | otherwise = ""
      makeUserMsg msgs =
        let msg = concat msgs
        in  if msg /= ""
              then
                   msg
                <> "You can omit this message by removing it from stack.yaml\n"
              else ""
      userMsg = makeUserMsg [dupPkgMsg, missingPkgMsg, extraDepMsg]
      gpdByDir =
        Map.fromList [ (parent fp, gpd) | (fp, gpd) <- Map.elems bundle]
      gpds = Map.elems $
        Map.mapMaybe (flip Map.lookup gpdByDir . resolvedAbsolute) rbundle
  deps <- for (Map.toList extraDeps) $ \(n, v) ->
    PLImmutable . cplComplete <$>
      completePackageLocation
        (RPLIHackage (PackageIdentifierRevision n v CFILatest) Nothing)
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
  prettyInfoL
    [ flow "Initialising Stack's project-level YAML configuration file \
           \using snapshot"
    , pretty (PrettyRawSnapshotLocation snapshotLoc) <> "."
    ]
  prettyInfoL $
    let n = Map.size bundle + length dupPkgs
    in  [ "Considered"
        , fromString $ show n
        , "user"
        , if n == 1 then "package." else "packages."
        ]
  when (dupPkgs /= []) $ do
    rels <- mapM makeRel dupPkgs
    prettyWarn $
         fillSep
           [ flow "Ignoring these"
           , fromString $ show (length dupPkgs)
           , flow "duplicate packages:"
           ]
      <> line
      <> bulletedList (map (style File . fromString) rels)
  when (Map.size ignored > 0) $ do
    rels <- mapM makeRel (Map.elems (fmap fst ignored))
    prettyWarn $
         fillSep
           [ flow "Ignoring these"
           , fromString $ show (Map.size ignored)
           , flow "packages due to dependency conflicts:"
           ]
      <> line
      <> bulletedList (map (style File . fromString) rels)
  when (Map.size extraDeps > 0) $
    prettyWarnL
      [ fromString $ show (Map.size extraDeps)
      , flow "external dependencies were added."
      ]
  prettyInfoL
    [ flow $ if exists
        then "Overwriting existing configuration file"
        else "Writing configuration to"
    , style File (fromString reldest) <> "."
    ]
  writeBinaryFileAtomic dest $ renderStackYaml p
    (Map.elems $ fmap (makeRelDir . parent . fst) ignored)
    (map (makeRelDir . parent) dupPkgs)
  prettyInfoS
    "Stack's project-level YAML configuration file has been initialised."

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
    <> goOthers (o `KeyMap.difference` KeyMap.fromList comments)
    <> B.byteString footerHelp
    <> "\n"
  goComment o (name, comment) =
    case (convert <$> KeyMap.lookup name o) <|> nonPresentValue name of
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
    in  commentPackages ignoredComment ignoredPackages
        <> commentPackages dupComment dupPackages
  commentPackages comment pkgs
    | pkgs /= [] =
           B.byteString comment
        <> B.byteString "\n"
        <> B.byteString (BC.pack $ concat
             $ map (\x -> "#- " ++ x ++ "\n") pkgs ++ ["\n"])
    | otherwise = ""
  goOthers o
    | KeyMap.null o = mempty
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
  footerHelp = commentHelp
    [ "Control whether we use the GHC we find on the path"
    , "system-ghc: true"
    , ""
    , "Require a specific version of Stack, using version ranges"
    , "require-stack-version: -any # Default"
    , "require-stack-version: \""
      ++ C.display (C.orLaterVersion stackMajorVersion) ++ "\""
    , ""
    , "Override the architecture used by Stack, especially useful on Windows"
    , "arch: i386"
    , "arch: x86_64"
    , ""
    , "Extra directories used by Stack for building"
    , "extra-include-dirs: [/path/to/dir]"
    , "extra-lib-dirs: [/path/to/dir]"
    , ""
    , "Allow a newer minor version of GHC than the snapshot specifies"
    , "compiler-check: newer-minor"
    ]

getSnapshots' :: HasConfig env => RIO env Snapshots
getSnapshots' = catchAny
  getSnapshots
  (prettyThrowIO . SnapshotDownloadFailure)

-- | Get the default resolver value
getDefaultResolver ::
     (HasConfig env, HasGHCVariant env)
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
      pure (c, sl)
  getWorkingResolverPlan initOpts pkgDirs candidate loc
 where
  -- TODO support selecting best across regular and custom snapshots
  selectSnapResolver = do
    snaps <- fmap getRecommendedSnapshots getSnapshots'
    (c, l, r) <- selectBestSnapshot (Map.elems pkgDirs) snaps
    case r of
      BuildPlanCheckFail {} | not (omitPackages initOpts)
              -> prettyThrowM $ NoMatchingSnapshot snaps
      _ -> pure (c, l)

getWorkingResolverPlan ::
     (HasConfig env, HasGHCVariant env)
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
  prettyInfoL
    [ flow "Selected the snapshot"
    , pretty (PrettyRawSnapshotLocation snapLoc) <> "."
    ]
  go pkgDirs0
 where
  go pkgDirs = do
    eres <- checkBundleResolver initOpts snapLoc snapCandidate (Map.elems pkgDirs)
    -- if some packages failed try again using the rest
    case eres of
      Right (f, edeps)-> pure (snapLoc, f, edeps, pkgDirs)
      Left ignored
        | Map.null available -> do
            prettyWarnS
              "Could not find a working plan for any of the user packages. \
              \Proceeding to create a YAML configuration file anyway."
            pure (snapLoc, Map.empty, Map.empty, Map.empty)
        | otherwise -> do
            when (Map.size available == Map.size pkgDirs) $
              throwM NoPackagesToIgnoreBug
            if length ignored > 1
              then
                prettyWarn
                  (  flow "Ignoring the following packages:"
                  <> line
                  <> bulletedList
                       (map (fromString . packageNameString) ignored)
                  )
              else
                prettyWarnL
                  [ flow "Ignoring package:"
                  , fromString
                      ( case ignored of
                          [] -> throwM NoPackagesToIgnoreBug
                          x:_ -> packageNameString x
                      )
                  ]
            go available
       where
        isAvailable k _ = k `notElem` ignored
        available       = Map.filterWithKey isAvailable pkgDirs

checkBundleResolver ::
     (HasConfig env, HasGHCVariant env)
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
    BuildPlanCheckOk f -> pure $ Right (f, Map.empty)
    BuildPlanCheckPartial _f e -> do -- FIXME:qrilka unused f
      if omitPackages initOpts
        then do
          warnPartial result
          prettyWarnS "Omitting packages with unsatisfied dependencies"
          pure $ Left $ failedUserPkgs e
        else
          prettyThrowM $ ResolverPartial snapshotLoc (show result)
    BuildPlanCheckFail _ e _
      | omitPackages initOpts -> do
          prettyWarn $
               fillSep
                 [ "Resolver compiler mismatch:"
                 , style Current (fromString . T.unpack $ textDisplay snapshotLoc)
                 ]
            <> line
            <> indent 4 (string $ show result)
          pure $ Left $ failedUserPkgs e
      | otherwise -> prettyThrowM $ ResolverMismatch snapshotLoc (show result)
 where
  warnPartial res = do
    prettyWarn $
         fillSep
           [ "Resolver"
           , style Current (fromString . T.unpack $ textDisplay snapshotLoc)
           , flow "will need external packages:"
           ]
      <> line
      <> indent 4 (string $ show res)

  failedUserPkgs e = Map.keys $ Map.unions (Map.elems (fmap deNeededBy e))

getRecommendedSnapshots :: Snapshots -> NonEmpty SnapName
getRecommendedSnapshots snapshots =
  -- in order - Latest LTS, Latest Nightly, all LTS most recent first
  case NonEmpty.nonEmpty supportedLtss of
    Just (mostRecent :| older) -> mostRecent :| (nightly : older)
    Nothing -> nightly :| []
 where
  ltss = map (uncurry LTS) (IntMap.toDescList $ snapshotsLts snapshots)
  supportedLtss = filter (>= minSupportedLts) ltss
  nightly = Nightly (snapshotsNightly snapshots)

-- |Yields the minimum LTS supported by Stack.
minSupportedLts :: SnapName
-- See https://github.com/commercialhaskell/stack/blob/master/ChangeLog.md
-- under Stack version 2.1.1.
minSupportedLts = LTS 3 0

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

findCabalDirs ::
     HasConfig env
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
ignoredDirs = Set.fromList ["dist"]

cabalPackagesCheck ::
     (HasConfig env, HasGHCVariant env)
  => [Path Abs Dir]
  -> RIO env
       ( Map PackageName (Path Abs File, C.GenericPackageDescription)
       , [Path Abs File]
       )
cabalPackagesCheck cabaldirs = do
  when (null cabaldirs) $
    prettyWarn $
         fillSep
           [ flow "Stack did not find any local package directories. You may \
                  \want to create a package with"
           , style Shell (flow "stack new")
           , flow "instead."
           ]
      <> blankLine
      <> fillSep
           [ flow "Stack will create an empty project. If this is not what \
                  \you want, please delete the generated"
           , style File "stack.yaml"
           , "file."
           ]
  relpaths <- mapM prettyPath cabaldirs
  unless (null relpaths) $
    prettyInfo $
         flow "Using the Cabal packages:"
      <> line
      <> bulletedList (map (style File . fromString) relpaths)
      <> line
  -- A package name cannot be empty or missing otherwise it will result in
  -- Cabal solver failure. Stack requires packages name to match the Cabal
  -- file name. Just the latter check is enough to cover both the cases.
  ePackages <- for cabaldirs $ \dir -> do
    -- Pantry's 'loadCabalFilePath' throws 'MismatchedCabalName' (error
    -- [S-910]) if the Cabal file name does not match the package it
    -- defines.
    (gpdio, _name, cabalfp) <- loadCabalFilePath (Just stackProgName') dir
    eres <- liftIO $ try (gpdio YesPrintWarnings)
    case eres :: Either PantryException C.GenericPackageDescription of
      Right gpd -> pure $ Right (cabalfp, gpd)
      Left (MismatchedCabalName fp name) -> pure $ Left (fp, name)
      Left e -> throwIO e
  let (nameMismatchPkgs, packages) = partitionEithers ePackages
  when (nameMismatchPkgs /= []) $
    prettyThrowIO $ PackageNameInvalid nameMismatchPkgs
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
    prettyWarn $
         flow "The following packages have duplicate package names:"
      <> line
      <> foldMap
           ( \dup ->    bulletedList (map fromString dup)
                     <> line
           )
           dups
      <> line
      <> flow "Packages with duplicate names will be ignored. Packages \
              \in upper level directories will be preferred."
      <> line
  pure (Map.fromList
          $ map (\(file, gpd) -> (gpdPackageName gpd,(file, gpd))) unique
         , map fst dupIgnored)

prettyPath ::
     (MonadIO m, RelPath (Path r t) ~ Path Rel t, AnyPath (Path r t))
  => Path r t
  -> m FilePath
prettyPath path = do
  eres <- liftIO $ try $ makeRelativeToCurrentDir path
  pure $ case eres of
    Left (_ :: PathException) -> toFilePath path
    Right res -> toFilePath res
