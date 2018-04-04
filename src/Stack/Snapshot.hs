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
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Reading in @SnapshotDef@s and converting them into
-- @LoadedSnapshot@s.
module Stack.Snapshot
  ( loadResolver
  , loadSnapshot
  , calculatePackagePromotion
  ) where

import           Stack.Prelude hiding (Display (..))
import           Control.Monad.State.Strict      (get, put, StateT, execStateT)
import           Crypto.Hash.Conduit (hashFile)
import           Data.Aeson (withObject, (.!=), (.:), (.:?), Value (Object))
import           Data.Aeson.Extended (WithJSONWarnings(..), logJSONWarnings, (..!=), (..:?), jsonSubWarningsT, withObjectWarnings, (..:))
import           Data.Aeson.Types (Parser, parseEither)
import           Data.Store.VersionTagged
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Time (toGregorian)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Yaml (decodeFileEither, ParseException (AesonException))
import           Distribution.InstalledPackageInfo (PError)
import           Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.PackageDescription as C
import qualified Distribution.Types.UnqualComponentName as C
import           Distribution.System (Platform)
import           Distribution.Text (display)
import qualified Distribution.Version as C
import           Network.HTTP.Client (Request)
import           Network.HTTP.Download
import qualified RIO
import           Network.URI (isURI)
import           Path
import           Path.IO
import           Stack.Constants
import           Stack.Package
import           Stack.PackageDump
import           Stack.PackageLocation
import           Stack.Types.BuildPlan
import           Stack.Types.FlagName
import           Stack.Types.GhcPkgId
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version
import           Stack.Types.VersionIntervals
import           Stack.Types.Config
import           Stack.Types.Urls
import           Stack.Types.Compiler
import           Stack.Types.Resolver
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath

type SinglePackageLocation = PackageLocationIndex FilePath

data SnapshotException
  = InvalidCabalFileInSnapshot !SinglePackageLocation !PError
  | PackageDefinedTwice !PackageName !SinglePackageLocation !SinglePackageLocation
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
  => Resolver
  -> RIO env SnapshotDef
loadResolver (ResolverStackage name) = do
    stackage <- view stackRootL
    file' <- parseRelFile $ T.unpack file
    cachePath <- (buildPlanCacheDir stackage </>) <$> parseRelFile (T.unpack (renderSnapName name <> ".cache"))
    let fp = buildPlanDir stackage </> file'
        tryDecode = tryAny $ $(versionedDecodeOrLoad snapshotDefVC) cachePath $ liftIO $ do
          evalue <- decodeFileEither $ toFilePath fp
          case evalue of
            Left e -> throwIO e
            Right value ->
              case parseEither parseStackageSnapshot value of
                Left s -> throwIO $ InvalidStackageException name s
                Right x -> return x
    logDebug $ "Decoding build plan from: " <> fromString (toFilePath fp)
    eres <- tryDecode
    case eres of
        Right sd -> return sd
        Left e -> do
            logDebug $
              "Decoding Stackage snapshot definition from file failed: " <>
              displayShow e
            ensureDir (parent fp)
            url <- buildBuildPlanUrl name file
            req <- parseRequest $ T.unpack url
            logSticky $ "Downloading " <> RIO.display name <> " build plan ..."
            logDebug $ "Downloading build plan from: " <> RIO.display url
            wasDownloaded <- redownload req fp
            if wasDownloaded
              then logStickyDone $ "Downloaded " <> RIO.display name <> " build plan."
              else logStickyDone $ "Skipped download of " <> RIO.display name <> " because its the stored entity tag matches the server version"
            tryDecode >>= either throwM return

  where
    file = renderSnapName name <> ".yaml"

    buildBuildPlanUrl :: (MonadReader env m, HasConfig env) => SnapName -> Text -> m Text
    buildBuildPlanUrl snapName file' = do
        urls <- view $ configL.to configUrls
        return $
            case snapName of
                LTS _ _ -> urlsLtsBuildPlans urls <> "/" <> file'
                Nightly _ -> urlsNightlyBuildPlans urls <> "/" <> file'

    parseStackageSnapshot = withObject "StackageSnapshotDef" $ \o -> do
        Object si <- o .: "system-info"
        ghcVersion <- si .:? "ghc-version"
        compilerVersion <- si .:? "compiler-version"
        compilerVersion' <-
            case (ghcVersion, compilerVersion) of
                (Just _, Just _) -> fail "can't have both compiler-version and ghc-version fields"
                (Just ghc, _) -> return (GhcVersion ghc)
                (_, Just compiler) -> return compiler
                _ -> fail "expected field \"ghc-version\" or \"compiler-version\" not present"
        let sdParent = Left compilerVersion'
        sdGlobalHints <- si .: "core-packages"

        packages <- o .: "packages"
        (Endo mkLocs, sdFlags, sdHidden) <- fmap mconcat $ mapM (uncurry goPkg) $ Map.toList packages
        let sdLocations = mkLocs []

        let sdGhcOptions = Map.empty -- Stackage snapshots do not allow setting GHC options

        -- Not dropping any packages in a Stackage snapshot
        let sdDropPackages = Set.empty

        let sdResolver = ResolverStackage name
            sdResolverName = renderSnapName name

        return SnapshotDef {..}
      where
        goPkg name' = withObject "StackagePackageDef" $ \o -> do
            version <- o .: "version"
            mcabalFileInfo <- o .:? "cabal-file-info"
            mcabalFileInfo' <- forM mcabalFileInfo $ \o' -> do
                msize <- Just <$> o' .: "size"
                cfiHashes <- o' .: "hashes"
                hash' <-
                  case HashMap.lookup ("SHA256" :: Text) cfiHashes of
                    Nothing -> fail "Could not find SHA256"
                    Just shaText ->
                      case mkCabalHashFromSHA256 shaText of
                        Left e -> fail $ "Invalid SHA256: " ++ show e
                        Right x -> return x
                return $ CFIHash msize hash'

            Object constraints <- o .: "constraints"

            flags <- constraints .: "flags"
            let flags' = Map.singleton name' flags

            hide <- constraints .:? "hide" .!= False
            let hide' = if hide then Map.singleton name' True else Map.empty

            let location = PLIndex $ PackageIdentifierRevision (PackageIdentifier name' version) (fromMaybe CFILatest mcabalFileInfo')

            return (Endo (location:), flags', hide')
loadResolver (ResolverCompiler compiler) = return SnapshotDef
    { sdParent = Left compiler
    , sdResolver = ResolverCompiler compiler
    , sdResolverName = compilerVersionText compiler
    , sdLocations = []
    , sdDropPackages = Set.empty
    , sdFlags = Map.empty
    , sdHidden = Map.empty
    , sdGhcOptions = Map.empty
    , sdGlobalHints = Map.empty
    }
loadResolver (ResolverCustom url loc) = do
  logDebug $ "Loading " <> RIO.display url <> " build plan from " <> displayShow loc
  case loc of
    Left req -> download' req >>= load . toFilePath
    Right fp -> load fp
  where
    download' :: Request -> RIO env (Path Abs File)
    download' req = do
      let urlHash = T.unpack $ trimmedSnapshotHash $ snapshotHashFromBS $ encodeUtf8 url
      hashFP <- parseRelFile $ urlHash ++ ".yaml"
      customPlanDir <- getCustomPlanDir
      let cacheFP = customPlanDir </> $(mkRelDir "yaml") </> hashFP
      void (download req cacheFP :: RIO env Bool)
      return cacheFP

    getCustomPlanDir = do
        root <- view stackRootL
        return $ root </> $(mkRelDir "custom-plan")

    load :: FilePath -> RIO env SnapshotDef
    load fp = do
      let resolveLocalArchives sd = sd {
            sdLocations = resolveLocalArchive <$> sdLocations sd
          }
          resolveLocalArchive (PLOther (PLArchive archive)) = 
            PLOther $ PLArchive $ archive {
              archiveUrl = T.pack $ resolveLocalFilePath (T.unpack $ archiveUrl archive)
            }
          resolveLocalArchive pl = pl
          resolveLocalFilePath path =
            if not $ isURI path && FilePath.isRelative path
              then FilePath.dropFileName fp FilePath.</> FilePath.normalise path
              else path

      WithJSONWarnings (sd0, mparentResolver, mcompiler) warnings <-
        liftIO (decodeFileEither fp) >>= either
          (throwM . CustomResolverException url loc)
          (either (throwM . CustomResolverException url loc . AesonException) return . parseEither parseCustom)
      logJSONWarnings (T.unpack url) warnings
      forM_ (sdLocations sd0) $ \loc' ->
        case loc' of
          PLOther (PLFilePath _) -> throwM $ FilepathInCustomSnapshot url
          _ -> return ()
      let sd0' = resolveLocalArchives sd0
      -- The fp above may just be the download location for a URL,
      -- which we don't want to use. Instead, look back at loc from
      -- above.
      mdir <-
        case loc of
          Left _ -> return Nothing
          Right fp' -> Just . parent <$> liftIO (Dir.canonicalizePath fp' >>= parseAbsFile)

      -- Deal with the dual nature of the compiler key, which either
      -- means "use this compiler" or "override the compiler in the
      -- resolver"
      (parentResolver, overrideCompiler) <-
        case (mparentResolver, mcompiler) of
          (Nothing, Nothing) -> throwM $ NeedResolverOrCompiler url
          (Just parentResolver, Nothing) -> return (parentResolver, id)
          (Nothing, Just compiler) -> return (ResolverCompiler compiler, id)
          (Just parentResolver, Just compiler) -> return
            ( parentResolver
            , setCompilerVersion compiler
            )

      parentResolver' <- parseCustomLocation mdir parentResolver

      -- Calculate the hash of the current file, and then combine it
      -- with parent hashes if necessary below.
      rawHash :: SnapshotHash <- snapshotHashFromDigest <$> hashFile fp :: RIO env SnapshotHash

      (parent', hash') <-
        case parentResolver' of
          ResolverCompiler cv -> return (Left cv, rawHash) -- just a small optimization
          _ -> do
            parent' :: SnapshotDef <- loadResolver (parentResolver' :: Resolver) :: RIO env SnapshotDef
            let hash' :: SnapshotHash
                hash' = combineHash rawHash $
                  case sdResolver parent' of
                    ResolverStackage snapName -> snapNameToHash snapName
                    ResolverCustom _ parentHash -> parentHash
                    ResolverCompiler _ -> error "loadResolver: Received ResolverCompiler in impossible location"
            return (Right parent', hash')
      return $ overrideCompiler sd0'
        { sdParent = parent'
        , sdResolver = ResolverCustom url hash'
        }

    -- | Note that the 'sdParent' and 'sdResolver' fields returned
    -- here are bogus, and need to be replaced with information only
    -- available after further processing.
    parseCustom :: Value
                -> Parser (WithJSONWarnings (SnapshotDef, Maybe (ResolverWith ()), Maybe (CompilerVersion 'CVWanted)))
    parseCustom = withObjectWarnings "CustomSnapshot" $ \o -> (,,)
        <$> (SnapshotDef (Left (error "loadResolver")) (ResolverStackage (LTS 0 0))
            <$> (o ..: "name")
            <*> jsonSubWarningsT (o ..:? "packages" ..!= [])
            <*> o ..:? "drop-packages" ..!= Set.empty
            <*> o ..:? "flags" ..!= Map.empty
            <*> o ..:? "hidden" ..!= Map.empty
            <*> o ..:? "ghc-options" ..!= Map.empty
            <*> o ..:? "global-hints" ..!= Map.empty)
        <*> (o ..:? "resolver")
        <*> (o ..:? "compiler")

    combineHash :: SnapshotHash -> SnapshotHash -> SnapshotHash
    combineHash x y = snapshotHashFromBS (snapshotHashToBS x <> snapshotHashToBS y)

    snapNameToHash :: SnapName -> SnapshotHash
    snapNameToHash = snapshotHashFromBS . encodeUtf8 . renderSnapName

-- | Fully load up a 'SnapshotDef' into a 'LoadedSnapshot'
loadSnapshot
  :: forall env.
     (HasConfig env, HasGHCVariant env)
  => Maybe (CompilerVersion 'CVActual) -- ^ installed GHC we should query; if none provided, use the global hints
  -> Path Abs Dir -- ^ project root, used for checking out necessary files
  -> SnapshotDef
  -> RIO env LoadedSnapshot
loadSnapshot mcompiler root =
    start
  where
    start (snapshotDefFixes -> sd) = do
      path <- configLoadedSnapshotCache
        sd
        (maybe GISSnapshotHints GISCompiler mcompiler)
      $(versionedDecodeOrLoad loadedSnapshotVC) path (inner sd)

    inner :: SnapshotDef -> RIO env LoadedSnapshot
    inner sd = do
      ls0 <-
        case sdParent sd of
          Left cv ->
            case mcompiler of
              Nothing -> return LoadedSnapshot
                { lsCompilerVersion = wantedToActual cv
                , lsGlobals = fromGlobalHints $ sdGlobalHints sd
                , lsPackages = Map.empty
                }
              Just cv' -> loadCompiler cv'
          Right sd' -> start sd'

      gpds <-
        (concat <$> mapM (parseMultiCabalFilesIndex root) (sdLocations sd))
        `onException` do
          logError "Unable to load cabal files for snapshot"
          case sdResolver sd of
            ResolverStackage name -> do
              stackRoot <- view stackRootL
              file <- parseRelFile $ T.unpack $ renderSnapName name <> ".yaml"
              let fp = buildPlanDir stackRoot </> file
              liftIO $ ignoringAbsence $ removeFile fp
              logError ""
              logError "----"
              logError $ "Deleting cached snapshot file: " <> fromString (toFilePath fp)
              logError "Recommendation: try running again. If this fails again, open an upstream issue at:"
              logError $
                case name of
                  LTS _ _ -> "https://github.com/fpco/lts-haskell/issues/new"
                  Nightly _ -> "https://github.com/fpco/stackage-nightly/issues/new"
              logError "----"
              logError ""
            _ -> return ()

      (globals, snapshot, locals) <-
        calculatePackagePromotion root ls0
        (map (\(x, y) -> (x, y, ())) gpds)
        (sdFlags sd) (sdHidden sd) (sdGhcOptions sd) (sdDropPackages sd)

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
  => Path Abs Dir -- ^ project root
  -> LoadedSnapshot
  -> [(GenericPackageDescription, SinglePackageLocation, localLocation)] -- ^ packages we want to add on top of this snapshot
  -> Map PackageName (Map FlagName Bool) -- ^ flags
  -> Map PackageName Bool -- ^ overrides whether a package should be registered hidden
  -> Map PackageName [Text] -- ^ GHC options
  -> Set PackageName -- ^ packages in the snapshot to drop
  -> RIO env
       ( Map PackageName (LoadedPackageInfo GhcPkgId) -- new globals
       , Map PackageName (LoadedPackageInfo SinglePackageLocation) -- new snapshot
       , Map PackageName (LoadedPackageInfo (SinglePackageLocation, Maybe localLocation)) -- new locals
       )
calculatePackagePromotion
  root (LoadedSnapshot compilerVersion globals0 parentPackages0)
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
          noLongerGlobals3 :: Map PackageName (LoadedPackageInfo SinglePackageLocation)
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
                $ mapM (recalculate root compilerVersion flags hide ghcOptions)
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
            => Path Abs Dir -- ^ root
            -> CompilerVersion 'CVActual
            -> Map PackageName (Map FlagName Bool)
            -> Map PackageName Bool -- ^ hide?
            -> Map PackageName [Text] -- ^ GHC options
            -> (PackageName, LoadedPackageInfo SinglePackageLocation)
            -> RIO env (PackageName, LoadedPackageInfo SinglePackageLocation)
recalculate root compilerVersion allFlags allHide allOptions (name, lpi0) = do
  let hide = fromMaybe (lpiHide lpi0) (Map.lookup name allHide)
      options = fromMaybe (lpiGhcOptions lpi0) (Map.lookup name allOptions)
  case Map.lookup name allFlags of
    Nothing -> return (name, lpi0 { lpiHide = hide, lpiGhcOptions = options }) -- optimization
    Just flags -> do
      let loc = lpiLocation lpi0
      gpd <- parseSingleCabalFileIndex root loc
      platform <- view platformL
      let res@(name', lpi) = calculate gpd platform compilerVersion loc flags hide options
      unless (name == name' && lpiVersion lpi0 == lpiVersion lpi) $ error "recalculate invariant violated"
      return res

fromGlobalHints :: Map PackageName (Maybe Version) -> Map PackageName (LoadedPackageInfo GhcPkgId)
fromGlobalHints =
    Map.unions . map go . Map.toList
  where
    go (_, Nothing) = Map.empty
    go (name, Just ver) = Map.singleton name LoadedPackageInfo
      { lpiVersion = ver
      -- For global hint purposes, we only care about the
      -- version. All other fields are ignored when checking
      -- project compatibility.
      , lpiLocation = either impureThrow id
                    $ parseGhcPkgId
                    $ packageIdentifierText
                    $ PackageIdentifier name ver
      , lpiFlags = Map.empty
      , lpiGhcOptions = []
      , lpiPackageDeps = Map.empty
      , lpiProvidedExes = Set.empty
      , lpiNeededExes = Map.empty
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
             => CompilerVersion 'CVActual
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
                , lpiProvidedExes = Set.empty
                , lpiNeededExes = Map.empty
                , lpiExposedModules = Set.fromList $ map (ModuleName . encodeUtf8) $ dpExposedModules dp
                , lpiHide = not $ dpIsExposed dp
                }

type FindPackageS localLocation =
    ( Map PackageName (LoadedPackageInfo (SinglePackageLocation, localLocation))
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
            -> CompilerVersion 'CVActual
            -> (GenericPackageDescription, SinglePackageLocation, localLocation)
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
    PackageIdentifier name _version = fromCabalPackageIdentifier $ C.package $ C.packageDescription gpd

-- | Some hard-coded fixes for build plans, only for hysterical raisins.
snapshotDefFixes :: SnapshotDef -> SnapshotDef
snapshotDefFixes sd | isOldStackage (sdResolver sd) = sd
    { sdFlags = Map.unionWith Map.union overrides $ sdFlags sd
    }
  where
    overrides = Map.fromList
      [ ($(mkPackageName "persistent-sqlite"), Map.singleton $(mkFlagName "systemlib") False)
      , ($(mkPackageName "yaml"), Map.singleton $(mkFlagName "system-libyaml") False)
      ]

    -- Only apply this hack to older Stackage snapshots. In
    -- particular, nightly-2018-03-13 did not contain these two
    -- packages.
    isOldStackage (ResolverStackage (LTS major _)) = major < 11
    isOldStackage (ResolverStackage (Nightly (toGregorian -> (year, _, _)))) = year < 2018
    isOldStackage _ = False
snapshotDefFixes sd = sd

-- | Convert a global 'LoadedPackageInfo' to a snapshot one by
-- creating a 'PackageLocation'.
globalToSnapshot :: PackageName -> LoadedPackageInfo loc -> LoadedPackageInfo (PackageLocationIndex FilePath)
globalToSnapshot name lpi = lpi
    { lpiLocation = PLIndex (PackageIdentifierRevision (PackageIdentifier name (lpiVersion lpi)) CFILatest)
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
          -> CompilerVersion 'CVActual
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
    PackageIdentifier name version = fromCabalPackageIdentifier $ C.package pd
    lpi = LoadedPackageInfo
      { lpiVersion = version
      , lpiLocation = loc
      , lpiFlags = flags
      , lpiGhcOptions = options
      , lpiPackageDeps = Map.map fromVersionRange
                       $ Map.filterWithKey (const . (/= name))
                       $ packageDependencies pconfig pd
      , lpiProvidedExes =
            Set.fromList
          $ map (ExeName . T.pack . C.unUnqualComponentName . C.exeName)
          $ C.executables pd
      , lpiNeededExes = Map.map fromVersionRange
                      $ packageDescTools pd
      , lpiExposedModules = maybe
          Set.empty
          (Set.fromList . map fromCabalModuleName . C.exposedModules)
          (C.library pd)
      , lpiHide = hide
      }
