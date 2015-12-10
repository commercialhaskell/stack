{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}

-- | Resolving a build plan for a set of packages in a given Stackage
-- snapshot.

module Stack.BuildPlan
    ( BuildPlanException (..)
    , MiniBuildPlan(..)
    , MiniPackageInfo(..)
    , Snapshots (..)
    , getSnapshots
    , loadMiniBuildPlan
    , resolveBuildPlan
    , findBuildPlan
    , ToolMap
    , getToolMap
    , shadowMiniBuildPlan
    , parseCustomMiniBuildPlan
    ) where

import           Control.Applicative
import           Control.Exception (assert)
import           Control.Monad (liftM, forM)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (asks)
import           Control.Monad.State.Strict      (State, execState, get, modify,
                                                  put)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.Aeson.Extended (FromJSON (..), withObject, withText, (.:), (.:?), (.!=))
import           Data.Binary.VersionTagged (taggedDecodeOrLoad)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Either (partitionEithers)
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time (Day)
import qualified Data.Traversable as Tr
import           Data.Typeable (Typeable)
import           Data.Yaml (decodeEither', decodeFileEither)
import           Distribution.PackageDescription (GenericPackageDescription,
                                                  flagDefault, flagManual,
                                                  flagName, genPackageFlags,
                                                  executables, exeName, library, libBuildInfo, buildable)
import qualified Distribution.Package as C
import qualified Distribution.PackageDescription as C
import qualified Distribution.Version as C
import           Distribution.Text (display)
import           Network.HTTP.Download
import           Network.HTTP.Types (Status(..))
import           Network.HTTP.Client (checkStatus)
import           Path
import           Path.IO
import           Prelude -- Fix AMP warning
import           Stack.Constants
import           Stack.Fetch
import           Stack.Package
import           Stack.Types
import           Stack.Types.StackT
import           System.Directory (canonicalizePath)
import qualified System.FilePath as FP

data BuildPlanException
    = UnknownPackages
        (Path Abs File) -- stack.yaml file
        (Map PackageName (Maybe Version, Set PackageName)) -- truly unknown
        (Map PackageName (Set PackageIdentifier)) -- shadowed
    | SnapshotNotFound SnapName
    deriving (Typeable)
instance Exception BuildPlanException
instance Show BuildPlanException where
    show (SnapshotNotFound snapName) = unlines
        [ "SnapshotNotFound " ++ snapName'
        , "Non existing resolver: " ++ snapName' ++ "."
        , "For a complete list of available snapshots see https://www.stackage.org/snapshots"
        ]
        where snapName' = show $ renderSnapName snapName
    show (UnknownPackages stackYaml unknown shadowed) =
        unlines $ unknown' ++ shadowed'
      where
        unknown' :: [String]
        unknown'
            | Map.null unknown = []
            | otherwise = concat
                [ ["The following packages do not exist in the build plan:"]
                , map go (Map.toList unknown)
                , case mapMaybe goRecommend $ Map.toList unknown of
                    [] -> []
                    rec ->
                        ("Recommended action: modify the extra-deps field of " ++
                        toFilePath stackYaml ++
                        " to include the following:")
                        : (rec
                        ++ ["Note: further dependencies may need to be added"])
                , case mapMaybe getNoKnown $ Map.toList unknown of
                    [] -> []
                    noKnown ->
                        [ "There are no known versions of the following packages:"
                        , intercalate ", " $ map packageNameString noKnown
                        ]
                ]
          where
            go (dep, (_, users)) | Set.null users = packageNameString dep
            go (dep, (_, users)) = concat
                [ packageNameString dep
                , " (used by "
                , intercalate ", " $ map packageNameString $ Set.toList users
                , ")"
                ]

            goRecommend (name, (Just version, _)) =
                Just $ "- " ++ packageIdentifierString (PackageIdentifier name version)
            goRecommend (_, (Nothing, _)) = Nothing

            getNoKnown (name, (Nothing, _)) = Just name
            getNoKnown (_, (Just _, _)) = Nothing

        shadowed' :: [String]
        shadowed'
            | Map.null shadowed = []
            | otherwise = concat
                [ ["The following packages are shadowed by local packages:"]
                , map go (Map.toList shadowed)
                , ["Recommended action: modify the extra-deps field of " ++
                   toFilePath stackYaml ++
                   " to include the following:"]
                , extraDeps
                , ["Note: further dependencies may need to be added"]
                ]
          where
            go (dep, users) | Set.null users = concat
                [ packageNameString dep
                , " (internal stack error: this should never be null)"
                ]
            go (dep, users) = concat
                [ packageNameString dep
                , " (used by "
                , intercalate ", "
                    $ map (packageNameString . packageIdentifierName)
                    $ Set.toList users
                , ")"
                ]

            extraDeps = map (\ident -> "- " ++ packageIdentifierString ident)
                      $ Set.toList
                      $ Set.unions
                      $ Map.elems shadowed

-- | Determine the necessary packages to install to have the given set of
-- packages available.
--
-- This function will not provide test suite and benchmark dependencies.
--
-- This may fail if a target package is not present in the @BuildPlan@.
resolveBuildPlan :: (MonadThrow m, MonadIO m, MonadReader env m, HasBuildConfig env, MonadLogger m, HasHttpManager env, MonadBaseControl IO m,MonadCatch m)
                 => MiniBuildPlan
                 -> (PackageName -> Bool) -- ^ is it shadowed by a local package?
                 -> Map PackageName (Set PackageName) -- ^ required packages, and users of it
                 -> m ( Map PackageName (Version, Map FlagName Bool)
                      , Map PackageName (Set PackageName)
                      )
resolveBuildPlan mbp isShadowed packages
    | Map.null (rsUnknown rs) && Map.null (rsShadowed rs) = return (rsToInstall rs, rsUsedBy rs)
    | otherwise = do
        bconfig <- asks getBuildConfig
        let maxVer =
                Map.fromListWith max $
                map toTuple $
                Map.keys (bcPackageCaches bconfig)
            unknown = flip Map.mapWithKey (rsUnknown rs) $ \ident x ->
                (Map.lookup ident maxVer, x)
        throwM $ UnknownPackages
            (bcStackYaml bconfig)
            unknown
            (rsShadowed rs)
  where
    rs = getDeps mbp isShadowed packages

data ResolveState = ResolveState
    { rsVisited   :: Map PackageName (Set PackageName) -- ^ set of shadowed dependencies
    , rsUnknown   :: Map PackageName (Set PackageName)
    , rsShadowed  :: Map PackageName (Set PackageIdentifier)
    , rsToInstall :: Map PackageName (Version, Map FlagName Bool)
    , rsUsedBy    :: Map PackageName (Set PackageName)
    }

toMiniBuildPlan :: (MonadIO m, MonadLogger m, MonadReader env m, HasHttpManager env, MonadThrow m, HasConfig env, MonadBaseControl IO m, MonadCatch m)
                => CompilerVersion -- ^ Compiler version
                -> Map PackageName Version -- ^ cores
                -> Map PackageName (Version, Map FlagName Bool) -- ^ non-core packages
                -> m MiniBuildPlan
toMiniBuildPlan compilerVersion corePackages packages = do
    $logInfo "Caching build plan"

    -- Determine the dependencies of all of the packages in the build plan. We
    -- handle core packages specially, because some of them will not be in the
    -- package index. For those, we allow missing packages to exist, and then
    -- remove those from the list of dependencies, since there's no way we'll
    -- ever reinstall them anyway.
    (cores, missingCores) <- addDeps True compilerVersion
        $ fmap (, Map.empty) corePackages

    (extras, missing) <- addDeps False compilerVersion packages

    assert (Set.null missing) $ return MiniBuildPlan
        { mbpCompilerVersion = compilerVersion
        , mbpPackages = Map.unions
            [ fmap (removeMissingDeps (Map.keysSet cores)) cores
            , extras
            , Map.fromList $ map goCore $ Set.toList missingCores
            ]
        }
  where
    goCore (PackageIdentifier name version) = (name, MiniPackageInfo
                { mpiVersion = version
                , mpiFlags = Map.empty
                , mpiPackageDeps = Set.empty
                , mpiToolDeps = Set.empty
                , mpiExes = Set.empty
                , mpiHasLibrary = True
                })

    removeMissingDeps cores mpi = mpi
        { mpiPackageDeps = Set.intersection cores (mpiPackageDeps mpi)
        }

-- | Add in the resolved dependencies from the package index
addDeps :: (MonadIO m, MonadLogger m, MonadReader env m, HasHttpManager env, MonadThrow m, HasConfig env, MonadBaseControl IO m, MonadCatch m)
        => Bool -- ^ allow missing
        -> CompilerVersion -- ^ Compiler version
        -> Map PackageName (Version, Map FlagName Bool)
        -> m (Map PackageName MiniPackageInfo, Set PackageIdentifier)
addDeps allowMissing compilerVersion toCalc = do
    menv <- getMinimalEnvOverride
    platform <- asks $ configPlatform . getConfig
    (resolvedMap, missingIdents) <-
        if allowMissing
            then do
                (missingNames, missingIdents, m) <-
                    resolvePackagesAllowMissing menv (Map.keysSet idents0) Set.empty
                assert (Set.null missingNames)
                    $ return (m, missingIdents)
            else do
                m <- resolvePackages menv (Map.keysSet idents0) Set.empty
                return (m, Set.empty)
    let byIndex = Map.fromListWith (++) $ flip map (Map.toList resolvedMap)
            $ \(ident, rp) ->
                (indexName $ rpIndex rp,
                    [( ident
                    , rpCache rp
                    , maybe Map.empty snd $ Map.lookup (packageIdentifierName ident) toCalc
                    )])
    res <- forM (Map.toList byIndex) $ \(indexName', pkgs) -> withCabalFiles indexName' pkgs
        $ \ident flags cabalBS -> do
            (_warnings,gpd) <- readPackageUnresolvedBS Nothing cabalBS
            let packageConfig = PackageConfig
                    { packageConfigEnableTests = False
                    , packageConfigEnableBenchmarks = False
                    , packageConfigFlags = flags
                    , packageConfigCompilerVersion = compilerVersion
                    , packageConfigPlatform = platform
                    }
                name = packageIdentifierName ident
                pd = resolvePackageDescription packageConfig gpd
                exes = Set.fromList $ map (ExeName . T.pack . exeName) $ executables pd
                notMe = Set.filter (/= name) . Map.keysSet
            return (name, MiniPackageInfo
                { mpiVersion = packageIdentifierVersion ident
                , mpiFlags = flags
                , mpiPackageDeps = notMe $ packageDependencies pd
                , mpiToolDeps = Map.keysSet $ packageToolDependencies pd
                , mpiExes = exes
                , mpiHasLibrary = maybe
                    False
                    (buildable . libBuildInfo)
                    (library pd)
                })
    return (Map.fromList $ concat res, missingIdents)
  where
    idents0 = Map.fromList
        $ map (\(n, (v, f)) -> (PackageIdentifier n v, Left f))
        $ Map.toList toCalc

-- | Resolve all packages necessary to install for
getDeps :: MiniBuildPlan
        -> (PackageName -> Bool) -- ^ is it shadowed by a local package?
        -> Map PackageName (Set PackageName)
        -> ResolveState
getDeps mbp isShadowed packages =
    execState (mapM_ (uncurry goName) $ Map.toList packages) ResolveState
        { rsVisited = Map.empty
        , rsUnknown = Map.empty
        , rsShadowed = Map.empty
        , rsToInstall = Map.empty
        , rsUsedBy = Map.empty
        }
  where
    toolMap = getToolMap mbp

    -- | Returns a set of shadowed packages we depend on.
    goName :: PackageName -> Set PackageName -> State ResolveState (Set PackageName)
    goName name users = do
        -- Even though we could check rsVisited first and short-circuit things
        -- earlier, lookup in mbpPackages first so that we can produce more
        -- usable error information on missing dependencies
        rs <- get
        put rs
            { rsUsedBy = Map.insertWith Set.union name users $ rsUsedBy rs
            }
        case Map.lookup name $ mbpPackages mbp of
            Nothing -> do
                modify $ \rs' -> rs'
                    { rsUnknown = Map.insertWith Set.union name users $ rsUnknown rs'
                    }
                return Set.empty
            Just mpi -> case Map.lookup name (rsVisited rs) of
              Just shadowed -> return shadowed
              Nothing -> do
                put rs { rsVisited = Map.insert name Set.empty $ rsVisited rs }
                let depsForTools = Set.unions $ mapMaybe (flip Map.lookup toolMap) (Set.toList $ mpiToolDeps mpi)
                let deps = Set.filter (/= name) (mpiPackageDeps mpi <> depsForTools)
                shadowed <- fmap F.fold $ Tr.forM (Set.toList deps) $ \dep ->
                    if isShadowed dep
                        then do
                            modify $ \rs' -> rs'
                                { rsShadowed = Map.insertWith
                                    Set.union
                                    dep
                                    (Set.singleton $ PackageIdentifier name (mpiVersion mpi))
                                    (rsShadowed rs')
                                }
                            return $ Set.singleton dep
                        else do
                            shadowed <- goName dep (Set.singleton name)
                            let m = Map.fromSet (\_ -> Set.singleton $ PackageIdentifier name (mpiVersion mpi)) shadowed
                            modify $ \rs' -> rs'
                                { rsShadowed = Map.unionWith Set.union m $ rsShadowed rs'
                                }
                            return shadowed
                modify $ \rs' -> rs'
                    { rsToInstall = Map.insert name (mpiVersion mpi, mpiFlags mpi) $ rsToInstall rs'
                    , rsVisited = Map.insert name shadowed $ rsVisited rs'
                    }
                return shadowed

-- | Look up with packages provide which tools.
type ToolMap = Map ByteString (Set PackageName)

-- | Map from tool name to package providing it
getToolMap :: MiniBuildPlan -> Map Text (Set PackageName)
getToolMap mbp =
      Map.unionsWith Set.union

    {- We no longer do this, following discussion at:

        https://github.com/commercialhaskell/stack/issues/308#issuecomment-112076704

    -- First grab all of the package names, for times where a build tool is
    -- identified by package name
    $ Map.fromList (map (packageNameByteString &&& Set.singleton) (Map.keys ps))
    -}

    -- And then get all of the explicit executable names
    $ concatMap goPair (Map.toList ps)
  where
    ps = mbpPackages mbp

    goPair (pname, mpi) =
        map (flip Map.singleton (Set.singleton pname) . unExeName)
      $ Set.toList
      $ mpiExes mpi

-- | Download the 'Snapshots' value from stackage.org.
getSnapshots :: (MonadThrow m, MonadIO m, MonadReader env m, HasHttpManager env, HasStackRoot env, HasConfig env)
             => m Snapshots
getSnapshots = askLatestSnapshotUrl >>= parseUrl . T.unpack >>= downloadJSON

-- | Most recent Nightly and newest LTS version per major release.
data Snapshots = Snapshots
    { snapshotsNightly :: !Day
    , snapshotsLts     :: !(IntMap Int)
    }
    deriving Show
instance FromJSON Snapshots where
    parseJSON = withObject "Snapshots" $ \o -> Snapshots
        <$> (o .: "nightly" >>= parseNightly)
        <*> (fmap IntMap.unions
                $ mapM (parseLTS . snd)
                $ filter (isLTS . fst)
                $ HM.toList o)
      where
        parseNightly t =
            case parseSnapName t of
                Left e -> fail $ show e
                Right (LTS _ _) -> fail "Unexpected LTS value"
                Right (Nightly d) -> return d

        isLTS = ("lts-" `T.isPrefixOf`)

        parseLTS = withText "LTS" $ \t ->
            case parseSnapName t of
                Left e -> fail $ show e
                Right (LTS x y) -> return $ IntMap.singleton x y
                Right (Nightly _) -> fail "Unexpected nightly value"

-- | Load up a 'MiniBuildPlan', preferably from cache
loadMiniBuildPlan
    :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader env m, HasHttpManager env, HasConfig env, HasGHCVariant env, MonadBaseControl IO m, MonadCatch m)
    => SnapName
    -> m MiniBuildPlan
loadMiniBuildPlan name = do
    path <- configMiniBuildPlanCache name
    taggedDecodeOrLoad path $ liftM buildPlanFixes $ do
        bp <- loadBuildPlan name
        toMiniBuildPlan
            (siCompilerVersion $ bpSystemInfo bp)
            (siCorePackages $ bpSystemInfo bp)
            (fmap goPP $ bpPackages bp)
  where
    goPP pp =
        ( ppVersion pp
        , pcFlagOverrides $ ppConstraints pp
        )

-- | Some hard-coded fixes for build plans, hopefully to be irrelevant over
-- time.
buildPlanFixes :: MiniBuildPlan -> MiniBuildPlan
buildPlanFixes mbp = mbp
    { mbpPackages = Map.fromList $ map go $ Map.toList $ mbpPackages mbp
    }
  where
    go (name, mpi) =
        (name, mpi
            { mpiFlags = goF (packageNameString name) (mpiFlags mpi)
            })

    goF "persistent-sqlite" = Map.insert $(mkFlagName "systemlib") False
    goF "yaml" = Map.insert $(mkFlagName "system-libyaml") False
    goF _ = id

-- | Load the 'BuildPlan' for the given snapshot. Will load from a local copy
-- if available, otherwise downloading from Github.
loadBuildPlan :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader env m, HasHttpManager env, HasStackRoot env)
              => SnapName
              -> m BuildPlan
loadBuildPlan name = do
    env <- ask
    let stackage = getStackRoot env
    file' <- parseRelFile $ T.unpack file
    let fp = buildPlanDir stackage </> file'
    $logDebug $ "Decoding build plan from: " <> T.pack (toFilePath fp)
    eres <- liftIO $ decodeFileEither $ toFilePath fp
    case eres of
        Right bp -> return bp
        Left e -> do
            $logDebug $ "Decoding build plan from file failed: " <> T.pack (show e)
            createTree (parent fp)
            req <- parseUrl $ T.unpack url
            $logSticky $ "Downloading " <> renderSnapName name <> " build plan ..."
            $logDebug $ "Downloading build plan from: " <> url
            _ <- download req { checkStatus = handle404 } fp
            $logStickyDone $ "Downloaded " <> renderSnapName name <> " build plan."
            liftIO (decodeFileEither $ toFilePath fp) >>= either throwM return

  where
    file = renderSnapName name <> ".yaml"
    reponame =
        case name of
            LTS _ _ -> "lts-haskell"
            Nightly _ -> "stackage-nightly"
    url = rawGithubUrl "fpco" reponame "master" file
    handle404 (Status 404 _) _ _ = Just $ SomeException $ SnapshotNotFound name
    handle404 _ _ _              = Nothing

-- | Find the set of @FlagName@s necessary to get the given
-- @GenericPackageDescription@ to compile against the given @BuildPlan@. Will
-- only modify non-manual flags, and will prefer default values for flags.
-- Returns @Nothing@ if no combination exists.
checkBuildPlan :: (MonadLogger m, MonadThrow m, MonadIO m, MonadReader env m, HasConfig env, MonadCatch m)
               => Map PackageName Version -- ^ locally available packages
               -> MiniBuildPlan
               -> GenericPackageDescription
               -> m (Either DepErrors (Map PackageName (Map FlagName Bool)))
checkBuildPlan locals mbp gpd = do
    platform <- asks (configPlatform . getConfig)
    return $ loop platform flagOptions
  where
    packages = Map.union locals $ fmap mpiVersion $ mbpPackages mbp
    loop _ [] = assert False $ Left Map.empty
    loop platform (flags:rest)
        | Map.null errs = Right $
            if Map.null flags
                then Map.empty
                else Map.singleton (packageName pkg) flags
        | null rest = Left errs
        | otherwise = loop platform rest
      where
        errs = checkDeps (packageName pkg) (packageDeps pkg) packages
        pkg = resolvePackage pkgConfig gpd
        pkgConfig = PackageConfig
            { packageConfigEnableTests = True
            , packageConfigEnableBenchmarks = True
            , packageConfigFlags = flags
            , packageConfigCompilerVersion = compilerVersion
            , packageConfigPlatform = platform
            }

    compilerVersion = mbpCompilerVersion mbp

    flagName' = fromCabalFlagName . flagName

    -- Avoid exponential complexity in flag combinations making us sad pandas.
    -- See: https://github.com/commercialhaskell/stack/issues/543
    maxFlagOptions = 128

    flagOptions = take maxFlagOptions $ map Map.fromList $ mapM getOptions $ genPackageFlags gpd
    getOptions f
        | flagManual f = [(flagName' f, flagDefault f)]
        | flagDefault f =
            [ (flagName' f, True)
            , (flagName' f, False)
            ]
        | otherwise =
            [ (flagName' f, False)
            , (flagName' f, True)
            ]

-- | Checks if the given package dependencies can be satisfied by the given set
-- of packages. Will fail if a package is either missing or has a version
-- outside of the version range.
checkDeps :: PackageName -- ^ package using dependencies, for constructing DepErrors
          -> Map PackageName VersionRange
          -> Map PackageName Version
          -> DepErrors
checkDeps myName deps packages =
    Map.unionsWith mappend $ map go $ Map.toList deps
  where
    go :: (PackageName, VersionRange) -> DepErrors
    go (name, range) =
        case Map.lookup name packages of
            Nothing -> Map.singleton name DepError
                { deVersion = Nothing
                , deNeededBy = Map.singleton myName range
                }
            Just v
                | withinRange v range -> Map.empty
                | otherwise -> Map.singleton name DepError
                    { deVersion = Just v
                    , deNeededBy = Map.singleton myName range
                    }

type DepErrors = Map PackageName DepError
data DepError = DepError
    { deVersion :: !(Maybe Version)
    , deNeededBy :: !(Map PackageName VersionRange)
    }
instance Monoid DepError where
    mempty = DepError Nothing Map.empty
    mappend (DepError a x) (DepError b y) = DepError
        (maybe a Just b)
        (Map.unionWith C.intersectVersionRanges x y)

-- | Find a snapshot and set of flags that is compatible with the given
-- 'GenericPackageDescription'. Returns 'Nothing' if no such snapshot is found.
findBuildPlan :: (MonadIO m, MonadCatch m, MonadLogger m, MonadReader env m, HasHttpManager env, HasConfig env, HasGHCVariant env, MonadBaseControl IO m)
              => [GenericPackageDescription]
              -> [SnapName]
              -> m (Maybe (SnapName, Map PackageName (Map FlagName Bool)))
findBuildPlan gpds0 =
    loop
  where
    loop [] = return Nothing
    loop (name:names') = do
        mbp <- loadMiniBuildPlan name
        $logInfo $ "Checking against build plan " <> renderSnapName name
        res <- mapM (checkBuildPlan localNames mbp) gpds0
        case partitionEithers res of
            ([], flags) -> return $ Just (name, Map.unions flags)
            (errs, _) -> do
                $logInfo ""
                $logInfo "* Build plan did not match your requirements:"
                displayDepErrors $ Map.unionsWith mappend errs
                $logInfo ""
                loop names'

    localNames = Map.fromList $ map (fromCabalIdent . C.package . C.packageDescription) gpds0

    fromCabalIdent (C.PackageIdentifier name version) =
        (fromCabalPackageName name, fromCabalVersion version)

displayDepErrors :: MonadLogger m => DepErrors -> m ()
displayDepErrors errs =
    F.forM_ (Map.toList errs) $ \(depName, DepError mversion neededBy) -> do
        $logInfo $ T.concat
            [ "    "
            , T.pack $ packageNameString depName
            , case mversion of
                Nothing -> " not found"
                Just version -> T.concat
                    [ " version "
                    , T.pack $ versionString version
                    , " found"
                    ]
            ]
        F.forM_ (Map.toList neededBy) $ \(user, range) -> $logInfo $ T.concat
            [ "    - "
            , T.pack $ packageNameString user
            , " requires "
            , T.pack $ display range
            ]
        $logInfo ""

shadowMiniBuildPlan :: MiniBuildPlan
                    -> Set PackageName
                    -> (MiniBuildPlan, Map PackageName MiniPackageInfo)
shadowMiniBuildPlan (MiniBuildPlan cv pkgs0) shadowed =
    (MiniBuildPlan cv $ Map.fromList met, Map.fromList unmet)
  where
    pkgs1 = Map.difference pkgs0 $ Map.fromSet (\_ -> ()) shadowed

    depsMet = flip execState Map.empty $ mapM_ (check Set.empty) (Map.keys pkgs1)

    check visited name
        | name `Set.member` visited =
            error $ "shadowMiniBuildPlan: cycle detected, your MiniBuildPlan is broken: " ++ show (visited, name)
        | otherwise = do
            m <- get
            case Map.lookup name m of
                Just x -> return x
                Nothing ->
                    case Map.lookup name pkgs1 of
                        Nothing
                            | name `Set.member` shadowed -> return False

                            -- In this case, we have to assume that we're
                            -- constructing a build plan on a different OS or
                            -- architecture, and therefore different packages
                            -- are being chosen. The common example of this is
                            -- the Win32 package.
                            | otherwise -> return True
                        Just mpi -> do
                            let visited' = Set.insert name visited
                            ress <- mapM (check visited') (Set.toList $ mpiPackageDeps mpi)
                            let res = and ress
                            modify $ \m' -> Map.insert name res m'
                            return res

    (met, unmet) = partitionEithers $ map toEither $ Map.toList pkgs1

    toEither pair@(name, _) =
        wrapper pair
      where
        wrapper =
            case Map.lookup name depsMet of
                Just True -> Left
                Just False -> Right
                Nothing -> assert False Right

parseCustomMiniBuildPlan :: (MonadIO m, MonadCatch m, MonadLogger m, MonadReader env m, HasHttpManager env, HasConfig env, MonadBaseControl IO m)
                         => Path Abs File -- ^ stack.yaml file location
                         -> T.Text -> m MiniBuildPlan
parseCustomMiniBuildPlan stackYamlFP url0 = do
    yamlFP <- getYamlFP url0

    yamlBS <- liftIO $ S.readFile $ toFilePath yamlFP
    let yamlHash = S8.unpack $ B16.encode $ SHA256.hash yamlBS
    binaryFilename <- parseRelFile $ yamlHash ++ ".bin"
    customPlanDir <- getCustomPlanDir
    let binaryFP = customPlanDir </> $(mkRelDir "bin") </> binaryFilename

    taggedDecodeOrLoad binaryFP $ do
        cs <- either throwM return $ decodeEither' yamlBS
        let addFlags :: PackageIdentifier -> (PackageName, (Version, Map FlagName Bool))
            addFlags (PackageIdentifier name ver) =
                (name, (ver, fromMaybe Map.empty $ Map.lookup name $ csFlags cs))
        toMiniBuildPlan
            (csCompilerVersion cs)
            Map.empty
            (Map.fromList $ map addFlags $ Set.toList $ csPackages cs)
  where
    getCustomPlanDir = do
        root <- asks $ configStackRoot . getConfig
        return $ root </> $(mkRelDir "custom-plan")

    -- Get the path to the YAML file
    getYamlFP url =
        case parseUrl $ T.unpack url of
            Just req -> getYamlFPFromReq url req
            Nothing -> getYamlFPFromFile url

    getYamlFPFromReq url req = do
        let hashStr = S8.unpack $ B16.encode $ SHA256.hash $ encodeUtf8 url
        hashFP <- parseRelFile $ hashStr ++ ".yaml"
        customPlanDir <- getCustomPlanDir

        let cacheFP = customPlanDir </> $(mkRelDir "yaml") </> hashFP
        _ <- download req cacheFP
        return cacheFP

    getYamlFPFromFile url = do
        fp <- liftIO $ canonicalizePath $ toFilePath (parent stackYamlFP) FP.</> T.unpack (fromMaybe url $
            T.stripPrefix "file://" url <|> T.stripPrefix "file:" url)
        parseAbsFile fp

data CustomSnapshot = CustomSnapshot
    { csCompilerVersion :: !CompilerVersion
    , csPackages :: !(Set PackageIdentifier)
    , csFlags :: !(Map PackageName (Map FlagName Bool))
    }
instance FromJSON CustomSnapshot where
    parseJSON = withObject "CustomSnapshot" $ \o -> CustomSnapshot
        <$> ((o .: "compiler") >>= \t ->
                case parseCompilerVersion t of
                    Nothing -> fail $ "Invalid compiler: " ++ T.unpack t
                    Just compilerVersion -> return compilerVersion)
        <*> o .: "packages"
        <*> o .:? "flags" .!= Map.empty
