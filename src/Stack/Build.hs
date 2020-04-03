{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Build the project.

module Stack.Build
  (build
  ,loadPackage
  ,mkBaseConfigOpts
  ,queryBuildInfo
  ,splitObjsWarning
  ,CabalVersionException(..))
  where

import           Stack.Prelude hiding (loadPackage)
import           Data.Aeson (Value (Object, Array), (.=), object)
import qualified Data.HashMap.Strict as HM
import           Data.List ((\\), isPrefixOf)
import           Data.List.Extra (groupSort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import           Data.Text.Read (decimal)
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import qualified Distribution.PackageDescription as C
import           Distribution.Types.Dependency (depLibraries)
import           Distribution.Version (mkVersion)
import           Path (parent)
import           Stack.Build.ConstructPlan
import           Stack.Build.Execute
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Package
import           Stack.Types.Build
import           Stack.Types.Config
import           Stack.Types.NamedComponent
import           Stack.Types.Package
import           Stack.Types.SourceMap

import           Stack.Types.Compiler (compilerVersionText, getGhcVersion)
import           System.Terminal (fixCodePage)

-- | Build.
--
--   If a buildLock is passed there is an important contract here.  That lock must
--   protect the snapshot, and it must be safe to unlock it if there are no further
--   modifications to the snapshot to be performed by this build.
build :: HasEnvConfig env
      => Maybe (Set (Path Abs File) -> IO ()) -- ^ callback after discovering all local files
      -> RIO env ()
build msetLocalFiles = do
  mcp <- view $ configL.to configModifyCodePage
  ghcVersion <- view $ actualCompilerVersionL.to getGhcVersion
  fixCodePage mcp ghcVersion $ do
    bopts <- view buildOptsL
    sourceMap <- view $ envConfigL.to envConfigSourceMap
    locals <- projectLocalPackages
    depsLocals <- localDependencies
    let allLocals = locals <> depsLocals

    checkSubLibraryDependencies (Map.elems $ smProject sourceMap)

    -- Set local files, necessary for file watching
    stackYaml <- view stackYamlL
    for_ msetLocalFiles $ \setLocalFiles -> do
      files <- sequence
        [lpFiles lp | lp <- allLocals]
      liftIO $ setLocalFiles $ Set.insert stackYaml $ Set.unions files

    checkComponentsBuildable allLocals

    installMap <- toInstallMap sourceMap
    (installedMap, globalDumpPkgs, snapshotDumpPkgs, localDumpPkgs) <-
        getInstalled installMap

    boptsCli <- view $ envConfigL.to envConfigBuildOptsCLI
    baseConfigOpts <- mkBaseConfigOpts boptsCli
    plan <- constructPlan baseConfigOpts localDumpPkgs loadPackage sourceMap installedMap (boptsCLIInitialBuildSteps boptsCli)

    allowLocals <- view $ configL.to configAllowLocals
    unless allowLocals $ case justLocals plan of
      [] -> return ()
      localsIdents -> throwM $ LocalPackagesPresent localsIdents

    checkCabalVersion
    warnAboutSplitObjs bopts
    warnIfExecutablesWithSameNameCouldBeOverwritten locals plan

    when (boptsPreFetch bopts) $
        preFetch plan

    if boptsCLIDryrun boptsCli
        then printPlan plan
        else executePlan boptsCli baseConfigOpts locals
                         globalDumpPkgs
                         snapshotDumpPkgs
                         localDumpPkgs
                         installedMap
                         (smtTargets $ smTargets sourceMap)
                         plan

justLocals :: Plan -> [PackageIdentifier]
justLocals =
    map taskProvides .
    filter ((== Local) . taskLocation) .
    Map.elems .
    planTasks

checkCabalVersion :: HasEnvConfig env => RIO env ()
checkCabalVersion = do
    allowNewer <- view $ configL.to configAllowNewer
    cabalVer <- view cabalVersionL
    -- https://github.com/haskell/cabal/issues/2023
    when (allowNewer && cabalVer < mkVersion [1, 22]) $ throwM $
        CabalVersionException $
            "Error: --allow-newer requires at least Cabal version 1.22, but version " ++
            versionString cabalVer ++
            " was found."
    -- Since --exact-configuration is always passed, some old cabal
    -- versions can no longer be used. See the following link for why
    -- it's 1.19.2:
    -- https://github.com/haskell/cabal/blob/580fe6b6bf4e1648b2f66c1cb9da9f1f1378492c/cabal-install/Distribution/Client/Setup.hs#L592
    when (cabalVer < mkVersion [1, 19, 2]) $ throwM $
        CabalVersionException $
            "Stack no longer supports Cabal versions older than 1.19.2, but version " ++
            versionString cabalVer ++
            " was found.  To fix this, consider updating the resolver to lts-3.0 or later / nightly-2015-05-05 or later."

newtype CabalVersionException = CabalVersionException { unCabalVersionException :: String }
    deriving (Typeable)

instance Show CabalVersionException where show = unCabalVersionException
instance Exception CabalVersionException

-- | See https://github.com/commercialhaskell/stack/issues/1198.
warnIfExecutablesWithSameNameCouldBeOverwritten
    :: HasLogFunc env => [LocalPackage] -> Plan -> RIO env ()
warnIfExecutablesWithSameNameCouldBeOverwritten locals plan = do
    logDebug "Checking if we are going to build multiple executables with the same name"
    forM_ (Map.toList warnings) $ \(exe,(toBuild,otherLocals)) -> do
        let exe_s
                | length toBuild > 1 = "several executables with the same name:"
                | otherwise = "executable"
            exesText pkgs =
                T.intercalate
                    ", "
                    ["'" <> T.pack (packageNameString p) <> ":" <> exe <> "'" | p <- pkgs]
        (logWarn . display . T.unlines . concat)
            [ [ "Building " <> exe_s <> " " <> exesText toBuild <> "." ]
            , [ "Only one of them will be available via 'stack exec' or locally installed."
              | length toBuild > 1
              ]
            , [ "Other executables with the same name might be overwritten: " <>
                exesText otherLocals <> "."
              | not (null otherLocals)
              ]
            ]
  where
    -- Cases of several local packages having executables with the same name.
    -- The Map entries have the following form:
    --
    --  executable name: ( package names for executables that are being built
    --                   , package names for other local packages that have an
    --                     executable with the same name
    --                   )
    warnings :: Map Text ([PackageName],[PackageName])
    warnings =
        Map.mapMaybe
            (\(pkgsToBuild,localPkgs) ->
                case (pkgsToBuild,NE.toList localPkgs \\ NE.toList pkgsToBuild) of
                    (_ :| [],[]) ->
                        -- We want to build the executable of single local package
                        -- and there are no other local packages with an executable of
                        -- the same name. Nothing to warn about, ignore.
                        Nothing
                    (_,otherLocals) ->
                        -- We could be here for two reasons (or their combination):
                        -- 1) We are building two or more executables with the same
                        --    name that will end up overwriting each other.
                        -- 2) In addition to the executable(s) that we want to build
                        --    there are other local packages with an executable of the
                        --    same name that might get overwritten.
                        -- Both cases warrant a warning.
                        Just (NE.toList pkgsToBuild,otherLocals))
            (Map.intersectionWith (,) exesToBuild localExes)
    exesToBuild :: Map Text (NonEmpty PackageName)
    exesToBuild =
        collect
            [ (exe,pkgName')
            | (pkgName',task) <- Map.toList (planTasks plan)
            , TTLocalMutable lp <- [taskType task]
            , exe <- (Set.toList . exeComponents . lpComponents) lp
            ]
    localExes :: Map Text (NonEmpty PackageName)
    localExes =
        collect
            [ (exe,packageName pkg)
            | pkg <- map lpPackage locals
            , exe <- Set.toList (packageExes pkg)
            ]
    collect :: Ord k => [(k,v)] -> Map k (NonEmpty v)
    collect = Map.map NE.fromList . Map.fromDistinctAscList . groupSort

warnAboutSplitObjs :: HasLogFunc env => BuildOpts -> RIO env ()
warnAboutSplitObjs bopts | boptsSplitObjs bopts = do
    logWarn $ "Building with --split-objs is enabled. " <> fromString splitObjsWarning
warnAboutSplitObjs _ = return ()

splitObjsWarning :: String
splitObjsWarning = unwords
     [ "Note that this feature is EXPERIMENTAL, and its behavior may be changed and improved."
     , "You will need to clean your workdirs before use. If you want to compile all dependencies"
     , "with split-objs, you will need to delete the snapshot (and all snapshots that could"
     , "reference that snapshot)."
     ]

-- | Get the @BaseConfigOpts@ necessary for constructing configure options
mkBaseConfigOpts :: (HasEnvConfig env)
                 => BuildOptsCLI -> RIO env BaseConfigOpts
mkBaseConfigOpts boptsCli = do
    bopts <- view buildOptsL
    snapDBPath <- packageDatabaseDeps
    localDBPath <- packageDatabaseLocal
    snapInstallRoot <- installationRootDeps
    localInstallRoot <- installationRootLocal
    packageExtraDBs <- packageDatabaseExtra
    return BaseConfigOpts
        { bcoSnapDB = snapDBPath
        , bcoLocalDB = localDBPath
        , bcoSnapInstallRoot = snapInstallRoot
        , bcoLocalInstallRoot = localInstallRoot
        , bcoBuildOpts = bopts
        , bcoBuildOptsCLI = boptsCli
        , bcoExtraDBs = packageExtraDBs
        }

-- | Provide a function for loading package information from the package index
loadPackage
  :: (HasBuildConfig env, HasSourceMap env)
  => PackageLocationImmutable
  -> Map FlagName Bool
  -> [Text] -- ^ GHC options
  -> [Text] -- ^ Cabal configure options
  -> RIO env Package
loadPackage loc flags ghcOptions cabalConfigOpts = do
  compiler <- view actualCompilerVersionL
  platform <- view platformL
  let pkgConfig = PackageConfig
        { packageConfigEnableTests = False
        , packageConfigEnableBenchmarks = False
        , packageConfigFlags = flags
        , packageConfigGhcOptions = ghcOptions
        , packageConfigCabalConfigOpts = cabalConfigOpts
        , packageConfigCompilerVersion = compiler
        , packageConfigPlatform = platform
        }
  resolvePackage pkgConfig <$> loadCabalFileImmutable loc

-- | Query information about the build and print the result to stdout in YAML format.
queryBuildInfo :: HasEnvConfig env
               => [Text] -- ^ selectors
               -> RIO env ()
queryBuildInfo selectors0 =
        rawBuildInfo
    >>= select id selectors0
    >>= liftIO . TIO.putStrLn . addGlobalHintsComment . decodeUtf8 . Yaml.encode
  where
    select _ [] value = return value
    select front (sel:sels) value =
        case value of
            Object o ->
                case HM.lookup sel o of
                    Nothing -> err "Selector not found"
                    Just value' -> cont value'
            Array v ->
                case decimal sel of
                    Right (i, "")
                        | i >= 0 && i < V.length v -> cont $ v V.! i
                        | otherwise -> err "Index out of range"
                    _ -> err "Encountered array and needed numeric selector"
            _ -> err $ "Cannot apply selector to " ++ show value
      where
        cont = select (front . (sel:)) sels
        err msg = throwString $ msg ++ ": " ++ show (front [sel])
    -- Include comments to indicate that this portion of the "stack
    -- query" API is not necessarily stable.
    addGlobalHintsComment
      | null selectors0 = T.replace globalHintsLine ("\n" <> globalHintsComment <> globalHintsLine)
      -- Append comment instead of pre-pending. The reasoning here is
      -- that something *could* expect that the result of 'stack query
      -- global-hints ghc-boot' is just a string literal. Seems easier
      -- for to expect the first line of the output to be the literal.
      | ["global-hints"] `isPrefixOf` selectors0 = (<> ("\n" <> globalHintsComment))
      | otherwise = id
    globalHintsLine = "\nglobal-hints:\n"
    globalHintsComment = T.concat
      [ "# Note: global-hints is experimental and may be renamed / removed in the future.\n"
      , "# See https://github.com/commercialhaskell/stack/issues/3796"
      ]
-- | Get the raw build information object
rawBuildInfo :: HasEnvConfig env => RIO env Value
rawBuildInfo = do
    locals <- projectLocalPackages
    wantedCompiler <- view $ wantedCompilerVersionL.to (utf8BuilderToText . display)
    actualCompiler <- view $ actualCompilerVersionL.to compilerVersionText
    return $ object
        [ "locals" .= Object (HM.fromList $ map localToPair locals)
        , "compiler" .= object
            [ "wanted" .= wantedCompiler
            , "actual" .= actualCompiler
            ]
        ]
  where
    localToPair lp =
        (T.pack $ packageNameString $ packageName p, value)
      where
        p = lpPackage lp
        value = object
            [ "version" .= CabalString (packageVersion p)
            , "path" .= toFilePath (parent $ lpCabalFile lp)
            ]

checkComponentsBuildable :: MonadThrow m => [LocalPackage] -> m ()
checkComponentsBuildable lps =
    unless (null unbuildable) $ throwM $ SomeTargetsNotBuildable unbuildable
  where
    unbuildable =
        [ (packageName (lpPackage lp), c)
        | lp <- lps
        , c <- Set.toList (lpUnbuildable lp)
        ]

-- | Find if sublibrary dependency exist in each project
checkSubLibraryDependencies :: HasLogFunc env => [ProjectPackage] -> RIO env ()
checkSubLibraryDependencies proj = do
  forM_ proj $ \p -> do
    C.GenericPackageDescription _ _ lib subLibs foreignLibs exes tests benches <- liftIO $ cpGPD . ppCommon $ p

    let dependencies = concatMap getDeps subLibs <>
                       concatMap getDeps foreignLibs <>
                       concatMap getDeps exes <>
                       concatMap getDeps tests <>
                       concatMap getDeps benches <>
                       maybe [] C.condTreeConstraints lib
        libraries = concatMap (toList . depLibraries) dependencies

    when (subLibDepExist libraries)
      (logWarn "SubLibrary dependency is not supported, this will almost certainly fail")
  where
    getDeps (_, C.CondNode _ dep _) = dep
    subLibDepExist lib = 
      any (\x ->
        case x of
          C.LSubLibName _ -> True
          C.LMainLibName  -> False
      ) lib
