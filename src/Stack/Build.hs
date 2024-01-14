{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Build the project.

module Stack.Build
  ( buildCmd
  , build
  , buildLocalTargets
  , loadPackage
  , mkBaseConfigOpts
  , splitObjsWarning
  ) where

import           Data.Attoparsec.Args ( EscapingMode (Escaping), parseArgs )
import           Data.List ( (\\) )
import           Data.List.Extra ( groupSort )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
-- import qualified Distribution.PackageDescription as C
-- import           Distribution.Types.Dependency ( Dependency (..), depLibraries )
import           Distribution.Version ( mkVersion )
import           RIO.NonEmpty ( nonEmpty )
import qualified RIO.NonEmpty as NE
import           Stack.Build.ConstructPlan ( constructPlan )
import           Stack.Build.Execute ( executePlan, preFetch, printPlan )
import           Stack.Build.Installed ( getInstalled, toInstallMap )
import           Stack.Build.Source ( localDependencies, projectLocalPackages )
import           Stack.Build.Target ( NeedTargets (..) )
import           Stack.FileWatch ( fileWatch, fileWatchPoll )
import           Stack.Package ( buildableExes, resolvePackage )
import           Stack.Prelude hiding ( loadPackage )
import           Stack.Runners ( ShouldReexec (..), withConfig, withEnvConfig )
import           Stack.Setup ( withNewLocalBuildTargets )
import           Stack.Types.Build
                   ( Plan (..), Task (..), TaskType (..), taskLocation
                   , taskProvides
                   )
import           Stack.Types.Build.Exception
                   ( BuildException (..), BuildPrettyException (..) )
import           Stack.Types.BuildConfig ( HasBuildConfig, stackYamlL )
import           Stack.Types.BuildOpts
                   ( BuildCommand (..), BuildOpts (..), BuildOptsCLI (..)
                   , FileWatchOpts (..), buildOptsMonoidBenchmarksL
                   , buildOptsMonoidHaddockL, buildOptsMonoidInstallExesL
                   , buildOptsMonoidTestsL
                   )
import           Stack.Types.Compiler ( getGhcVersion )
import           Stack.Types.CompilerPaths ( cabalVersionL )
import           Stack.Types.Config
                   ( Config (..), HasConfig (..), buildOptsL
                   )
import           Stack.Types.ConfigureOpts ( BaseConfigOpts (..) )
import           Stack.Types.EnvConfig
                   ( EnvConfig (..), HasEnvConfig (..), HasSourceMap
                   , actualCompilerVersionL, installationRootDeps
                   , installationRootLocal, packageDatabaseDeps
                   , packageDatabaseExtra, packageDatabaseLocal
                   )
import           Stack.Types.GlobalOpts ( globalOptsBuildOptsMonoidL )
import           Stack.Types.NamedComponent ( exeComponents )
import           Stack.Types.Package
                   ( InstallLocation (..), LocalPackage (..), Package (..)
                   , PackageConfig (..), lpFiles, lpFilesForComponents
                   )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Runner ( Runner, globalOptsL )
import           Stack.Types.SourceMap
                   ( SMTargets (..)
                   , SourceMap (..), Target (..) )
import           System.Terminal ( fixCodePage )

newtype CabalVersionPrettyException
  = CabalVersionNotSupported Version
  deriving (Show, Typeable)

instance Pretty CabalVersionPrettyException where
  pretty (CabalVersionNotSupported cabalVer) =
    "[S-5973]"
    <> line
    <> fillSep
         [ flow "Stack does not support Cabal versions before 1.24, but \
                \version"
         , fromString $ versionString cabalVer
         , flow "was found. To fix this, consider updating the snapshot to"
         , style Shell "lts-7.0"
         , flow "or later or to"
         , style Shell "nightly-2016-05-26"
         , flow "or later."
         ]

instance Exception CabalVersionPrettyException

-- | Helper for build and install commands
buildCmd :: BuildOptsCLI -> RIO Runner ()
buildCmd opts = do
  when (any (("-prof" `elem`) . fromRight [] . parseArgs Escaping) opts.ghcOptions) $
    prettyThrowIO GHCProfOptionInvalid
  local (over globalOptsL modifyGO) $
    case opts.fileWatch of
      FileWatchPoll -> fileWatchPoll (inner . Just)
      FileWatch -> fileWatch (inner . Just)
      NoFileWatch -> inner Nothing
 where
  inner ::
       Maybe (Set (Path Abs File) -> IO ())
    -> RIO Runner ()
  inner setLocalFiles = withConfig YesReexec $ withEnvConfig NeedTargets opts $
      Stack.Build.build setLocalFiles
  -- Read the build command from the CLI and enable it to run
  modifyGO =
    case opts.command of
      Test -> set
        (globalOptsBuildOptsMonoidL . buildOptsMonoidTestsL)
        (Just True)
      Haddock -> set
        (globalOptsBuildOptsMonoidL . buildOptsMonoidHaddockL)
        (Just True)
      Bench -> set
        (globalOptsBuildOptsMonoidL . buildOptsMonoidBenchmarksL)
        (Just True)
      Install -> set
        (globalOptsBuildOptsMonoidL . buildOptsMonoidInstallExesL)
        (Just True)
      Build -> id -- Default case is just Build

-- | Build.
--
--   If a buildLock is passed there is an important contract here.  That lock must
--   protect the snapshot, and it must be safe to unlock it if there are no further
--   modifications to the snapshot to be performed by this build.
build :: HasEnvConfig env
      => Maybe (Set (Path Abs File) -> IO ()) -- ^ callback after discovering all local files
      -> RIO env ()
build msetLocalFiles = do
  mcp <- view $ configL . to (.modifyCodePage)
  ghcVersion <- view $ actualCompilerVersionL . to getGhcVersion
  fixCodePage mcp ghcVersion $ do
    bopts <- view buildOptsL
    sourceMap <- view $ envConfigL . to (.sourceMap)
    locals <- projectLocalPackages
    depsLocals <- localDependencies
    let allLocals = locals <> depsLocals

    boptsCli <- view $ envConfigL . to (.buildOptsCLI)
    -- Set local files, necessary for file watching
    stackYaml <- view stackYamlL
    for_ msetLocalFiles $ \setLocalFiles -> do
      files <-
        if boptsCli.watchAll
        then sequence [lpFiles lp | lp <- allLocals]
        else forM allLocals $ \lp -> do
          let pn = lp.package.name
          case Map.lookup pn sourceMap.smTargets.smtTargets of
            Nothing ->
              pure Set.empty
            Just (TargetAll _) ->
              lpFiles lp
            Just (TargetComps components) ->
              lpFilesForComponents components lp
      liftIO $ setLocalFiles $ Set.insert stackYaml $ Set.unions files

    checkComponentsBuildable allLocals

    installMap <- toInstallMap sourceMap
    (installedMap, globalDumpPkgs, snapshotDumpPkgs, localDumpPkgs) <-
        getInstalled installMap

    baseConfigOpts <- mkBaseConfigOpts boptsCli
    plan <- constructPlan
              baseConfigOpts
              localDumpPkgs
              loadPackage
              sourceMap
              installedMap
              boptsCli.initialBuildSteps

    allowLocals <- view $ configL . to (.allowLocals)
    unless allowLocals $ case justLocals plan of
      [] -> pure ()
      localsIdents -> throwM $ LocalPackagesPresent localsIdents

    checkCabalVersion
    warnAboutSplitObjs bopts
    warnIfExecutablesWithSameNameCouldBeOverwritten locals plan

    when bopts.preFetch $
        preFetch plan

    if boptsCli.dryrun
      then printPlan plan
      else executePlan
             boptsCli
             baseConfigOpts
             locals
             globalDumpPkgs
             snapshotDumpPkgs
             localDumpPkgs
             installedMap
             sourceMap.smTargets.smtTargets
             plan

buildLocalTargets ::
     HasEnvConfig env
  => NonEmpty Text
  -> RIO env (Either SomeException ())
buildLocalTargets targets =
  tryAny $ withNewLocalBuildTargets (NE.toList targets) $ build Nothing

justLocals :: Plan -> [PackageIdentifier]
justLocals =
  map taskProvides .
  filter ((== Local) . taskLocation) .
  Map.elems .
  (.tasks)

checkCabalVersion :: HasEnvConfig env => RIO env ()
checkCabalVersion = do
  cabalVer <- view cabalVersionL
  when (cabalVer < mkVersion [1, 24]) $
    prettyThrowM $ CabalVersionNotSupported cabalVer

-- | See https://github.com/commercialhaskell/stack/issues/1198.
warnIfExecutablesWithSameNameCouldBeOverwritten ::
     HasTerm env
  => [LocalPackage]
  -> Plan
  -> RIO env ()
warnIfExecutablesWithSameNameCouldBeOverwritten locals plan = do
  logDebug "Checking if we are going to build multiple executables with the same name"
  forM_ (Map.toList warnings) $ \(exe, (toBuild, otherLocals)) -> do
    let exe_s
          | length toBuild > 1 = flow "several executables with the same name:"
          | otherwise = "executable"
        exesText pkgs =
          fillSep $ punctuate
            ","
            [ style
                PkgComponent
                (fromString $ packageNameString p <> ":" <> T.unpack exe)
            | p <- pkgs
            ]
    prettyWarnL $
         [ "Building"
         , exe_s
         , exesText toBuild <> "."
         ]
      <> [ fillSep
             [ flow "Only one of them will be available via"
             , style Shell "stack exec"
             , flow "or locally installed."
             ]
         | length toBuild > 1
         ]
      <> [ fillSep
             [ flow "Other executables with the same name might be overwritten:"
             , exesText otherLocals <> "."
             ]
         | not (null otherLocals)
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
      (\(pkgsToBuild, localPkgs) ->
        case (pkgsToBuild, NE.toList localPkgs \\ NE.toList pkgsToBuild) of
          (_ :| [], []) ->
            -- We want to build the executable of single local package
            -- and there are no other local packages with an executable of
            -- the same name. Nothing to warn about, ignore.
            Nothing
          (_, otherLocals) ->
            -- We could be here for two reasons (or their combination):
            -- 1) We are building two or more executables with the same
            --    name that will end up overwriting each other.
            -- 2) In addition to the executable(s) that we want to build
            --    there are other local packages with an executable of the
            --    same name that might get overwritten.
            -- Both cases warrant a warning.
            Just (NE.toList pkgsToBuild, otherLocals))
      (Map.intersectionWith (,) exesToBuild localExes)
  exesToBuild :: Map Text (NonEmpty PackageName)
  exesToBuild =
    collect
      [ (exe, pkgName')
      | (pkgName', task) <- Map.toList plan.tasks
      , TTLocalMutable lp <- [task.taskType]
      , exe <- (Set.toList . exeComponents . (.components)) lp
      ]
  localExes :: Map Text (NonEmpty PackageName)
  localExes =
    collect
      [ (exe, pkg.name)
      | pkg <- map (.package) locals
      , exe <- Set.toList (buildableExes pkg)
      ]
  collect :: Ord k => [(k, v)] -> Map k (NonEmpty v)
  collect = Map.mapMaybe nonEmpty . Map.fromDistinctAscList . groupSort

warnAboutSplitObjs :: HasTerm env => BuildOpts -> RIO env ()
warnAboutSplitObjs bopts |  bopts.splitObjs =
  prettyWarnL
    [ flow "Building with"
    , style Shell "--split-objs"
    , flow "is enabled."
    , flow splitObjsWarning
    ]
warnAboutSplitObjs _ = pure ()

splitObjsWarning :: String
splitObjsWarning =
  "Note that this feature is EXPERIMENTAL, and its behavior may be changed and \
  \improved. You will need to clean your workdirs before use. If you want to \
  \compile all dependencies with split-objs, you will need to delete the \
  \snapshot (and all snapshots that could reference that snapshot)."

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
  pure BaseConfigOpts
    { snapDB = snapDBPath
    , localDB = localDBPath
    , snapInstallRoot = snapInstallRoot
    , localInstallRoot = localInstallRoot
    , buildOpts = bopts
    , buildOptsCLI = boptsCli
    , extraDBs = packageExtraDBs
    }

-- | Provide a function for loading package information from the package index
loadPackage ::
     (HasBuildConfig env, HasSourceMap env)
  => PackageLocationImmutable
  -> Map FlagName Bool
  -> [Text] -- ^ GHC options
  -> [Text] -- ^ Cabal configure options
  -> RIO env Package
loadPackage loc flags ghcOptions cabalConfigOpts = do
  compiler <- view actualCompilerVersionL
  platform <- view platformL
  let pkgConfig = PackageConfig
        { enableTests = False
        , enableBenchmarks = False
        , flags = flags
        , ghcOptions = ghcOptions
        , cabalConfigOpts = cabalConfigOpts
        , compilerVersion = compiler
        , platform = platform
        }
  resolvePackage pkgConfig <$> loadCabalFileImmutable loc

checkComponentsBuildable :: MonadThrow m => [LocalPackage] -> m ()
checkComponentsBuildable lps =
  unless (null unbuildable) $
    prettyThrowM $ SomeTargetsNotBuildable unbuildable
 where
  unbuildable =
    [ (lp.package.name, c)
    | lp <- lps
    , c <- Set.toList lp.unbuildable
    ]
