{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Stack.Build
Description : Build the project.
License     : BSD-3-Clause

Build the project.
-}

module Stack.Build
  ( buildCmd
  , build
  , buildLocalTargets
  , loadPackage
  , mkBaseConfigOpts
  , splitObjsWarning
  ) where

import           Data.Attoparsec.Args ( EscapingMode (Escaping), parseArgs )
import qualified Data.Either.Extra as EE
import           Data.List ( (\\) )
import           Data.List.Extra ( groupSort )
import qualified Data.Map as Map
import qualified Data.Set as Set
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
import           Stack.Types.Build.Exception
                   ( BuildException (..), BuildPrettyException (..) )
import           Stack.Types.BuildConfig ( HasBuildConfig, configFileL )
import           Stack.Types.BuildOpts ( BuildOpts (..) )
import           Stack.Types.BuildOptsCLI
                   ( BuildCommand (..), BuildOptsCLI (..), FileWatchOpts (..) )
import           Stack.Types.BuildOptsMonoid
                   ( buildOptsMonoidBenchmarksL, buildOptsMonoidHaddockL
                   , buildOptsMonoidInstallExesL, buildOptsMonoidTestsL
                   )
import           Stack.Types.CompilerPaths ( HasCompiler, cabalVersionL )
import           Stack.Types.ComponentUtils
                   ( StackUnqualCompName, unqualCompToString )
import           Stack.Types.Config ( Config (..), HasConfig (..), buildOptsL )
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
import           Stack.Types.Plan
                   ( Plan (..), Task (..), TaskType (..), taskLocation
                   , taskProvides
                   )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Runner ( Runner, globalOptsL )
import           Stack.Types.SourceMap
                   ( SMTargets (..), SourceMap (..), Target (..) )

newtype CabalVersionPrettyException
  = CabalVersionNotSupported Version
  deriving (Show, Typeable)

instance Pretty CabalVersionPrettyException where
  pretty (CabalVersionNotSupported cabalVer) =
    "[S-5973]"
    <> line
    <> fillSep
         [ flow "Stack builds with the version of the Cabal package that comes \
                \with the specified version of GHC. However, Stack no longer \
                \supports such Cabal versions before 2.2. Version"
         , fromString $ versionString cabalVer
         , flow "was found. To fix this, either use Stack"
         , downgradeRecommendation
         , flow "or earlier or use a snapshot that specifies a version of GHC \
                \that is 8.4 or later. Stackage LTS Haskell 12.0"
         , parens (style Shell "lts-12.0")
         , flow "or later or Nightly 2018-03-13"
         , parens (style Shell "nightly-2018-03-13")
         , flow "or later specify such GHC versions."
         ]
   where
    -- Due to a bug, Stack 2.15.1 does not support Cabal < 2.
    downgradeRecommendation = if cabalVer < mkVersion [2]
      then "2.15.3 or 2.13.1"
      else "2.15.3"

instance Exception CabalVersionPrettyException

-- | Helper for build and install commands
buildCmd :: BuildOptsCLI -> RIO Runner ()
buildCmd opts = do
  when (any (("-prof" `elem`) . fromRight [] . parseArgs Escaping) opts.ghcOptions) $
    prettyThrowIO GHCProfOptionInvalid
  local (over globalOptsL modifyGO) $
    case opts.fileWatch of
      FileWatchPoll -> withFileWatchHook fileWatchPoll
      FileWatch -> withFileWatchHook fileWatch
      NoFileWatch -> inner Nothing
 where
  withFileWatchHook fileWatchAction =
    -- This loads the full configuration in order to obtain the file-watch-hook
    -- setting. That is likely not the most efficient approach.
    withConfig YesReexec $ withEnvConfig NeedTargets opts $
      fileWatchAction (inner . Just)
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
  bopts <- view buildOptsL
  sourceMap <- view $ envConfigL . to (.sourceMap)
  locals <- projectLocalPackages
  depsLocals <- localDependencies
  boptsCli <- view $ envConfigL . to (.buildOptsCLI)
  -- Set local files, necessary for file watching
  configFile <- view configFileL
  let allLocals = locals <> depsLocals
      -- We are indifferent as to whether the configuration file is a
      -- user-specifc global or a project-level one.
      eitherConfigFile = EE.fromEither configFile
  for_ msetLocalFiles $ \setLocalFiles -> do
    files <-
      if boptsCli.watchAll
      then sequence [lpFiles lp | lp <- allLocals]
      else forM allLocals $ \lp -> do
        let pn = lp.package.name
        case Map.lookup pn sourceMap.targets.targets of
          Nothing ->
            pure Set.empty
          Just (TargetAll _) ->
            lpFiles lp
          Just (TargetComps components) ->
            lpFilesForComponents components lp
    liftIO $ setLocalFiles $ Set.insert eitherConfigFile $ Set.unions files

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
  haddockCompsSupported <- warnAboutHaddockComps bopts
  let disableHaddockComps =
        local $ over buildOptsL $ \bo ->
          bo
            { haddockExecutables = False
            , haddockTests = False
            , haddockBenchmarks = False
            }
      withHaddockCompsGuarded = if haddockCompsSupported
        then id
        else disableHaddockComps
  warnAboutSplitObjs bopts
  warnIfExecutablesWithSameNameCouldBeOverwritten locals plan

  when bopts.preFetch $
      preFetch plan

  if boptsCli.dryrun
    then
      printPlan plan
    else
      withHaddockCompsGuarded $ executePlan
        boptsCli
        baseConfigOpts
        locals
        globalDumpPkgs
        snapshotDumpPkgs
        localDumpPkgs
        installedMap
        sourceMap.targets.targets
        plan

-- | Build one or more local targets.
buildLocalTargets ::
     HasEnvConfig env
  => NonEmpty Text
     -- ^ Local targets to build.
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
  when (cabalVer < mkVersion [2, 2]) $
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
                (fromString $ packageNameString p <> ":" <> unqualCompToString exe)
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
  -- Cases of several project packages having executables with the same name.
  -- The Map entries have the following form:
  --
  --  executable name: ( package names for executables that are being built
  --                   , package names for other project packages that have an
  --                     executable with the same name
  --                   )
  warnings :: Map StackUnqualCompName ([PackageName],[PackageName])
  warnings =
    Map.mapMaybe
      (\(pkgsToBuild, localPkgs) ->
        case (pkgsToBuild, NE.toList localPkgs \\ NE.toList pkgsToBuild) of
          (_ :| [], []) ->
            -- We want to build the executable of single project package
            -- and there are no other project packages with an executable of
            -- the same name. Nothing to warn about, ignore.
            Nothing
          (_, otherLocals) ->
            -- We could be here for two reasons (or their combination):
            -- 1) We are building two or more executables with the same
            --    name that will end up overwriting each other.
            -- 2) In addition to the executable(s) that we want to build
            --    there are other project packages with an executable of the
            --    same name that might get overwritten.
            -- Both cases warrant a warning.
            Just (NE.toList pkgsToBuild, otherLocals))
      (Map.intersectionWith (,) exesToBuild localExes)
  exesToBuild :: Map StackUnqualCompName (NonEmpty PackageName)
  exesToBuild =
    collect
      [ (exe, pkgName')
      | (pkgName', task) <- Map.toList plan.tasks
      , TTLocalMutable lp <- [task.taskType]
      , exe <- (Set.toList . exeComponents . (.components)) lp
      ]
  localExes :: Map StackUnqualCompName (NonEmpty PackageName)
  localExes =
    collect
      [ (exe, pkg.name)
      | pkg <- map (.package) locals
      , exe <- Set.toList (buildableExes pkg)
      ]
  collect :: Ord k => [(k, v)] -> Map k (NonEmpty v)
  collect = Map.mapMaybe nonEmpty . Map.fromDistinctAscList . groupSort

warnAboutHaddockComps ::
     (HasCompiler env, HasTerm env)
  => BuildOpts
  -> RIO env Bool
warnAboutHaddockComps bopts = do
  let haddockCompsWanted =
           bopts.haddockExecutables
        || bopts.haddockTests
        || bopts.haddockBenchmarks
  cabalVer <- view cabalVersionL
  if haddockCompsWanted && cabalVer < mkVersion [3, 8, 1]
    then do
      prettyWarnL
        [ flow "Stack builds Haddock documentation with the version of the \
               \Cabal package that comes with the specified version of GHC. \
               \Version"
        , fromString $ versionString cabalVer
        , flow "was found, which does not support the building of \
               \documentation for executables, test suites or benchmarks. \
               \Options to build such documentation will be ignored. To use \
               \the options, use a snapshot that specifies a version of GHC \
               \that is 9.4 or later. Stackage LTS Haskell 21.0"
        , parens (style Shell "lts-21.0")
        , flow "or later or Nightly 2022-11-19"
        , parens (style Shell "nightly-2022-11-19")
        , flow "or later specify such GHC versions."
        ]
      pure False
    else pure haddockCompsWanted

warnAboutSplitObjs :: HasTerm env => BuildOpts -> RIO env ()
warnAboutSplitObjs bopts |  bopts.splitObjs =
  prettyWarnL
    [ flow "Building with"
    , style Shell "--split-objs"
    , flow "is enabled."
    , flow splitObjsWarning
    ]
warnAboutSplitObjs _ = pure ()

-- | Text warning about the experimental nature of Stack's @--split-objs@ flag.
splitObjsWarning :: String
splitObjsWarning =
  "Note that this feature is EXPERIMENTAL, and its behavior may be changed and \
  \improved. You will need to clean your workdirs before use. If you want to \
  \compile all dependencies with split-objs, you will need to delete the \
  \snapshot (and all snapshots that could reference that snapshot)."

-- | Get the @BaseConfigOpts@ necessary for constructing configure options
mkBaseConfigOpts :: (HasEnvConfig env)
                 => BuildOptsCLI -> RIO env BaseConfigOpts
mkBaseConfigOpts buildOptsCLI = do
  buildOpts <- view buildOptsL
  snapDB <- packageDatabaseDeps
  localDB <- packageDatabaseLocal
  snapInstallRoot <- installationRootDeps
  localInstallRoot <- installationRootLocal
  extraDBs <- packageDatabaseExtra
  pure BaseConfigOpts
    { snapDB
    , localDB
    , snapInstallRoot
    , localInstallRoot
    , buildOpts
    , buildOptsCLI
    , extraDBs
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
  compilerVersion <- view actualCompilerVersionL
  platform <- view platformL
  let pkgConfig = PackageConfig
        { enableTests = False
        , enableBenchmarks = False
        , flags
        , ghcOptions
        , cabalConfigOpts
        , compilerVersion
        , platform
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
