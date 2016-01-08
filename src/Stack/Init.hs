{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Stack.Init
    ( initProject
    , InitOpts (..)
    , SnapPref (..)
    , Method (..)
    ) where

import           Control.Exception               (assert)
import           Control.Exception.Enclosed      (catchAny, handleIO)
import           Control.Monad                   (liftM, when)
import           Control.Monad.Catch             (MonadMask, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader            (MonadReader)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import qualified Data.ByteString.Builder         as B
import qualified Data.ByteString.Lazy            as L
import qualified Data.HashMap.Strict             as HM
import qualified Data.IntMap                     as IntMap
import qualified Data.Foldable                   as F
import           Data.List                       (sortBy)
import           Data.List.Extra                 (nubOrd)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (mapMaybe)
import           Data.Monoid
import qualified Data.Text                       as T
import qualified Data.Yaml                       as Yaml
import qualified Distribution.PackageDescription as C
import           Network.HTTP.Client.Conduit     (HasHttpManager)
import           Path
import           Path.IO
import           Stack.BuildPlan
import           Stack.Constants
import           Stack.Solver
import           Stack.Types
import           Stack.Types.Internal            ( HasTerminal, HasReExec
                                                 , HasLogLevel)
import           System.Directory                ( getDirectoryContents
                                                 , makeRelativeToCurrentDirectory)
import           Stack.Config                    ( getSnapshots
                                                 , makeConcreteResolver)

-- | Generate stack.yaml
initProject
    :: ( MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m
       , MonadReader env m, HasConfig env , HasGHCVariant env
       , HasHttpManager env , HasLogLevel env , HasReExec env
       , HasTerminal env)
    => Path Abs Dir
    -> InitOpts
    -> m ()
initProject currDir initOpts = do
    let dest = currDir </> stackDotYaml
        dest' = toFilePath dest

    reldest <- liftIO $ makeRelativeToCurrentDirectory dest'

    exists <- fileExists dest
    when (not (forceOverwrite initOpts) && exists) $ do
        error ("Stack configuration file " <> reldest <>
               " exists, use 'stack solver' to fix the existing config file or \
               \'--force' to overwrite it.")

    let noPkgMsg =  "In order to init, you should have an existing .cabal \
                    \file. Please try \"stack new\" instead."

        dupPkgFooter = "You have the following options:\n"
            <> "- Use '--ignore-subdirs' command line switch to ignore "
            <> "packages in subdirectories. You can init subdirectories as "
            <> "independent projects.\n"
            <> "- Put selected packages in the stack config file "
            <> "and then use 'stack solver' command to automatically resolve "
            <> "dependencies and update the config file."

    cabalfps <- findCabalFiles (includeSubDirs initOpts) currDir
    gpds <- cabalPackagesCheck cabalfps noPkgMsg dupPkgFooter

    (r, flags, extraDeps) <-
        getDefaultResolver dest (map parent cabalfps) gpds initOpts
    let p = Project
            { projectPackages = pkgs
            , projectExtraDeps = extraDeps
            , projectFlags = removeSrcPkgDefaultFlags gpds flags
            , projectResolver = r
            , projectCompiler = Nothing
            , projectExtraPackageDBs = []
            }
        pkgs = map toPkg cabalfps
        toPkg fp = PackageEntry
            { peValidWanted = Nothing
            , peExtraDepMaybe = Nothing
            , peLocation = PLFilePath $
                case stripDir currDir $ parent fp of
                    Nothing
                        | currDir == parent fp -> "."
                        | otherwise -> assert False $ toFilePath $ parent fp
                    Just rel -> toFilePath rel
            , peSubdirs = []
            }

    $logInfo $ "Initialising configuration using resolver: " <> resolverName r
    $logInfo $
        (if exists then "Overwriting existing configuration file: "
         else "Writing configuration to file: ")
        <> T.pack reldest
    liftIO $ L.writeFile dest' $ B.toLazyByteString $ renderStackYaml p
    $logInfo "All done."

-- | Render a stack.yaml file with comments, see:
-- https://github.com/commercialhaskell/stack/issues/226
renderStackYaml :: Project -> B.Builder
renderStackYaml p =
    case Yaml.toJSON p of
        Yaml.Object o -> renderObject o
        _ -> assert False $ B.byteString $ Yaml.encode p
  where
    renderObject o =
        B.byteString "# For more information, see: http://docs.haskellstack.org/en/stable/yaml_configuration.html\n\n" <>
        F.foldMap (goComment o) comments <>
        goOthers (o `HM.difference` HM.fromList comments) <>
        B.byteString
            "# Control whether we use the GHC we find on the path\n\
            \# system-ghc: true\n\n\
            \# Require a specific version of stack, using version ranges\n\
            \# require-stack-version: -any # Default\n\
            \# require-stack-version: >= 1.0.0\n\n\
            \# Override the architecture used by stack, especially useful on Windows\n\
            \# arch: i386\n\
            \# arch: x86_64\n\n\
            \# Extra directories used by stack for building\n\
            \# extra-include-dirs: [/path/to/dir]\n\
            \# extra-lib-dirs: [/path/to/dir]\n\n\
            \# Allow a newer minor version of GHC than the snapshot specifies\n\
            \# compiler-check: newer-minor\n"

    comments =
        [ ("resolver", "Specifies the GHC version and set of packages available (e.g., lts-3.5, nightly-2015-09-21, ghc-7.10.2)")
        , ("packages", "Local packages, usually specified by relative directory name")
        , ("extra-deps", "Packages to be pulled from upstream that are not in the resolver (e.g., acme-missiles-0.3)")
        , ("flags", "Override default flag values for local packages and extra-deps")
        , ("extra-package-dbs", "Extra package databases containing global packages")
        ]

    goComment o (name, comment) =
        case HM.lookup name o of
            Nothing -> assert False mempty
            Just v ->
                B.byteString "# " <>
                B.byteString comment <>
                B.byteString "\n" <>
                B.byteString (Yaml.encode $ Yaml.object [(name, v)]) <>
                B.byteString "\n"

    goOthers o
        | HM.null o = mempty
        | otherwise = assert False $ B.byteString $ Yaml.encode o

getSnapshots' :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
              => m (Maybe Snapshots)
getSnapshots' =
    liftM Just getSnapshots `catchAny` \e -> do
        $logError $
            "Unable to download snapshot list, and therefore could " <>
            "not generate a stack.yaml file automatically"
        $logError $
            "This sometimes happens due to missing Certificate Authorities " <>
            "on your system. For more information, see:"
        $logError ""
        $logError "    https://github.com/commercialhaskell/stack/issues/234"
        $logError ""
        $logError "You can try again, or create your stack.yaml file by hand. See:"
        $logError ""
        $logError "    http://docs.haskellstack.org/en/stable/yaml_configuration.html"
        $logError ""
        $logError $ "Exception was: " <> T.pack (show e)
        return Nothing

-- | Get the default resolver value
getDefaultResolver
    :: ( MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m
       , MonadReader env m, HasConfig env , HasGHCVariant env
       , HasHttpManager env , HasLogLevel env , HasReExec env
       , HasTerminal env)
    => Path Abs File   -- ^ stack.yaml
    -> [Path Abs Dir]  -- ^ cabal dirs
    -> [C.GenericPackageDescription] -- ^ cabal descriptions
    -> InitOpts
    -> m ( Resolver
         , Map PackageName (Map FlagName Bool)
         , Map PackageName Version)
getDefaultResolver stackYaml cabalDirs gpds initOpts = do
    resolver <- getResolver (ioMethod initOpts)
    result   <- checkResolverSpec gpds Nothing resolver

    case result of
        BuildPlanCheckOk f-> return (resolver, f, Map.empty)
        BuildPlanCheckPartial f e
            | needSolver resolver initOpts -> solve (resolver, f)
            | otherwise ->
                throwM $ ResolverPartial resolver (showDepErrors f e)
        BuildPlanCheckFail f e c ->
            throwM $ ResolverMismatch resolver (showCompilerErrors f e c)

    where
      solve (res, f) = do
          let srcConstraints = mergeConstraints (gpdPackages gpds) f
          mresolver <- solveResolverSpec stackYaml cabalDirs
                                         (res, srcConstraints, Map.empty)
          case mresolver of
              Just (src, ext) -> do
                  return (res, fmap snd (Map.union src ext), fmap fst ext)
              Nothing
                  | forceOverwrite initOpts -> do
                      $logWarn "\nSolver could not arrive at a workable build \
                               \plan.\nProceeding to create a config with an \
                               \incomplete plan anyway..."
                      return (res, f, Map.empty)
                  | otherwise -> throwM (SolverGiveUp giveUpMsg)

      giveUpMsg = concat
          [ "    - Use '--ignore-subdirs' to skip packages in subdirectories.\n"
          , "    - Update external packages with 'stack update' and try again.\n"
          , "    - Use '--force' to create an initial "
          , toFilePath stackDotYaml <> ", tweak it and run 'stack solver':\n"
          , "        - Remove any unnecessary packages.\n"
          , "        - Add any missing remote packages.\n"
          , "        - Add extra dependencies to guide solver.\n"
          ]

      -- TODO support selecting best across regular and custom snapshots
      getResolver (MethodSnapshot snapPref)  = selectSnapResolver snapPref
      getResolver (MethodResolver aresolver) = makeConcreteResolver aresolver

      selectSnapResolver snapPref = do
          msnaps <- getSnapshots'
          snaps <- maybe (error "No snapshots to select from.")
                         (getRecommendedSnapshots snapPref)
                         msnaps
          selectBestSnapshot gpds snaps
            >>= maybe (throwM (NoMatchingSnapshot snaps))
                      (return . ResolverSnapshot)

      needSolver _ (InitOpts {useSolver = True}) = True
      needSolver (ResolverCompiler _)  _ = True
      needSolver _ _ = False

getRecommendedSnapshots :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, HasGHCVariant env, MonadLogger m, MonadBaseControl IO m)
                        => SnapPref
                        -> Snapshots
                        -> m [SnapName]
getRecommendedSnapshots pref snapshots = do
    -- Get the most recent LTS and Nightly in the snapshots directory and
    -- prefer them over anything else, since odds are high that something
    -- already exists for them.
    existing <-
        liftM (sortBy (flip compare) . mapMaybe (parseSnapName . T.pack)) $
        snapshotsDir >>=
        liftIO . handleIO (const $ return [])
               . getDirectoryContents . toFilePath
    let isLTS LTS{} = True
        isLTS Nightly{} = False
        isNightly Nightly{} = True
        isNightly LTS{} = False

        names = nubOrd $ concat
            [ take 2 $ filter isLTS existing
            , take 2 $ filter isNightly existing
            , map (uncurry LTS)
                (take 2 $ reverse $ IntMap.toList $ snapshotsLts snapshots)
            , [Nightly $ snapshotsNightly snapshots]
            ]

        namesLTS = filter isLTS names
        namesNightly = filter isNightly names

    case pref of
        PrefNone -> return names
        PrefLTS -> return $ namesLTS ++ namesNightly
        PrefNightly -> return $ namesNightly ++ namesLTS

data InitOpts = InitOpts
    { ioMethod       :: !Method
    -- ^ Use solver
    , useSolver :: Bool
    -- ^ Preferred snapshots
    , forceOverwrite :: Bool
    -- ^ Overwrite existing files
    , includeSubDirs :: Bool
    -- ^ If True, include all .cabal files found in any sub directories
    }

data SnapPref = PrefNone | PrefLTS | PrefNightly

-- | Method of initializing
data Method = MethodSnapshot SnapPref | MethodResolver AbstractResolver
