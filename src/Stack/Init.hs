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
import           Control.Monad.Reader            (asks, MonadReader)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import qualified Data.ByteString.Builder         as B
import qualified Data.ByteString.Lazy            as L
import qualified Data.ByteString.Char8           as BC
import qualified Data.HashMap.Strict             as HM
import qualified Data.IntMap                     as IntMap
import qualified Data.Foldable                   as F
import           Data.List                       (intersect, sortBy)
import           Data.List.Extra                 (nubOrd)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromJust, mapMaybe)
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

    cabalfps <- findCabalFiles (includeSubDirs initOpts) currDir
    (bundle, dupPkgs)  <- cabalPackagesCheck cabalfps noPkgMsg Nothing

    (r, flags, extraDeps, rbundle) <- getDefaultResolver dest initOpts bundle

    let ignored = Map.difference bundle rbundle
        dupPkgMsg
            | (dupPkgs /= []) =
                "Warning: Some packages were found to have names conflicting \
                \with others and have been commented out in the \
                \packages section.\n"
            | otherwise = ""

        missingPkgMsg
            | (Map.size ignored > 0) =
                "Warning: Some packages were found to be incompatible with \
                \the resolver and have been left commented out in the \
                \packages section.\n"
            | otherwise = ""

        extraDepMsg
            | (Map.size extraDeps > 0) =
                "Warning: Specified resolver could not satisfy all \
                \dependencies. Some external packages have been added \
                \as dependencies.\n"
            | otherwise = ""

        makeUserMsg msgs =
            let msg = concat msgs
            in if msg /= "" then
                  msg <> "You can suppress this message by removing it from \
                         \stack.yaml\n"
                 else ""

        userMsg = makeUserMsg [dupPkgMsg, missingPkgMsg, extraDepMsg]

        gpds = Map.elems $ fmap snd rbundle
        p = Project
            { projectUserMsg = if userMsg == "" then Nothing else Just userMsg
            , projectPackages = pkgs
            , projectExtraDeps = extraDeps
            , projectFlags = removeSrcPkgDefaultFlags gpds flags
            , projectResolver = r
            , projectCompiler = Nothing
            , projectExtraPackageDBs = []
            }

        makeRelDir dir =
            case stripDir currDir dir of
                Nothing
                    | currDir == dir -> "."
                    | otherwise -> assert False $ toFilePath dir
                Just rel -> toFilePath rel

        makeRel = liftIO . makeRelativeToCurrentDirectory . toFilePath

        pkgs = map toPkg $ Map.elems (fmap (parent . fst) rbundle)
        toPkg dir = PackageEntry
            { peValidWanted = Nothing
            , peExtraDepMaybe = Nothing
            , peLocation = PLFilePath $ makeRelDir dir
            , peSubdirs = []
            }
        indent t = T.unlines $ fmap ("    " <>) (T.lines t)

    $logInfo $ "Initialising configuration using resolver: " <> resolverName r
    $logInfo $ "Total number of packages considered: "
               <> (T.pack $ show $ (Map.size bundle + length dupPkgs))

    when (dupPkgs /= []) $ do
        $logWarn $ "Warning! Ignoring "
                   <> (T.pack $ show $ length dupPkgs)
                   <> " duplicate packages:"
        rels <- mapM makeRel dupPkgs
        $logWarn $ indent $ showItems rels

    when (Map.size ignored > 0) $ do
        $logWarn $ "Warning! Ignoring "
                   <> (T.pack $ show $ Map.size ignored)
                   <> " packages due to dependency conflicts:"
        rels <- mapM makeRel (Map.elems (fmap fst ignored))
        $logWarn $ indent $ showItems $ rels

    when (Map.size extraDeps > 0) $ do
        $logWarn $ "Warning! " <> (T.pack $ show $ Map.size extraDeps)
                   <> " external dependencies were added."
    $logInfo $
        (if exists then "Overwriting existing configuration file: "
         else "Writing configuration to file: ")
        <> T.pack reldest
    liftIO $ L.writeFile dest'
           $ B.toLazyByteString
           $ renderStackYaml p
               (Map.elems $ fmap (makeRelDir . parent . fst) ignored)
               (map (makeRelDir . parent) dupPkgs)
    $logInfo "All done."

-- | Render a stack.yaml file with comments, see:
-- https://github.com/commercialhaskell/stack/issues/226
renderStackYaml :: Project -> [FilePath] -> [FilePath] -> B.Builder
renderStackYaml p ignoredPackages dupPackages =
    case Yaml.toJSON p of
        Yaml.Object o -> renderObject o
        _ -> assert False $ B.byteString $ Yaml.encode p
  where
    renderObject o =
        B.byteString "# This file was automatically generated by stack init\n" <>
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
        [ ("user-message", "A message to be displayed to the user. Used when autogenerated config ignored some packages or added extra deps.")
        , ("resolver", "Specifies the GHC version and set of packages available (e.g., lts-3.5, nightly-2015-09-21, ghc-7.10.2)")
        , ("packages", "Local packages, usually specified by relative directory name")
        , ("extra-deps", "Packages to be pulled from upstream that are not in the resolver (e.g., acme-missiles-0.3)")
        , ("flags", "Override default flag values for local packages and extra-deps")
        , ("extra-package-dbs", "Extra package databases containing global packages")
        ]

    commentedPackages =
        let ignoredComment = "# The following packages have been ignored \
                \due to incompatibility with the resolver compiler or \
                \dependency conflicts with other packages"
            dupComment = "# The following packages have been ignored due \
                \to package name conflict with other packages"
        in commentPackages ignoredComment ignoredPackages
           <> commentPackages dupComment dupPackages

    commentPackages comment pkgs
        | pkgs /= [] =
            B.byteString (BC.pack $ comment ++ "\n")
            <> (B.byteString $ BC.pack $ concat
                 $ (map (\x -> "#- " ++ x ++ "\n") pkgs) ++ ["\n"])
        | otherwise = ""

    goComment o (name, comment) =
        case HM.lookup name o of
            Nothing -> assert (name == "user-message") mempty
            Just v ->
                B.byteString "# " <>
                B.byteString comment <>
                B.byteString "\n" <>
                B.byteString (Yaml.encode $ Yaml.object [(name, v)]) <>
                if (name == "packages") then commentedPackages else "" <>
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
    -> InitOpts
    -> Map PackageName (Path Abs File, C.GenericPackageDescription)
       -- ^ Src package name: cabal dir, cabal package description
    -> m ( Resolver
         , Map PackageName (Map FlagName Bool)
         , Map PackageName Version
         , Map PackageName (Path Abs File, C.GenericPackageDescription))
       -- ^ ( Resolver
       --   , Flags for src packages and extra deps
       --   , Extra dependencies
       --   , Src packages actually considered)
getDefaultResolver stackYaml initOpts bundle =
    getResolver (ioMethod initOpts)
      >>= getWorkingResolverPlan stackYaml initOpts bundle
    where
        -- TODO support selecting best across regular and custom snapshots
        getResolver (MethodSnapshot snapPref)  = selectSnapResolver snapPref
        getResolver (MethodResolver aresolver) = makeConcreteResolver aresolver

        selectSnapResolver snapPref = do
            msnaps <- getSnapshots'
            snaps <- maybe (error "No snapshots to select from.")
                           (getRecommendedSnapshots snapPref)
                           msnaps
            let gpds = Map.elems (fmap snd bundle)
            (s, r) <- selectBestSnapshot gpds snaps
            case r of
                (BuildPlanCheckFail _ _ _) | not (forceOverwrite initOpts)
                        -> throwM (NoMatchingSnapshot snaps)
                _ -> return $ ResolverSnapshot s

getWorkingResolverPlan
    :: ( MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m
       , MonadReader env m, HasConfig env , HasGHCVariant env
       , HasHttpManager env , HasLogLevel env , HasReExec env
       , HasTerminal env)
    => Path Abs File   -- ^ stack.yaml
    -> InitOpts
    -> Map PackageName (Path Abs File, C.GenericPackageDescription)
       -- ^ Src package name: cabal dir, cabal package description
    -> Resolver
    -> m ( Resolver
         , Map PackageName (Map FlagName Bool)
         , Map PackageName Version
         , Map PackageName (Path Abs File, C.GenericPackageDescription))
       -- ^ ( Resolver
       --   , Flags for src packages and extra deps
       --   , Extra dependencies
       --   , Src packages actually considered)
getWorkingResolverPlan stackYaml initOpts bundle resolver = do
    $logInfo $ "Selected resolver: " <> resolverName resolver
    go bundle
    where
        go info = do
            eres <- checkBundleResolver stackYaml initOpts info resolver
            -- if some packages failed try again using the rest
            case eres of
                Right (f, edeps)-> return (resolver, f, edeps, info)
                Left ignored
                    | Map.null available -> do
                        $logWarn "*** Could not find a working plan for any of \
                                 \the user packages.\nProceeding to create a \
                                 \config anyway."
                        return (resolver, Map.empty, Map.empty, Map.empty)
                    | otherwise -> do
                        when ((Map.size available) == (Map.size info)) $
                            error "Bug: No packages to ignore"

                        if length ignored > 1 then do
                          $logWarn "*** Ignoring packages:"
                          $logWarn $ indent $ showItems ignored
                        else
                          $logWarn $ "*** Ignoring package: "
                                 <> (T.pack $ packageNameString (head ignored))

                        go available
                    where
                      indent t   = T.unlines $ fmap ("    " <>) (T.lines t)
                      isAvailable k _ = not (k `elem` ignored)
                      available       = Map.filterWithKey isAvailable info

checkBundleResolver
    :: ( MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m
       , MonadReader env m, HasConfig env , HasGHCVariant env
       , HasHttpManager env , HasLogLevel env , HasReExec env
       , HasTerminal env)
    => Path Abs File   -- ^ stack.yaml
    -> InitOpts
    -> Map PackageName (Path Abs File, C.GenericPackageDescription)
       -- ^ Src package name: cabal dir, cabal package description
    -> Resolver
    -> m (Either [PackageName] ( Map PackageName (Map FlagName Bool)
                               , Map PackageName Version))
checkBundleResolver stackYaml initOpts bundle resolver = do
    result <- checkResolverSpec gpds Nothing resolver
    case result of
        BuildPlanCheckOk f -> return $ Right (f, Map.empty)
        (BuildPlanCheckPartial f _)
            | needSolver resolver initOpts -> do
                $logWarn $ "*** Resolver " <> resolverName resolver
                            <> " will need external packages: "
                $logWarn $ indent $ T.pack $ show result
                solve f
            | otherwise -> throwM $ ResolverPartial resolver (show result)
        (BuildPlanCheckFail _ e _)
            | (forceOverwrite initOpts) -> do
                $logWarn $ "*** Resolver compiler mismatch: "
                           <> resolverName resolver
                $logWarn $ indent $ T.pack $ show result
                let failed = Map.unions (Map.elems (fmap deNeededBy e))
                return $ Left (Map.keys failed)
            | otherwise -> throwM $ ResolverMismatch resolver (show result)
    where
      indent t    = T.unlines $ fmap ("    " <>) (T.lines t)
      gpds        = Map.elems (fmap snd bundle)
      solve flags = do
          let cabalDirs      = map parent (Map.elems (fmap fst bundle))
              srcConstraints = mergeConstraints (gpdPackages gpds) flags

          eresult <- solveResolverSpec stackYaml cabalDirs
                                       (resolver, srcConstraints, Map.empty)
          case eresult of
              Right (src, ext) ->
                  return $ Right (fmap snd (Map.union src ext), fmap fst ext)
              Left packages
                  | forceOverwrite initOpts, srcpkgs /= []-> do
                      pkg <- findOneIndependent srcpkgs flags
                      return $ Left [pkg]
                  | otherwise -> throwM (SolverGiveUp giveUpMsg)
                  where srcpkgs = intersect (Map.keys bundle) packages

      -- among a list of packages find one on which none among the rest of the
      -- packages depend. This package is a good candidate to be removed from
      -- the list of packages when there is conflict in dependencies among this
      -- set of packages.
      findOneIndependent packages flags = do
          platform <- asks (configPlatform . getConfig)
          (compiler, _) <- getResolverConstraints stackYaml resolver
          let getGpd pkg = snd (fromJust (Map.lookup pkg bundle))
              getFlags pkg = fromJust (Map.lookup pkg flags)
              deps pkg = gpdPackageDeps (getGpd pkg) compiler platform
                                        (getFlags pkg)
              allDeps = concat $ map (Map.keys . deps) packages
              isIndependent pkg = not $ pkg `elem` allDeps
          return $ head (filter isIndependent packages)

      giveUpMsg = concat
          [ "    - Use '--ignore-subdirs' to skip packages in subdirectories.\n"
          , "    - Update external packages with 'stack update' and try again.\n"
          , "    - Use '--force' to create an initial "
          , toFilePath stackDotYaml <> ", tweak it and run 'stack solver':\n"
          , "        - Remove any unnecessary packages.\n"
          , "        - Add any missing remote packages.\n"
          , "        - Add extra dependencies to guide solver.\n"
          ]

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
    -- TODO Include all major compiler versions available
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
