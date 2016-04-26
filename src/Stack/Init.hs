{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Stack.Init
    ( initProject
    , InitOpts (..)
    ) where

import           Control.Exception               (assert)
import           Control.Exception.Enclosed      (catchAny)
import           Control.Monad
import           Control.Monad.Catch             (MonadMask, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader            (MonadReader, asks)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import qualified Data.ByteString.Builder         as B
import qualified Data.ByteString.Char8           as BC
import qualified Data.ByteString.Lazy            as L
import qualified Data.Foldable                   as F
import           Data.Function                   (on)
import qualified Data.HashMap.Strict             as HM
import qualified Data.IntMap                     as IntMap
import           Data.List                       ( intercalate, intersect
                                                 , maximumBy)
import           Data.List.NonEmpty              (NonEmpty(..))
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                       as T
import qualified Data.Yaml                       as Yaml
import qualified Distribution.PackageDescription as C
import qualified Distribution.Text               as C
import qualified Distribution.Version            as C
import           Network.HTTP.Client.Conduit     (HasHttpManager)
import           Path
import           Path.IO
import qualified Paths_stack                     as Meta
import           Stack.BuildPlan
import           Stack.Config                    (getSnapshots,
                                                  makeConcreteResolver)
import           Stack.Constants
import           Stack.Solver
import           Stack.Types
import           Stack.Types.Internal            (HasLogLevel, HasReExec,
                                                  HasTerminal)
import qualified System.FilePath                 as FP

-- | Generate stack.yaml
initProject
    :: ( MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m
       , MonadReader env m, HasConfig env , HasGHCVariant env
       , HasHttpManager env , HasLogLevel env , HasReExec env
       , HasTerminal env)
    => Path Abs Dir
    -> InitOpts
    -> Maybe AbstractResolver
    -> m ()
initProject currDir initOpts mresolver = do
    let dest = currDir </> stackDotYaml

    reldest <- toFilePath `liftM` makeRelativeToCurrentDir dest

    exists <- doesFileExist dest
    when (not (forceOverwrite initOpts) && exists) $ do
        error ("Stack configuration file " <> reldest <>
               " exists, use 'stack solver' to fix the existing config file or \
               \'--force' to overwrite it.")

    dirs <- mapM (resolveDir' . T.unpack) (searchDirs initOpts)
    let noPkgMsg =  "In order to init, you should have an existing .cabal \
                    \file. Please try \"stack new\" instead."
        find  = findCabalFiles (includeSubDirs initOpts)
        dirs' = if null dirs then [currDir] else dirs
    $logInfo "Looking for .cabal or package.yaml files to use to init the project."
    cabalfps <- liftM concat $ mapM find dirs'
    (bundle, dupPkgs)  <- cabalPackagesCheck cabalfps noPkgMsg Nothing

    (r, flags, extraDeps, rbundle) <- getDefaultResolver dest initOpts
                                                         mresolver bundle

    let ignored = Map.difference bundle rbundle
        dupPkgMsg
            | (dupPkgs /= []) =
                "Warning (added by new or init): Some packages were found to \
                \have names conflicting with others and have been commented \
                \out in the packages section.\n"
            | otherwise = ""

        missingPkgMsg
            | (Map.size ignored > 0) =
                "Warning (added by new or init): Some packages were found to \
                \be incompatible with the resolver and have been left commented \
                \out in the packages section.\n"
            | otherwise = ""

        extraDepMsg
            | (Map.size extraDeps > 0) =
                "Warning (added by new or init): Specified resolver could not \
                \satisfy all dependencies. Some external packages have been \
                \added as dependencies.\n"
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

        makeRel = fmap toFilePath . makeRelativeToCurrentDir

        pkgs = map toPkg $ Map.elems (fmap (parent . fst) rbundle)
        toPkg dir = PackageEntry
            { peExtraDep = False
            , peLocation = PLFilePath $ makeRelDir dir
            , peSubdirs = []
            }
        indent t = T.unlines $ fmap ("    " <>) (T.lines t)

    $logInfo $ "Initialising configuration using resolver: " <> resolverName r
    $logInfo $ "Total number of user packages considered: "
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
    liftIO $ L.writeFile (toFilePath dest)
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
           B.byteString headerHelp
        <> B.byteString "\n\n"
        <> F.foldMap (goComment o) comments
        <> goOthers (o `HM.difference` HM.fromList comments)
        <> B.byteString footerHelp

    goComment o (name, comment) =
        case HM.lookup name o of
            Nothing -> assert (name == "user-message") mempty
            Just v ->
                B.byteString comment <>
                B.byteString "\n" <>
                B.byteString (Yaml.encode $ Yaml.object [(name, v)]) <>
                if (name == "packages") then commentedPackages else "" <>
                B.byteString "\n"

    commentHelp = BC.pack .  intercalate "\n" . map ("# " ++)
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
            <> (B.byteString $ BC.pack $ concat
                 $ (map (\x -> "#- " ++ x ++ "\n") pkgs) ++ ["\n"])
        | otherwise = ""

    goOthers o
        | HM.null o = mempty
        | otherwise = assert False $ B.byteString $ Yaml.encode o

    -- Per Section Help
    comments =
        [ ("user-message"     , userMsgHelp)
        , ("resolver"         , resolverHelp)
        , ("packages"         , packageHelp)
        , ("extra-deps"       , "# Dependency packages to be pulled from upstream that are not in the resolver\n# (e.g., acme-missiles-0.3)")
        , ("flags"            , "# Override default flag values for local packages and extra-deps")
        , ("extra-package-dbs", "# Extra package databases containing global packages")
        ]

    -- Help strings
    headerHelp = commentHelp
        [ "This file was automatically generated by 'stack init'"
        , ""
        , "Some commonly used options have been documented as comments in this file."
        , "For advanced use and comprehensive documentation of the format, please see:"
        , "http://docs.haskellstack.org/en/stable/yaml_configuration/"
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
        , "resolver:"
        , " name: custom-snapshot"
        , " location: \"./custom-snapshot.yaml\""
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
        , "  extra-dep: true"
        , " subdirs:"
        , " - auto-update"
        , " - wai"
        , ""
        , "A package marked 'extra-dep: true' will only be built if demanded by a"
        , "non-dependency (i.e. a user package), and its test suites and benchmarks"
        , "will not be run. This is useful for tweaking upstream packages."
        ]

    footerHelp =
        let major = toCabalVersion
                    $ toMajorVersion $ fromCabalVersion Meta.version
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

getSnapshots' :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
              => m Snapshots
getSnapshots' =
    getSnapshots `catchAny` \e -> do
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
        $logError "    http://docs.haskellstack.org/en/stable/yaml_configuration/"
        $logError ""
        $logError $ "Exception was: " <> T.pack (show e)
        error ""

-- | Get the default resolver value
getDefaultResolver
    :: ( MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m
       , MonadReader env m, HasConfig env , HasGHCVariant env
       , HasHttpManager env , HasLogLevel env , HasReExec env
       , HasTerminal env)
    => Path Abs File   -- ^ stack.yaml
    -> InitOpts
    -> Maybe AbstractResolver
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
getDefaultResolver stackYaml initOpts mresolver bundle =
    maybe selectSnapResolver makeConcreteResolver mresolver
      >>= getWorkingResolverPlan stackYaml initOpts bundle
    where
        -- TODO support selecting best across regular and custom snapshots
        selectSnapResolver = do
            let gpds = Map.elems (fmap snd bundle)
            snaps <- fmap getRecommendedSnapshots getSnapshots'
            (s, r) <- selectBestSnapshot gpds snaps
            case r of
                BuildPlanCheckFail {} | not (omitPackages initOpts)
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
        BuildPlanCheckPartial f e
            | needSolver resolver initOpts -> do
                warnPartial result
                solve f
            | omitPackages initOpts -> do
                warnPartial result
                $logWarn "*** Omitting packages with unsatisfied dependencies"
                return $ Left $ failedUserPkgs e
            | otherwise -> throwM $ ResolverPartial resolver (show result)
        BuildPlanCheckFail _ e _
            | omitPackages initOpts -> do
                $logWarn $ "*** Resolver compiler mismatch: "
                           <> resolverName resolver
                $logWarn $ indent $ T.pack $ show result
                return $ Left $ failedUserPkgs e
            | otherwise -> throwM $ ResolverMismatch resolver (show result)
    where
      indent t  = T.unlines $ fmap ("    " <>) (T.lines t)
      warnPartial res = do
          $logWarn $ "*** Resolver " <> resolverName resolver
                      <> " will need external packages: "
          $logWarn $ indent $ T.pack $ show res

      failedUserPkgs e = Map.keys $ Map.unions (Map.elems (fmap deNeededBy e))

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
                  | omitPackages initOpts, srcpkgs /= []-> do
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

              -- prefer to reject packages in deeper directories
              path pkg = fst (fromJust (Map.lookup pkg bundle))
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

      needSolver _ (InitOpts {useSolver = True}) = True
      needSolver (ResolverCompiler _)  _ = True
      needSolver _ _ = False

getRecommendedSnapshots :: Snapshots -> (NonEmpty SnapName)
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
