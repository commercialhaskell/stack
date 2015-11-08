{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

-- | Run a GHCi configured with the user's package(s).

module Stack.Ghci (GhciOpts(..),GhciPkgInfo(..), ghciSetup, ghci) where

import           Control.Monad.Catch
import           Control.Exception.Enclosed (tryAny)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Either
import           Data.Function
import           Data.List
import           Data.List.Extra (nubOrd)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Distribution.ModuleName (ModuleName)
import           Distribution.Text (display)
import           Network.HTTP.Client.Conduit
import           Path
import           Path.IO
import           Prelude
import           Stack.Build
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Build.Target
import           Stack.Constants
import           Stack.Exec
import           Stack.Package
import           Stack.Types
import           Stack.Types.Internal
import           System.Directory (getTemporaryDirectory)

-- | Command-line options for GHC.
data GhciOpts = GhciOpts
    {ghciTargets            :: ![Text]
    ,ghciArgs               :: ![String]
    ,ghciGhcCommand         :: !(Maybe FilePath)
    ,ghciNoLoadModules      :: !Bool
    ,ghciAdditionalPackages :: ![String]
    ,ghciMainIs             :: !(Maybe Text)
    ,ghciBuildFirst         :: !(Maybe BuildSubset)
    } deriving (Show,Eq)

-- | Necessary information to load a package or its components.
data GhciPkgInfo = GhciPkgInfo
  { ghciPkgName :: PackageName
  , ghciPkgOpts :: [(NamedComponent, BuildInfoOpts)]
  , ghciPkgDir :: Path Abs Dir
  , ghciPkgModules :: Set ModuleName
  , ghciPkgModFiles :: Set (Path Abs File) -- ^ Module file paths.
  , ghciPkgCFiles :: Set (Path Abs File) -- ^ C files.
  , ghciPkgMainIs :: Map NamedComponent (Set (Path Abs File))
  } deriving Show

-- | Launch a GHCi session for the given local package targets with the
-- given options and configure it with the load paths and extensions
-- of those targets.
ghci
    :: (HasConfig r, HasBuildConfig r, HasHttpManager r, MonadMask m, HasLogLevel r, HasTerminal r, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m, MonadBaseControl IO m)
    => GhciOpts -> m ()
ghci GhciOpts{..} = do
    (targets,mainIsTargets,pkgs) <- ghciSetup ghciBuildFirst ghciMainIs ghciTargets
    bconfig <- asks getBuildConfig
    mainFile <- figureOutMainFile mainIsTargets targets pkgs
    wc <- getWhichCompiler
    let pkgopts =
            (if null pkgs then [] else ["-hide-all-packages"]) ++
            nubOrd (concatMap (concatMap (bioGeneratedOpts . snd) . ghciPkgOpts) pkgs) ++
            (concatMap (concatMap (bioGhcOpts . snd) . ghciPkgOpts) pkgs)
        modulesToLoad
          | ghciNoLoadModules = []
          | otherwise =
              nub
                  (maybe [] (return . toFilePath) mainFile <>
                   concatMap (map display . S.toList . ghciPkgModules) pkgs)
        odir =
            [ "-odir=" <> toFilePath (objectInterfaceDir bconfig)
            , "-hidir=" <> toFilePath (objectInterfaceDir bconfig)]
    $logInfo
        ("Configuring GHCi with the following packages: " <>
         T.intercalate ", " (map (packageNameText . ghciPkgName) pkgs))
    tmp <- liftIO getTemporaryDirectory
    withCanonicalizedTempDirectory
        tmp
        "ghci-script"
        (\tmpDir ->
              do let scriptPath = tmpDir </> $(mkRelFile "ghci-script")
                     fp = toFilePath scriptPath
                     loadModules = ":load " <> unwords modulesToLoad
                     bringIntoScope = ":module + " <> unwords modulesToLoad
                 liftIO (writeFile fp (unlines [loadModules,bringIntoScope]))
                 finally (exec
                              defaultEnvSettings
                              (fromMaybe (compilerExeName wc) ghciGhcCommand)
                              ("--interactive" :
                              -- This initial "-i" resets the include directories to not
                              -- include CWD.
                               "-i" :
                               odir <> pkgopts <> ghciArgs <>
                               ["-ghci-script=" <> fp]))
                         (removeFile scriptPath))

-- | Figure out the main-is file to load based on the targets. Sometimes there
-- is none, sometimes it's unambiguous, sometimes it's
-- ambiguous. Warns and returns nothing if it's ambiguous.
figureOutMainFile
    :: (Monad m, MonadLogger m)
    => Maybe (Map PackageName SimpleTarget)
    -> Map PackageName SimpleTarget
    -> [GhciPkgInfo]
    -> m (Maybe (Path Abs File))
figureOutMainFile mainIsTargets targets0 packages =
    case candidates of
        [] -> return Nothing
        [c@(_,_,fp)] -> do $logInfo ("Using main module: " <> renderCandidate c)
                           return (Just fp)
        candidate:_ -> borderedWarning $ do
            $logWarn "The main module to load is ambiguous. Candidates are: "
            forM_ (map renderCandidate candidates) $logWarn
            $logWarn
                "None will be loaded. You can specify which one to pick by: "
            $logWarn
                (" 1) Specifying targets to stack ghci e.g. stack ghci " <>
                 sampleTargetArg candidate)
            $logWarn
                (" 2) Specifying what the main is e.g. stack ghci " <>
                 sampleMainIsArg candidate)
            return Nothing
  where
    targets = fromMaybe targets0 mainIsTargets
    candidates = do
        pkg <- packages
        case M.lookup (ghciPkgName pkg) targets of
            Nothing -> []
            Just target -> do
                (component,mains) <-
                    M.toList
                        (M.filterWithKey wantedComponent (ghciPkgMainIs pkg))
                main <- S.toList mains
                return (ghciPkgName pkg, component, main)
                where wantedComponent namedC _ =
                          case target of
                              STLocalAll -> True
                              STLocalComps cs -> S.member namedC cs
                              _ -> False
    renderCandidate (pkgName,namedComponent,mainIs) =
        "Package `" <> packageNameText pkgName <> "' component " <>
        renderComp namedComponent <>
        " with main-is file: " <>
        T.pack (toFilePath mainIs)
    renderComp c =
        case c of
            CLib -> "lib"
            CExe name -> "exe:" <> name
            CTest name -> "test:" <> name
            CBench name -> "bench:" <> name
    sampleTargetArg (pkg,comp,_) =
        packageNameText pkg <> ":" <> renderComp comp
    sampleMainIsArg (pkg,comp,_) =
        "--main-is " <> packageNameText pkg <> ":" <> renderComp comp

-- | Create a list of infos for each target containing necessary
-- information to load that package/components.
ghciSetup
    :: (HasConfig r, HasHttpManager r, HasBuildConfig r, MonadMask m, HasTerminal r, HasLogLevel r, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m, MonadBaseControl IO m)
    => Maybe BuildSubset
    -> Maybe Text
    -> [Text]
    -> m (Map PackageName SimpleTarget, Maybe (Map PackageName SimpleTarget), [GhciPkgInfo])
ghciSetup mbuildFirst mainIs stringTargets = do
    (_,_,targets) <-
        parseTargetsFromBuildOpts
            AllowNoTargets
            defaultBuildOpts
            { boptsTargets = stringTargets
            }
    mainIsTargets <-
        case mainIs of
            Nothing -> return Nothing
            Just target -> do
                (_,_,targets') <-
                    parseTargetsFromBuildOpts
                        AllowNoTargets
                        defaultBuildOpts
                        { boptsTargets = [target]
                        }
                return (Just targets')
    let bopts = makeBuildOpts targets
    econfig <- asks getEnvConfig
    (realTargets,_,_,_,sourceMap) <- loadSourceMap AllowNoTargets (bopts BSAll)
    menv <- getMinimalEnvOverride
    (installedMap, _, _, _) <- getInstalled
        menv
        GetInstalledOpts
            { getInstalledProfiling = False
            , getInstalledHaddock   = False
            }
        sourceMap
    locals <-
        liftM catMaybes $
        forM (M.toList (envConfigPackages econfig)) $
        \(dir,validWanted) ->
             do cabalfp <- getCabalFileName dir
                name <- parsePackageNameFromFilePath cabalfp
                if validWanted
                    then case M.lookup name targets of
                             Just simpleTargets ->
                                 return (Just (name, (cabalfp, simpleTargets)))
                             Nothing -> return Nothing
                    else return Nothing
    -- Try to build, but optimistically launch GHCi anyway if it fails (#1065)
    case mbuildFirst of
        Just buildFirst -> do
            eres <- tryAny $ build (const (return ())) Nothing (bopts buildFirst)
            case eres of
                Left err -> do
                    $logError $ T.pack (show err)
                    $logWarn "Warning: build failed, but optimistically launching GHCi anyway"
                Right () -> return ()
        Nothing -> return ()
    -- Load the list of modules _after_ building, to catch changes in unlisted dependencies (#1180)
    let localLibs = [name | (name, (_, target)) <- locals, hasLocalComp isCLib target]
    infos <-
        forM locals $
        \(name,(cabalfp,target)) ->
             makeGhciPkgInfo sourceMap installedMap localLibs name cabalfp target
    warnAboutPotentialIssues infos
    return (realTargets, mainIsTargets, infos)
  where
    makeBuildOpts targets buildFirst =
        base
        { boptsTargets = stringTargets
        , boptsTests = any (hasLocalComp isCTest) elems
        , boptsBenchmarks = any (hasLocalComp isCBench) elems
        , boptsTestOpts = (boptsTestOpts base)
          { toDisableRun = True
          , toRerunTests = False
          }
        , boptsBenchmarkOpts = (boptsBenchmarkOpts base)
          { beoDisableRun = True
          }
        , boptsBuildSubset = buildFirst
        }
      where
        base = defaultBuildOpts
        elems = M.elems targets
    hasLocalComp p t =
        case t of
            STLocalComps s -> any p (S.toList s)
            STLocalAll -> True
            _ -> False
    isCLib nc =
        case nc of
            CLib{} -> True
            _ -> False
    isCTest nc =
        case nc of
            CTest{} -> True
            _ -> False
    isCBench nc =
        case nc of
            CBench{} -> True
            _ -> False

-- | Make information necessary to load the given package in GHCi.
makeGhciPkgInfo
    :: (MonadReader r m, HasEnvConfig r, MonadLogger m, MonadIO m, MonadCatch m)
    => SourceMap
    -> InstalledMap
    -> [PackageName]
    -> PackageName
    -> Path Abs File
    -> SimpleTarget
    -> m GhciPkgInfo
makeGhciPkgInfo sourceMap installedMap locals name cabalfp target = do
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig
    let config =
            PackageConfig
            { packageConfigEnableTests = True
            , packageConfigEnableBenchmarks = True
            , packageConfigFlags = localFlags mempty bconfig name
            , packageConfigCompilerVersion = envConfigCompilerVersion econfig
            , packageConfigPlatform = configPlatform (getConfig bconfig)
            }
    (warnings,pkg) <- readPackage config cabalfp
    mapM_ (printCabalFileWarning cabalfp) warnings
    (mods,files,opts) <- getPackageOpts (packageOpts pkg) sourceMap installedMap locals cabalfp
    let filteredOpts = filterWanted opts
        omitUnwanted bio = bio { bioGhcOpts = filter (not . badForGhci) (bioGhcOpts bio) }
        omitted = nubOrd $ filter badForGhci $ concatMap bioGhcOpts (M.elems filteredOpts)
    unless (null omitted) $
        $logWarn
            ("The following GHC options are incompatible with GHCi and have not been passed to it: " <>
             T.unwords (map T.pack omitted))
    return
        GhciPkgInfo
        { ghciPkgName = packageName pkg
        , ghciPkgOpts = M.toList (M.map omitUnwanted filteredOpts)
        , ghciPkgDir = parent cabalfp
        , ghciPkgModules = mconcat (M.elems (filterWanted mods))
        , ghciPkgModFiles = mconcat (M.elems (filterWanted (M.map (setMapMaybe dotCabalModulePath) files)))
        , ghciPkgMainIs = M.map (setMapMaybe dotCabalMainPath) files
        , ghciPkgCFiles = mconcat (M.elems (filterWanted (M.map (setMapMaybe dotCabalCFilePath) files)))
        }
  where
    badForGhci :: String -> Bool
    badForGhci x =
        isPrefixOf "-O" x || elem x (words "-debug -threaded -ticky -static")
    setMapMaybe f = S.fromList . mapMaybe f . S.toList
    filterWanted m = M.filterWithKey (\k _ -> wantedComponent k) m
    wantedComponent k =
        case target of
            STLocalComps cs -> S.member k cs
            _ -> True

warnAboutPotentialIssues :: MonadLogger m => [GhciPkgInfo] -> m ()
warnAboutPotentialIssues pkgs = unless (null issues) $ borderedWarning $ do
    $logWarn "There are issues with this project which may prevent GHCi from working properly."
    $logWarn ""
    mapM_ $logWarn $ intercalate [""] issues
    $logWarn ""
    $logWarn "To resolve, remove the flag(s) from the cabal file(s) and instead put them at the top of the haskell files."
    $logWarn ""
    $logWarn "It isn't yet possible to load multiple packages into GHCi in all cases - see"
    $logWarn "https://ghc.haskell.org/trac/ghc/ticket/10827"
  where
    issues = concat
        [ mixedFlag "-XNoImplicitPrelude"
          [ "-XNoImplicitPrelude will be used, but GHCi will likely fail to build things which depend on the implicit prelude." ]
        , mixedFlag "-XCPP"
          [ "-XCPP will be used, but it can cause issues with multiline strings."
          , "See https://downloads.haskell.org/~ghc/7.10.2/docs/html/users_guide/options-phases.html#cpp-string-gaps"
          ]
        , mixedFlag "-XNoTraditionalRecordSyntax"
          [ "-XNoTraditionalRecordSyntax will be used, but it break modules which use record syntax." ]
        , mixedFlag "-XTemplateHaskell"
          [ "-XTemplateHaskell will be used, but it may cause compilation issues due to different parsing of ($)." ]
        , mixedFlag "-XSafe"
          [ "-XSafe will be used, but it will fail to compile unsafe modules." ]
        ]
    mixedFlag flag msgs =
        let x = partitionComps (== flag) in
        [ msgs ++ showWhich x | mixedSettings x ]
    mixedSettings (xs, ys) = xs /= [] && ys /= []
    showWhich (haveIt, don'tHaveIt) =
        [ "It is specified for:"
        , "    " <> renderPkgComps haveIt
        , "But not for: "
        , "    " <> renderPkgComps don'tHaveIt
        ]
    renderPkgComps = T.intercalate " " . map renderPkgComp
    renderPkgComp (pkg, comp) = packageNameText pkg <> ":" <> decodeUtf8 (renderComponent comp)
    compsWithOpts = concat
        [ [ ((ghciPkgName pkg, c), bioGeneratedOpts bio ++ bioGhcOpts bio)
          | (c, bio) <- ghciPkgOpts pkg
          ]
        | pkg <- pkgs ]
    partitionComps f = (map fst xs, map fst ys)
      where
        (xs, ys) = partition (any f . snd) compsWithOpts

borderedWarning :: MonadLogger m => m a -> m a
borderedWarning f = do
    $logWarn ""
    $logWarn "* * * * * * * *"
    x <- f
    $logWarn "* * * * * * * *"
    $logWarn ""
    return x
