{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

-- | Run a GHCi configured with the user's package(s).

module Stack.Ghci
    ( GhciOpts(..)
    , GhciPkgInfo(..)
    , ghciSetup
    , ghci
    ) where

import           Control.Exception.Enclosed (tryAny)
import           Control.Monad.Catch
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
import           Data.Maybe.Extra (forMaybeM)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Distribution.ModuleName (ModuleName)
import           Distribution.Text (display)
import           Network.HTTP.Client.Conduit
import           Path
import           Path.Extra (toFilePathNoTrailingSep)
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
    { ghciNoBuild            :: !Bool
    , ghciArgs               :: ![String]
    , ghciGhcCommand         :: !(Maybe FilePath)
    , ghciNoLoadModules      :: !Bool
    , ghciAdditionalPackages :: ![String]
    , ghciMainIs             :: !(Maybe Text)
    , ghciBuildOpts          :: !BuildOpts
    } deriving Show

-- | Necessary information to load a package or its components.
data GhciPkgInfo = GhciPkgInfo
    { ghciPkgName :: !PackageName
    , ghciPkgOpts :: ![(NamedComponent, BuildInfoOpts)]
    , ghciPkgDir :: !(Path Abs Dir)
    , ghciPkgModules :: !(Set ModuleName)
    , ghciPkgModFiles :: !(Set (Path Abs File)) -- ^ Module file paths.
    , ghciPkgCFiles :: !(Set (Path Abs File)) -- ^ C files.
    , ghciPkgMainIs :: !(Map NamedComponent (Set (Path Abs File)))
    , ghciPkgPackage :: !Package
    } deriving Show

-- | Launch a GHCi session for the given local package targets with the
-- given options and configure it with the load paths and extensions
-- of those targets.
ghci
    :: (HasConfig r, HasBuildConfig r, HasHttpManager r, MonadMask m, HasLogLevel r, HasTerminal r, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m, MonadBaseControl IO m)
    => GhciOpts -> m ()
ghci GhciOpts{..} = do
    let bopts = ghciBuildOpts
            { boptsTestOpts = (boptsTestOpts ghciBuildOpts) { toDisableRun = True }
            , boptsBenchmarkOpts = (boptsBenchmarkOpts ghciBuildOpts) { beoDisableRun = True }
            }
    (targets,mainIsTargets,pkgs) <- ghciSetup bopts ghciNoBuild ghciMainIs
    config <- asks getConfig
    bconfig <- asks getBuildConfig
    mainFile <- figureOutMainFile bopts mainIsTargets targets pkgs
    wc <- getWhichCompiler
    let pkgopts = hidePkgOpt ++ genOpts ++ ghcOpts
        hidePkgOpt = if null pkgs then [] else ["-hide-all-packages"]
        genOpts = nubOrd (concatMap (concatMap (bioOneWordOpts . snd) . ghciPkgOpts) pkgs)
        (omittedOpts, ghcOpts) = partition badForGhci $
            concatMap (concatMap (bioOpts . snd) . ghciPkgOpts) pkgs ++
            getUserOptions Nothing ++
            concatMap (getUserOptions . Just . ghciPkgName) pkgs
        getUserOptions mpkg =
            map T.unpack (M.findWithDefault [] mpkg (configGhcOptions config))
        badForGhci x =
            isPrefixOf "-O" x || elem x (words "-debug -threaded -ticky -static -Werror")
    unless (null omittedOpts) $
        $logWarn
            ("The following GHC options are incompatible with GHCi and have not been passed to it: " <>
             T.unwords (map T.pack (nubOrd omittedOpts)))
    let modulesToLoad = nubOrd $
            maybe [] (return . toFilePath) mainFile <>
            concatMap (map display . S.toList . ghciPkgModules) pkgs
        odir =
            [ "-odir=" <> toFilePathNoTrailingSep (objectInterfaceDir bconfig)
            , "-hidir=" <> toFilePathNoTrailingSep (objectInterfaceDir bconfig)]
    $logInfo
        ("Configuring GHCi with the following packages: " <>
         T.intercalate ", " (map (packageNameText . ghciPkgName) pkgs))
    let execGhci extras =
            exec defaultEnvSettings
                 (fromMaybe (compilerExeName wc) ghciGhcCommand)
                 ("--interactive" :
                 -- This initial "-i" resets the include directories to not
                 -- include CWD.
                  "-i" :
                  odir <> pkgopts <> ghciArgs <> extras)
    case ghciNoLoadModules of
        True -> execGhci []
        False -> do
            tmp <- liftIO getTemporaryDirectory
            withCanonicalizedTempDirectory
                tmp
                "ghci-script"
                (\tmpDir ->
                      do let scriptPath = tmpDir </> $(mkRelFile "ghci-script")
                             fp = toFilePath scriptPath
                             loadModules = ":load " <> unwords (map show modulesToLoad)
                             bringIntoScope = ":module + " <> unwords (map show modulesToLoad)
                         liftIO (writeFile fp (unlines [loadModules,bringIntoScope]))
                         finally (execGhci ["-ghci-script=" <> fp])
                                 (removeFile scriptPath))

-- | Figure out the main-is file to load based on the targets. Sometimes there
-- is none, sometimes it's unambiguous, sometimes it's
-- ambiguous. Warns and returns nothing if it's ambiguous.
figureOutMainFile
    :: (Monad m, MonadLogger m)
    => BuildOpts
    -> Maybe (Map PackageName SimpleTarget)
    -> Map PackageName SimpleTarget
    -> [GhciPkgInfo]
    -> m (Maybe (Path Abs File))
figureOutMainFile bopts mainIsTargets targets0 packages =
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
                    M.toList $
                    M.filterWithKey (\k _ -> k `S.member` wantedComponents)
                                    (ghciPkgMainIs pkg)
                main <- S.toList mains
                return (ghciPkgName pkg, component, main)
              where
                wantedComponents =
                    wantedPackageComponents bopts target (ghciPkgPackage pkg)
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
    => BuildOpts
    -> Bool
    -> Maybe Text
    -> m (Map PackageName SimpleTarget, Maybe (Map PackageName SimpleTarget), [GhciPkgInfo])
ghciSetup bopts noBuild mainIs = do
    (_,_,targets) <- parseTargetsFromBuildOpts AllowNoTargets bopts
    mainIsTargets <-
        case mainIs of
            Nothing -> return Nothing
            Just target -> do
                (_,_,targets') <- parseTargetsFromBuildOpts AllowNoTargets bopts { boptsTargets = [target] }
                return (Just targets')
    econfig <- asks getEnvConfig
    (realTargets,_,_,_,sourceMap) <- loadSourceMap AllowNoTargets bopts
    menv <- getMinimalEnvOverride
    (installedMap, _, _, _) <- getInstalled
        menv
        GetInstalledOpts
            { getInstalledProfiling = False
            , getInstalledHaddock   = False
            }
        sourceMap
    locals <-
        forMaybeM (M.toList (envConfigPackages econfig)) $
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
    unless noBuild $ do
        eres <- tryAny $ build (const (return ())) Nothing bopts
        case eres of
            Right () -> return ()
            Left err -> do
                $logError $ T.pack (show err)
                $logWarn "Warning: build failed, but optimistically launching GHCi anyway"
    -- Load the list of modules _after_ building, to catch changes in unlisted dependencies (#1180)
    let localLibs = [name | (name, (_, target)) <- locals, hasLocalComp isCLib target]
    infos <-
        forM locals $
        \(name,(cabalfp,target)) ->
             makeGhciPkgInfo bopts sourceMap installedMap localLibs name cabalfp target
    checkForIssues infos
    return (realTargets, mainIsTargets, infos)
  where
    hasLocalComp p t =
        case t of
            STLocalComps s -> any p (S.toList s)
            STLocalAll -> True
            _ -> False

-- | Make information necessary to load the given package in GHCi.
makeGhciPkgInfo
    :: (MonadReader r m, HasEnvConfig r, MonadLogger m, MonadIO m, MonadCatch m)
    => BuildOpts
    -> SourceMap
    -> InstalledMap
    -> [PackageName]
    -> PackageName
    -> Path Abs File
    -> SimpleTarget
    -> m GhciPkgInfo
makeGhciPkgInfo bopts sourceMap installedMap locals name cabalfp target = do
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig
    let config =
            PackageConfig
            { packageConfigEnableTests = True
            , packageConfigEnableBenchmarks = True
            , packageConfigFlags = localFlags (boptsFlags bopts) bconfig name
            , packageConfigCompilerVersion = envConfigCompilerVersion econfig
            , packageConfigPlatform = configPlatform (getConfig bconfig)
            }
    (warnings,pkg) <- readPackage config cabalfp
    mapM_ (printCabalFileWarning cabalfp) warnings
    (mods,files,opts) <- getPackageOpts (packageOpts pkg) sourceMap installedMap locals cabalfp
    let filteredOpts = filterWanted opts
        filterWanted = M.filterWithKey (\k _ -> k `S.member` allWanted)
        allWanted = wantedPackageComponents bopts target pkg
        setMapMaybe f = S.fromList . mapMaybe f . S.toList
    return
        GhciPkgInfo
        { ghciPkgName = packageName pkg
        , ghciPkgOpts = M.toList filteredOpts
        , ghciPkgDir = parent cabalfp
        , ghciPkgModules = mconcat (M.elems (filterWanted mods))
        , ghciPkgModFiles = mconcat (M.elems (filterWanted (M.map (setMapMaybe dotCabalModulePath) files)))
        , ghciPkgMainIs = M.map (setMapMaybe dotCabalMainPath) files
        , ghciPkgCFiles = mconcat (M.elems (filterWanted (M.map (setMapMaybe dotCabalCFilePath) files)))
        , ghciPkgPackage = pkg
        }

-- NOTE: this should make the same choices as the components code in
-- 'loadLocalPackage'. Unfortunately for now we reiterate this logic
-- (differently).
wantedPackageComponents :: BuildOpts -> SimpleTarget -> Package -> Set NamedComponent
wantedPackageComponents _ (STLocalComps cs) _ = cs
wantedPackageComponents bopts STLocalAll pkg = S.fromList $
    (if packageHasLibrary pkg then [CLib] else []) ++
    map CExe (S.toList (packageExes pkg)) <>
    (if boptsTests bopts then map CTest (S.toList (packageTests pkg)) else []) <>
    (if boptsBenchmarks bopts then map CBench (S.toList (packageBenchmarks pkg)) else [])
wantedPackageComponents _ _ _ = S.empty

checkForIssues :: (MonadThrow m, MonadLogger m) => [GhciPkgInfo] -> m ()
checkForIssues pkgs = do
    unless (null issues) $ borderedWarning $ do
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
          [ "-XTemplateHaskell will be used, but it may cause compilation issues due to different parsing of '$' when there's no space after it." ]
        , mixedFlag "-XQuasiQuotes"
          [ "-XQuasiQuotes will be used, but it may cause parse failures due to a different meaning for list comprehension syntax like [x| ... ]" ]
        , mixedFlag "-XSafe"
          [ "-XSafe will be used, but it will fail to compile unsafe modules." ]
        , mixedFlag "-XArrows"
          [ "-XArrows will be used, but it will cause non-arrow usages of proc, (-<), (-<<) to fail" ]
        , mixedFlag "-XOverloadedStrings"
          [ "-XOverloadedStrings will be used, but it can cause type ambiguity in code not usually compiled with it." ]
        , mixedFlag "-XOverloadedLists"
          [ "-XOverloadedLists will be used, but it can cause type ambiguity in code not usually compiled with it." ]
        , mixedFlag "-XMonoLocalBinds"
          [ "-XMonoLocalBinds will be used, but it can cause type errors in code which expects generalized local bindings." ]
        , mixedFlag "-XTypeFamilies"
          [ "-XTypeFamilies will be used, but it implies -XMonoLocalBinds, and so can cause type errors in code which expects generalized local bindings." ]
        , mixedFlag "-XGADTs"
          [ "-XGADTs will be used, but it implies -XMonoLocalBinds, and so can cause type errors in code which expects generalized local bindings." ]
        , mixedFlag "-XNewQualifiedOperators"
          [ "-XNewQualifiedOperators will be used, but this will break usages of the old qualified operator syntax." ]
        ]
    mixedFlag flag msgs =
        let x = partitionComps (== flag) in
        [ msgs ++ showWhich x | mixedSettings x ]
    mixedSettings (xs, ys) = xs /= [] && ys /= []
    showWhich (haveIt, don'tHaveIt) =
        [ "It is specified for:"
        , "    " <> renderPkgComponents haveIt
        , "But not for: "
        , "    " <> renderPkgComponents don'tHaveIt
        ]
    partitionComps f = (map fst xs, map fst ys)
      where
        (xs, ys) = partition (any f . snd) compsWithOpts
    compsWithOpts = map (\(k, bio) -> (k, bioOneWordOpts bio ++ bioOpts bio)) compsWithBios
    compsWithBios =
        [ ((ghciPkgName pkg, c), bio)
        | pkg <- pkgs
        , (c, bio) <- ghciPkgOpts pkg
        ]

borderedWarning :: MonadLogger m => m a -> m a
borderedWarning f = do
    $logWarn ""
    $logWarn "* * * * * * * *"
    x <- f
    $logWarn "* * * * * * * *"
    $logWarn ""
    return x
