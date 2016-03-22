{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

-- | Run a GHCi configured with the user's package(s).

module Stack.Ghci
    ( GhciOpts(..)
    , GhciPkgInfo(..)
    , GhciException(..)
    , ghciSetup
    , ghci
    ) where

import           Control.Applicative
import           Control.Exception.Enclosed (tryAny)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as S8
import           Data.Either
import           Data.Function
import           Data.List
import           Data.List.Extra (nubOrd)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Maybe.Extra (forMaybeM)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
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
import           Text.Read (readMaybe)

#ifndef WINDOWS
import qualified System.Posix.Files as Posix
#endif

-- | Command-line options for GHC.
data GhciOpts = GhciOpts
    { ghciNoBuild            :: !Bool
    , ghciArgs               :: ![String]
    , ghciGhcCommand         :: !(Maybe FilePath)
    , ghciNoLoadModules      :: !Bool
    , ghciAdditionalPackages :: ![String]
    , ghciMainIs             :: !(Maybe Text)
    , ghciLoadLocalDeps      :: !Bool
    , ghciSkipIntermediate   :: !Bool
    , ghciHidePackages       :: !Bool
    , ghciBuildOptsCLI       :: !BuildOptsCLI
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

data GhciException
    = InvalidPackageOption String
    | LoadingDuplicateModules
    deriving (Typeable)

instance Exception GhciException

instance Show GhciException where
    show (InvalidPackageOption name) =
        "Failed to parse --package option " ++ name
    show LoadingDuplicateModules = unlines
        [ "Not attempting to start ghci due to these duplicate modules."
        , "Use --no-load to try to start it anyway, without loading any modules (but these are still likely to cause errors)"
        ]

-- | Launch a GHCi session for the given local package targets with the
-- given options and configure it with the load paths and extensions
-- of those targets.
ghci
    :: (HasConfig r, HasBuildConfig r, HasHttpManager r, MonadMask m, HasLogLevel r, HasTerminal r, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadBaseControl IO m)
    => GhciOpts -> m ()
ghci opts@GhciOpts{..} = do
    bopts <- asks (configBuild . getConfig)
    (targets,mainIsTargets,pkgs) <- ghciSetup opts
    config <- asks getConfig
    bconfig <- asks getBuildConfig
    wc <- getWhichCompiler
    let pkgopts = hidePkgOpt ++ genOpts ++ ghcOpts
        hidePkgOpt = if null pkgs || not ghciHidePackages then [] else ["-hide-all-packages"]
        oneWordOpts bio
            | ghciHidePackages = bioOneWordOpts bio ++ bioPackageFlags bio
            | otherwise = bioOneWordOpts bio
        genOpts = nubOrd (concatMap (concatMap (oneWordOpts . snd) . ghciPkgOpts) pkgs)
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
    allModules <- checkForDuplicateModules ghciNoLoadModules pkgs
    oiDir <- objectInterfaceDir bconfig
    (modulesToLoad, mainFile) <- if ghciNoLoadModules then return ([], Nothing) else do
        mainFile <- figureOutMainFile bopts mainIsTargets targets pkgs
        return (allModules, mainFile)
    let odir =
            [ "-odir=" <> toFilePathNoTrailingSep oiDir
            , "-hidir=" <> toFilePathNoTrailingSep oiDir ]
    $logInfo
        ("Configuring GHCi with the following packages: " <>
         T.intercalate ", " (map (packageNameText . ghciPkgName) pkgs))
    let execGhci extras = do
            menv <- liftIO $ configEnvOverride config defaultEnvSettings
            execSpawn menv
                 (fromMaybe (compilerExeName wc) ghciGhcCommand)
                 ("--interactive" :
                 -- This initial "-i" resets the include directories to not
                 -- include CWD.
                  "-i" :
                  odir <> pkgopts <> ghciArgs <> extras)
    withSystemTempDir "ghci" $ \tmpDir -> do
        let macrosFile = tmpDir </> $(mkRelFile "cabal_macros.h")
        macrosOpts <- preprocessCabalMacros pkgs macrosFile
        if ghciNoLoadModules
            then execGhci macrosOpts
            else do
                let scriptPath = tmpDir </> $(mkRelFile "ghci-script")
                    fp = toFilePath scriptPath
                    loadModules = ":load " <> unwords (map show modulesToLoad)
                    addMainFile = maybe "" ((":add " <>) . toFilePath) mainFile
                    bringIntoScope = ":module + " <> unwords modulesToLoad
                liftIO (writeFile fp (unlines [loadModules,addMainFile,bringIntoScope]))
                setScriptPerms fp
                execGhci (macrosOpts ++ ["-ghci-script=" <> fp])

-- | Figure out the main-is file to load based on the targets. Sometimes there
-- is none, sometimes it's unambiguous, sometimes it's
-- ambiguous. Warns and returns nothing if it's ambiguous.
figureOutMainFile
    :: (Monad m, MonadLogger m, MonadIO m)
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
        candidate:_ -> do
          borderedWarning $ do
            $logWarn "The main module to load is ambiguous. Candidates are: "
            forM_ (map renderCandidate candidates) $logWarn
            $logWarn
                "You can specify which one to pick by: "
            $logWarn
                (" * Specifying targets to stack ghci e.g. stack ghci " <>
                 sampleTargetArg candidate)
            $logWarn
                (" * Specifying what the main is e.g. stack ghci " <>
                 sampleMainIsArg candidate)
            $logWarn
                (" * Choosing from the candidate above [1.." <>
                T.pack (show $ length candidates) <> "]")
          liftIO userOption
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
    renderCandidate c@(pkgName,namedComponent,mainIs) =
        let candidateIndex = T.pack . show . (+1) . fromMaybe 0 . elemIndex c
        in  candidateIndex candidates <> ". Package `" <>
            packageNameText pkgName <>
            "' component " <>
            renderComp namedComponent <>
            " with main-is file: " <>
            T.pack (toFilePath mainIs)
    candidateIndices = take (length candidates) [1 :: Int ..]
    userOption = do
      putStr "Specify main module to use (press enter to load none): "
      option <- getLine
      let selected = fromMaybe
                      ((+1) $ length candidateIndices)
                      (readMaybe option :: Maybe Int)
      case elemIndex selected candidateIndices  of
        Nothing -> do
            putStrLn
              "Not loading any main modules, as no valid module selected"
            putStrLn ""
            return Nothing
        Just op -> do
            let (_,_,fp) = candidates !! op
            putStrLn
              ("Loading main module from cadidate " <>
              show (op + 1) <> ", --main-is " <>
              toFilePath fp)
            putStrLn ""
            return $ Just fp
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
    :: (HasConfig r, HasHttpManager r, HasBuildConfig r, MonadMask m, HasTerminal r, HasLogLevel r, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadBaseControl IO m)
    => GhciOpts
    -> m (Map PackageName SimpleTarget, Maybe (Map PackageName SimpleTarget), [GhciPkgInfo])
ghciSetup GhciOpts{..} = do
    (_,_,targets) <- parseTargetsFromBuildOpts AllowNoTargets ghciBuildOptsCLI 
    mainIsTargets <-
        case ghciMainIs of
            Nothing -> return Nothing
            Just target -> do
                (_,_,targets') <- parseTargetsFromBuildOpts AllowNoTargets ghciBuildOptsCLI { boptsCLITargets = [target] }
                return (Just targets')
    addPkgs <- forM ghciAdditionalPackages $ \name -> do
        let mres = (packageIdentifierName <$> parsePackageIdentifierFromString name)
                <|> parsePackageNameFromString name
        maybe (throwM $ InvalidPackageOption name) return mres
    let boptsCli = ghciBuildOptsCLI
            { boptsCLITargets = boptsCLITargets ghciBuildOptsCLI ++ map T.pack ghciAdditionalPackages
            }
    -- Try to build, but optimistically launch GHCi anyway if it fails (#1065)
    unless ghciNoBuild $ do
        eres <- tryAny $ build (const (return ())) Nothing boptsCli
        case eres of
            Right () -> return ()
            Left err -> do
                $logError $ T.pack (show err)
                $logWarn "Warning: build failed, but optimistically launching GHCi anyway"
    econfig <- asks getEnvConfig
    (realTargets,_,_,_,sourceMap) <- loadSourceMap AllowNoTargets boptsCli
    menv <- getMinimalEnvOverride
    (installedMap, _, _, _) <- getInstalled
        menv
        GetInstalledOpts
            { getInstalledProfiling = False
            , getInstalledHaddock   = False
            }
        sourceMap
    directlyWanted <-
        forMaybeM (M.toList (envConfigPackages econfig)) $
        \(dir,treatLikeExtraDep) ->
             do cabalfp <- findOrGenerateCabalFile dir
                name <- parsePackageNameFromFilePath cabalfp
                if treatLikeExtraDep
                    then return Nothing
                    else case M.lookup name targets of
                             Just simpleTargets ->
                                 return (Just (name, (cabalfp, simpleTargets)))
                             Nothing -> return Nothing
    let extraLoadDeps = getExtraLoadDeps ghciLoadLocalDeps sourceMap directlyWanted
    wanted <-
        if (ghciSkipIntermediate && not ghciLoadLocalDeps) || null extraLoadDeps
            then return directlyWanted
            else do
                let extraList = T.intercalate ", " (map (packageNameText . fst) extraLoadDeps)
                if ghciLoadLocalDeps
                    then $logInfo $ T.concat
                        [ "The following libraries will also be loaded into GHCi because "
                        , "they are local dependencies of your targets, and you specified --load-local-deps:\n    "
                        , extraList
                        ]
                    else $logInfo $ T.concat
                        [ "The following libraries will also be loaded into GHCi because "
                        , "they are intermediate dependencies of your targets:\n    "
                        , extraList
                        , "\n(Use --skip-intermediate-deps to omit these)"
                        ]
                return (directlyWanted ++ extraLoadDeps)
    -- Load the list of modules _after_ building, to catch changes in unlisted dependencies (#1180)
    let localLibs = [name | (name, (_, target)) <- wanted, hasLocalComp isCLib target]
    infos <-
        forM wanted $
        \(name,(cabalfp,target)) ->
             makeGhciPkgInfo boptsCli sourceMap installedMap localLibs addPkgs name cabalfp target
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
    => BuildOptsCLI
    -> SourceMap
    -> InstalledMap
    -> [PackageName]
    -> [PackageName]
    -> PackageName
    -> Path Abs File
    -> SimpleTarget
    -> m GhciPkgInfo
makeGhciPkgInfo boptsCli sourceMap installedMap locals addPkgs name cabalfp target = do
    bopts <- asks (configBuild . getConfig)
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig
    let config =
            PackageConfig
            { packageConfigEnableTests = True
            , packageConfigEnableBenchmarks = True
            , packageConfigFlags = localFlags (boptsCLIFlags boptsCli) bconfig name
            , packageConfigCompilerVersion = envConfigCompilerVersion econfig
            , packageConfigPlatform = configPlatform (getConfig bconfig)
            }
    (warnings,pkg) <- readPackage config cabalfp
    mapM_ (printCabalFileWarning cabalfp) warnings
    (mods,files,opts) <- getPackageOpts (packageOpts pkg) sourceMap installedMap locals addPkgs cabalfp
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
    (if boptsTests bopts then map CTest (M.keys (packageTests pkg)) else []) <>
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

checkForDuplicateModules :: (MonadThrow m, MonadLogger m) => Bool -> [GhciPkgInfo] -> m [String]
checkForDuplicateModules noLoadModules pkgs = do
    unless (null duplicates) $ do
        borderedWarning $ do
            $logWarn "The following modules are present in multiple packages:"
            forM_ duplicates $ \(mn, pns) -> do
                $logWarn (" * " <> T.pack mn <> " (in " <> T.intercalate ", " (map packageNameText pns) <> ")")
        unless noLoadModules $ throwM LoadingDuplicateModules
    return (map fst allModules)
  where
    duplicates, allModules :: [(String, [PackageName])]
    duplicates = filter (not . null . tail . snd) allModules
    allModules =
        M.toList $ M.fromListWith (++) $
        concatMap (\pkg -> map (, [ghciPkgName pkg]) (map display (S.toList (ghciPkgModules pkg)))) pkgs

-- Adds in intermediate dependencies between ghci targets. Note that it
-- will return a Lib component for these intermediate dependencies even
-- if they don't have a library (but that's fine for the usage within
-- this module).
--
-- If 'True' is passed for loadAllDeps, this loads all local deps, even
-- if they aren't intermediate.
getExtraLoadDeps
    :: Bool
    -> SourceMap
    -> [(PackageName, (Path Abs File, SimpleTarget))]
    -> [(PackageName, (Path Abs File, SimpleTarget))]
getExtraLoadDeps loadAllDeps sourceMap targets =
    M.toList $
    (\mp -> foldl' (flip M.delete) mp (map fst targets)) $
    M.mapMaybe id $
    execState (mapM_ (mapM_ go . getDeps . fst) targets)
              (M.fromList (map (\(k, x) -> (k, Just x)) targets))
  where
    getDeps :: PackageName -> [PackageName]
    getDeps name =
        case M.lookup name sourceMap of
            Just (PSLocal lp) -> M.keys (packageDeps (lpPackage lp))
            _ -> []
    go :: PackageName -> State (Map PackageName (Maybe (Path Abs File, SimpleTarget))) Bool
    go name = do
        cache <- get
        case (M.lookup name cache, M.lookup name sourceMap) of
            (Just (Just _), _) -> return True
            (Just Nothing, _) | not loadAllDeps -> return False
            (_, Just (PSLocal lp)) -> do
                let deps = M.keys (packageDeps (lpPackage lp))
                shouldLoad <- liftM or $ mapM go deps
                if shouldLoad
                    then do
                        modify (M.insert name (Just (lpCabalFile lp, STLocalComps (S.singleton CLib))))
                        return True
                    else do
                        modify (M.insert name Nothing)
                        return False
            (_, Just PSUpstream{}) -> return loadAllDeps
            (_, _) -> return False

preprocessCabalMacros :: MonadIO m => [GhciPkgInfo] -> Path Abs File -> m [String]
preprocessCabalMacros pkgs out = liftIO $ do
    let fps = nubOrd (concatMap (mapMaybe (bioCabalMacros . snd) . ghciPkgOpts) pkgs)
    files <- mapM (S8.readFile . toFilePath) fps
    if null files then return [] else do
        S8.writeFile (toFilePath out) $ S8.concat $ map (<> "\n#undef CURRENT_PACKAGE_KEY\n") files
        return ["-optP-include", "-optP" <> toFilePath out]

setScriptPerms :: MonadIO m => FilePath -> m ()
#ifdef WINDOWS
setScriptPerms _ = do
    return ()
#else
setScriptPerms fp = do
    liftIO $ Posix.setFileMode fp $ foldl1 Posix.unionFileModes
        [ Posix.ownerReadMode
        , Posix.ownerWriteMode
        , Posix.groupReadMode
        , Posix.otherReadMode
        ]
#endif
