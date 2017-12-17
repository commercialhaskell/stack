{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Run a GHCi configured with the user's package(s).

module Stack.Ghci
    ( GhciOpts(..)
    , GhciPkgInfo(..)
    , GhciException(..)
    , ghci
    ) where

import           Stack.Prelude
import           Control.Monad.State.Strict (State, execState, get, modify)
import qualified Data.ByteString.Char8 as S8
import           Data.List
import           Data.List.Extra (nubOrd)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Distribution.PackageDescription as C
import qualified Distribution.Text as C
import           Path
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.IO hiding (withSystemTempDir)
import           Stack.Build
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Build.Target
import           Stack.Config (getLocalPackages)
import           Stack.Constants.Config
import           Stack.Exec
import           Stack.Ghci.Script
import           Stack.Package
import           Stack.PrettyPrint
import           Stack.Types.Build
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.FlagName
import           Stack.Types.Package
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Runner
import           System.IO (putStrLn, putStr, getLine)

#ifndef WINDOWS
import qualified System.Posix.Files as Posix
#endif

-- | Command-line options for GHC.
data GhciOpts = GhciOpts
    { ghciTargets            :: ![Text]
    , ghciArgs               :: ![String]
    , ghciGhcOptions         :: ![Text]
    , ghciFlags              :: !(Map (Maybe PackageName) (Map FlagName Bool))
    , ghciGhcCommand         :: !(Maybe FilePath)
    , ghciNoLoadModules      :: !Bool
    , ghciAdditionalPackages :: ![String]
    , ghciMainIs             :: !(Maybe Text)
    , ghciLoadLocalDeps      :: !Bool
    , ghciSkipIntermediate   :: !Bool
    , ghciHidePackages       :: !(Maybe Bool)
    , ghciNoBuild            :: !Bool
    , ghciOnlyMain           :: !Bool
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
    , ghciPkgTargetFiles :: !(Maybe (Set (Path Abs File)))
    , ghciPkgPackage :: !Package
    } deriving Show

data GhciException
    = InvalidPackageOption String
    | LoadingDuplicateModules
    | MissingFileTarget String
    | Can'tSpecifyFilesAndTargets
    | Can'tSpecifyFilesAndMainIs
    | GhciTargetParseException [Text]
    deriving (Typeable)

instance Exception GhciException

instance Show GhciException where
    show (InvalidPackageOption name) =
        "Failed to parse --package option " ++ name
    show LoadingDuplicateModules = unlines
        [ "Not attempting to start ghci due to these duplicate modules."
        , "Use --no-load to try to start it anyway, without loading any modules (but these are still likely to cause errors)"
        ]
    show (MissingFileTarget name) =
        "Cannot find file target " ++ name
    show Can'tSpecifyFilesAndTargets =
        "Cannot use 'stack ghci' with both file targets and package targets"
    show Can'tSpecifyFilesAndMainIs =
        "Cannot use 'stack ghci' with both file targets and --main-is flag"
    show (GhciTargetParseException xs) =
        show (TargetParseException xs) ++
        "\nNote that to specify options to be passed to GHCi, use the --ghci-options flag"

-- | Launch a GHCi session for the given local package targets with the
-- given options and configure it with the load paths and extensions
-- of those targets.
ghci :: HasEnvConfig env => GhciOpts -> RIO env ()
ghci opts@GhciOpts{..} = do
    let buildOptsCLI = defaultBuildOptsCLI
            { boptsCLITargets = []
            , boptsCLIFlags = ghciFlags
            }
    -- Load source map, without explicit targets, to collect all info.
    (locals, sourceMap) <- loadSourceMap AllowNoTargets buildOptsCLI
    -- Parse --main-is argument.
    mainIsTargets <- parseMainIsTargets buildOptsCLI ghciMainIs
    -- Parse to either file targets or build targets
    etargets <- preprocessTargets buildOptsCLI ghciTargets
    (inputTargets, mfileTargets) <- case etargets of
        Right packageTargets -> return (packageTargets, Nothing)
        Left rawFileTargets -> do
            case mainIsTargets of
                Nothing -> return ()
                Just _ -> throwM Can'tSpecifyFilesAndMainIs
            -- Figure out targets based on filepath targets
            (targetMap, fileInfo, extraFiles) <- findFileTargets locals rawFileTargets
            return (targetMap, Just (fileInfo, extraFiles))
    -- Get a list of all the local target packages.
    localTargets <- getAllLocalTargets opts inputTargets mainIsTargets sourceMap
    -- Get a list of all the non-local target packages.
    nonLocalTargets <- getAllNonLocalTargets inputTargets
    -- Check if additional package arguments are sensible.
    addPkgs <- checkAdditionalPackages ghciAdditionalPackages
    -- Build required dependencies and setup local packages.
    stackYaml <- view stackYamlL
    buildDepsAndInitialSteps opts (map (packageNameText . fst) localTargets)
    targetWarnings stackYaml localTargets nonLocalTargets mfileTargets
    -- Load the list of modules _after_ building, to catch changes in unlisted dependencies (#1180)
    pkgs <- getGhciPkgInfos buildOptsCLI sourceMap addPkgs (fmap fst mfileTargets) localTargets
    checkForIssues pkgs
    -- Finally, do the invocation of ghci
    runGhci opts localTargets mainIsTargets pkgs (maybe [] snd mfileTargets) (nonLocalTargets ++ addPkgs)

preprocessTargets :: HasEnvConfig env => BuildOptsCLI -> [Text] -> RIO env (Either [Path Abs File] (Map PackageName Target))
preprocessTargets buildOptsCLI rawTargets = do
    let (fileTargetsRaw, normalTargetsRaw) =
            partition (\t -> ".hs" `T.isSuffixOf` t || ".lhs" `T.isSuffixOf` t)
                      rawTargets
    -- Only use file targets if we have no normal targets.
    if not (null fileTargetsRaw) && null normalTargetsRaw
        then do
            fileTargets <- forM fileTargetsRaw $ \fp0 -> do
                let fp = T.unpack fp0
                mpath <- liftIO $ forgivingAbsence (resolveFile' fp)
                case mpath of
                    Nothing -> throwM (MissingFileTarget fp)
                    Just path -> return path
            return (Left fileTargets)
        else do
            -- Try parsing targets before checking if both file and
            -- module targets are specified (see issue#3342).
            (_,_,normalTargets) <- parseTargets AllowNoTargets buildOptsCLI { boptsCLITargets = normalTargetsRaw }
                `catch` \ex -> case ex of
                    TargetParseException xs -> throwM (GhciTargetParseException xs)
                    _ -> throwM ex
            unless (null fileTargetsRaw) $ throwM Can'tSpecifyFilesAndTargets
            return (Right normalTargets)

parseMainIsTargets :: HasEnvConfig env => BuildOptsCLI -> Maybe Text -> RIO env (Maybe (Map PackageName Target))
parseMainIsTargets buildOptsCLI mtarget = forM mtarget $ \target -> do
     (_,_,targets) <- parseTargets AllowNoTargets buildOptsCLI
         { boptsCLITargets = [target] }
     return targets

findFileTargets
    :: HasEnvConfig env
    => [LocalPackage]
    -> [Path Abs File]
    -> RIO env (Map PackageName Target, Map PackageName (Set (Path Abs File)), [Path Abs File])
findFileTargets locals fileTargets = do
    filePackages <- forM locals $ \lp -> do
        (_,compFiles,_,_) <- getPackageFiles (packageFiles (lpPackage lp)) (lpCabalFile lp)
        return (lp, M.map (S.map dotCabalGetPath) compFiles)
    let foundFileTargetComponents :: [(Path Abs File, [(PackageName, NamedComponent)])]
        foundFileTargetComponents =
            map (\fp -> (fp, ) $ sort $
                        concatMap (\(lp, files) -> map ((packageName (lpPackage lp), ) . fst)
                                                       (filter (S.member fp . snd) (M.toList files))
                                  ) filePackages
                ) fileTargets
    results <- forM foundFileTargetComponents $ \(fp, xs) ->
        case xs of
            [] -> do
                prettyWarn $ vsep
                    [ "Couldn't find a component for file target" <+>
                      display fp <>
                      ". This means that the correct ghc options might not be used."
                    , "Attempting to load the file anyway."
                    ]
                return $ Left fp
            [x] -> do
                prettyInfo $
                    "Using configuration for" <+> display x <+>
                    "to load" <+> display fp
                return $ Right (fp, x)
            (x:_) -> do
                prettyWarn $
                    "Multiple components contain file target" <+>
                    display fp <> ":" <+>
                    mconcat (intersperse ", " (map display xs)) <> line <>
                    "Guessing the first one," <+> display x <> "."
                return $ Right (fp, x)
    let (extraFiles, associatedFiles) = partitionEithers results
        targetMap =
            foldl unionTargets M.empty $
            map (\(_, (name, comp)) -> M.singleton name (TargetComps (S.singleton comp)))
                associatedFiles
        infoMap =
            foldl (M.unionWith S.union) M.empty $
            map (\(fp, (name, _)) -> M.singleton name (S.singleton fp))
                associatedFiles
    return (targetMap, infoMap, extraFiles)

getAllLocalTargets
    :: HasEnvConfig env
    => GhciOpts
    -> Map PackageName Target
    -> Maybe (Map PackageName Target)
    -> SourceMap
    -> RIO env [(PackageName, (Path Abs File, Target))]
getAllLocalTargets GhciOpts{..} targets0 mainIsTargets sourceMap = do
    -- Use the 'mainIsTargets' as normal targets, for CLI concision. See
    -- #1845. This is a little subtle - we need to do the target parsing
    -- independently in order to handle the case where no targets are
    -- specified.
    let targets = maybe targets0 (unionTargets targets0) mainIsTargets
    packages <- lpProject <$> getLocalPackages
    -- Find all of the packages that are directly demanded by the
    -- targets.
    directlyWanted <-
        forMaybeM (M.toList packages) $
        \(name, lpv) ->
                case M.lookup name targets of
                  Just simpleTargets -> return (Just (name, (lpvCabalFP lpv, simpleTargets)))
                  Nothing -> return Nothing
    -- Figure out
    let extraLoadDeps = getExtraLoadDeps ghciLoadLocalDeps sourceMap directlyWanted
    if (ghciSkipIntermediate && not ghciLoadLocalDeps) || null extraLoadDeps
        then return directlyWanted
        else do
            let extraList = T.intercalate ", " (map (packageNameText . fst) extraLoadDeps)
            if ghciLoadLocalDeps
                then logInfo $ T.concat
                    [ "The following libraries will also be loaded into GHCi because "
                    , "they are local dependencies of your targets, and you specified --load-local-deps:\n    "
                    , extraList
                    ]
                else logInfo $ T.concat
                    [ "The following libraries will also be loaded into GHCi because "
                    , "they are intermediate dependencies of your targets:\n    "
                    , extraList
                    , "\n(Use --skip-intermediate-deps to omit these)"
                    ]
            return (directlyWanted ++ extraLoadDeps)

getAllNonLocalTargets
    :: Map PackageName Target
    -> RIO env [PackageName]
getAllNonLocalTargets targets = do
  let isNonLocal (TargetAll Dependency) = True
      isNonLocal _ = False
  return $ map fst $ filter (isNonLocal . snd) (M.toList targets)

buildDepsAndInitialSteps :: HasEnvConfig env => GhciOpts -> [Text] -> RIO env ()
buildDepsAndInitialSteps GhciOpts{..} targets0 = do
    let targets = targets0 ++ map T.pack ghciAdditionalPackages
    -- If necessary, do the build, for local packagee targets, only do
    -- 'initialBuildSteps'.
    when (not ghciNoBuild && not (null targets)) $ do
        eres <- tryAny $ build (const (return ())) Nothing defaultBuildOptsCLI
            { boptsCLITargets = targets
            , boptsCLIInitialBuildSteps = True
            , boptsCLIFlags = ghciFlags
            , boptsCLIGhcOptions = ghciGhcOptions
            }
        case eres of
            Right () -> return ()
            Left err -> do
                prettyError $ fromString (show err)
                prettyWarn "Build failed, but trying to launch GHCi anyway"

checkAdditionalPackages :: MonadThrow m => [String] -> m [PackageName]
checkAdditionalPackages pkgs = forM pkgs $ \name -> do
    let mres = (packageIdentifierName <$> parsePackageIdentifierFromString name)
            <|> parsePackageNameFromString name
    maybe (throwM $ InvalidPackageOption name) return mres

runGhci
    :: HasEnvConfig env
    => GhciOpts
    -> [(PackageName, (Path Abs File, Target))]
    -> Maybe (Map PackageName Target)
    -> [GhciPkgInfo]
    -> [Path Abs File]
    -> [PackageName]
    -> RIO env ()
runGhci GhciOpts{..} targets mainIsTargets pkgs extraFiles exposePackages = do
    config <- view configL
    wc <- view $ actualCompilerVersionL.whichCompilerL
    let pkgopts = hidePkgOpts ++ genOpts ++ ghcOpts
        shouldHidePackages =
          fromMaybe (not (null pkgs && null exposePackages)) ghciHidePackages
        hidePkgOpts =
          if shouldHidePackages
            then "-hide-all-packages" :
              -- This is necessary, because current versions of ghci
              -- will entirely fail to start if base isn't visible. This
              -- is because it tries to use the interpreter to set
              -- buffering options on standard IO.
              "-package" : "base" :
              concatMap (\n -> ["-package", packageNameString n]) exposePackages
            else []
        oneWordOpts bio
            | shouldHidePackages = bioOneWordOpts bio ++ bioPackageFlags bio
            | otherwise = bioOneWordOpts bio
        genOpts = nubOrd (concatMap (concatMap (oneWordOpts . snd) . ghciPkgOpts) pkgs)
        (omittedOpts, ghcOpts) = partition badForGhci $
            concatMap (concatMap (bioOpts . snd) . ghciPkgOpts) pkgs ++ map T.unpack
              ( fold (configGhcOptionsByCat config) -- include everything, locals, and targets
             ++ concatMap (getUserOptions . ghciPkgName) pkgs
              )
        getUserOptions pkg = M.findWithDefault [] pkg (configGhcOptionsByName config)
        badForGhci x =
            isPrefixOf "-O" x || elem x (words "-debug -threaded -ticky -static -Werror")
    unless (null omittedOpts) $
        logWarn
            ("The following GHC options are incompatible with GHCi and have not been passed to it: " <>
             T.unwords (map T.pack (nubOrd omittedOpts)))
    oiDir <- view objectInterfaceDirL
    let odir =
            [ "-odir=" <> toFilePathNoTrailingSep oiDir
            , "-hidir=" <> toFilePathNoTrailingSep oiDir ]
    logInfo
        ("Configuring GHCi with the following packages: " <>
         T.intercalate ", " (map (packageNameText . ghciPkgName) pkgs))
    let execGhci extras = do
            menv <- liftIO $ configEnvOverride config defaultEnvSettings
            execSpawn menv
                 (fromMaybe (compilerExeName wc) ghciGhcCommand)
                 (("--interactive" : ) $
                 -- This initial "-i" resets the include directories to
                 -- not include CWD. If there aren't any packages, CWD
                 -- is included.
                  (if null pkgs then id else ("-i" : )) $
                  odir <> pkgopts <> extras <> map T.unpack ghciGhcOptions <> ghciArgs)
        -- TODO: Consider optimizing this check. Perhaps if no
        -- "with-ghc" is specified, assume that it is not using intero.
        checkIsIntero =
            -- Optimization dependent on the behavior of renderScript -
            -- it doesn't matter if it's intero or ghci when loading
            -- multiple packages.
            case pkgs of
                [_] -> do
                    menv <- liftIO $ configEnvOverride config defaultEnvSettings
                    output <- execObserve menv (fromMaybe (compilerExeName wc) ghciGhcCommand) ["--version"]
                    return $ "Intero" `isPrefixOf` output
                _ -> return False
    withSystemTempDir "ghci" $ \tmpDirectory -> do
        macrosOptions <- writeMacrosFile tmpDirectory pkgs
        if ghciNoLoadModules
            then execGhci macrosOptions
            else do
                checkForDuplicateModules pkgs
                isIntero <- checkIsIntero
                bopts <- view buildOptsL
                mainFile <- figureOutMainFile bopts mainIsTargets targets pkgs
                scriptPath <- writeGhciScript tmpDirectory (renderScript isIntero pkgs mainFile ghciOnlyMain extraFiles)
                execGhci (macrosOptions ++ ["-ghci-script=" <> toFilePath scriptPath])

writeMacrosFile :: (MonadIO m) => Path Abs Dir -> [GhciPkgInfo] -> m [String]
writeMacrosFile tmpDirectory packages = do
    preprocessCabalMacros packages macrosFile
  where
    macrosFile = tmpDirectory </> $(mkRelFile "cabal_macros.h")

writeGhciScript :: (MonadIO m) => Path Abs Dir -> GhciScript -> m (Path Abs File)
writeGhciScript tmpDirectory script = do
    liftIO $ scriptToFile scriptPath script
    setScriptPerms scriptFilePath
    return scriptPath
  where
    scriptPath = tmpDirectory </> $(mkRelFile "ghci-script")
    scriptFilePath = toFilePath scriptPath

renderScript :: Bool -> [GhciPkgInfo] -> Maybe (Path Abs File) -> Bool -> [Path Abs File] -> GhciScript
renderScript isIntero pkgs mainFile onlyMain extraFiles = do
    let cdPhase = case (isIntero, pkgs) of
          -- If only loading one package, set the cwd properly.
          -- Otherwise don't try. See
          -- https://github.com/commercialhaskell/stack/issues/3309
          (True, [pkg]) -> cmdCdGhc (ghciPkgDir pkg)
          _ -> mempty
        addPhase = cmdAdd $ S.fromList (map Left allModules ++ addMain)
        addMain = case mainFile of
            Just path -> [Right path]
            _ -> []
        modulePhase = cmdModule $ S.fromList allModules
        allModules = concatMap (S.toList . ghciPkgModules) pkgs
    case getFileTargets pkgs <> extraFiles of
        [] ->
          if onlyMain
            then cdPhase <> if isJust mainFile then cmdAdd (S.fromList addMain) else mempty
            else cdPhase <> addPhase <> modulePhase
        fileTargets -> cmdAdd (S.fromList (map Right fileTargets))

-- Hacky check if module / main phase should be omitted. This should be
-- improved if / when we have a better per-component load.
getFileTargets :: [GhciPkgInfo] -> [Path Abs File]
getFileTargets = concatMap (concatMap S.toList . maybeToList . ghciPkgTargetFiles)

-- | Figure out the main-is file to load based on the targets. Sometimes there
-- is none, sometimes it's unambiguous, sometimes it's
-- ambiguous. Warns and returns nothing if it's ambiguous.
figureOutMainFile
    :: HasRunner env
    => BuildOpts
    -> Maybe (Map PackageName Target)
    -> [(PackageName, (Path Abs File, Target))]
    -> [GhciPkgInfo]
    -> RIO env (Maybe (Path Abs File))
figureOutMainFile bopts mainIsTargets targets0 packages = do
    case candidates of
        [] -> return Nothing
        [c@(_,_,fp)] -> do logInfo ("Using main module: " <> renderCandidate c)
                           return (Just fp)
        candidate:_ -> do
          borderedWarning $ do
            logWarn "The main module to load is ambiguous. Candidates are: "
            forM_ (map renderCandidate candidates) logWarn
            logWarn
                "You can specify which one to pick by: "
            logWarn
                (" * Specifying targets to stack ghci e.g. stack ghci " <>
                 sampleTargetArg candidate)
            logWarn
                (" * Specifying what the main is e.g. stack ghci " <>
                 sampleMainIsArg candidate)
            logWarn
                (" * Choosing from the candidate above [1.." <>
                T.pack (show $ length candidates) <> "]")
          liftIO userOption
  where
    targets = fromMaybe (M.fromList $ map (\(k, (_, x)) -> (k, x)) targets0)
                        mainIsTargets
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
              ("Loading main module from candidate " <>
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

getGhciPkgInfos
    :: HasEnvConfig env
    => BuildOptsCLI
    -> SourceMap
    -> [PackageName]
    -> Maybe (Map PackageName (Set (Path Abs File)))
    -> [(PackageName, (Path Abs File, Target))]
    -> RIO env [GhciPkgInfo]
getGhciPkgInfos buildOptsCLI sourceMap addPkgs mfileTargets localTargets = do
    menv <- getMinimalEnvOverride
    (installedMap, _, _, _) <- getInstalled
        menv
        GetInstalledOpts
            { getInstalledProfiling = False
            , getInstalledHaddock   = False
            , getInstalledSymbols   = False
            }
        sourceMap
    let localLibs = [name | (name, (_, target)) <- localTargets, hasLocalComp isCLib target]
    forM localTargets $ \(name, (cabalfp, target)) ->
        makeGhciPkgInfo buildOptsCLI sourceMap installedMap localLibs addPkgs mfileTargets name cabalfp target

-- | Make information necessary to load the given package in GHCi.
makeGhciPkgInfo
    :: HasEnvConfig env
    => BuildOptsCLI
    -> SourceMap
    -> InstalledMap
    -> [PackageName]
    -> [PackageName]
    -> Maybe (Map PackageName (Set (Path Abs File)))
    -> PackageName
    -> Path Abs File
    -> Target
    -> RIO env GhciPkgInfo
makeGhciPkgInfo buildOptsCLI sourceMap installedMap locals addPkgs mfileTargets name cabalfp target = do
    bopts <- view buildOptsL
    econfig <- view envConfigL
    bconfig <- view buildConfigL
    compilerVersion <- view actualCompilerVersionL
    let config =
            PackageConfig
            { packageConfigEnableTests = True
            , packageConfigEnableBenchmarks = True
            , packageConfigFlags = getLocalFlags bconfig buildOptsCLI name
            , packageConfigGhcOptions = getGhcOptions bconfig buildOptsCLI name True True
            , packageConfigCompilerVersion = compilerVersion
            , packageConfigPlatform = view platformL econfig
            }
    -- TODO we've already parsed this information, otherwise we
    -- wouldn't have figured out the cabalfp already. In the future:
    -- retain that GenericPackageDescription in the relevant data
    -- structures to avoid reparsing.
    (gpkgdesc, _cabalfp) <- readPackageUnresolvedDir (parent cabalfp) True

    -- Source the package's *.buildinfo file created by configure if any. See
    -- https://www.haskell.org/cabal/users-guide/developing-packages.html#system-dependent-parameters
    buildinfofp <- parseRelFile (T.unpack (packageNameText name) ++ ".buildinfo")
    hasDotBuildinfo <- doesFileExist (parent cabalfp </> buildinfofp)
    let mbuildinfofp
          | hasDotBuildinfo = Just (parent cabalfp </> buildinfofp)
          | otherwise = Nothing
    mbuildinfo <- forM mbuildinfofp readDotBuildinfo
    let pdp = resolvePackageDescription config gpkgdesc
        pkg =
            packageFromPackageDescription config (C.genPackageFlags gpkgdesc) $
            maybe
              pdp
              (\bi ->
               let PackageDescriptionPair x y = pdp
                in PackageDescriptionPair
                    (C.updatePackageDescription bi x)
                    (C.updatePackageDescription bi y))
              mbuildinfo

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
        , ghciPkgTargetFiles = mfileTargets >>= M.lookup name
        , ghciPkgPackage = pkg
        }

-- NOTE: this should make the same choices as the components code in
-- 'loadLocalPackage'. Unfortunately for now we reiterate this logic
-- (differently).
wantedPackageComponents :: BuildOpts -> Target -> Package -> Set NamedComponent
wantedPackageComponents _ (TargetComps cs) _ = cs
wantedPackageComponents bopts (TargetAll ProjectPackage) pkg = S.fromList $
    (case packageLibraries pkg of
       NoLibraries -> []
       HasLibraries _names -> [CLib]) ++ -- FIXME. This ignores sub libraries and foreign libraries. Is that OK?
    map CExe (S.toList (packageExes pkg)) <>
    (if boptsTests bopts then map CTest (M.keys (packageTests pkg)) else []) <>
    (if boptsBenchmarks bopts then map CBench (S.toList (packageBenchmarks pkg)) else [])
wantedPackageComponents _ _ _ = S.empty

checkForIssues :: (MonadThrow m, MonadLogger m) => [GhciPkgInfo] -> m ()
checkForIssues pkgs = do
    unless (null issues) $ borderedWarning $ do
        logWarn "Warning: There are cabal settings for this project which may prevent GHCi from loading your code properly."
        logWarn "In some cases it can also load some projects which would otherwise fail to build."
        logWarn ""
        mapM_ logWarn $ intercalate [""] issues
        logWarn ""
        logWarn "To resolve, remove the flag(s) from the cabal file(s) and instead put them at the top of the haskell files."
        logWarn ""
        logWarn "It isn't yet possible to load multiple packages into GHCi in all cases - see"
        logWarn "https://ghc.haskell.org/trac/ghc/ticket/10827"
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
    logWarn ""
    logWarn "* * * * * * * *"
    x <- f
    logWarn "* * * * * * * *"
    logWarn ""
    return x

checkForDuplicateModules :: (MonadThrow m, MonadLogger m) => [GhciPkgInfo] -> m ()
checkForDuplicateModules pkgs = do
    unless (null duplicates) $ do
        borderedWarning $ do
            logWarn "The following modules are present in multiple packages:"
            forM_ duplicates $ \(mn, pns) -> do
                logWarn (" * " <> T.pack mn <> " (in " <> T.intercalate ", " (map packageNameText pns) <> ")")
        throwM LoadingDuplicateModules
  where
    duplicates, allModules :: [(String, [PackageName])]
    duplicates = filter (not . null . tail . snd) allModules
    allModules =
        M.toList $ M.fromListWith (++) $
        concatMap (\pkg -> map ((, [ghciPkgName pkg]) . C.display) (S.toList (ghciPkgModules pkg))) pkgs

targetWarnings
  :: HasRunner env
  => Path Abs File
  -> [(PackageName, (Path Abs File, Target))]
  -> [PackageName]
  -> Maybe (Map PackageName (Set (Path Abs File)), [Path Abs File])
  -> RIO env ()
targetWarnings stackYaml localTargets nonLocalTargets mfileTargets = do
  unless (null nonLocalTargets) $
    prettyWarnL
      [ flow "Some targets"
      , parens $ fillSep $ punctuate "," $ map (styleGood . display) nonLocalTargets
      , flow "are not local packages, and so cannot be directly loaded."
      , flow "In future versions of stack, this might be supported - see"
      , styleUrl "https://github.com/commercialhaskell/stack/issues/1441"
      , "."
      , flow "It can still be useful to specify these, as they will be passed to ghci via -package flags."
      ]
  when (null localTargets && isNothing mfileTargets) $
      prettyWarn $ vsep
          [ flow "No local targets specified, so ghci will not use any options from your package.yaml / *.cabal files."
          , ""
          , flow "Potential ways to resolve this:"
          , bulletedList
              [ fillSep
                  [ flow "If you want to use the package.yaml / *.cabal package in the current directory, use"
                  , styleShell "stack init"
                  , flow "to create a new stack.yaml."
                  ]
              , flow "Add to the 'packages' field of" <+> display stackYaml
              ]
          , ""
          ]

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
    -> [(PackageName, (Path Abs File, Target))]
    -> [(PackageName, (Path Abs File, Target))]
getExtraLoadDeps loadAllDeps sourceMap targets =
    M.toList $
    (\mp -> foldl' (flip M.delete) mp (map fst targets)) $
    M.mapMaybe id $
    execState (mapM_ (mapM_ go . getDeps . fst) targets)
              (M.fromList (map (second Just) targets))
  where
    getDeps :: PackageName -> [PackageName]
    getDeps name =
        case M.lookup name sourceMap of
            Just (PSFiles lp _) -> M.keys (packageDeps (lpPackage lp)) -- FIXME just Local?
            _ -> []
    go :: PackageName -> State (Map PackageName (Maybe (Path Abs File, Target))) Bool
    go name = do
        cache <- get
        case (M.lookup name cache, M.lookup name sourceMap) of
            (Just (Just _), _) -> return True
            (Just Nothing, _) | not loadAllDeps -> return False
            (_, Just (PSFiles lp _)) -> do
                let deps = M.keys (packageDeps (lpPackage lp))
                shouldLoad <- liftM or $ mapM go deps
                if shouldLoad
                    then do
                        modify (M.insert name (Just (lpCabalFile lp, TargetComps (S.singleton CLib))))
                        return True
                    else do
                        modify (M.insert name Nothing)
                        return False
            (_, Just PSIndex{}) -> return loadAllDeps
            (_, _) -> return False

preprocessCabalMacros :: MonadIO m => [GhciPkgInfo] -> Path Abs File -> m [String]
preprocessCabalMacros pkgs out = liftIO $ do
    let fps = nubOrd (concatMap (mapMaybe (bioCabalMacros . snd) . ghciPkgOpts) pkgs)
    files <- mapM (S8.readFile . toFilePath) fps
    if null files then return [] else do
        S8.writeFile (toFilePath out) $ S8.concat $ map (<> "\n#undef CURRENT_PACKAGE_KEY\n#undef CURRENT_COMPONENT_ID\n") files
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

unionTargets :: Ord k => Map k Target -> Map k Target -> Map k Target
unionTargets = M.unionWith $ \l r ->
    case (l, r) of
        (TargetAll Dependency, _) -> r
        (TargetComps sl, TargetComps sr) -> TargetComps (S.union sl sr)
        (TargetComps _, TargetAll ProjectPackage) -> TargetAll ProjectPackage
        (TargetComps _, _) -> l
        (TargetAll ProjectPackage, _) -> TargetAll ProjectPackage

hasLocalComp :: (NamedComponent -> Bool) -> Target -> Bool
hasLocalComp p t =
    case t of
        TargetComps s -> any p (S.toList s)
        TargetAll ProjectPackage -> True
        _ -> False


{- Copied from Stack.Ide, may be useful in the future

-- | Get options and target files for the given package info.
getPackageOptsAndTargetFiles
    :: (MonadThrow m, MonadIO m, MonadReader env m, HasEnvConfig env)
    => Path Abs Dir -> GhciPkgInfo -> m ([FilePath], [FilePath])
getPackageOptsAndTargetFiles pwd pkg = do
    dist <- distDirFromDir (ghciPkgDir pkg)
    let autogen = autogenDir dist
    paths_foo <-
        liftM
            (autogen </>)
            (parseRelFile
                 ("Paths_" ++ packageNameString (ghciPkgName pkg) ++ ".hs"))
    paths_foo_exists <- doesFileExist paths_foo
    let ghcOptions bio =
            bioOneWordOpts bio ++
            bioOpts bio ++
            bioPackageFlags bio ++
            maybe [] (\cabalMacros -> ["-optP-include", "-optP" <> toFilePath cabalMacros]) (bioCabalMacros bio)
    return
        ( ("--dist-dir=" <> toFilePathNoTrailingSep dist) :
          -- FIXME: use compilerOptionsCabalFlag
          map ("--ghc-option=" ++) (concatMap (ghcOptions . snd) (ghciPkgOpts pkg))
        , mapMaybe
              (fmap toFilePath . stripProperPrefix pwd)
              (S.toList (ghciPkgCFiles pkg) <> S.toList (ghciPkgModFiles pkg) <>
               [paths_foo | paths_foo_exists]))

-- | List load targets for a package target.
targetsCmd :: Text -> GlobalOpts -> IO ()
targetsCmd target go@GlobalOpts{..} =
    withBuildConfig go $
    do let boptsCli = defaultBuildOptsCLI { boptsCLITargets = [target] }
       (_realTargets,_,pkgs) <- ghciSetup (ideGhciOpts boptsCli)
       pwd <- getCurrentDir
       targets <-
           fmap
               (concat . snd . unzip)
               (mapM (getPackageOptsAndTargetFiles pwd) pkgs)
       forM_ targets (liftIO . putStrLn)
-}
