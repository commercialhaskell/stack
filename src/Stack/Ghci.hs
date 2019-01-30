{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Run a GHCi configured with the user's package(s).

module Stack.Ghci
    ( GhciOpts(..)
    , GhciPkgInfo(..)
    , GhciException(..)
    , ghci
    ) where

import           Stack.Prelude hiding (Display (..))
import           Control.Monad.State.Strict (State, execState, get, modify)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as LBS
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Distribution.PackageDescription as C
import           Path
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.IO hiding (withSystemTempDir)
import qualified RIO
import           RIO.PrettyPrint
import           RIO.Process (HasProcessContext, exec, proc, readProcess_)
import           Stack.Build
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Build.Target
import           Stack.Constants
import           Stack.Constants.Config
import           Stack.Ghci.Script
import           Stack.Package
import           Stack.Setup (withNewLocalBuildTargets)
import           Stack.Types.Build
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.NamedComponent
import           Stack.Types.Package
import           Stack.Types.Runner
import           Stack.Types.SourceMap
import           System.IO (putStrLn)
import           System.IO.Temp (getCanonicalTemporaryDirectory)
import           System.Permissions (setScriptPerms)

-- | Command-line options for GHC.
data GhciOpts = GhciOpts
    { ghciTargets            :: ![Text]
    , ghciArgs               :: ![String]
    , ghciGhcOptions         :: ![Text]
    , ghciFlags              :: !(Map ApplyCLIFlag (Map FlagName Bool))
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
--
-- NOTE: GhciPkgInfo has paths as list instead of a Set to preserve files order
-- as a workaround for bug https://ghc.haskell.org/trac/ghc/ticket/13786
data GhciPkgInfo = GhciPkgInfo
    { ghciPkgName :: !PackageName
    , ghciPkgOpts :: ![(NamedComponent, BuildInfoOpts)]
    , ghciPkgDir :: !(Path Abs Dir)
    , ghciPkgModules :: !ModuleMap
    , ghciPkgCFiles :: ![Path Abs File] -- ^ C files.
    , ghciPkgMainIs :: !(Map NamedComponent [Path Abs File])
    , ghciPkgTargetFiles :: !(Maybe [Path Abs File])
    , ghciPkgPackage :: !Package
    } deriving Show

-- | Loaded package description and related info.
data GhciPkgDesc = GhciPkgDesc
    { ghciDescPkg :: !Package
    , ghciDescCabalFp :: !(Path Abs File)
    , ghciDescTarget :: !Target
    }

-- Mapping from a module name to a map with all of the paths that use
-- that name. Each of those paths is associated with a set of components
-- that contain it. Purpose of this complex structure is for use in
-- 'checkForDuplicateModules'.
type ModuleMap = Map ModuleName (Map (Path Abs File) (Set (PackageName, NamedComponent)))

unionModuleMaps :: [ModuleMap] -> ModuleMap
unionModuleMaps = M.unionsWith (M.unionWith S.union)

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
    sourceMap <- view $ envConfigL.to envConfigSourceMap
    installMap <- toInstallMap sourceMap
    locals <- projectLocalPackages
    depLocals <- localDependencies
    let localMap =
          M.fromList [(packageName $ lpPackage lp, lp) | lp <- locals ++ depLocals]
        -- FIXME:qrilka this looks wrong to go back to SMActual
        sma = SMActual
             { smaCompiler = smCompiler sourceMap
             , smaProject = smProject sourceMap
             , smaDeps = smDeps sourceMap
             , smaGlobal = smGlobal sourceMap
             }
    -- Parse --main-is argument.
    mainIsTargets <- parseMainIsTargets buildOptsCLI sma ghciMainIs
    -- Parse to either file targets or build targets
    etargets <- preprocessTargets buildOptsCLI sma ghciTargets
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
    localTargets <- getAllLocalTargets opts inputTargets mainIsTargets localMap
    -- Get a list of all the non-local target packages.
    nonLocalTargets <- getAllNonLocalTargets inputTargets
    -- Check if additional package arguments are sensible.
    addPkgs <- checkAdditionalPackages ghciAdditionalPackages
    -- Load package descriptions.
    pkgDescs <- loadGhciPkgDescs buildOptsCLI localTargets
    -- If necessary, ask user about which main module to load.
    bopts <- view buildOptsL
    mainFile <-
        if ghciNoLoadModules
            then return Nothing
            else do
              -- Figure out package files, in order to ask the user
              -- about which main module to load. See the note below for
              -- why this is done again after the build. This could
              -- potentially be done more efficiently, because all we
              -- need is the location of main modules, not the rest.
              pkgs0 <- getGhciPkgInfos installMap addPkgs (fmap fst mfileTargets) pkgDescs
              figureOutMainFile bopts mainIsTargets localTargets pkgs0
    -- Build required dependencies and setup local packages.
    stackYaml <- view stackYamlL
    buildDepsAndInitialSteps opts (map (T.pack . packageNameString . fst) localTargets)
    targetWarnings stackYaml localTargets nonLocalTargets mfileTargets
    -- Load the list of modules _after_ building, to catch changes in
    -- unlisted dependencies (#1180)
    pkgs <- getGhciPkgInfos installMap addPkgs (fmap fst mfileTargets) pkgDescs
    checkForIssues pkgs
    -- Finally, do the invocation of ghci
    runGhci opts localTargets mainFile pkgs (maybe [] snd mfileTargets) (nonLocalTargets ++ addPkgs)

preprocessTargets
    :: HasEnvConfig env
    => BuildOptsCLI
    -> SMActual
    -> [Text]
    -> RIO env (Either [Path Abs File] (Map PackageName Target))
preprocessTargets buildOptsCLI sma rawTargets = do
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
            let boptsCLI = buildOptsCLI { boptsCLITargets = normalTargetsRaw }
            normalTargets <- parseTargets AllowNoTargets False boptsCLI sma
                `catch` \ex -> case ex of
                    TargetParseException xs -> throwM (GhciTargetParseException xs)
                    _ -> throwM ex
            unless (null fileTargetsRaw) $ throwM Can'tSpecifyFilesAndTargets
            return (Right $ smtTargets normalTargets)

parseMainIsTargets
     :: HasEnvConfig env
     => BuildOptsCLI
     -> SMActual
     -> Maybe Text
     -> RIO env (Maybe (Map PackageName Target))
parseMainIsTargets buildOptsCLI sma mtarget = forM mtarget $ \target -> do
     let boptsCLI = buildOptsCLI { boptsCLITargets = [target] }
     targets <- parseTargets AllowNoTargets False boptsCLI sma
     return $ smtTargets targets

-- | Display PackageName + NamedComponent
displayPkgComponent :: (PackageName, NamedComponent) -> StyleDoc
displayPkgComponent = style PkgComponent . fromString . T.unpack . renderPkgComponent

findFileTargets
    :: HasEnvConfig env
    => [LocalPackage]
    -> [Path Abs File]
    -> RIO env (Map PackageName Target, Map PackageName [Path Abs File], [Path Abs File])
findFileTargets locals fileTargets = do
    filePackages <- forM locals $ \lp -> do
        (_,compFiles,_,_) <- getPackageFiles (packageFiles (lpPackage lp)) (lpCabalFile lp)
        return (lp, M.map (map dotCabalGetPath) compFiles)
    let foundFileTargetComponents :: [(Path Abs File, [(PackageName, NamedComponent)])]
        foundFileTargetComponents =
            map (\fp -> (fp, ) $ sort $
                        concatMap (\(lp, files) -> map ((packageName (lpPackage lp), ) . fst)
                                                       (filter (elem fp . snd) (M.toList files))
                                  ) filePackages
                ) fileTargets
    results <- forM foundFileTargetComponents $ \(fp, xs) ->
        case xs of
            [] -> do
                prettyWarn $ vsep
                    [ "Couldn't find a component for file target" <+>
                      pretty fp <>
                      ". This means that the correct ghc options might not be used."
                    , "Attempting to load the file anyway."
                    ]
                return $ Left fp
            [x] -> do
                prettyInfo $
                    "Using configuration for" <+> displayPkgComponent x <+>
                    "to load" <+> pretty fp
                return $ Right (fp, x)
            (x:_) -> do
                prettyWarn $
                    "Multiple components contain file target" <+>
                    pretty fp <> ":" <+>
                    mconcat (intersperse ", " (map displayPkgComponent xs)) <> line <>
                    "Guessing the first one," <+> displayPkgComponent x <> "."
                return $ Right (fp, x)
    let (extraFiles, associatedFiles) = partitionEithers results
        targetMap =
            foldl unionTargets M.empty $
            map (\(_, (name, comp)) -> M.singleton name (TargetComps (S.singleton comp)))
                associatedFiles
        infoMap =
            foldl (M.unionWith (<>)) M.empty $
            map (\(fp, (name, _)) -> M.singleton name [fp])
                associatedFiles
    return (targetMap, infoMap, extraFiles)

getAllLocalTargets
    :: HasEnvConfig env
    => GhciOpts
    -> Map PackageName Target
    -> Maybe (Map PackageName Target)
    -> Map PackageName LocalPackage
    -> RIO env [(PackageName, (Path Abs File, Target))]
getAllLocalTargets GhciOpts{..} targets0 mainIsTargets localMap = do
    -- Use the 'mainIsTargets' as normal targets, for CLI concision. See
    -- #1845. This is a little subtle - we need to do the target parsing
    -- independently in order to handle the case where no targets are
    -- specified.
    let targets = maybe targets0 (unionTargets targets0) mainIsTargets
    packages <- view $ envConfigL.to envConfigSourceMap.to smProject
    -- Find all of the packages that are directly demanded by the
    -- targets.
    let directlyWanted = flip mapMaybe (M.toList packages) $
          \(name, pp) ->
                case M.lookup name targets of
                  Just simpleTargets -> Just (name, (ppCabalFP pp, simpleTargets))
                  Nothing -> Nothing
    -- Figure out
    let extraLoadDeps = getExtraLoadDeps ghciLoadLocalDeps localMap directlyWanted
    if (ghciSkipIntermediate && not ghciLoadLocalDeps) || null extraLoadDeps
        then return directlyWanted
        else do
            let extraList =
                  mconcat $ intersperse ", " (map (fromString . packageNameString . fst) extraLoadDeps)
            if ghciLoadLocalDeps
                then logInfo $
                  "The following libraries will also be loaded into GHCi because " <>
                  "they are local dependencies of your targets, and you specified --load-local-deps:\n    " <>
                  extraList
                else logInfo $
                  "The following libraries will also be loaded into GHCi because " <>
                  "they are intermediate dependencies of your targets:\n    " <>
                  extraList <>
                  "\n(Use --skip-intermediate-deps to omit these)"
            return (directlyWanted ++ extraLoadDeps)

getAllNonLocalTargets
    :: Map PackageName Target
    -> RIO env [PackageName]
getAllNonLocalTargets targets = do
  let isNonLocal (TargetAll PTDependency) = True
      isNonLocal _ = False
  return $ map fst $ filter (isNonLocal . snd) (M.toList targets)

buildDepsAndInitialSteps :: HasEnvConfig env => GhciOpts -> [Text] -> RIO env ()
buildDepsAndInitialSteps GhciOpts{..} localTargets = do
    let targets = localTargets ++ map T.pack ghciAdditionalPackages
    -- If necessary, do the build, for local packagee targets, only do
    -- 'initialBuildSteps'.
    when (not ghciNoBuild && not (null targets)) $ do
        -- only new local targets could appear here
        eres <- tryAny $ withNewLocalBuildTargets targets $ build Nothing Nothing
        case eres of
            Right () -> return ()
            Left err -> do
                prettyError $ fromString (show err)
                prettyWarn "Build failed, but trying to launch GHCi anyway"

checkAdditionalPackages :: MonadThrow m => [String] -> m [PackageName]
checkAdditionalPackages pkgs = forM pkgs $ \name -> do
    let mres = (pkgName <$> parsePackageIdentifier name)
            <|> parsePackageNameThrowing name
    maybe (throwM $ InvalidPackageOption name) return mres

runGhci
    :: HasEnvConfig env
    => GhciOpts
    -> [(PackageName, (Path Abs File, Target))]
    -> Maybe (Path Abs File)
    -> [GhciPkgInfo]
    -> [Path Abs File]
    -> [PackageName]
    -> RIO env ()
runGhci GhciOpts{..} targets mainFile pkgs extraFiles exposePackages = do
    config <- view configL
    wc <- view $ actualCompilerVersionL.whichCompilerL
    let pkgopts = hidePkgOpts ++ genOpts ++ ghcOpts
        shouldHidePackages =
          fromMaybe (not (null pkgs && null exposePackages)) ghciHidePackages
        hidePkgOpts =
          if shouldHidePackages
            then
              ["-hide-all-packages"] ++
              -- This is necessary, because current versions of ghci
              -- will entirely fail to start if base isn't visible. This
              -- is because it tries to use the interpreter to set
              -- buffering options on standard IO.
              (if null targets then ["-package", "base"] else []) ++
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
             mconcat (intersperse " " (fromString <$> nubOrd omittedOpts)))
    oiDir <- view objectInterfaceDirL
    let odir =
            [ "-odir=" <> toFilePathNoTrailingSep oiDir
            , "-hidir=" <> toFilePathNoTrailingSep oiDir ]
    logInfo $
      "Configuring GHCi with the following packages: " <>
      mconcat (intersperse ", " (map (fromString . packageNameString . ghciPkgName) pkgs))
    let execGhci extras = do
            menv <- liftIO $ configProcessContextSettings config defaultEnvSettings
            withProcessContext menv $ exec
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
                    menv <- liftIO $ configProcessContextSettings config defaultEnvSettings
                    output <- withProcessContext menv
                            $ runGrabFirstLine (fromMaybe (compilerExeName wc) ghciGhcCommand) ["--version"]
                    return $ "Intero" `isPrefixOf` output
                _ -> return False
    -- Since usage of 'exec' does not return, we cannot do any cleanup
    -- on ghci exit. So, instead leave the generated files. To make this
    -- more efficient and avoid gratuitous generation of garbage, the
    -- file names are determined by hashing. This also has the nice side
    -- effect of making it possible to copy the ghci invocation out of
    -- the log and have it still work.
    tmpDirectory <-
        (</> relDirHaskellStackGhci) <$>
        (parseAbsDir =<< liftIO getCanonicalTemporaryDirectory)
    ghciDir <- view ghciDirL
    ensureDir ghciDir
    ensureDir tmpDirectory
    macrosOptions <- writeMacrosFile ghciDir pkgs
    if ghciNoLoadModules
        then execGhci macrosOptions
        else do
            checkForDuplicateModules pkgs
            isIntero <- checkIsIntero
            scriptOptions <- writeGhciScript tmpDirectory (renderScript isIntero pkgs mainFile ghciOnlyMain extraFiles)
            execGhci (macrosOptions ++ scriptOptions)

writeMacrosFile :: HasTerm env => Path Abs Dir -> [GhciPkgInfo] -> RIO env [String]
writeMacrosFile outputDirectory pkgs = do
    fps <- fmap (nubOrd . catMaybes . concat) $
        forM pkgs $ \pkg -> forM (ghciPkgOpts pkg) $ \(_, bio) -> do
            let cabalMacros = bioCabalMacros bio
            exists <- liftIO $ doesFileExist cabalMacros
            if exists
                then return $ Just cabalMacros
                else do
                    prettyWarnL ["Didn't find expected autogen file:", pretty cabalMacros]
                    return Nothing
    files <- liftIO $ mapM (S8.readFile . toFilePath) fps
    if null files then return [] else do
        out <- liftIO $ writeHashedFile outputDirectory relFileCabalMacrosH $
            S8.concat $ map (<> "\n#undef CURRENT_PACKAGE_KEY\n#undef CURRENT_COMPONENT_ID\n") files
        return ["-optP-include", "-optP" <> toFilePath out]

writeGhciScript :: (MonadIO m) => Path Abs Dir -> GhciScript -> m [String]
writeGhciScript outputDirectory script = do
    scriptPath <- liftIO $ writeHashedFile outputDirectory relFileGhciScript $
        LBS.toStrict $ scriptToLazyByteString script
    let scriptFilePath = toFilePath scriptPath
    setScriptPerms scriptFilePath
    return ["-ghci-script=" <> scriptFilePath]

writeHashedFile :: Path Abs Dir -> Path Rel File -> ByteString -> IO (Path Abs File)
writeHashedFile outputDirectory relFile contents = do
    relSha <- shaPathForBytes contents
    let outDir = outputDirectory </> relSha
        outFile = outDir </> relFile
    alreadyExists <- doesFileExist outFile
    unless alreadyExists $ do
        ensureDir outDir
        S8.writeFile (toFilePath outFile) contents
    return outFile

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
        allModules = nubOrd $ concatMap (M.keys . ghciPkgModules) pkgs
    case getFileTargets pkgs <> extraFiles of
        [] ->
          if onlyMain
            then cdPhase <> if isJust mainFile then cmdAdd (S.fromList addMain) else mempty
            else cdPhase <> addPhase <> modulePhase
        fileTargets -> cmdAdd (S.fromList (map Right fileTargets))

-- Hacky check if module / main phase should be omitted. This should be
-- improved if / when we have a better per-component load.
getFileTargets :: [GhciPkgInfo] -> [Path Abs File]
getFileTargets = concatMap (concat . maybeToList . ghciPkgTargetFiles)

-- | Figure out the main-is file to load based on the targets. Asks the
-- user for input if there is more than one candidate main-is file.
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
        [c@(_,_,fp)] -> do logInfo ("Using main module: " <> RIO.display (renderCandidate c))
                           return (Just fp)
        candidate:_ -> do
          borderedWarning $ do
            logWarn "The main module to load is ambiguous. Candidates are: "
            forM_ (map renderCandidate candidates) (logWarn . RIO.display)
            logWarn
                "You can specify which one to pick by: "
            logWarn
                (" * Specifying targets to stack ghci e.g. stack ghci " <>
                RIO.display ( sampleTargetArg candidate))
            logWarn
                (" * Specifying what the main is e.g. stack ghci " <>
                 RIO.display (sampleMainIsArg candidate))
            logWarn
                (" * Choosing from the candidate above [1.." <>
                RIO.display (length candidates) <> "]")
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
                main <- mains
                return (ghciPkgName pkg, component, main)
              where
                wantedComponents =
                    wantedPackageComponents bopts target (ghciPkgPackage pkg)
    renderCandidate c@(pkgName,namedComponent,mainIs) =
        let candidateIndex = T.pack . show . (+1) . fromMaybe 0 . elemIndex c
        in  candidateIndex candidates <> ". Package `" <>
            T.pack (packageNameString pkgName) <>
            "' component " <>
            renderComp namedComponent <>
            " with main-is file: " <>
            T.pack (toFilePath mainIs)
    candidateIndices = take (length candidates) [1 :: Int ..]
    userOption = do
      option <- prompt "Specify main module to use (press enter to load none): "
      let selected = fromMaybe
                      ((+1) $ length candidateIndices)
                      (readMaybe (T.unpack option) :: Maybe Int)
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
            CInternalLib name -> "internal-lib:" <> name
            CExe name -> "exe:" <> name
            CTest name -> "test:" <> name
            CBench name -> "bench:" <> name
    sampleTargetArg (pkg,comp,_) =
        T.pack (packageNameString pkg) <> ":" <> renderComp comp
    sampleMainIsArg (pkg,comp,_) =
        "--main-is " <> T.pack (packageNameString pkg) <> ":" <> renderComp comp

loadGhciPkgDescs
    :: HasEnvConfig env
    => BuildOptsCLI
    -> [(PackageName, (Path Abs File, Target))]
    -> RIO env [GhciPkgDesc]
loadGhciPkgDescs buildOptsCLI localTargets =
    forM localTargets $ \(name, (cabalfp, target)) ->
        loadGhciPkgDesc buildOptsCLI name cabalfp target

-- | Load package description information for a ghci target.
loadGhciPkgDesc
    :: HasEnvConfig env
    => BuildOptsCLI
    -> PackageName
    -> Path Abs File
    -> Target
    -> RIO env GhciPkgDesc
loadGhciPkgDesc buildOptsCLI name cabalfp target = do
    econfig <- view envConfigL
    compilerVersion <- view actualCompilerVersionL
    let SourceMap{..} = envConfigSourceMap econfig
        -- Currently this source map is being build with
        -- the default targets
        sourceMapGhcOptions = fromMaybe [] $
          (cpGhcOptions . ppCommon <$> M.lookup name smProject)
          <|>
          (cpGhcOptions . dpCommon <$> M.lookup name smDeps)
        config =
            PackageConfig
            { packageConfigEnableTests = True
            , packageConfigEnableBenchmarks = True
            , packageConfigFlags = getLocalFlags buildOptsCLI name
            , packageConfigGhcOptions = sourceMapGhcOptions
            , packageConfigCompilerVersion = compilerVersion
            , packageConfigPlatform = view platformL econfig
            }
    -- TODO we've already parsed this information, otherwise we
    -- wouldn't have figured out the cabalfp already. In the future:
    -- retain that GenericPackageDescription in the relevant data
    -- structures to avoid reparsing.
    (gpdio, _name, _cabalfp) <- loadCabalFilePath (parent cabalfp)
    gpkgdesc <- liftIO $ gpdio YesPrintWarnings

    -- Source the package's *.buildinfo file created by configure if any. See
    -- https://www.haskell.org/cabal/users-guide/developing-packages.html#system-dependent-parameters
    buildinfofp <- parseRelFile (packageNameString name ++ ".buildinfo")
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
    return GhciPkgDesc
      { ghciDescPkg = pkg
      , ghciDescCabalFp = cabalfp
      , ghciDescTarget = target
      }

getGhciPkgInfos
    :: HasEnvConfig env
    => InstallMap
    -> [PackageName]
    -> Maybe (Map PackageName [Path Abs File])
    -> [GhciPkgDesc]
    -> RIO env [GhciPkgInfo]
getGhciPkgInfos installMap addPkgs mfileTargets localTargets = do
    (installedMap, _, _, _) <- getInstalled
        GetInstalledOpts
            { getInstalledProfiling = False
            , getInstalledHaddock   = False
            , getInstalledSymbols   = False
            }
        installMap
    let localLibs =
            [ packageName (ghciDescPkg desc)
            | desc <- localTargets
            , hasLocalComp isCLib (ghciDescTarget desc)
            ]
    forM localTargets $ \pkgDesc ->
      makeGhciPkgInfo installMap installedMap localLibs addPkgs mfileTargets pkgDesc

-- | Make information necessary to load the given package in GHCi.
makeGhciPkgInfo
    :: HasEnvConfig env
    => InstallMap
    -> InstalledMap
    -> [PackageName]
    -> [PackageName]
    -> Maybe (Map PackageName [Path Abs File])
    -> GhciPkgDesc
    -> RIO env GhciPkgInfo
makeGhciPkgInfo installMap installedMap locals addPkgs mfileTargets pkgDesc = do
    bopts <- view buildOptsL
    let pkg = ghciDescPkg pkgDesc
        cabalfp = ghciDescCabalFp pkgDesc
        target = ghciDescTarget pkgDesc
        name = packageName pkg
    (mods,files,opts) <- getPackageOpts (packageOpts pkg) installMap installedMap locals addPkgs cabalfp
    let filteredOpts = filterWanted opts
        filterWanted = M.filterWithKey (\k _ -> k `S.member` allWanted)
        allWanted = wantedPackageComponents bopts target pkg
    return
        GhciPkgInfo
        { ghciPkgName = name
        , ghciPkgOpts = M.toList filteredOpts
        , ghciPkgDir = parent cabalfp
        , ghciPkgModules = unionModuleMaps $
          map (\(comp, mp) -> M.map (\fp -> M.singleton fp (S.singleton (packageName pkg, comp))) mp)
              (M.toList (filterWanted mods))
        , ghciPkgMainIs = M.map (mapMaybe dotCabalMainPath) files
        , ghciPkgCFiles = mconcat (M.elems (filterWanted (M.map (mapMaybe dotCabalCFilePath) files)))
        , ghciPkgTargetFiles = mfileTargets >>= M.lookup name
        , ghciPkgPackage = pkg
        }

-- NOTE: this should make the same choices as the components code in
-- 'loadLocalPackage'. Unfortunately for now we reiterate this logic
-- (differently).
wantedPackageComponents :: BuildOpts -> Target -> Package -> Set NamedComponent
wantedPackageComponents _ (TargetComps cs) _ = cs
wantedPackageComponents bopts (TargetAll PTProject) pkg = S.fromList $
    (case packageLibraries pkg of
       NoLibraries -> []
       HasLibraries names -> CLib : map CInternalLib (S.toList names)) ++
    map CExe (S.toList (packageExes pkg)) <>
    map CInternalLib (S.toList $ packageInternalLibraries pkg) <>
    (if boptsTests bopts then map CTest (M.keys (packageTests pkg)) else []) <>
    (if boptsBenchmarks bopts then map CBench (S.toList (packageBenchmarks pkg)) else [])
wantedPackageComponents _ _ _ = S.empty

checkForIssues :: HasLogFunc env => [GhciPkgInfo] -> RIO env ()
checkForIssues pkgs = do
    unless (null issues) $ borderedWarning $ do
        logWarn "Warning: There are cabal settings for this project which may prevent GHCi from loading your code properly."
        logWarn "In some cases it can also load some projects which would otherwise fail to build."
        logWarn ""
        mapM_ (logWarn . RIO.display) $ intercalate [""] issues
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

borderedWarning :: HasLogFunc env => RIO env a -> RIO env a
borderedWarning f = do
    logWarn ""
    logWarn "* * * * * * * *"
    x <- f
    logWarn "* * * * * * * *"
    logWarn ""
    return x

-- TODO: Should this also tell the user the filepaths, not just the
-- module name?
checkForDuplicateModules :: HasTerm env => [GhciPkgInfo] -> RIO env ()
checkForDuplicateModules pkgs = do
    unless (null duplicates) $ do
        borderedWarning $ do
            prettyError $ "Multiple files use the same module name:" <>
              line <> bulletedList (map prettyDuplicate duplicates)
        throwM LoadingDuplicateModules
  where
    duplicates :: [(ModuleName, Map (Path Abs File) (Set (PackageName, NamedComponent)))]
    duplicates =
      filter (\(_, mp) -> M.size mp > 1) $
      M.toList $
      unionModuleMaps (map ghciPkgModules pkgs)
    prettyDuplicate :: (ModuleName, Map (Path Abs File) (Set (PackageName, NamedComponent))) -> StyleDoc
    prettyDuplicate (mn, mp) =
      style Error (pretty mn) <+> "found at the following paths" <> line <>
      bulletedList (map fileDuplicate (M.toList mp))
    fileDuplicate :: (Path Abs File, Set (PackageName, NamedComponent)) -> StyleDoc
    fileDuplicate (fp, comps) =
      pretty fp <+> parens (fillSep (punctuate "," (map displayPkgComponent (S.toList comps))))

targetWarnings
  :: HasTerm env
  => Path Abs File
  -> [(PackageName, (Path Abs File, Target))]
  -> [PackageName]
  -> Maybe (Map PackageName [Path Abs File], [Path Abs File])
  -> RIO env ()
targetWarnings stackYaml localTargets nonLocalTargets mfileTargets = do
  unless (null nonLocalTargets) $
    prettyWarnL
      [ flow "Some targets"
      , parens $ fillSep $ punctuate "," $ map (style Good . fromString . packageNameString) nonLocalTargets
      , flow "are not local packages, and so cannot be directly loaded."
      , flow "In future versions of stack, this might be supported - see"
      , style Url "https://github.com/commercialhaskell/stack/issues/1441"
      , "."
      , flow "It can still be useful to specify these, as they will be passed to ghci via -package flags."
      ]
  when (null localTargets && isNothing mfileTargets) $
      prettyNote $ vsep
          [ flow "No local targets specified, so a plain ghci will be started with no package hiding or package options."
          , ""
          , flow "If you want to use package hiding and options, then you can try one of the following:"
          , ""
          , bulletedList
              [ fillSep
                  [ flow "If you want to start a different project configuration than" <+> pretty stackYaml <> ", then you can use"
                  , style Shell "stack init"
                  , flow "to create a new stack.yaml for the packages in the current directory."
                  , line
                  ]
              , flow "If you want to use the project configuration at" <+> pretty stackYaml <> ", then you can add to its 'packages' field."
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
    -> Map PackageName LocalPackage
    -> [(PackageName, (Path Abs File, Target))]
    -> [(PackageName, (Path Abs File, Target))]
getExtraLoadDeps loadAllDeps localMap targets =
    M.toList $
    (\mp -> foldl' (flip M.delete) mp (map fst targets)) $
    M.mapMaybe id $
    execState (mapM_ (mapM_ go . getDeps . fst) targets)
              (M.fromList (map (second Just) targets))
  where
    getDeps :: PackageName -> [PackageName]
    getDeps name =
        case M.lookup name localMap of
            Just lp -> M.keys (packageDeps (lpPackage lp)) -- FIXME just Local?
            _ -> []
    go :: PackageName -> State (Map PackageName (Maybe (Path Abs File, Target))) Bool
    go name = do
        cache <- get
        case (M.lookup name cache, M.lookup name localMap) of
            (Just (Just _), _) -> return True
            (Just Nothing, _) | not loadAllDeps -> return False
            (_, Just lp) -> do
                let deps = M.keys (packageDeps (lpPackage lp))
                shouldLoad <- liftM or $ mapM go deps
                if shouldLoad
                    then do
                        modify (M.insert name (Just (lpCabalFile lp, TargetComps (S.singleton CLib))))
                        return True
                    else do
                        modify (M.insert name Nothing)
                        return False
            (_, _) -> return False

unionTargets :: Ord k => Map k Target -> Map k Target -> Map k Target
unionTargets = M.unionWith $ \l r ->
    case (l, r) of
        (TargetAll PTDependency, _) -> r
        (TargetComps sl, TargetComps sr) -> TargetComps (S.union sl sr)
        (TargetComps _, TargetAll PTProject) -> TargetAll PTProject
        (TargetComps _, _) -> l
        (TargetAll PTProject, _) -> TargetAll PTProject

hasLocalComp :: (NamedComponent -> Bool) -> Target -> Bool
hasLocalComp p t =
    case t of
        TargetComps s -> any p (S.toList s)
        TargetAll PTProject -> True
        _ -> False

-- | Run a command and grab the first line of stdout, dropping
-- stderr's contexts completely.
runGrabFirstLine :: (HasProcessContext env, HasLogFunc env) => String -> [String] -> RIO env String
runGrabFirstLine cmd0 args =
  proc cmd0 args $ \pc -> do
    (out, _err) <- readProcess_ pc
    return
      $ TL.unpack
      $ TL.filter (/= '\r')
      $ TL.concat
      $ take 1
      $ TL.lines
      $ TLE.decodeUtf8With lenientDecode out
