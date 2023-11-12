{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Types and functions related to Stack's @ghci@ and @repl@ commands.
module Stack.Ghci
  ( GhciOpts (..)
  , GhciPkgInfo (..)
  , GhciException (..)
  , GhciPrettyException (..)
  , ghciCmd
  , ghci
  ) where

import           Control.Monad.State.Strict ( State, execState, get, modify )
import           Data.ByteString.Builder ( byteString )
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import           Data.List.Extra ( (!?) )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Distribution.PackageDescription as C
import           Path ((</>), parent, parseRelFile )
import           Path.Extra ( forgivingResolveFile', toFilePathNoTrailingSep )
import           Path.IO
                   ( XdgDirectory (..), doesFileExist, ensureDir, getXdgDir )
import           RIO.NonEmpty ( nonEmpty )
import           RIO.Process
                   ( HasProcessContext, exec, proc, readProcess_
                   , withWorkingDir
                   )
import           Stack.Build ( buildLocalTargets )
import           Stack.Build.Installed ( getInstalled, toInstallMap )
import           Stack.Build.Source
                   ( getLocalFlags, localDependencies, projectLocalPackages )
import           Stack.Build.Target ( NeedTargets (..), parseTargets )
import           Stack.Constants
                   ( relDirGhciScript, relDirStackProgName, relFileCabalMacrosH
                   , relFileGhciScript, stackProgName'
                   )
import           Stack.Constants.Config ( ghciDirL, objectInterfaceDirL )
import           Stack.Ghci.Script
                   ( GhciScript, ModuleName, cmdAdd, cmdCdGhc, cmdModule
                   , scriptToLazyByteString
                   )
import           Stack.Package
                   ( PackageDescriptionPair (..), packageFromPackageDescription
                   , readDotBuildinfo, resolvePackageDescription, hasMainBuildableLibrary
                   )
import           Stack.Prelude
import           Stack.Runners ( ShouldReexec (..), withConfig, withEnvConfig )
import           Stack.Types.Build.Exception
                   ( BuildPrettyException (..), pprintTargetParseErrors )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..), stackYamlL )
import           Stack.Types.BuildOpts
                   ( ApplyCLIFlag, BenchmarkOpts (..), BuildOpts (..)
                   , BuildOptsCLI (..), TestOpts (..), defaultBuildOptsCLI
                   )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), HasCompiler (..) )
import           Stack.Types.Config ( Config (..), HasConfig (..), buildOptsL )
import           Stack.Types.EnvConfig
                   ( EnvConfig (..), HasEnvConfig (..), actualCompilerVersionL
                   , shaPathForBytes
                   )
import           Stack.Types.EnvSettings ( defaultEnvSettings )
import           Stack.Types.NamedComponent
                   ( NamedComponent (..), isCLib, renderPkgComponent )
import           Stack.Types.Package
                   ( BuildInfoOpts (..), InstallMap, InstalledMap
                   , LocalPackage (..), Package (..), PackageConfig (..)
                   , dotCabalCFilePath, dotCabalGetPath
                   , dotCabalMainPath, getPackageOpts
                   )
import           Stack.Types.PackageFile ( getPackageFiles )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Runner ( HasRunner, Runner )
import           Stack.Types.SourceMap
                   ( CommonPackage (..), DepPackage (..), GlobalPackage
                   , PackageType (..), ProjectPackage (..), SMActual (..)
                   , SMTargets (..), SMWanted (..), SourceMap (..), Target (..)
                   )
import           System.IO ( putStrLn )
import           System.Permissions ( setScriptPerms )
import           Stack.Types.CompCollection ( getBuildableListText )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Ghci" module.
data GhciException
  = InvalidPackageOption !String
  | LoadingDuplicateModules
  | MissingFileTarget !String
  | Can'tSpecifyFilesAndTargets
  | Can'tSpecifyFilesAndMainIs
  deriving (Show, Typeable)

instance Exception GhciException where
  displayException (InvalidPackageOption name) =
    "Error: [S-6716]\n"
    ++ "Failed to parse '--package' option " ++ name ++ "."
  displayException LoadingDuplicateModules = unlines
    [ "Error: [S-9632]"
    , "Not attempting to start ghci due to these duplicate modules."
    , "Use '--no-load' to try to start it anyway, without loading any \
      \modules (but these are still likely to cause errors)."
    ]
  displayException (MissingFileTarget name) =
    "Error: [S-3600]\n"
    ++ "Cannot find file target " ++ name ++ "."
  displayException Can'tSpecifyFilesAndTargets =
    "Error: [S-9906]\n"
    ++ "Cannot use 'stack ghci' with both file targets and package targets."
  displayException Can'tSpecifyFilesAndMainIs =
    "Error: [S-5188]\n"
    ++ "Cannot use 'stack ghci' with both file targets and '--main-is' \
       \flag."

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.Ghci" module.
data GhciPrettyException
  = GhciTargetParseException ![StyleDoc]
  | CandidatesIndexOutOfRangeBug
  deriving (Show, Typeable)

instance Pretty GhciPrettyException where
  pretty (GhciTargetParseException errs) =
       "[S-6948]"
    <> pprintTargetParseErrors errs
    <> blankLine
    <> fillSep
         [ flow "Note that to specify options to be passed to GHCi, use the"
         , style Shell "--ghci-options"
         , "option."
         ]
  pretty CandidatesIndexOutOfRangeBug = bugPrettyReport "[S-1939]" $
    flow "figureOutMainFile: index out of range."

instance Exception GhciPrettyException

-- | Typre respresenting command line options for the @stack ghci@ and
-- @stack repl@ commands.
data GhciOpts = GhciOpts
  { ghciTargets            :: ![Text]
  , ghciArgs               :: ![String]
  , ghciGhcOptions         :: ![String]
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
  }
  deriving Show

-- | Type representing information required to load a package or its components.
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
  }
  deriving Show

-- | Type representing loaded package description and related information.
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

-- | Function underlying the @stack ghci@ and @stack repl@ commands. Run GHCi in
-- the context of a project.
ghciCmd :: GhciOpts -> RIO Runner ()
ghciCmd ghciOpts =
  let boptsCLI = defaultBuildOptsCLI
        -- using only additional packages, targets then get overridden in `ghci`
        { boptsCLITargets = map T.pack (ghciAdditionalPackages  ghciOpts)
        , boptsCLIInitialBuildSteps = True
        , boptsCLIFlags = ghciFlags ghciOpts
        , boptsCLIGhcOptions = map T.pack (ghciGhcOptions ghciOpts)
        }
  in  withConfig YesReexec $ withEnvConfig AllowNoTargets boptsCLI $ do
        bopts <- view buildOptsL
        -- override env so running of tests and benchmarks is disabled
        let boptsLocal = bopts
              { boptsTestOpts = (boptsTestOpts bopts) { toDisableRun = True }
              , boptsBenchmarkOpts =
                  (boptsBenchmarkOpts bopts) { beoDisableRun = True }
              }
        local (set buildOptsL boptsLocal) (ghci ghciOpts)

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
    Right packageTargets -> pure (packageTargets, Nothing)
    Left rawFileTargets -> do
      case mainIsTargets of
        Nothing -> pure ()
        Just _ -> throwM Can'tSpecifyFilesAndMainIs
      -- Figure out targets based on filepath targets
      (targetMap, fileInfo, extraFiles) <- findFileTargets locals rawFileTargets
      pure (targetMap, Just (fileInfo, extraFiles))
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
  mainFile <- if ghciNoLoadModules
    then pure Nothing
    else do
      -- Figure out package files, in order to ask the user about which main
      -- module to load. See the note below for why this is done again after the
      -- build. This could potentially be done more efficiently, because all we
      -- need is the location of main modules, not the rest.
      pkgs0 <- getGhciPkgInfos installMap addPkgs (fmap fst mfileTargets) pkgDescs
      figureOutMainFile bopts mainIsTargets localTargets pkgs0
  let pkgTargets pn targets =
        case targets of
          TargetAll _  -> [T.pack (packageNameString pn)]
          TargetComps comps -> [renderPkgComponent (pn, c) | c <- toList comps]
  -- Build required dependencies and setup local packages.
  buildDepsAndInitialSteps opts $
    concatMap (\(pn, (_, t)) -> pkgTargets pn t) localTargets
  targetWarnings localTargets nonLocalTargets mfileTargets
  -- Load the list of modules _after_ building, to catch changes in
  -- unlisted dependencies (#1180)
  pkgs <- getGhciPkgInfos installMap addPkgs (fmap fst mfileTargets) pkgDescs
  checkForIssues pkgs
  -- Finally, do the invocation of ghci
  runGhci
    opts
    localTargets
    mainFile
    pkgs
    (maybe [] snd mfileTargets)
    (nonLocalTargets ++ addPkgs)

preprocessTargets ::
     HasEnvConfig env
  => BuildOptsCLI
  -> SMActual GlobalPackage
  -> [Text]
  -> RIO env (Either [Path Abs File] (Map PackageName Target))
preprocessTargets buildOptsCLI sma rawTargets = do
  let (fileTargetsRaw, normalTargetsRaw) =
        L.partition
          (\t -> ".hs" `T.isSuffixOf` t || ".lhs" `T.isSuffixOf` t)
          rawTargets
  -- Only use file targets if we have no normal targets.
  if not (null fileTargetsRaw) && null normalTargetsRaw
    then do
      fileTargets <- forM fileTargetsRaw $ \fp0 -> do
        let fp = T.unpack fp0
        mpath <- forgivingResolveFile' fp
        case mpath of
          Nothing -> throwM (MissingFileTarget fp)
          Just path -> pure path
      pure (Left fileTargets)
    else do
      -- Try parsing targets before checking if both file and
      -- module targets are specified (see issue#3342).
      let boptsCLI = buildOptsCLI { boptsCLITargets = normalTargetsRaw }
      normalTargets <- parseTargets AllowNoTargets False boptsCLI sma
        `catch` \pex@(PrettyException ex) ->
          case fromException $ toException ex of
            Just (TargetParseException xs) ->
              prettyThrowM $ GhciTargetParseException xs
            _ -> throwM pex
      unless (null fileTargetsRaw) $ throwM Can'tSpecifyFilesAndTargets
      pure (Right $ smtTargets normalTargets)

parseMainIsTargets ::
     HasEnvConfig env
  => BuildOptsCLI
  -> SMActual GlobalPackage
  -> Maybe Text
  -> RIO env (Maybe (Map PackageName Target))
parseMainIsTargets buildOptsCLI sma mtarget = forM mtarget $ \target -> do
  let boptsCLI = buildOptsCLI { boptsCLITargets = [target] }
  targets <- parseTargets AllowNoTargets False boptsCLI sma
  pure $ smtTargets targets

-- | Display PackageName + NamedComponent
displayPkgComponent :: (PackageName, NamedComponent) -> StyleDoc
displayPkgComponent =
  style PkgComponent . fromString . T.unpack . renderPkgComponent

findFileTargets ::
     HasEnvConfig env
  => [LocalPackage]
  -> [Path Abs File]
  -> RIO env (Map PackageName Target, Map PackageName [Path Abs File], [Path Abs File])
findFileTargets locals fileTargets = do
  filePackages <- forM locals $ \lp -> do
    (_,compFiles,_,_) <- getPackageFiles (packageFiles (lpPackage lp)) (lpCabalFile lp)
    pure (lp, M.map (map dotCabalGetPath) compFiles)
  let foundFileTargetComponents :: [(Path Abs File, [(PackageName, NamedComponent)])]
      foundFileTargetComponents =
        map (\fp -> (fp, ) $ L.sort $
                    concatMap (\(lp, files) -> map ((packageName (lpPackage lp), ) . fst)
                                                   (filter (elem fp . snd) (M.toList files))
                              ) filePackages
            ) fileTargets
  results <- forM foundFileTargetComponents $ \(fp, xs) ->
    case xs of
      [] -> do
        prettyWarnL
          [ flow "Couldn't find a component for file target"
          , pretty fp <> "."
          , flow "This means that the correct GHC options might not be used. \
                 \Attempting to load the file anyway."
          ]
        pure $ Left fp
      [x] -> do
        prettyInfoL
          [ flow "Using configuration for"
          , displayPkgComponent x
          , flow "to load"
          , pretty fp
          ]
        pure $ Right (fp, x)
      (x:_) -> do
        prettyWarn $
             fillSep
               [ flow "Multiple components contain file target"
               , pretty fp <> ":"
               , fillSep $ punctuate "," (map displayPkgComponent xs)
               ]
          <> line
          <> fillSep
               [ flow "Guessing the first one,"
               , displayPkgComponent x <> "."
               ]
        pure $ Right (fp, x)
  let (extraFiles, associatedFiles) = partitionEithers results
      targetMap =
          foldl' unionTargets M.empty $
          map (\(_, (name, comp)) -> M.singleton name (TargetComps (S.singleton comp)))
              associatedFiles
      infoMap =
          foldl' (M.unionWith (<>)) M.empty $
          map (\(fp, (name, _)) -> M.singleton name [fp])
              associatedFiles
  pure (targetMap, infoMap, extraFiles)

getAllLocalTargets ::
     HasEnvConfig env
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
    then pure directlyWanted
    else do
      let extraList' =
            map (fromString . packageNameString . fst) extraLoadDeps :: [StyleDoc]
          extraList = mkNarrativeList (Just Current) False extraList'
      if ghciLoadLocalDeps
        then prettyInfo $
          fillSep $
                [ flow "The following libraries will also be loaded into \
                       \GHCi because they are local dependencies of your \
                       \targets, and you specified"
                , style Shell "--load-local-deps" <> ":"
                ]
            <> extraList
        else prettyInfo $
             fillSep
               ( flow "The following libraries will also be loaded into \
                      \GHCi because they are intermediate dependencies of \
                      \your targets:"
               : extraList
               )
          <> line
          <> fillSep
               [ "(Use"
               , style Shell "--skip-intermediate-deps"
               , flow "to omit these.)"
               ]
      pure (directlyWanted ++ extraLoadDeps)

getAllNonLocalTargets ::
     Map PackageName Target
  -> RIO env [PackageName]
getAllNonLocalTargets targets = do
  let isNonLocal (TargetAll PTDependency) = True
      isNonLocal _ = False
  pure $ map fst $ filter (isNonLocal . snd) (M.toList targets)

buildDepsAndInitialSteps :: HasEnvConfig env => GhciOpts -> [Text] -> RIO env ()
buildDepsAndInitialSteps GhciOpts{..} localTargets = do
  let targets = localTargets ++ map T.pack ghciAdditionalPackages
  -- If necessary, do the build, for local packagee targets, only do
  -- 'initialBuildSteps'.
  case nonEmpty targets of
    -- only new local targets could appear here
    Just nonEmptyTargets | not ghciNoBuild -> do
      eres <- buildLocalTargets nonEmptyTargets
      case eres of
        Right () -> pure ()
        Left err -> do
          case fromException err of
            Just (PrettyException prettyErr) -> prettyError $ pretty prettyErr
            Nothing -> prettyError $ fromString (displayException err)
          prettyWarn "Build failed, but trying to launch GHCi anyway"
    _ ->
      pure ()

checkAdditionalPackages :: MonadThrow m => [String] -> m [PackageName]
checkAdditionalPackages pkgs = forM pkgs $ \name -> do
  let mres = (pkgName <$> parsePackageIdentifier name)
        <|> parsePackageNameThrowing name
  maybe (throwM $ InvalidPackageOption name) pure mres

runGhci ::
     HasEnvConfig env
  => GhciOpts
  -> [(PackageName, (Path Abs File, Target))]
  -> Maybe (Path Abs File)
  -> [GhciPkgInfo]
  -> [Path Abs File]
  -> [PackageName]
  -> RIO env ()
runGhci GhciOpts{..} targets mainFile pkgs extraFiles exposePackages = do
    config <- view configL
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
        (omittedOpts, ghcOpts) = L.partition badForGhci $
            concatMap (concatMap (bioOpts . snd) . ghciPkgOpts) pkgs ++ map T.unpack
              ( fold (configGhcOptionsByCat config) -- include everything, locals, and targets
             ++ concatMap (getUserOptions . ghciPkgName) pkgs
              )
        getUserOptions pkg = M.findWithDefault [] pkg (configGhcOptionsByName config)
        badForGhci x =
            L.isPrefixOf "-O" x || elem x (words "-debug -threaded -ticky -static -Werror")
    unless (null omittedOpts) $
      prettyWarn $
           fillSep
             ( flow "The following GHC options are incompatible with GHCi and \
                    \have not been passed to it:"
             : mkNarrativeList (Just Current) False
                 (map fromString (nubOrd omittedOpts) :: [StyleDoc])
             )
        <> line
    oiDir <- view objectInterfaceDirL
    let odir =
            [ "-odir=" <> toFilePathNoTrailingSep oiDir
            , "-hidir=" <> toFilePathNoTrailingSep oiDir ]
    prettyInfoL
      ( flow "Configuring GHCi with the following packages:"
      : mkNarrativeList (Just Current) False
          (map (fromString . packageNameString . ghciPkgName) pkgs :: [StyleDoc])
      )
    compilerExeName <- view $ compilerPathsL.to cpCompiler.to toFilePath
    let execGhci extras = do
            menv <- liftIO $ configProcessContextSettings config defaultEnvSettings
            withPackageWorkingDir $ withProcessContext menv $ exec
                 (fromMaybe compilerExeName ghciGhcCommand)
                 (("--interactive" : ) $
                 -- This initial "-i" resets the include directories to
                 -- not include CWD. If there aren't any packages, CWD
                 -- is included.
                  (if null pkgs then id else ("-i" : )) $
                  odir <> pkgopts <> extras <> ghciGhcOptions <> ghciArgs)
        withPackageWorkingDir =
            case pkgs of
              [pkg] -> withWorkingDir (toFilePath $ ghciPkgDir pkg)
              _ -> id
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
                            $ runGrabFirstLine (fromMaybe compilerExeName ghciGhcCommand) ["--version"]
                    pure $ "Intero" `L.isPrefixOf` output
                _ -> pure False
    -- Since usage of 'exec' does not pure, we cannot do any cleanup
    -- on ghci exit. So, instead leave the generated files. To make this
    -- more efficient and avoid gratuitous generation of garbage, the
    -- file names are determined by hashing. This also has the nice side
    -- effect of making it possible to copy the ghci invocation out of
    -- the log and have it still work.
    tmpDirectory <- getXdgDir XdgCache $
      Just (relDirStackProgName </> relDirGhciScript)
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

writeMacrosFile ::
     HasTerm env
  => Path Abs Dir
  -> [GhciPkgInfo]
  -> RIO env [String]
writeMacrosFile outputDirectory pkgs = do
  fps <- fmap (nubOrd . catMaybes . concat) $
    forM pkgs $ \pkg -> forM (ghciPkgOpts pkg) $ \(_, bio) -> do
      let cabalMacros = bioCabalMacros bio
      exists <- liftIO $ doesFileExist cabalMacros
      if exists
        then pure $ Just cabalMacros
        else do
          prettyWarnL ["Didn't find expected autogen file:", pretty cabalMacros]
          pure Nothing
  files <- liftIO $ mapM (S8.readFile . toFilePath) fps
  if null files then pure [] else do
    out <- liftIO $ writeHashedFile outputDirectory relFileCabalMacrosH $
      S8.concat $ map (<> "\n#undef CURRENT_PACKAGE_KEY\n#undef CURRENT_COMPONENT_ID\n") files
    pure ["-optP-include", "-optP" <> toFilePath out]

writeGhciScript :: (MonadIO m) => Path Abs Dir -> GhciScript -> m [String]
writeGhciScript outputDirectory script = do
  scriptPath <- liftIO $ writeHashedFile outputDirectory relFileGhciScript $
    LBS.toStrict $ scriptToLazyByteString script
  let scriptFilePath = toFilePath scriptPath
  setScriptPerms scriptFilePath
  pure ["-ghci-script=" <> scriptFilePath]

writeHashedFile ::
     Path Abs Dir
  -> Path Rel File
  -> ByteString
  -> IO (Path Abs File)
writeHashedFile outputDirectory relFile contents = do
  relSha <- shaPathForBytes contents
  let outDir = outputDirectory </> relSha
      outFile = outDir </> relFile
  alreadyExists <- doesFileExist outFile
  unless alreadyExists $ do
    ensureDir outDir
    writeBinaryFileAtomic outFile $ byteString contents
  pure outFile

renderScript ::
     Bool
  -> [GhciPkgInfo]
  -> Maybe (Path Abs File)
  -> Bool
  -> [Path Abs File]
  -> GhciScript
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
figureOutMainFile ::
     (HasRunner env, HasTerm env)
  => BuildOpts
  -> Maybe (Map PackageName Target)
  -> [(PackageName, (Path Abs File, Target))]
  -> [GhciPkgInfo]
  -> RIO env (Maybe (Path Abs File))
figureOutMainFile bopts mainIsTargets targets0 packages =
    case candidates of
      [] -> pure Nothing
      [c@(_,_,fp)] -> do
        prettyInfo $
             fillSep
               [ "Using"
               , style Current "main"
               , "module:"
               ]
          <> line
          <> renderCandidate c
          <> line
        pure (Just fp)
      candidate:_ -> do
        prettyWarn $
             fillSep
               [ "The"
               , style Current "main"
               , flow "module to load is ambiguous. Candidates are:"
               ]
          <> line
          <> mconcat (L.intersperse line (map renderCandidate candidates))
          <> blankLine
          <> flow "You can specify which one to pick by:"
          <> line
          <> bulletedList
               [ fillSep
                   [ flow "Specifying targets to"
                   , style Shell (flow "stack ghci")
                   , "e.g."
                   , style Shell ( fillSep
                                     [ flow "stack ghci"
                                     , sampleTargetArg candidate
                                     ]
                                 ) <> "."
                   ]
               , fillSep
                   [ flow "Specifying what the"
                   , style Current "main"
                   , flow "is e.g."
                   , style Shell ( fillSep
                                     [ flow "stack ghci"
                                     , sampleMainIsArg candidate
                                     ]
                                 ) <> "."
                   ]
                , flow
                    $  "Choosing from the candidate above [1.."
                    <> show (length candidates)
                    <> "]."
               ]
          <> line
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
        pure (ghciPkgName pkg, component, main)
       where
        wantedComponents =
          wantedPackageComponents bopts target (ghciPkgPackage pkg)
  renderCandidate c@(pkgName, namedComponent, mainIs) =
    let candidateIndex =
          fromString . show . (+1) . fromMaybe 0 . L.elemIndex c
        pkgNameText = fromString $ packageNameString pkgName
    in  hang 4
          $  fill 4 ( candidateIndex candidates <> ".")
          <> fillSep
               [ "Package"
               , style Current pkgNameText <> ","
               , "component"
        --       This is the format that can be directly copy-pasted as
        --       an argument to `stack ghci`.
               , style
                   PkgComponent
                   (  pkgNameText
                   <> ":"
                   <> renderComp namedComponent
                   )
                 <> ","
               , "with"
               , style Shell "main-is"
               , "file:"
               , pretty mainIs <> "."
               ]
  candidateIndices = take (length candidates) [1 :: Int ..]
  userOption = do
    option <- prompt "Specify main module to use (press enter to load none): "
    let selected = fromMaybe
                    ((+1) $ length candidateIndices)
                    (readMaybe (T.unpack option) :: Maybe Int)
    case L.elemIndex selected candidateIndices  of
      Nothing -> do
        putStrLn
          "Not loading any main modules, as no valid module selected"
        putStrLn ""
        pure Nothing
      Just op -> do
        (_, _, fp) <- maybe
              (prettyThrowIO CandidatesIndexOutOfRangeBug)
              pure
              (candidates !? op)
        putStrLn
          ("Loading main module from candidate " <>
          show (op + 1) <> ", --main-is " <>
          toFilePath fp)
        putStrLn ""
        pure $ Just fp
  renderComp c =
    case c of
      CLib -> "lib"
      CSubLib name -> "sub-lib:" <> fromString (T.unpack name)
      CExe name -> "exe:" <> fromString (T.unpack name)
      CTest name -> "test:" <> fromString ( T.unpack name)
      CBench name -> "bench:" <> fromString (T.unpack name)
  sampleTargetArg (pkg, comp, _) =
       fromString (packageNameString pkg)
    <> ":"
    <> renderComp comp
  sampleMainIsArg (pkg, comp, _) =
    fillSep
      [ "--main-is"
      , fromString (packageNameString pkg) <> ":" <> renderComp comp
      ]

loadGhciPkgDescs ::
     HasEnvConfig env
  => BuildOptsCLI
  -> [(PackageName, (Path Abs File, Target))]
  -> RIO env [GhciPkgDesc]
loadGhciPkgDescs buildOptsCLI localTargets =
  forM localTargets $ \(name, (cabalfp, target)) ->
    loadGhciPkgDesc buildOptsCLI name cabalfp target

-- | Load package description information for a ghci target.
loadGhciPkgDesc ::
     HasEnvConfig env
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
      sourceMapCabalConfigOpts = fromMaybe [] $
        (cpCabalConfigOpts . ppCommon <$> M.lookup name smProject)
        <|>
        (cpCabalConfigOpts . dpCommon <$> M.lookup name smDeps)
      sourceMapFlags =
        maybe mempty (cpFlags . ppCommon) $ M.lookup name smProject
      config = PackageConfig
        { packageConfigEnableTests = True
        , packageConfigEnableBenchmarks = True
        , packageConfigFlags =
            getLocalFlags buildOptsCLI name `M.union` sourceMapFlags
        , packageConfigGhcOptions = sourceMapGhcOptions
        , packageConfigCabalConfigOpts = sourceMapCabalConfigOpts
        , packageConfigCompilerVersion = compilerVersion
        , packageConfigPlatform = view platformL econfig
        }
  -- TODO we've already parsed this information, otherwise we
  -- wouldn't have figured out the cabalfp already. In the future:
  -- retain that GenericPackageDescription in the relevant data
  -- structures to avoid reparsing.
  (gpdio, _name, _cabalfp) <-
      loadCabalFilePath (Just stackProgName') (parent cabalfp)
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
           in  PackageDescriptionPair
                 (C.updatePackageDescription bi x)
                 (C.updatePackageDescription bi y))
          mbuildinfo
  pure GhciPkgDesc
    { ghciDescPkg = pkg
    , ghciDescCabalFp = cabalfp
    , ghciDescTarget = target
    }

getGhciPkgInfos ::
     HasEnvConfig env
  => InstallMap
  -> [PackageName]
  -> Maybe (Map PackageName [Path Abs File])
  -> [GhciPkgDesc]
  -> RIO env [GhciPkgInfo]
getGhciPkgInfos installMap addPkgs mfileTargets localTargets = do
  (installedMap, _, _, _) <- getInstalled installMap
  let localLibs =
        [ packageName (ghciDescPkg desc)
        | desc <- localTargets
        , hasLocalComp isCLib (ghciDescTarget desc)
        ]
  forM localTargets $ \pkgDesc ->
    makeGhciPkgInfo installMap installedMap localLibs addPkgs mfileTargets pkgDesc

-- | Make information necessary to load the given package in GHCi.
makeGhciPkgInfo ::
     HasEnvConfig env
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
  pure GhciPkgInfo
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
  (if hasMainBuildableLibrary pkg then CLib : map CSubLib (getBuildableListText $ packageForeignLibraries pkg)
     else []) ++
  map CExe (S.toList (packageExes pkg)) <>
  map CSubLib (getBuildableListText $ packageSubLibraries pkg) <>
  (if boptsTests bopts then map CTest (M.keys (packageTests pkg)) else []) <>
  (if boptsBenchmarks bopts then map CBench (S.toList (packageBenchmarks pkg)) else [])
wantedPackageComponents _ _ _ = S.empty

checkForIssues :: HasTerm env => [GhciPkgInfo] -> RIO env ()
checkForIssues pkgs =
  when (length pkgs > 1) $ do
    -- Cabal flag issues could arise only when there are at least 2 packages
    unless (null cabalFlagIssues) $ do
      prettyWarn $
           flow "There are Cabal flags for this project which may prevent \
                \GHCi from loading your code properly. In some cases it \
                \can also load some projects which would otherwise fail to \
                \build."
        <> blankLine
        <> mconcat (L.intersperse blankLine cabalFlagIssues)
        <> blankLine
        <> flow "To resolve, remove the flag(s) from the Cabal file(s) and \
                \instead put them at the top of the Haskell files."
        <> blankLine
    prettyWarnL
      [ flow "It isn't yet possible to load multiple packages into GHCi in \
             \all cases. For further information, see"
      , style Url "https://ghc.haskell.org/trac/ghc/ticket/10827" <> "."
      ]
 where
  cabalFlagIssues = concatMap mixedFlag
    [ ( "-XNoImplicitPrelude"
      , [ flow "-XNoImplicitPrelude will be used, but GHCi will likely fail to \
               \build things which depend on the implicit prelude."
        ]
      )
    , ( "-XCPP"
      , [ flow "-XCPP will be used, but it can cause issues with multiline \
               \strings. For further information, see"
        , style Url "https://downloads.haskell.org/~ghc/7.10.2/docs/html/users_guide/options-phases.html#cpp-string-gaps" <> "."
        ]
      )
    , ( "-XNoTraditionalRecordSyntax"
      , [ flow "-XNoTraditionalRecordSyntax will be used, but it break modules \
               \which use record syntax."
        ]
      )
    , ( "-XTemplateHaskell"
      , [ flow "-XTemplateHaskell will be used, but it may cause compilation \
               \issues due to different parsing of '$' when there's no space \
               \after it."
        ]
      )
    , ( "-XQuasiQuotes"
      , [ flow "-XQuasiQuotes will be used, but it may cause parse failures \
               \due to a different meaning for list comprehension syntax like \
               \[x| ... ]"
          ]
      )
    , ( "-XSafe"
      , [ flow "-XSafe will be used, but it will fail to compile unsafe \
               \modules."
        ]
      )
    , ( "-XArrows"
      , [ flow "-XArrows will be used, but it will cause non-arrow usages of \
               \proc, (-<), (-<<) to fail"
        ]
      )
    , ( "-XOverloadedStrings"
      , [ flow "-XOverloadedStrings will be used, but it can cause type \
               \ambiguity in code not usually compiled with it."
        ]
      )
    , ( "-XOverloadedLists"
      , [ flow "-XOverloadedLists will be used, but it can cause type \
               \ambiguity in code not usually compiled with it."
        ]
      )
    , ( "-XMonoLocalBinds"
      , [ flow "-XMonoLocalBinds will be used, but it can cause type errors in \
               \code which expects generalized local bindings." ]
      )
    , ( "-XTypeFamilies"
      , [ flow "-XTypeFamilies will be used, but it implies -XMonoLocalBinds, \
               \and so can cause type errors in code which expects generalized \
               \local bindings." ]
      )
    , ( "-XGADTs"
      , [ flow "-XGADTs will be used, but it implies -XMonoLocalBinds, and so \
               \can cause type errors in code which expects generalized local \
               \bindings." ]
      )
    , ( "-XNewQualifiedOperators"
      , [ flow "-XNewQualifiedOperators will be used, but this will break \
               \usages of the old qualified operator syntax." ]
      )
    ]
  mixedFlag (flag, msgs) =
    let x = partitionComps (== flag) in
    [ fillSep $ msgs ++ showWhich x | mixedSettings x ]
  mixedSettings (xs, ys) = xs /= [] && ys /= []
  showWhich (haveIt, don'tHaveIt) =
       [ flow "It is specified for:" ]
    <> mkNarrativeList (Just PkgComponent) False
         (map (fromString . T.unpack . renderPkgComponent) haveIt :: [StyleDoc])
    <> [ flow "But not for:" ]
    <> mkNarrativeList (Just PkgComponent) False
         (map (fromString . T.unpack . renderPkgComponent) don'tHaveIt :: [StyleDoc])
  partitionComps f = (map fst xs, map fst ys)
   where
    (xs, ys) = L.partition (any f . snd) compsWithOpts
  compsWithOpts = map (\(k, bio) ->
    (k, bioOneWordOpts bio ++ bioOpts bio)) compsWithBios
  compsWithBios =
    [ ((ghciPkgName pkg, c), bio)
    | pkg <- pkgs
    , (c, bio) <- ghciPkgOpts pkg
    ]

-- TODO: Should this also tell the user the filepaths, not just the
-- module name?
checkForDuplicateModules :: HasTerm env => [GhciPkgInfo] -> RIO env ()
checkForDuplicateModules pkgs =
  unless (null duplicates) $
    prettyWarn $
         flow "Multiple files use the same module name:"
      <> line
      <> bulletedList (map prettyDuplicate duplicates)
      <> line
    -- MSS 2020-10-13 Disabling, may remove entirely in the future
    -- See: https://github.com/commercialhaskell/stack/issues/5407#issuecomment-707339928
    -- throwM LoadingDuplicateModules
 where
  duplicates ::
    [(ModuleName, Map (Path Abs File) (Set (PackageName, NamedComponent)))]
  duplicates =
    filter (\(_, mp) -> M.size mp > 1) $
    M.toList $
    unionModuleMaps (map ghciPkgModules pkgs)
  prettyDuplicate ::
       (ModuleName, Map (Path Abs File) (Set (PackageName, NamedComponent)))
    -> StyleDoc
  prettyDuplicate (mn, mp) =
       fillSep
         [ style Error (pretty mn)
         , flow "found at the following paths"
         ]
    <> line
    <> bulletedList (map fileDuplicate (M.toList mp))
  fileDuplicate ::
    (Path Abs File, Set (PackageName, NamedComponent)) -> StyleDoc
  fileDuplicate (fp, comps) =
    fillSep
      [ pretty fp
      , parens $
          fillSep $ punctuate "," (map displayPkgComponent (S.toList comps))
      ]

targetWarnings ::
     HasBuildConfig env
  => [(PackageName, (Path Abs File, Target))]
  -> [PackageName]
  -> Maybe (Map PackageName [Path Abs File], [Path Abs File])
  -> RIO env ()
targetWarnings localTargets nonLocalTargets mfileTargets = do
  unless (null nonLocalTargets) $
    prettyWarnL
      [ flow "Some targets"
      , parens $ fillSep $ punctuate "," $ map (style Good . fromString . packageNameString) nonLocalTargets
      , flow "are not local packages, and so cannot be directly loaded."
      , flow "In future versions of Stack, this might be supported - see"
      , style Url "https://github.com/commercialhaskell/stack/issues/1441"
      , "."
      , flow "It can still be useful to specify these, as they will be passed to ghci via -package flags."
      ]
  when (null localTargets && isNothing mfileTargets) $ do
      smWanted <- view $ buildConfigL.to bcSMWanted
      stackYaml <- view stackYamlL
      prettyNote $ vsep
          [ flow "No local targets specified, so a plain ghci will be started with no package hiding or package options."
          , ""
          , flow $ T.unpack $ utf8BuilderToText $
                   "You are using snapshot: " <>
                   display (smwSnapshotLocation smWanted)
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
getExtraLoadDeps ::
     Bool
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
            (Just (Just _), _) -> pure True
            (Just Nothing, _) | not loadAllDeps -> pure False
            (_, Just lp) -> do
                let deps = M.keys (packageDeps (lpPackage lp))
                shouldLoad <- or <$> mapM go deps
                if shouldLoad
                    then do
                        modify (M.insert name (Just (lpCabalFile lp, TargetComps (S.singleton CLib))))
                        pure True
                    else do
                        modify (M.insert name Nothing)
                        pure False
            (_, _) -> pure False

unionTargets :: Ord k => Map k Target -> Map k Target -> Map k Target
unionTargets = M.unionWith $ \l r -> case (l, r) of
  (TargetAll PTDependency, _) -> r
  (TargetComps sl, TargetComps sr) -> TargetComps (S.union sl sr)
  (TargetComps _, TargetAll PTProject) -> TargetAll PTProject
  (TargetComps _, _) -> l
  (TargetAll PTProject, _) -> TargetAll PTProject

hasLocalComp :: (NamedComponent -> Bool) -> Target -> Bool
hasLocalComp p t = case t of
  TargetComps s -> any p (S.toList s)
  TargetAll PTProject -> True
  _ -> False

-- | Run a command and grab the first line of stdout, dropping
-- stderr's contexts completely.
runGrabFirstLine ::
     (HasProcessContext env, HasLogFunc env)
  => String
  -> [String]
  -> RIO env String
runGrabFirstLine cmd0 args =
  proc cmd0 args $ \pc -> do
    (out, _err) <- readProcess_ pc
    pure
      $ TL.unpack
      $ TL.filter (/= '\r')
      $ TL.concat
      $ take 1
      $ TL.lines
      $ TLE.decodeUtf8With lenientDecode out
