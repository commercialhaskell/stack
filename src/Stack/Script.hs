{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Stack.Script
Description : Types and functions related to Stack's @script@ command.
License     : BSD-3-Clause

Types and functions related to Stack's @script@ command.
-}

module Stack.Script
  ( ScriptOpts (..)
  , ScriptExecute (..)
  , ShouldRun (..)
  , scriptCmd
  ) where

import           Data.ByteString.Builder ( toLazyByteString )
import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit.List as CL
import qualified Data.List.NonEmpty as NE
import           Data.List.Split ( splitWhen )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Distribution.Compiler ( CompilerFlavor (..) )
import           Distribution.ModuleName ( ModuleName )
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Types.CondTree as C
import           Distribution.Types.ModuleReexport ( moduleReexportName )
import           Distribution.Types.PackageName ( mkPackageName )
import           Distribution.Types.VersionRange ( withinRange )
import           Distribution.System ( Platform (..) )
import qualified Pantry.SHA256 as SHA256
import           Path
                   ( (</>), filename, fromAbsDir, fromAbsFile, fromRelFile
                   , parent, parseRelDir, replaceExtension, splitExtension
                   )
import           Path.IO ( getModificationTime, resolveFile' )
import qualified RIO.Directory as Dir
import           RIO.Process
                   ( HasProcessContext, exec, proc, readProcessStdout_
                   , withWorkingDir
                   )
import qualified RIO.Text as T
import           Stack.Build ( build )
import           Stack.Build.Installed ( getInstalled, toInstallMap )
import           Stack.Constants ( osIsWindows, relDirScripts )
import           Stack.Prelude
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withDefaultEnvConfig )
import           Stack.Setup ( withNewLocalBuildTargets )
import           Stack.SourceMap ( getCompilerInfo, immutableLocSha )
import           Stack.Types.Compiler ( ActualCompiler (..) )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), GhcPkgExe (..), HasCompiler (..) )
import           Stack.Types.Config ( Config (..), HasConfig (..), stackRootL )
import           Stack.Types.ConfigMonoid ( ConfigMonoid (..) )
import qualified Stack.Types.ConfigMonoid as ConfigMonoid ( ConfigMonoid (..) )
import           Stack.Types.DumpPackage ( DumpPackage (..) )
import           Stack.Types.EnvConfig
                   ( EnvConfig (..), HasEnvConfig (..), actualCompilerVersionL
                   , appropriateGhcColorFlag
                   )
import           Stack.Types.EnvSettings ( defaultEnvSettings )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Runner ( Runner, globalOptsL )
import           Stack.Types.SourceMap
                   ( CommonPackage (..), DepPackage (..), SourceMap (..) )
import           Stack.Types.StackYamlLoc ( StackYamlLoc (..) )
import           System.FilePath ( splitDrive )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Script" module.
data ScriptException
  = MutableDependenciesForScript [PackageName]
  | AmbiguousModuleName ModuleName [PackageName]
  | ArgumentsWithNoRunInvalid
  | NoRunWithoutCompilationInvalid
  | FailedToParseScriptFileAsDirBug (Path Rel File)
  | FailedToParseFileAsDirBug (Path Abs Dir)
  deriving Show

instance Exception ScriptException where
  displayException (MutableDependenciesForScript names) = unlines
    $ "Error: [S-4994]"
    : "No mutable packages are allowed in the 'script' command. Mutable \
      \packages found:"
    : map (\name -> "- " ++ packageNameString name) names
  displayException (AmbiguousModuleName mname pkgs) = unlines
    $ "Error: [S-1691]"
    : (  "Module "
      ++ moduleNameString mname
      ++ " appears in multiple packages: "
      )
    : [ unwords $ map packageNameString pkgs ]
  displayException ArgumentsWithNoRunInvalid =
    "Error: [S-5067]\n"
    ++ "'--no-run' incompatible with arguments."
  displayException NoRunWithoutCompilationInvalid =
    "Error: [S-9469]\n"
    ++ "'--no-run' requires either '--compile' or '--optimize'."
  displayException (FailedToParseScriptFileAsDirBug fp) = bugReport "[S-5055]" $
       "Failed to parse script file name as directory:\n"
    <> fromRelFile fp <> "\n"
  displayException (FailedToParseFileAsDirBug p) = bugReport "[S-9464]" $
       "Failed to parse path to script file as directory:\n"
    <> fromAbsDir p <> "\n"

-- | Type representing choices of interpreting, compiling (without optimisation)
-- and compiling (with optimisation).
data ScriptExecute
  = SEInterpret
  | SECompile
    -- ^ Without optimisation.
  | SEOptimize
    -- ^ Compile with optimisation.
  deriving Show

-- | Type representing choices of whether to run or not.
data ShouldRun
  = YesRun
    -- ^ Run.
  | NoRun
    -- ^ Do not run.
  deriving Show

-- | Type representing command line options for the @stack script@ command.
data ScriptOpts = ScriptOpts
  { packages :: ![String]
  , file :: !FilePath
  , args :: ![String]
  , compile :: !ScriptExecute
  , useRoot :: !Bool
  , ghcOptions :: ![String]
  , scriptExtraDeps :: ![Unresolved (NonEmpty RawPackageLocationImmutable)]
  , shouldRun :: !ShouldRun
  }

-- | Run a Stack Script
scriptCmd :: ScriptOpts -> RIO Runner ()
scriptCmd opts = do
  -- Some warnings in case the user somehow tries to set a stack.yaml location.
  -- Note that in this functions we use logError instead of logWarn because,
  -- when using the interpreter mode, only error messages are shown. See:
  -- https://github.com/commercialhaskell/stack/issues/3007
  view (globalOptsL . to (.stackYaml)) >>= \case
    SYLOverride fp -> logError $
         "Ignoring override stack.yaml file for script command: "
      <> fromString (toFilePath fp)
    SYLGlobalProject -> logError "Ignoring SYLGlobalProject for script command"
    SYLDefault -> pure ()
    SYLNoProject _ -> assert False (pure ())

  file <- resolveFile' opts.file
  let scriptFile = filename file
      scriptRoot = parent file

  isNoRunCompile <- fromFirstFalse . (.noRunCompile) <$>
                           view (globalOptsL . to (.configMonoid))

  resolvedExtraDeps <-
    mapM (resolvePaths (Just scriptRoot)) opts.scriptExtraDeps
  let scriptDir = parent file
      extraDeps = concatMap NE.toList resolvedExtraDeps
      modifyGO go = go
        { configMonoid = go.configMonoid
            { ConfigMonoid.installGHC = FirstTrue $ Just True
            }
        , stackYaml = SYLNoProject extraDeps
        }
      (shouldRun, shouldCompile) = if isNoRunCompile
        then (NoRun, SECompile)
        else (opts.shouldRun, opts.compile)

  outputDir <- if opts.useRoot
    then do
      root <- local (over globalOptsL modifyGO) $
        withConfig NoReexec $ view stackRootL
      scriptFileAsDir <- maybe
        (throwIO $ FailedToParseScriptFileAsDirBug scriptFile)
        pure
        (parseRelDir $ fromRelFile scriptFile)
      let fileAsDir = scriptDir </> scriptFileAsDir
          -- We drop the information about the drive. On Windows, in principle,
          -- the drive could distinguish between two otherwise identical
          -- fileAsDir (eg C:\MyScript.hs\ D:\MyScript.hs\). In pactice, we
          -- tolerate that possibility as being unlikely.
          (_, escaped) = splitDrive (fromAbsDir fileAsDir)
      escapedRelDir <- maybe
        (throwIO $ FailedToParseFileAsDirBug fileAsDir)
        pure
        (parseRelDir escaped)
      pure $ root </> relDirScripts </> escapedRelDir
    else pure scriptDir

  -- path does not necessarily end with an extension.
  let dropExtension path = pure $ maybe path fst $ splitExtension path

  exe <- if osIsWindows
    then replaceExtension ".exe" (outputDir </> scriptFile)
    else dropExtension (outputDir </> scriptFile)

  case shouldRun of
    YesRun -> pure ()
    NoRun -> do
      unless (null opts.args) $ throwIO ArgumentsWithNoRunInvalid
      case shouldCompile of
        SEInterpret -> throwIO NoRunWithoutCompilationInvalid
        SECompile -> pure ()
        SEOptimize -> pure ()

  -- Optimization: if we're compiling, and the executable is newer than the
  -- source file, run it immediately.
  local (over globalOptsL modifyGO) $
    case shouldCompile of
      SEInterpret -> longWay shouldRun shouldCompile file exe
      SECompile -> shortCut shouldRun shouldCompile file exe
      SEOptimize -> shortCut shouldRun shouldCompile file exe

 where
  runCompiled ::
       (HasProcessContext env, HasTerm env)
    => ShouldRun
    -> Path Abs File
    -> RIO env ()
  runCompiled shouldRun exe = do
    case shouldRun of
      YesRun -> exec (fromAbsFile exe) opts.args
      NoRun -> prettyInfoL
        [ flow "Compilation finished, executable available at"
        , style File (fromString (fromAbsFile exe)) <> "."
        ]

  shortCut shouldRun shouldCompile file exe =
    handleIO (const $ longWay shouldRun shouldCompile file exe) $ do
      srcMod <- getModificationTime file
      exeMod <- Dir.getModificationTime (fromAbsFile exe)
      if srcMod < exeMod
        then runCompiled shouldRun exe
        else longWay shouldRun shouldCompile file exe

  longWay shouldRun shouldCompile file exe =
    withConfig YesReexec $
    withDefaultEnvConfig $ do
      config <- view configL
      menv <- liftIO $ config.processContextSettings defaultEnvSettings
      withProcessContext menv $ do
        colorFlag <- appropriateGhcColorFlag

        targetsSet <-
          case opts.packages of
            [] -> getPackagesFromImports opts.file -- Using the import parser
            packages -> do
              let targets = concatMap wordsComma packages
              targets' <- mapM parsePackageNameThrowing targets
              pure $ Set.fromList targets'

        GhcPkgExe pkg <- view $ compilerPathsL . to (.pkg)
        let ghcPkgPath = toFilePath pkg
        unless (Set.null targetsSet) $ do
          -- Optimization: use the relatively cheap ghc-pkg list --simple-output
          -- to check which packages are installed already. If all needed
          -- packages are available, we can skip the (rather expensive) build
          -- call below.
          -- https://github.com/haskell/process/issues/251
          bss <- snd <$> sinkProcessStderrStdout
                   ghcPkgPath
                   ["list", "--simple-output"]
                   CL.sinkNull
                   CL.consume
          -- ^ FIXME use the package info from envConfigPackages, or is that crazy?
          let installed = Set.fromList
                        $ map toPackageName
                        $ words
                        $ S8.unpack
                        $ S8.concat bss
          if Set.null $ Set.difference (Set.map packageNameString targetsSet) installed
            then logDebug "All packages already installed"
            else do
              logDebug "Missing packages, performing installation"
              let targets =
                    map (T.pack . packageNameString) $ Set.toList targetsSet
              withNewLocalBuildTargets targets $ build Nothing

        let packagesSet = Set.insert (mkPackageName "base") targetsSet
            -- Yields 'raw' strings with trailing whitespace. Assumes that the
            -- ghc-pkg application will find a package of the given name.
            getRawPackageId :: PackageName -> RIO EnvConfig [ByteString]
            getRawPackageId target = snd <$> sinkProcessStderrStdout
              ghcPkgPath
              ["field", packageNameString target, "id", "--simple-output"]
              CL.sinkNull
              CL.consume
        rawPackageIds <- mapM getRawPackageId $ Set.toList packagesSet
        let packageIds = words $ S8.unpack $ S8.concat $ concat rawPackageIds
            -- ^ The use of words will eliminate whitespace between 'raw' items
            ghcArgs = concat
              [ ["-i", "-i" ++ fromAbsDir (parent file)]
              , ["-hide-all-packages"]
              , maybeToList colorFlag
                -- We use GHC's -package-id option rather than -package because
                -- there is a bug in the latter. For packages with a public
                -- sublibrary, -package <pkg> can expose an installed package
                -- that is not listed by ghc-pkg list <pkg>. See:
                -- https://gitlab.haskell.org/ghc/ghc/-/issues/25025
              , map ("-package-id=" ++) packageIds
              , case shouldCompile of
                  SEInterpret -> []
                  SECompile -> []
                  SEOptimize -> ["-O2"]
              , opts.ghcOptions
              , if opts.useRoot
                  then
                    [ "-outputdir=" ++ fromAbsDir (parent exe)
                    , "-o", fromAbsFile exe
                    ]
                  else []
              ]
        case shouldCompile of
          SEInterpret -> do
            interpret <- view $ compilerPathsL . to (.interpreter)
            exec (toFilePath interpret)
                (ghcArgs ++ toFilePath file : opts.args)
          _ -> do
            -- Use readProcessStdout_ so that (1) if GHC does send any output
            -- to stdout, we capture it and stop it from being sent to our
            -- stdout, which could break scripts, and (2) if there's an
            -- exception, the standard output we did capture will be reported
            -- to the user.
            liftIO $ Dir.createDirectoryIfMissing True (fromAbsDir (parent exe))
            compilerExeName <-
              view $ compilerPathsL . to (.compiler) . to toFilePath
            withWorkingDir (fromAbsDir (parent file)) $ proc
              compilerExeName
              (ghcArgs ++ [toFilePath file])
              (void . readProcessStdout_)
            runCompiled shouldRun exe

  toPackageName = reverse . drop 1 . dropWhile (/= '-') . reverse

  -- Like words, but splits on both commas and spaces
  wordsComma = splitWhen (\c -> c == ' ' || c == ',')

getPackagesFromImports ::
     FilePath -- ^ script filename
  -> RIO EnvConfig (Set PackageName)
getPackagesFromImports scriptFP = do
  (pns, mns) <- liftIO $ parseImports <$> S8.readFile scriptFP
  if Set.null mns
    then pure pns
    else Set.union pns <$> getPackagesFromModuleNames mns

getPackagesFromModuleNames ::
     Set ModuleName
  -> RIO EnvConfig (Set PackageName)
getPackagesFromModuleNames mns = do
  hash <- hashSnapshot
  withSnapshotCache hash mapSnapshotPackageModules $ \getModulePackages -> do
    pns <- forM (Set.toList mns) $ \mn -> do
      pkgs <- getModulePackages mn
      case pkgs of
        [] -> pure Set.empty
        [pn] -> pure $ Set.singleton pn
        _ -> throwM $ AmbiguousModuleName mn pkgs
    pure $ Set.unions pns `Set.difference` blacklist

hashSnapshot :: RIO EnvConfig SnapshotCacheHash
hashSnapshot = do
  sourceMap <- view $ envConfigL . to (.sourceMap)
  compilerInfo <- getCompilerInfo
  let eitherPliHash (pn, dep)
        | PLImmutable pli <- dep.location = Right $ immutableLocSha pli
        | otherwise = Left pn
      deps = Map.toList sourceMap.deps
  case partitionEithers (map eitherPliHash deps) of
    ([], pliHashes) -> do
      let hashedContent = mconcat $ compilerInfo : pliHashes
      pure
        $ SnapshotCacheHash (SHA256.hashLazyBytes
        $ toLazyByteString hashedContent)
    (mutables, _) -> throwM $ MutableDependenciesForScript mutables

mapSnapshotPackageModules :: RIO EnvConfig (Map PackageName (Set ModuleName))
mapSnapshotPackageModules = do
  sourceMap <- view $ envConfigL . to (.sourceMap)
  installMap <- toInstallMap sourceMap
  (_installedMap, globalDumpPkgs, snapshotDumpPkgs, _localDumpPkgs) <-
    getInstalled installMap
  let globals = dumpedPackageModules sourceMap.globalPkgs globalDumpPkgs
      notHidden = Map.filter (not . (.hidden))
      notHiddenDeps = notHidden sourceMap.deps
      installedDeps = dumpedPackageModules notHiddenDeps snapshotDumpPkgs
      dumpPkgs =
        Set.fromList $ map (pkgName . (.packageIdent)) snapshotDumpPkgs
      notInstalledDeps = Map.withoutKeys notHiddenDeps dumpPkgs
  otherDeps <- for notInstalledDeps $ \dep -> do
    gpd <- liftIO dep.depCommon.gpd
    Set.fromList <$> allExposedModules gpd
  -- source map construction process should guarantee unique package names in
  -- these maps
  pure $ globals <> installedDeps <> otherDeps

dumpedPackageModules ::
     Map PackageName a
  -> [DumpPackage]
  -> Map PackageName (Set ModuleName)
dumpedPackageModules pkgs dumpPkgs =
  let pnames = Map.keysSet pkgs `Set.difference` blacklist
  in  Map.fromList
        [ (pn, dp.exposedModules)
        | dp <- dumpPkgs
        , let PackageIdentifier pn _ = dp.packageIdent
        , pn `Set.member` pnames
        ]

allExposedModules :: PD.GenericPackageDescription -> RIO EnvConfig [ModuleName]
allExposedModules gpd = do
  Platform curArch curOs <- view platformL
  curCompiler <- view actualCompilerVersionL
  let checkCond (PD.OS os) = pure $ os == curOs
      checkCond (PD.Arch arch) = pure $ arch == curArch
      checkCond (PD.Impl compiler range) = case curCompiler of
        ACGhc version ->
          pure $ compiler == GHC && version `withinRange` range
        ACGhcGit {} ->
          pure $ compiler == GHC
      -- currently we don't do flag checking here
      checkCond other = Left other
      mlibrary = snd . C.simplifyCondTree checkCond <$> PD.condLibrary gpd
  pure $ case mlibrary of
    Just lib -> PD.exposedModules lib ++
                map moduleReexportName (PD.reexportedModules lib)
    Nothing  -> mempty

-- | The Stackage project introduced the concept of hidden packages, to deal
-- with conflicting module names. However, this is a relatively recent addition
-- (at time of writing). See:
-- http://www.snoyman.com/blog/2017/01/conflicting-module-names. To kick this
-- thing off a bit better, we're included a blacklist of packages that should
-- never be auto-parsed in.
blacklist :: Set PackageName
blacklist = Set.fromList
  [ mkPackageName "Glob"
  , mkPackageName "HTF"
  , mkPackageName "async-dejafu"
  , mkPackageName "binary-ieee754"
  , mkPackageName "cipher-aes"
  , mkPackageName "cipher-blowfish"
  , mkPackageName "cipher-camellia"
  , mkPackageName "cipher-des"
  , mkPackageName "cipher-rc4"
  , mkPackageName "control-monad-free"
  , mkPackageName "courier"
  , mkPackageName "crypto-api"
  , mkPackageName "crypto-cipher-types"
  , mkPackageName "crypto-numbers"
  , mkPackageName "crypto-pubkey"
  , mkPackageName "crypto-random"
  , mkPackageName "cryptohash"
  , mkPackageName "cryptohash-conduit"
  , mkPackageName "cryptohash-md5"
  , mkPackageName "cryptohash-sha1"
  , mkPackageName "cryptohash-sha256"
  , mkPackageName "fay-base"
  , mkPackageName "gl"
  , mkPackageName "gtk3"
  , mkPackageName "hashmap"
  , mkPackageName "hledger-web"
  , mkPackageName "hxt-unicode"
  , mkPackageName "kawhi"
  , mkPackageName "language-c"
  , mkPackageName "log"
  , mkPackageName "monad-extras"
  , mkPackageName "monads-tf"
  , mkPackageName "nanospec"
  , mkPackageName "newtype-generics"
  , mkPackageName "objective"
  , mkPackageName "plot-gtk3"
  , mkPackageName "prompt"
  , mkPackageName "regex-compat-tdfa"
  , mkPackageName "regex-pcre-builtin"
  , mkPackageName "rerebase"
  , mkPackageName "svg-tree"
  , mkPackageName "zip"
  ]

parseImports :: ByteString -> (Set PackageName, Set ModuleName)
parseImports =
  fold . mapMaybe (parseLine . stripCR') . S8.lines
 where
  -- Remove any carriage pure character present at the end, to support
  -- Windows-style line endings (CRLF)
  stripCR' bs
    | S8.null bs = bs
    | S8.last bs == '\r' = S8.init bs
    | otherwise = bs

  stripPrefix x y
    | x `S8.isPrefixOf` y = Just $ S8.drop (S8.length x) y
    | otherwise = Nothing

  parseLine bs0 = do
    bs1 <- stripPrefix "import " bs0
    let bs2 = S8.dropWhile (== ' ') bs1
        bs3 = fromMaybe bs2 $ stripPrefix "qualified " bs2
    case stripPrefix "\"" bs3 of
      Just bs4 -> do
        pn <- parsePackageNameThrowing $ S8.unpack $ S8.takeWhile (/= '"') bs4
        Just (Set.singleton pn, Set.empty)
      Nothing -> Just
        ( Set.empty
        , Set.singleton
            $ fromString
            $ T.unpack
            $ decodeUtf8With lenientDecode
            $ S8.takeWhile (\c -> c /= ' ' && c /= '(') bs3
        )
