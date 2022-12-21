{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Stack.Script
    ( scriptCmd
    ) where

import           Data.ByteString.Builder ( toLazyByteString )
import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit.List as CL
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
import           Path ( parent )
import           Path.IO ( getModificationTime, resolveFile' )
import qualified RIO.Directory as Dir
import           RIO.Process ( exec, proc, readProcessStdout_, withWorkingDir )
import qualified RIO.Text as T
import qualified Stack.Build
import           Stack.Build.Installed
import           Stack.Constants ( osIsWindows )
import           Stack.PackageDump
import           Stack.Prelude
import           Stack.Options.ScriptParser
import           Stack.Runners
import           Stack.Setup ( withNewLocalBuildTargets )
import           Stack.SourceMap ( getCompilerInfo, immutableLocSha )
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.SourceMap
import           System.FilePath ( dropExtension, replaceExtension )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Script" module.
data ScriptException
    = MutableDependenciesForScript [PackageName]
    | AmbiguousModuleName ModuleName [PackageName]
    | ArgumentsWithNoRunInvalid
    | NoRunWithoutCompilationInvalid
  deriving (Show, Typeable)

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

-- | Run a Stack Script
scriptCmd :: ScriptOpts -> RIO Runner ()
scriptCmd opts = do
    -- Some warnings in case the user somehow tries to set a
    -- stack.yaml location. Note that in this functions we use
    -- logError instead of logWarn because, when using the
    -- interpreter mode, only error messages are shown. See:
    -- https://github.com/commercialhaskell/stack/issues/3007
    view (globalOptsL.to globalStackYaml) >>= \case
      SYLOverride fp -> logError $
        "Ignoring override stack.yaml file for script command: " <>
        fromString (toFilePath fp)
      SYLGlobalProject -> logError "Ignoring SYLGlobalProject for script command"
      SYLDefault -> pure ()
      SYLNoProject _ -> assert False (pure ())

    file <- resolveFile' $ soFile opts

    isNoRunCompile <- fromFirstFalse . configMonoidNoRunCompile <$>
                             view (globalOptsL.to globalConfigMonoid)

    let scriptDir = parent file
        modifyGO go = go
            { globalConfigMonoid = (globalConfigMonoid go)
                { configMonoidInstallGHC = FirstTrue $ Just True
                }
            , globalStackYaml = SYLNoProject $ soScriptExtraDeps opts
            }
        (shouldRun, shouldCompile) = if isNoRunCompile
          then (NoRun, SECompile)
          else (soShouldRun opts, soCompile opts)

    case shouldRun of
      YesRun -> pure ()
      NoRun -> do
        unless (null $ soArgs opts) $ throwIO ArgumentsWithNoRunInvalid
        case shouldCompile of
          SEInterpret -> throwIO NoRunWithoutCompilationInvalid
          SECompile -> pure ()
          SEOptimize -> pure ()

    -- Optimization: if we're compiling, and the executable is newer
    -- than the source file, run it immediately.
    local (over globalOptsL modifyGO) $
      case shouldCompile of
        SEInterpret -> longWay shouldRun shouldCompile file scriptDir
        SECompile -> shortCut shouldRun shouldCompile file scriptDir
        SEOptimize -> shortCut shouldRun shouldCompile file scriptDir

  where
  runCompiled shouldRun file = do
    let exeName = toExeName $ toFilePath file
    case shouldRun of
      YesRun -> exec exeName (soArgs opts)
      NoRun -> logInfo $ "Compilation finished, executable available at " <> fromString exeName

  shortCut shouldRun shouldCompile file scriptDir =
    handleIO (const $ longWay shouldRun shouldCompile file scriptDir) $ do
      srcMod <- getModificationTime file
      exeMod <- Dir.getModificationTime $ toExeName $ toFilePath file
      if srcMod < exeMod
        then runCompiled shouldRun file
        else longWay shouldRun shouldCompile file scriptDir

  longWay shouldRun shouldCompile file scriptDir =
    withConfig YesReexec $
    withDefaultEnvConfig $ do
      config <- view configL
      menv <- liftIO $ configProcessContextSettings config defaultEnvSettings
      withProcessContext menv $ do
        colorFlag <- appropriateGhcColorFlag

        targetsSet <-
            case soPackages opts of
                [] -> do
                    -- Using the import parser
                    getPackagesFromImports (soFile opts)
                packages -> do
                    let targets = concatMap wordsComma packages
                    targets' <- mapM parsePackageNameThrowing targets
                    pure $ Set.fromList targets'

        unless (Set.null targetsSet) $ do
            -- Optimization: use the relatively cheap ghc-pkg list
            -- --simple-output to check which packages are installed
            -- already. If all needed packages are available, we can
            -- skip the (rather expensive) build call below.
            GhcPkgExe pkg <- view $ compilerPathsL.to cpPkg
            -- https://github.com/haskell/process/issues/251
            bss <- snd <$> sinkProcessStderrStdout (toFilePath pkg)
                ["list", "--simple-output"] CL.sinkNull CL.consume -- FIXME use the package info from envConfigPackages, or is that crazy?
            let installed = Set.fromList
                          $ map toPackageName
                          $ words
                          $ S8.unpack
                          $ S8.concat bss
            if Set.null $ Set.difference (Set.map packageNameString targetsSet) installed
                then logDebug "All packages already installed"
                else do
                    logDebug "Missing packages, performing installation"
                    let targets = map (T.pack . packageNameString) $ Set.toList targetsSet
                    withNewLocalBuildTargets targets $ Stack.Build.build Nothing

        let ghcArgs = concat
                [ ["-i", "-i" ++ toFilePath scriptDir]
                , ["-hide-all-packages"]
                , maybeToList colorFlag
                , map (\x -> "-package" ++ x)
                    $ Set.toList
                    $ Set.insert "base"
                    $ Set.map packageNameString targetsSet
                , case shouldCompile of
                    SEInterpret -> []
                    SECompile -> []
                    SEOptimize -> ["-O2"]
                , soGhcOptions opts
                ]
        case shouldCompile of
          SEInterpret -> do
            interpret <- view $ compilerPathsL.to cpInterpreter
            exec (toFilePath interpret)
                (ghcArgs ++ toFilePath file : soArgs opts)
          _ -> do
            -- Use readProcessStdout_ so that (1) if GHC does send any output
            -- to stdout, we capture it and stop it from being sent to our
            -- stdout, which could break scripts, and (2) if there's an
            -- exception, the standard output we did capture will be reported
            -- to the user.
            compilerExeName <- view $ compilerPathsL.to cpCompiler.to toFilePath
            withWorkingDir (toFilePath scriptDir) $ proc
              compilerExeName
              (ghcArgs ++ [toFilePath file])
              (void . readProcessStdout_)
            runCompiled shouldRun file

  toPackageName = reverse . drop 1 . dropWhile (/= '-') . reverse

  -- Like words, but splits on both commas and spaces
  wordsComma = splitWhen (\c -> c == ' ' || c == ',')

  toExeName fp =
    if osIsWindows
      then replaceExtension fp "exe"
      else dropExtension fp

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
    sourceMap <- view $ envConfigL . to envConfigSourceMap
    compilerInfo <- getCompilerInfo
    let eitherPliHash (pn, dep) | PLImmutable pli <- dpLocation dep =
                                    Right $ immutableLocSha pli
                                | otherwise =
                                    Left pn
        deps = Map.toList (smDeps sourceMap)
    case partitionEithers (map eitherPliHash deps) of
        ([], pliHashes) -> do
            let hashedContent = mconcat $ compilerInfo : pliHashes
            pure $ SnapshotCacheHash (SHA256.hashLazyBytes $ toLazyByteString hashedContent)
        (mutables, _) ->
            throwM $ MutableDependenciesForScript mutables

mapSnapshotPackageModules :: RIO EnvConfig (Map PackageName (Set ModuleName))
mapSnapshotPackageModules = do
    sourceMap <- view $ envConfigL . to envConfigSourceMap
    installMap <- toInstallMap sourceMap
    (_installedMap, globalDumpPkgs, snapshotDumpPkgs, _localDumpPkgs) <-
        getInstalled installMap
    let globals = dumpedPackageModules (smGlobal sourceMap) globalDumpPkgs
        notHidden = Map.filter (not . dpHidden)
        notHiddenDeps = notHidden $ smDeps sourceMap
        installedDeps = dumpedPackageModules notHiddenDeps snapshotDumpPkgs
        dumpPkgs = Set.fromList $ map (pkgName . dpPackageIdent) snapshotDumpPkgs
        notInstalledDeps = Map.withoutKeys notHiddenDeps dumpPkgs
    otherDeps <- for notInstalledDeps $ \dep -> do
        gpd <- liftIO $ cpGPD (dpCommon dep)
        Set.fromList <$> allExposedModules gpd
    -- source map construction process should guarantee unique package names
    -- in these maps
    pure $ globals <> installedDeps <> otherDeps

dumpedPackageModules :: Map PackageName a
                     -> [DumpPackage]
                     -> Map PackageName (Set ModuleName)
dumpedPackageModules pkgs dumpPkgs =
    let pnames = Map.keysSet pkgs `Set.difference` blacklist
    in Map.fromList
           [ (pn, dpExposedModules)
           | DumpPackage {..} <- dumpPkgs
           , let PackageIdentifier pn _ = dpPackageIdent
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
  pure $ case mlibrary  of
    Just lib -> PD.exposedModules lib ++
                map moduleReexportName (PD.reexportedModules lib)
    Nothing  -> mempty

-- | The Stackage project introduced the concept of hidden packages,
-- to deal with conflicting module names. However, this is a
-- relatively recent addition (at time of writing). See:
-- http://www.snoyman.com/blog/2017/01/conflicting-module-names. To
-- kick this thing off a bit better, we're included a blacklist of
-- packages that should never be auto-parsed in.
blacklist :: Set PackageName
blacklist = Set.fromList
    [ mkPackageName "async-dejafu"
    , mkPackageName "monads-tf"
    , mkPackageName "crypto-api"
    , mkPackageName "fay-base"
    , mkPackageName "hashmap"
    , mkPackageName "hxt-unicode"
    , mkPackageName "hledger-web"
    , mkPackageName "plot-gtk3"
    , mkPackageName "gtk3"
    , mkPackageName "regex-pcre-builtin"
    , mkPackageName "regex-compat-tdfa"
    , mkPackageName "log"
    , mkPackageName "zip"
    , mkPackageName "monad-extras"
    , mkPackageName "control-monad-free"
    , mkPackageName "prompt"
    , mkPackageName "kawhi"
    , mkPackageName "language-c"
    , mkPackageName "gl"
    , mkPackageName "svg-tree"
    , mkPackageName "Glob"
    , mkPackageName "nanospec"
    , mkPackageName "HTF"
    , mkPackageName "courier"
    , mkPackageName "newtype-generics"
    , mkPackageName "objective"
    , mkPackageName "binary-ieee754"
    , mkPackageName "rerebase"
    , mkPackageName "cipher-aes"
    , mkPackageName "cipher-blowfish"
    , mkPackageName "cipher-camellia"
    , mkPackageName "cipher-des"
    , mkPackageName "cipher-rc4"
    , mkPackageName "crypto-cipher-types"
    , mkPackageName "crypto-numbers"
    , mkPackageName "crypto-pubkey"
    , mkPackageName "crypto-random"
    , mkPackageName "cryptohash"
    , mkPackageName "cryptohash-conduit"
    , mkPackageName "cryptohash-md5"
    , mkPackageName "cryptohash-sha1"
    , mkPackageName "cryptohash-sha256"
    ]

parseImports :: ByteString -> (Set PackageName, Set ModuleName)
parseImports =
    fold . mapMaybe (parseLine . stripCR') . S8.lines
  where
    -- Remove any carriage pure character present at the end, to
    -- support Windows-style line endings (CRLF)
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
