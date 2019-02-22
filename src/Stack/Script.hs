{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Stack.Script
    ( scriptCmd
    ) where

import           Stack.Prelude
import qualified Data.ByteString.Char8      as S8
import qualified Data.Conduit.List          as CL
import           Data.List.Split            (splitWhen)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import qualified Distribution.PackageDescription as PD
import           Distribution.Types.PackageName (mkPackageName)
import           Path
import           Path.IO
import qualified Stack.Build
import           Stack.Build.Installed
import           Stack.Constants            (osIsWindows)
import           Stack.GhcPkg               (ghcPkgExeName)
import           Stack.PackageDump
import           Stack.Options.ScriptParser
import           Stack.Runners
import           Stack.Setup                (withNewLocalBuildTargets)
import           Stack.Types.BuildPlan
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.SourceMap
import           System.FilePath            (dropExtension, replaceExtension)
import           RIO.Process
import qualified RIO.Text as T

-- | Run a Stack Script
scriptCmd :: ScriptOpts -> GlobalOpts -> IO ()
scriptCmd opts go' = do
    file <- resolveFile' $ soFile opts
    let scriptDir = parent file
        go = go'
            { globalConfigMonoid = (globalConfigMonoid go')
                { configMonoidInstallGHC = First $ Just True
                }
            , globalStackYaml = SYLNoConfig scriptDir
            }
    withDefaultBuildConfigAndLock go $ \lk -> do
      -- Some warnings in case the user somehow tries to set a
      -- stack.yaml location. Note that in this functions we use
      -- logError instead of logWarn because, when using the
      -- interpreter mode, only error messages are shown. See:
      -- https://github.com/commercialhaskell/stack/issues/3007
      case globalStackYaml go' of
        SYLOverride fp -> logError $
          "Ignoring override stack.yaml file for script command: " <>
          fromString fp
        SYLDefault -> return ()
        SYLNoConfig _ -> assert False (return ())

      config <- view configL
      menv <- liftIO $ configProcessContextSettings config defaultEnvSettings
      withProcessContext menv $ do
        wc <- view $ actualCompilerVersionL.whichCompilerL
        colorFlag <- appropriateGhcColorFlag

        targetsSet <-
            case soPackages opts of
                [] -> do
                    -- Using the import parser
                    moduleInfo <- getModuleInfo
                    getPackagesFromModuleInfo moduleInfo (soFile opts)
                packages -> do
                    let targets = concatMap wordsComma packages
                    targets' <- mapM parsePackageNameThrowing targets
                    return $ Set.fromList targets'

        unless (Set.null targetsSet) $ do
            -- Optimization: use the relatively cheap ghc-pkg list
            -- --simple-output to check which packages are installed
            -- already. If all needed packages are available, we can
            -- skip the (rather expensive) build call below.
            bss <- sinkProcessStdout
                (ghcPkgExeName wc)
                ["list", "--simple-output"] CL.consume -- FIXME use the package info from envConfigPackages, or is that crazy?
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
                    withNewLocalBuildTargets targets $ Stack.Build.build Nothing lk

        let ghcArgs = concat
                [ ["-i", "-i" ++ toFilePath scriptDir]
                , ["-hide-all-packages"]
                , maybeToList colorFlag
                , map (\x -> "-package" ++ x)
                    $ Set.toList
                    $ Set.insert "base"
                    $ Set.map packageNameString targetsSet
                , case soCompile opts of
                    SEInterpret -> []
                    SECompile -> []
                    SEOptimize -> ["-O2"]
                , soGhcOptions opts
                ]
        munlockFile lk -- Unlock before transferring control away.
        case soCompile opts of
          SEInterpret -> exec ("run" ++ compilerExeName wc)
                (ghcArgs ++ toFilePath file : soArgs opts)
          _ -> do
            -- Use readProcessStdout_ so that (1) if GHC does send any output
            -- to stdout, we capture it and stop it from being sent to our
            -- stdout, which could break scripts, and (2) if there's an
            -- exception, the standard output we did capture will be reported
            -- to the user.
            withWorkingDir (toFilePath scriptDir) $ proc
              (compilerExeName wc)
              (ghcArgs ++ [toFilePath file])
              (void . readProcessStdout_)
            exec (toExeName $ toFilePath file) (soArgs opts)
  where
    toPackageName = reverse . drop 1 . dropWhile (/= '-') . reverse

    -- Like words, but splits on both commas and spaces
    wordsComma = splitWhen (\c -> c == ' ' || c == ',')

    toExeName fp =
      if osIsWindows
        then replaceExtension fp "exe"
        else dropExtension fp

getPackagesFromModuleInfo
  :: ModuleInfo
  -> FilePath -- ^ script filename
  -> RIO EnvConfig (Set PackageName)
getPackagesFromModuleInfo mi scriptFP = do
    (pns1, mns) <- liftIO $ parseImports <$> S8.readFile scriptFP
    pns2 <-
        if Set.null mns
            then return Set.empty
            else do
                pns <- forM (Set.toList mns) $ \mn ->
                    case Map.lookup mn $ miModules mi of
                        Just pns ->
                            case Set.toList pns of
                                [] -> assert False $ return Set.empty
                                [pn] -> return $ Set.singleton pn
                                pns' -> throwString $ concat
                                    [ "Module "
                                    , moduleNameString mn
                                    , " appears in multiple packages: "
                                    , unwords $ map packageNameString pns'
                                    ]
                        Nothing -> return Set.empty
                return $ Set.unions pns `Set.difference` blacklist
    return $ Set.union pns1 pns2

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

getModuleInfo :: HasEnvConfig env => RIO env ModuleInfo
getModuleInfo = do
    sourceMap <- view $ envConfigL . to envConfigSourceMap
    installMap <- toInstallMap sourceMap
    (_installedMap, globalDumpPkgs, snapshotDumpPkgs, _localDumpPkgs) <-
        getInstalled installMap
    let globals = toModuleInfo (smGlobal sourceMap) globalDumpPkgs
        notHiddenDeps = notHidden $ smDeps sourceMap
        installedDeps = toModuleInfo notHiddenDeps snapshotDumpPkgs
        dumpPkgs = Set.fromList $ map (pkgName . dpPackageIdent) snapshotDumpPkgs
        notInstalledDeps = Map.withoutKeys notHiddenDeps dumpPkgs
    otherDeps <- liftIO $
                 fmap (Map.fromListWith mappend . concat) $
                 forM (Map.toList notInstalledDeps) $ \(pname, dep) -> do
        gpd <- cpGPD (dpCommon dep)
        let modules = maybe [] PD.exposedModules $
              maybe (PD.library $ PD.packageDescription gpd) (Just . PD.condTreeData) $
              PD.condLibrary gpd
        return [ (m, Set.singleton pname) | m <- modules ]
    return $ globals <> installedDeps <> ModuleInfo otherDeps
  where
    notHidden = Map.filter (not . dpHidden)
    toModuleInfo pkgs dumpPkgs =
        let pnames = Map.keysSet pkgs `Set.difference` blacklist
            modules =
                Map.fromListWith mappend
                    [ (m, Set.singleton pn)
                    | DumpPackage {..} <- dumpPkgs
                    , let PackageIdentifier pn _ = dpPackageIdent
                    , pn `Set.member` pnames
                    , m <- Set.toList dpExposedModules
                    ]
        in ModuleInfo modules

parseImports :: ByteString -> (Set PackageName, Set ModuleName)
parseImports =
    fold . mapMaybe (parseLine . stripCR') . S8.lines
  where
    -- Remove any carriage return character present at the end, to
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
