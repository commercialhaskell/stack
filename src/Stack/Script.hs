{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Stack.Script
    ( scriptCmd
    ) where

import           Control.Exception          (assert)
import           Control.Exception.Safe     (throwM)
import           Control.Monad              (unless, forM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Logger
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as S8
import qualified Data.Conduit.List          as CL
import           Data.Foldable              (fold)
import           Data.List.Split            (splitWhen)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe, mapMaybe)
import           Data.Monoid
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Store.VersionTagged   (versionedDecodeOrLoad)
import qualified Data.Text                  as T
import           Path
import           Path.IO
import qualified Stack.Build
import           Stack.BuildPlan            (loadResolver, loadSnapshot)
import           Stack.Exec
import           Stack.GhcPkg               (ghcPkgExeName)
import           Stack.Options.ScriptParser
import           Stack.PackageDump          (getGlobalModuleInfo)
import           Stack.Runners
import           Stack.Types.BuildPlan
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.PackageName
import           Stack.Types.Resolver
import           Stack.Types.StackT
import           Stack.Types.StringError
import           System.FilePath            (dropExtension, replaceExtension)
import           System.Process.Read

-- | Run a Stack Script
scriptCmd :: ScriptOpts -> GlobalOpts -> IO ()
scriptCmd opts go' = do
    let go = go'
            { globalConfigMonoid = (globalConfigMonoid go')
                { configMonoidInstallGHC = First $ Just True
                }
            , globalStackYaml = SYLNoConfig
            }
    withBuildConfigAndLock go $ \lk -> do
        -- Some warnings in case the user somehow tries to set a
        -- stack.yaml location. Note that in this functions we use
        -- logError instead of logWarn because, when using the
        -- interpreter mode, only error messages are shown. See:
        -- https://github.com/commercialhaskell/stack/issues/3007
        case globalStackYaml go' of
          SYLOverride fp -> $logError $ T.pack
            $ "Ignoring override stack.yaml file for script command: " ++ fp
          SYLDefault -> return ()
          SYLNoConfig -> assert False (return ())

        config <- view configL
        menv <- liftIO $ configEnvOverride config defaultEnvSettings
        wc <- view $ actualCompilerVersionL.whichCompilerL

        targetsSet <-
            case soPackages opts of
                [] ->
                    -- Using the import parser
                    getPackagesFromImports (globalResolver go) (soFile opts)
                packages -> do
                    let targets = concatMap wordsComma packages
                    targets' <- mapM parsePackageNameFromString targets
                    return $ Set.fromList targets'

        unless (Set.null targetsSet) $ do
            -- Optimization: use the relatively cheap ghc-pkg list
            -- --simple-output to check which packages are installed
            -- already. If all needed packages are available, we can
            -- skip the (rather expensive) build call below.
            bss <- sinkProcessStdout
                Nothing menv (ghcPkgExeName wc)
                ["list", "--simple-output"] CL.consume -- FIXME use the package info from envConfigPackages, or is that crazy?
            let installed = Set.fromList
                          $ map toPackageName
                          $ words
                          $ S8.unpack
                          $ S8.concat bss
            if Set.null $ Set.difference (Set.map packageNameString targetsSet) installed
                then $logDebug "All packages already installed"
                else do
                    $logDebug "Missing packages, performing installation"
                    Stack.Build.build (const $ return ()) lk defaultBuildOptsCLI
                        { boptsCLITargets = map packageNameText $ Set.toList targetsSet
                        }

        let ghcArgs = concat
                [ ["-hide-all-packages"]
                , map (\x -> "-package" ++ x)
                    $ Set.toList
                    $ Set.insert "base"
                    $ Set.map packageNameString targetsSet
                , case soCompile opts of
                    SEInterpret -> []
                    SECompile -> []
                    SEOptimize -> ["-O2"]
                ]
        munlockFile lk -- Unlock before transferring control away.
        case soCompile opts of
          SEInterpret -> exec menv ("run" ++ compilerExeName wc)
                (ghcArgs ++ soFile opts : soArgs opts)
          _ -> do
            file <- resolveFile' $ soFile opts
            let dir = parent file
            -- use sinkProcessStdout to ensure a ProcessFailed
            -- exception is generated for better error messages
            sinkProcessStdout
              (Just dir)
              menv
              (compilerExeName wc)
              (ghcArgs ++ [soFile opts])
              CL.sinkNull
            exec menv (toExeName $ toFilePath file) (soArgs opts)
  where
    toPackageName = reverse . drop 1 . dropWhile (/= '-') . reverse

    -- Like words, but splits on both commas and spaces
    wordsComma = splitWhen (\c -> c == ' ' || c == ',')

    toExeName fp =
      if isWindows
        then replaceExtension fp "exe"
        else dropExtension fp

isWindows :: Bool
#ifdef WINDOWS
isWindows = True
#else
isWindows = False
#endif

-- | Returns packages that need to be installed, and all of the core
-- packages. Reason for the core packages:

-- Ideally we'd have the list of modules per core package listed in
-- the build plan, but that doesn't exist yet. Next best would be to
-- list the modules available at runtime, but that gets tricky with when we install GHC. Instead, we'll just list all core packages
getPackagesFromImports :: Maybe AbstractResolver
                       -> FilePath
                       -> StackT EnvConfig IO (Set PackageName)
getPackagesFromImports Nothing _ = throwM NoResolverWhenUsingNoLocalConfig
getPackagesFromImports (Just (ARResolver (ResolverSnapshot name))) scriptFP = do
    mi <- loadModuleInfo name
    getPackagesFromModuleInfo mi scriptFP
getPackagesFromImports (Just (ARResolver (ResolverCompiler compiler))) scriptFP = do
  menv <- getMinimalEnvOverride
  mi <- getGlobalModuleInfo menv $ whichCompiler compiler
  getPackagesFromModuleInfo mi scriptFP
getPackagesFromImports (Just aresolver) _ = throwM $ InvalidResolverForNoLocalConfig $ show aresolver

getPackagesFromModuleInfo
  :: ModuleInfo
  -> FilePath -- ^ script filename
  -> StackT EnvConfig IO (Set PackageName)
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
                                    , S8.unpack $ unModuleName mn
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
    [ $(mkPackageName "async-dejafu")
    , $(mkPackageName "monads-tf")
    , $(mkPackageName "crypto-api")
    , $(mkPackageName "fay-base")
    , $(mkPackageName "hashmap")
    , $(mkPackageName "hxt-unicode")
    , $(mkPackageName "hledger-web")
    , $(mkPackageName "plot-gtk3")
    , $(mkPackageName "gtk3")
    , $(mkPackageName "regex-pcre-builtin")
    , $(mkPackageName "regex-compat-tdfa")
    , $(mkPackageName "log")
    , $(mkPackageName "zip")
    , $(mkPackageName "monad-extras")
    , $(mkPackageName "control-monad-free")
    , $(mkPackageName "prompt")
    , $(mkPackageName "kawhi")
    , $(mkPackageName "language-c")
    , $(mkPackageName "gl")
    , $(mkPackageName "svg-tree")
    , $(mkPackageName "Glob")
    , $(mkPackageName "nanospec")
    , $(mkPackageName "HTF")
    , $(mkPackageName "courier")
    , $(mkPackageName "newtype-generics")
    , $(mkPackageName "objective")
    , $(mkPackageName "binary-ieee754")
    , $(mkPackageName "rerebase")
    , $(mkPackageName "cipher-aes")
    , $(mkPackageName "cipher-blowfish")
    , $(mkPackageName "cipher-camellia")
    , $(mkPackageName "cipher-des")
    , $(mkPackageName "cipher-rc4")
    , $(mkPackageName "crypto-cipher-types")
    , $(mkPackageName "crypto-numbers")
    , $(mkPackageName "crypto-pubkey")
    , $(mkPackageName "crypto-random")
    , $(mkPackageName "cryptohash")
    , $(mkPackageName "cryptohash-conduit")
    , $(mkPackageName "cryptohash-md5")
    , $(mkPackageName "cryptohash-sha1")
    , $(mkPackageName "cryptohash-sha256")
    ]

toModuleInfo :: LoadedSnapshot -> ModuleInfo
toModuleInfo =
      mconcat
    . map (\(pn, lpi) ->
            ModuleInfo
            $ Map.fromList
            $ map (\mn -> (mn, Set.singleton pn))
            $ Set.toList
            $ lpiExposedModules lpi)
    . filter (\(pn, lpi) ->
            not (lpiHide lpi) &&
            pn `Set.notMember` blacklist)
    . Map.toList
    . lsPackages

-- | Where to store module info caches
moduleInfoCache :: SnapName -> StackT EnvConfig IO (Path Abs File)
moduleInfoCache name = do
    root <- view stackRootL
    platform <- platformGhcVerOnlyRelDir
    name' <- parseRelDir $ T.unpack $ renderSnapName name
    -- These probably can't vary at all based on platform, even in the
    -- future, so it's safe to call this unnecessarily paranoid.
    return (root </> $(mkRelDir "script") </> name' </> platform </> $(mkRelFile "module-info.cache"))

loadModuleInfo :: SnapName -> StackT EnvConfig IO ModuleInfo
loadModuleInfo name = do
    path <- moduleInfoCache name
    $(versionedDecodeOrLoad moduleInfoVC) path $
      fmap toModuleInfo $ loadResolver (ResolverSnapshot name) >>= loadSnapshot

parseImports :: ByteString -> (Set PackageName, Set ModuleName)
parseImports =
    fold . mapMaybe (parseLine . stripCR) . S8.lines
  where
    -- Remove any carriage return character present at the end, to
    -- support Windows-style line endings (CRLF)
    stripCR bs
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
                pn <- parsePackageNameFromString $ S8.unpack $ S8.takeWhile (/= '"') bs4
                Just (Set.singleton pn, Set.empty)
            Nothing -> Just
                ( Set.empty
                , Set.singleton
                    $ ModuleName
                    $ S8.takeWhile (\c -> c /= ' ' && c /= '(') bs3
                )
