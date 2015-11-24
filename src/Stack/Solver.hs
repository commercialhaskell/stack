{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Stack.Solver
    ( cabalSolver
    , solveExtraDeps
    ) where

import           Control.Applicative
import           Control.Exception.Enclosed  (tryIO)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Aeson.Extended         (object, (.=), toJSON, logJSONWarnings)
import qualified Data.ByteString             as S
import           Data.Either
import qualified Data.HashMap.Strict         as HashMap
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import qualified Data.Yaml                   as Yaml
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Path.IO                     (parseRelAsAbsDir)
import           Prelude
import           Stack.BuildPlan
import           Stack.Setup.Installed
import           Stack.Types
import           System.Directory            (copyFile,
                                              createDirectoryIfMissing,
                                              getTemporaryDirectory)
import qualified System.FilePath             as FP
import           System.IO.Temp              (withSystemTempDirectory)
import           System.Process.Read

cabalSolver :: (MonadIO m, MonadLogger m, MonadMask m, MonadBaseControl IO m, MonadReader env m, HasConfig env)
            => WhichCompiler
            -> [Path Abs Dir] -- ^ cabal files
            -> Map PackageName Version -- ^ constraints
            -> Map PackageName (Map FlagName Bool) -- ^ user-specified flags
            -> [String] -- ^ additional arguments
            -> m (CompilerVersion, Map PackageName (Version, Map FlagName Bool))
cabalSolver wc cabalfps constraints userFlags cabalArgs = withSystemTempDirectory "cabal-solver" $ \dir -> do
    when (null cabalfps) $ throwM SolverNoCabalFiles
    configLines <- getCabalConfig dir constraints
    let configFile = dir FP.</> "cabal.config"
    liftIO $ S.writeFile configFile $ encodeUtf8 $ T.unlines configLines

    menv0 <- getMinimalEnvOverride
    mghc <- findExecutable menv0 "ghc"
    platform <- asks getPlatform
    menv <-
        case mghc of
            Just _ -> return menv0
            Nothing -> do
                localPrograms <- asks $ configLocalPrograms . getConfig
                tools <- listInstalled localPrograms
                let ghcName = $(mkPackageName "ghc")
                case [version | Tool (PackageIdentifier name version) <- tools, name == ghcName] of
                    [] -> throwM SolverMissingGHC
                    versions -> do
                        let version = maximum versions
                        $logInfo $ "No GHC on path, selecting: " <>
                                   T.pack (versionString version)
                        ed <- extraDirs $ Tool $ PackageIdentifier ghcName version
                        mkEnvOverride platform
                            $ augmentPathMap (edBins ed)
                            $ unEnvOverride menv0
    mcabal <- findExecutable menv "cabal"
    case mcabal of
        Nothing -> throwM SolverMissingCabalInstall
        Just _ -> return ()

    compilerVersion <- getCompilerVersion menv wc

    -- Run from a temporary directory to avoid cabal getting confused by any
    -- sandbox files, see:
    -- https://github.com/commercialhaskell/stack/issues/356
    --
    -- In theory we could use --ignore-sandbox, but not all versions of cabal
    -- support it.
    tmpdir <- liftIO getTemporaryDirectory >>= parseRelAsAbsDir

    let args = ("--config-file=" ++ configFile)
             : "install"
             : "--enable-tests"
             : "--enable-benchmarks"
             : "-v"
             : "--dry-run"
             : "--only-dependencies"
             : "--reorder-goals"
             : "--max-backjumps=-1"
             : "--package-db=clear"
             : "--package-db=global"
             : cabalArgs ++
               toConstraintArgs userFlags ++
               fmap toFilePath cabalfps ++
               ["--ghcjs" | wc == Ghcjs]

    $logInfo "Asking cabal to calculate a build plan, please wait"

    menv' <- mkEnvOverride platform
           $ Map.delete "GHCJS_PACKAGE_PATH"
           $ Map.delete "GHC_PACKAGE_PATH" $ unEnvOverride menv
    bs <- readProcessStdout (Just tmpdir) menv' "cabal" args
    let ls = drop 1
           $ dropWhile (not . T.isPrefixOf "In order, ")
           $ T.lines
           $ decodeUtf8 bs
        (errs, pairs) = partitionEithers $ map parseLine ls
    if null errs
        then return (compilerVersion, Map.fromList pairs)
        else error $ "Could not parse cabal-install output: " ++ show errs
  where
    parseLine t0 = maybe (Left t0) Right $ do
        -- get rid of (new package) and (latest: ...) bits
        ident':flags' <- Just $ T.words $ T.takeWhile (/= '(') t0
        PackageIdentifier name version <-
            parsePackageIdentifierFromString $ T.unpack ident'
        flags <- mapM parseFlag flags'
        Just (name, (version, Map.fromList flags))
    parseFlag t0 = do
        flag <- parseFlagNameFromString $ T.unpack t1
        return (flag, enabled)
      where
        (t1, enabled) =
            case T.stripPrefix "-" t0 of
                Nothing ->
                    case T.stripPrefix "+" t0 of
                        Nothing -> (t0, True)
                        Just x -> (x, True)
                Just x -> (x, False)
    toConstraintArgs userFlagMap =
        [formatFlagConstraint package flag enabled | (package, fs) <- Map.toList userFlagMap
                                                   , (flag, enabled) <- Map.toList fs]
    formatFlagConstraint package flag enabled =
        let sign = if enabled then '+' else '-'
        in
        "--constraint=" ++ unwords [packageNameString package, sign : flagNameString flag]

getCabalConfig :: (MonadReader env m, HasConfig env, MonadIO m, MonadThrow m)
               => FilePath -- ^ temp dir
               -> Map PackageName Version -- ^ constraints
               -> m [Text]
getCabalConfig dir constraints = do
    indices <- asks $ configPackageIndices . getConfig
    remotes <- mapM goIndex indices
    let cache = T.pack $ "remote-repo-cache: " ++ dir
    return $ cache : remotes ++ map goConstraint (Map.toList constraints)
  where
    goIndex index = do
        src <- configPackageIndex $ indexName index
        let dstdir = dir FP.</> T.unpack (indexNameText $ indexName index)
            dst = dstdir FP.</> "00-index.tar"
        liftIO $ void $ tryIO $ do
            createDirectoryIfMissing True dstdir
            copyFile (toFilePath src) dst
        return $ T.concat
            [ "remote-repo: "
            , indexNameText $ indexName index
            , ":http://0.0.0.0/fake-url"
            ]

    goConstraint (name, version) = T.concat
        [ "constraint: "
        , T.pack $ packageNameString name
        , "=="
        , T.pack $ versionString version
        ]

-- | Determine missing extra-deps
solveExtraDeps :: (MonadReader env m, HasEnvConfig env, MonadIO m, MonadMask m, MonadLogger m, MonadBaseControl IO m, HasHttpManager env)
               => Bool -- ^ modify stack.yaml?
               -> m ()
solveExtraDeps modStackYaml = do
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig
    snapshot <-
        case bcResolver bconfig of
            ResolverSnapshot snapName -> liftM mbpPackages $ loadMiniBuildPlan snapName
            ResolverCompiler _ -> return Map.empty
            ResolverCustom _ url -> liftM mbpPackages $ parseCustomMiniBuildPlan
                (bcStackYaml bconfig)
                url

    let packages = Map.union
            (bcExtraDeps bconfig)
            (mpiVersion <$> snapshot)

    wc <- getWhichCompiler
    (_compilerVersion, extraDeps) <- cabalSolver
        wc
        (Map.keys $ envConfigPackages econfig)
        packages
        (bcFlags bconfig)
        []

    let newDeps = extraDeps `Map.difference` packages
        newFlags = Map.filter (not . Map.null) $ fmap snd newDeps

    $logInfo "This command is not guaranteed to give you a perfect build plan"
    if Map.null newDeps
        then $logInfo "No needed changes found"
        else do
            $logInfo "It's possible that even with the changes generated below, you will still need to do some manual tweaking"
            let o = object
                    $ ("extra-deps" .= map fromTuple (Map.toList $ fmap fst newDeps))
                    : (if Map.null newFlags
                        then []
                        else ["flags" .= newFlags])
            mapM_ $logInfo $ T.lines $ decodeUtf8 $ Yaml.encode o

    if modStackYaml
      then do
        let fp = toFilePath $ bcStackYaml bconfig
        obj <- liftIO (Yaml.decodeFileEither fp) >>= either throwM return
        (ProjectAndConfigMonoid project _, warnings) <-
            liftIO (Yaml.decodeFileEither fp) >>= either throwM return
        logJSONWarnings fp warnings
        let obj' =
                HashMap.insert "extra-deps"
                    (toJSON $ map fromTuple $ Map.toList
                            $ Map.union (projectExtraDeps project) (fmap fst newDeps))
              $ HashMap.insert ("flags" :: Text)
                    (toJSON $ Map.union (projectFlags project) newFlags)
                obj
        liftIO $ Yaml.encodeFile fp obj'
        $logInfo $ T.pack $ "Updated " ++ fp
      else do
        $logInfo ""
        $logInfo "To automatically modify your stack.yaml file, rerun with '--modify-stack-yaml'"
