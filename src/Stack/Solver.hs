{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Stack.Solver
    ( cabalSolver
    , getGhcVersion
    , solveExtraDeps
    ) where

import           Control.Exception.Enclosed  (tryIO)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Aeson                  (object, (.=), toJSON)
import qualified Data.ByteString             as S
import qualified Data.ByteString.Char8       as S8
import           Data.Either
import qualified Data.HashMap.Strict         as HashMap
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import qualified Data.Yaml                   as Yaml
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Stack.BuildPlan
import           Stack.Types
import           System.Directory            (copyFile,
                                              createDirectoryIfMissing,
                                              getTemporaryDirectory)
import qualified System.FilePath             as FP
import           System.IO.Temp
import           System.Process.Read

cabalSolver :: (MonadIO m, MonadLogger m, MonadMask m, MonadBaseControl IO m, MonadReader env m, HasConfig env)
            => [Path Abs Dir] -- ^ cabal files
            -> [String] -- ^ additional arguments, usually constraints
            -> m (MajorVersion, Map PackageName (Version, Map FlagName Bool))
cabalSolver cabalfps cabalArgs = withSystemTempDirectory "cabal-solver" $ \dir -> do
    configLines <- getCabalConfig dir
    let configFile = dir FP.</> "cabal.config"
    liftIO $ S.writeFile configFile $ encodeUtf8 $ T.unlines configLines

    menv <- getMinimalEnvOverride
    ghcMajorVersion <- getGhcMajorVersion menv

    -- Run from a temporary directory to avoid cabal getting confused by any
    -- sandbox files, see:
    -- https://github.com/commercialhaskell/stack/issues/356
    --
    -- In theory we could use --ignore-sandbox, but not all versions of cabal
    -- support it.
    tmpdir <- liftIO getTemporaryDirectory >>= parseAbsDir

    let args = ("--config-file=" ++ configFile)
             : "install"
             : "-v"
             : "--dry-run"
             : "--only-dependencies"
             : "--reorder-goals"
             : "--max-backjumps=-1"
             : "--package-db=clear"
             : "--package-db=global"
             : cabalArgs ++
               (map toFilePath cabalfps)

    $logInfo "Asking cabal to calculate a build plan, please wait"

    bs <- readProcessStdout (Just tmpdir) menv "cabal" args
    let ls = drop 1
           $ dropWhile (not . T.isPrefixOf "In order, ")
           $ T.lines
           $ decodeUtf8 bs
        (errs, pairs) = partitionEithers $ map parseLine ls
    if null errs
        then return (ghcMajorVersion, Map.fromList pairs)
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

getGhcVersion :: (MonadLogger m, MonadCatch m, MonadBaseControl IO m, MonadIO m)
              => EnvOverride -> m Version
getGhcVersion menv = do
    bs <- readProcessStdout Nothing menv "ghc" ["--numeric-version"]
    parseVersion $ S8.takeWhile isValid bs
  where
    isValid c = c == '.' || ('0' <= c && c <= '9')

getGhcMajorVersion :: (MonadLogger m, MonadCatch m, MonadBaseControl IO m, MonadIO m)
                   => EnvOverride -> m MajorVersion
getGhcMajorVersion menv = do
    version <- getGhcVersion menv
    return $ getMajorVersion version

getCabalConfig :: (MonadReader env m, HasConfig env, MonadIO m, MonadThrow m)
               => FilePath -- ^ temp dir
               -> m [Text]
getCabalConfig dir = do
    indices <- asks $ configPackageIndices . getConfig
    remotes <- mapM goIndex indices
    let cache = T.pack $ "remote-repo-cache: " ++ dir
    return $ cache : remotes
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

-- | Determine missing extra-deps
solveExtraDeps :: (MonadReader env m, HasEnvConfig env, MonadIO m, MonadMask m, MonadLogger m, MonadBaseControl IO m, HasHttpManager env)
               => Bool -- ^ modify stack.yaml?
               -> m ()
solveExtraDeps modStackYaml = do
    $logInfo "This command is not guaranteed to give you a perfect build plan"
    $logInfo "It's possible that even with the changes generated below, you will still need to do some manual tweaking"
    bconfig <- asks getBuildConfig
    snapshot <-
        case bcResolver bconfig of
            ResolverSnapshot snapName -> liftM mbpPackages $ loadMiniBuildPlan snapName
            ResolverGhc _ -> return Map.empty

    let packages = Map.union
            (bcExtraDeps bconfig)
            (fmap mpiVersion snapshot)
        constraints = map
            (\(k, v) -> concat
                [ "--constraint="
                , packageNameString k
                , "=="
                , versionString v
                ])
            (Map.toList packages)

    (_ghc, extraDeps) <- cabalSolver
        (Map.keys $ bcPackages bconfig)
        constraints

    let newDeps = extraDeps `Map.difference` packages
        newFlags = Map.filter (not . Map.null) $ fmap snd newDeps

    if Map.null newDeps
        then $logInfo "No needed changes found"
        else do
            let o = object
                    $ ("extra-deps" .= (map fromTuple $ Map.toList $ fmap fst newDeps))
                    : (if Map.null newFlags
                        then []
                        else ["flags" .= newFlags])
            mapM_ $logInfo $ T.lines $ decodeUtf8 $ Yaml.encode o

    when modStackYaml $ do
        let fp = toFilePath $ bcStackYaml bconfig
        obj <- liftIO (Yaml.decodeFileEither fp) >>= either throwM return
        ProjectAndConfigMonoid project _ <- liftIO (Yaml.decodeFileEither fp) >>= either throwM return
        let obj' =
                HashMap.insert "extra-deps"
                    (toJSON $ map fromTuple $ Map.toList
                            $ Map.union (projectExtraDeps project) (fmap fst newDeps))
              $ HashMap.insert ("flags" :: Text)
                    (toJSON $ Map.union (projectFlags project) newFlags)
                obj
        liftIO $ Yaml.encodeFile fp obj'
        $logInfo $ T.pack $ "Updated " ++ fp
