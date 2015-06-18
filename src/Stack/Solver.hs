{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Stack.Solver
    ( cabalSolver
    ) where

import           Control.Exception.Enclosed  (tryIO)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import qualified Data.ByteString             as S
import qualified Data.ByteString.Char8       as S8
import           Data.Either
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Path
import           Stack.Types
import           System.Directory            (copyFile,
                                              createDirectoryIfMissing)
import qualified System.FilePath             as FP
import           System.IO.Temp
import           System.Process.Read

cabalSolver :: (MonadIO m, MonadLogger m, MonadMask m, MonadBaseControl IO m, MonadReader env m, HasConfig env)
            => [Path Abs File] -- ^ cabal files
            -> m (MajorVersion, Map PackageName (Version, Map FlagName Bool))
cabalSolver cabalfps = withSystemTempDirectory "cabal-solver" $ \dir -> do
    $logInfo "Trying out the cabal dependency solver as a last resort"

    configLines <- getCabalConfig dir
    let configFile = dir FP.</> "cabal.config"
    liftIO $ S.writeFile configFile $ encodeUtf8 $ T.unlines configLines

    menv <- getMinimalEnvOverride
    ghcMajorVersion <- getGhcMajorVersion menv

    let args = ("--config-file=" ++ configFile)
             : "install"
             : "-v"
             : "--dry-run"
             : "--only-dependencies"
             : "--reorder-goals"
             : "--max-backjumps=-1"
             : "--package-db=clear"
             : "--package-db=global"
             : map (toFilePath . parent) cabalfps
    bs <- readProcessStdout Nothing menv "cabal" args
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
                Nothing -> (t0, True)
                Just x -> (x, False)

getGhcMajorVersion :: (MonadLogger m, MonadCatch m, MonadBaseControl IO m, MonadIO m)
                   => EnvOverride -> m MajorVersion
getGhcMajorVersion menv = do
    bs <- readProcessStdout Nothing menv "ghc" ["--numeric-version"]
    version <- parseVersion $ S8.takeWhile isValid bs
    return $ getMajorVersion version
  where
    isValid c = c == '.' || ('0' <= c && c <= '9')

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
