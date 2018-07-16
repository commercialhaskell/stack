{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pantry.Hackage
  ( updateHackageIndex
  , hackageIndexTarballL
  ) where

import RIO
import Conduit
import Data.Conduit.Tar
import qualified RIO.Text as T
import Data.Text.Unsafe (unsafeTail)
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import Pantry.Types
import Pantry.Storage
import Network.URI (parseURI)
import Network.HTTP.Client.TLS (getGlobalManager)
import Data.Time (getCurrentTime)
import RIO.FilePath ((</>))
import qualified Distribution.Text
import Distribution.Types.PackageName (mkPackageName)

import qualified Hackage.Security.Client as HS
import qualified Hackage.Security.Client.Repository.Cache as HS
import qualified Hackage.Security.Client.Repository.Remote as HS
import qualified Hackage.Security.Client.Repository.HttpLib.HttpClient as HS
import qualified Hackage.Security.Util.Path as HS
import qualified Hackage.Security.Util.Pretty as HS

hackageDirL :: HasPantryConfig env => SimpleGetter env FilePath
hackageDirL = pantryConfigL.to ((</> "hackage") . pcRootDir)

hackageIndexTarballL :: HasPantryConfig env => SimpleGetter env FilePath
hackageIndexTarballL = hackageDirL.to (</> "00-index.tar")

-- | Download the most recent 01-index.tar file from Hackage and
-- update the database tables.
updateHackageIndex
  :: (HasPantryConfig env, HasLogFunc env)
  => RIO env ()
updateHackageIndex = do
    pc <- view pantryConfigL
    let HackageSecurityConfig keyIds threshold url = pcHackageSecurity pc
    root <- view hackageDirL
    tarball <- view hackageIndexTarballL
    baseURI <-
        case parseURI $ T.unpack url of
            Nothing -> throwString $ "Invalid Hackage Security base URL: " ++ T.unpack url
            Just x -> return x
    manager <- liftIO getGlobalManager
    run <- askRunInIO
    let logTUF = run . logInfo . fromString . HS.pretty
        withRepo = HS.withRepository
            (HS.makeHttpLib manager)
            [baseURI]
            HS.defaultRepoOpts
            HS.Cache
                { HS.cacheRoot = HS.fromAbsoluteFilePath root
                , HS.cacheLayout = HS.cabalCacheLayout
                }
            HS.hackageRepoLayout
            HS.hackageIndexLayout
            logTUF
    didUpdate <- liftIO $ withRepo $ \repo -> HS.uncheckClientErrors $ do
        needBootstrap <- HS.requiresBootstrap repo
        when needBootstrap $ do
            HS.bootstrap
                repo
                (map (HS.KeyId . T.unpack) keyIds)
                (HS.KeyThreshold $ fromIntegral threshold)
        now <- getCurrentTime
        HS.checkForUpdates repo (Just now)

    case didUpdate of
        HS.NoUpdates -> logInfo "No package index update available"
        HS.HasUpdates -> logInfo "Updated package index downloaded"

    withStorage $ do
      clearHackageRevisions
      populateCache tarball `onException`
        lift (logStickyDone "Failed populating package index cache")
    logStickyDone "Package index cache populated"

-- | Populate the SQLite tables with Hackage index information.
populateCache
  :: (HasPantryConfig env, HasLogFunc env)
  => FilePath -- ^ tarball
  -> ReaderT SqlBackend (RIO env) ()
populateCache fp = do
  lift $ logInfo "Populating package index cache ..."
  counter <- newIORef (0 :: Int)
  withSourceFile fp $ \src -> runConduit $ src .| untar (perFile counter)
  where

    perFile counter fi
      | FTNormal <- fileType fi
      , Right path <- decodeUtf8' $ filePath fi
      , Just (name, version) <- parseNameVersionCabal path = do
          (BL.toStrict <$> sinkLazy) >>= lift . addCabal name version

          count <- readIORef counter
          let count' = count + 1
          writeIORef counter count'
          when (count' `mod` 400 == 0) $
            lift $ lift $
            logSticky $ "Processed " <> display count' <> " cabal files"
      | otherwise = pure ()

    addCabal name version bs = do
      (blobTableId, _blobKey) <- storeBlob bs

      storeHackageRevision name version blobTableId

      -- Some older Stackage snapshots ended up with slightly
      -- modified cabal files, in particular having DOS-style
      -- line endings (CRLF) converted to Unix-style (LF). As a
      -- result, we track both hashes with and without CR
      -- characters stripped for compatibility with these older
      -- snapshots.
      --
      -- FIXME let's convert all old snapshots, correct the
      -- hashes, and drop this hack!
      let cr = 13
      when (cr `B.elem` bs) $ void $ storeBlob $ B.filter (/= cr) bs

    breakSlash x
        | T.null z = Nothing
        | otherwise = Just (y, unsafeTail z)
      where
        (y, z) = T.break (== '/') x

    parseNameVersionCabal t1 = do
        t2 <- T.stripSuffix ".cabal" t1

        (name, t3) <- breakSlash t2
        (version, base) <- breakSlash t3

        guard (base == name)

        version' <- Distribution.Text.simpleParse $ T.unpack version

        Just (mkPackageName $ T.unpack name, version')
