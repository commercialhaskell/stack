{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Deal with downloading, cloning, or whatever else is necessary for
-- getting a 'PackageLocation' into something Stack can work with.
module Stack.PackageLocation
  ( parseSingleCabalFile
  , parseSingleCabalFileIndex
  , parseMultiCabalFilesIndex
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import Stack.Prelude
import           Crypto.Hash (hashWith, SHA256(..))
import qualified Data.ByteArray as Mem (convert)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Distribution.PackageDescription (GenericPackageDescription)
import Network.HTTP.StackClient (parseUrlThrow)
import Network.HTTP.Download.Verified
import Pantry.StaticSHA256
import Path
import Path.Extra
import Path.IO
import Stack.Package
import Stack.Types.BuildPlan
import Stack.Types.Config
import Stack.Types.PackageIdentifier
import qualified System.Directory as Dir
import RIO.Process

flattenPackageLocation :: Traversable t => t Subdirs -> [t FilePath]
flattenPackageLocation =
    traverse go
  where
    go :: Subdirs -> [FilePath]
    go DefaultSubdirs = [""]
    go (ExplicitSubdirs subs) = map go' subs

    go' :: FilePath -> FilePath
    go' = T.unpack
        . T.intercalate "/"
        . filter (\t -> not (T.null t) && t /= ".")
        . T.split (== '/')
        . T.pack


-- | Parse the cabal files present in the given
-- 'PackageLocationIndex FilePath'.
parseSingleCabalFileIndex
  :: forall env.
     HasConfig env
  => Path Abs Dir -- ^ project root, used for checking out necessary files
  -> PackageLocationIndex FilePath
  -> RIO env GenericPackageDescription
-- Need special handling of PLIndex for efficiency (just read from the
-- index tarball) and correctness (get the cabal file from the index,
-- not the package tarball itself, yay Hackage revisions).
parseSingleCabalFileIndex _ (PLIndex pir) = readPackageUnresolvedIndex pir
parseSingleCabalFileIndex root (PLOther (PLFilePath fp)) = do
  dir <- resolveDir root fp
  lpvGPD <$> parseSingleCabalFile False dir

parseSingleCabalFile
  :: forall env. HasConfig env
  => Bool -- ^ print warnings?
  -> Path Abs Dir
  -> RIO env LocalPackageView
parseSingleCabalFile printWarnings dir = do
  (gpd, cabalfp) <- readPackageUnresolvedDir dir printWarnings
  return LocalPackageView
    { lpvCabalFP = cabalfp
    , lpvGPD = gpd
    }

-- | 'parseMultiCabalFiles' but supports 'PLIndex'
parseMultiCabalFilesIndex
  :: forall env. HasConfig env
  => Path Abs Dir -- ^ project root, used for checking out necessary files
  -> PackageLocationIndex Subdirs
  -> RIO env [(GenericPackageDescription, PackageLocationIndex FilePath)]
parseMultiCabalFilesIndex root pl0 = for (flattenPackageLocation pl0) $ \pl ->
  (, pl) <$> parseSingleCabalFileIndex root pl
