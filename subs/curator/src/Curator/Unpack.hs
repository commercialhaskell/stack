{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Unpack packages and write out a stack.yaml
module Curator.Unpack
  ( unpackSnapshot
  ) where

import RIO
import RIO.Process (HasProcessContext)
import Pantry
import Curator.Types
import Path
import Path.IO
import qualified RIO.Text as T
import Data.Yaml
import qualified RIO.Map as Map
import qualified RIO.Set as Set

unpackSnapshot
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Constraints
  -> RawSnapshot
  -> Path Abs Dir
  -> RIO env ()
unpackSnapshot cons snap root = do
  unpacked <- parseRelDir "unpacked"
  (suffixes, flags, skipTest, skipBench, skipHaddock) <- fmap fold $ for (rsPackages snap) $ \sp -> do
    let pl = rspLocation sp
    TreeKey (BlobKey sha _size) <- getRawPackageLocationTreeKey pl
    PackageIdentifier name version <- getRawPackageLocationIdent pl
    pc <-
      case Map.lookup name $ consPackages cons of
        Nothing -> error $ "Package not found in constraints: " ++ packageNameString name
        Just pc -> pure pc
    unless (pcFlags pc == rspFlags sp) $ error "mismatched flags!"
    if pcSkipBuild pc
      then pure mempty
      else do
        let suffixBuilder =
              fromString (packageNameString name) <>
              "-" <>
              fromString (versionString version) <>
              "-" <>
              display sha
        suffixTmp <- parseRelDir $ T.unpack $ utf8BuilderToText $ suffixBuilder <> ".tmp"
        let destTmp = root </> unpacked </> suffixTmp
        suffix <- parseRelDir $ T.unpack $ utf8BuilderToText suffixBuilder
        let dest = root </> unpacked </> suffix
        exists <- doesDirExist dest
        unless exists $ do
          ignoringAbsence $ removeDirRecur destTmp
          ensureDir destTmp
          logInfo $ "Unpacking " <> display pl
          unpackPackageLocationRaw destTmp pl
          renameDir destTmp dest
        pure
          ( Set.singleton suffix
          , if Map.null (pcFlags pc) then Map.empty else Map.singleton name (pcFlags pc)
          , case pcTests pc of
              CAExpectSuccess -> mempty
              _ -> Set.singleton name -- FIXME this and others, want to differentiate skip and expect failure
          , case pcBenchmarks pc of
              CAExpectSuccess -> mempty
              _ -> Set.singleton name
          , case pcHaddock pc of
              CAExpectSuccess -> mempty
              _ -> Set.singleton name
          )
  stackYaml <- parseRelFile "stack.yaml"
  let stackYamlFP = toFilePath $ root </> stackYaml
  liftIO $ encodeFile stackYamlFP $ object
    [ "resolver" .= ("ghc-" ++ versionString (consGhcVersion cons))
    , "packages" .= Set.map (\suffix -> toFilePath (unpacked </> suffix)) suffixes
    , "flags" .= fmap toCabalStringMap (toCabalStringMap flags)
    , "curator" .= object
        [ "skip-test" .= Set.map CabalString skipTest
        , "skip-bench" .= Set.map CabalString skipBench
        , "skip-haddock" .= Set.map CabalString skipHaddock
        ]
    ]
