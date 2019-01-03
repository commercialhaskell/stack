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
  (suffixes, flags, (skipTest, expectTestFailure), skipBench,
   (skipHaddock, expectHaddockFailure)) <- fmap fold $ for (rsPackages snap) $ \sp -> do
    let pl = rspLocation sp
    TreeKey (BlobKey sha _size) <- getRawPackageLocationTreeKey pl
    PackageIdentifier name version <- getRawPackageLocationIdent pl
    let (flags, skipBuild, test, bench, haddock) =
          case Map.lookup name $ consPackages cons of
            Nothing ->
              (mempty, False, CAExpectSuccess, CAExpectSuccess, CAExpectSuccess)
            Just pc ->
              (pcFlags pc, pcSkipBuild pc, pcTests pc, pcBenchmarks pc, pcHaddock pc)
    unless (flags == rspFlags sp) $ error $ unlines
      [ "mismatched flags for " ++ show pl
      , " snapshot: " ++ show (rspFlags sp)
      , " constraints: " ++ show flags
      ]
    if skipBuild
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
          , if Map.null flags then Map.empty else Map.singleton name flags
          , case test of
              CAExpectSuccess -> mempty
              CAExpectFailure -> (mempty, Set.singleton name)
              CASkip ->  (Set.singleton name, mempty)
          , case bench of
              CASkip -> Set.singleton name
              _ -> mempty -- FIXME maybe we want to differentiate skip and expect failure but
                          -- we don't run benchmarks, only compile them
          , case haddock of
              CAExpectSuccess -> mempty
              CAExpectFailure -> (mempty, Set.singleton name)
              CASkip -> (Set.singleton name, mempty)
          )
  stackYaml <- parseRelFile "stack.yaml"
  let stackYamlFP = toFilePath $ root </> stackYaml
  liftIO $ encodeFile stackYamlFP $ object
    [ "resolver" .= ("ghc-" ++ versionString (consGhcVersion cons))
    , "packages" .= Set.map (\suffix -> toFilePath (unpacked </> suffix)) suffixes
    , "flags" .= fmap toCabalStringMap (toCabalStringMap flags)
    , "curator" .= object
        [ "skip-test" .= Set.map CabalString skipTest
        , "expect-test-failure" .= Set.map CabalString expectTestFailure
        , "skip-bench" .= Set.map CabalString skipBench
        , "skip-haddock" .= Set.map CabalString skipHaddock
        , "expect-haddock-failure" .= Set.map CabalString expectHaddockFailure
        ]
    ]
