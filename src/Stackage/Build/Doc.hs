{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Utilities for built documentation, shared between @stackage-build@ and @fpdoc@.
module Stackage.Build.Doc where

import           Control.Monad
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text as T
import           Path as FL
import           Stackage.Constants
import           Stackage.PackageName
import           Stackage.Version
import           System.Directory
import           System.FilePath (takeFileName)

-- | Get all packages included in documentation from directory.
getDocPackages :: Path Abs Dir -> IO (Map PackageName [Version])
getDocPackages loc =
  do ls <- fmap (map (FL.toFilePath loc ++)) (getDirectoryContents (FL.toFilePath loc))
     mdirs <- forM ls (\e -> do isDir <- doesDirectoryExist e
                                return $ if isDir then (Just e) else Nothing)
     let sorted = -- Sort by package name ascending, version descending
                  sortBy (\(pa,va) (pb,vb) ->
                            case compare pa pb of
                              EQ -> compare vb va
                              o -> o)
                         (mapMaybe breakPkgVer (catMaybes mdirs))
     return (M.fromAscListWith (++) (map (\(k,v) -> (k,[v])) sorted))

-- | Split a documentation directory name into package name and version.
breakPkgVer :: FilePath -> Maybe (PackageName,Version)
breakPkgVer pkgPath =
  case T.breakOnEnd "-"
                    (T.pack (takeFileName pkgPath)) of
    ("",_) -> Nothing
    (pkgD,verT) ->
      let pkgstr = T.dropEnd 1 pkgD
      in case parseVersionFromString (T.unpack verT) of
           Just v
             | Just pkg <-
                parsePackageNameFromString (T.unpack pkgstr) ->
               Just (pkg,v)
           _ -> Nothing

-- | Construct a documentation directory name from package name and version.
joinPkgVer :: (PackageName,Version) -> FilePath
joinPkgVer (pkg,ver) = (packageNameString pkg ++ "-" ++ versionString ver)

-- | Get location of user-generated documentation.
getUserDocLoc :: IO (Path Abs Dir)
getUserDocLoc = do
    homeDir <- parseAbsDir =<< getHomeDirectory
    return (userDocsDir homeDir)

-- | Get location of user-generated documentation if it exists.
getExistingUserDocLoc :: IO (Maybe (Path Abs Dir))
getExistingUserDocLoc = do
    docPath <- getUserDocLoc
    docExists <- doesDirectoryExist (FL.toFilePath docPath)
    if docExists
        then return (Just docPath)
        else return Nothing

-- | Get locations of global package docs and GHC docs.
getGlobalDocLocs :: IO (Maybe (Path Abs Dir),Maybe (Path Abs Dir))
getGlobalDocLocs = do
    maybeGhcPathS <- findExecutable "ghc"
    case maybeGhcPathS of
        Nothing -> return (Nothing,Nothing)
        Just ghcPathS -> do
          ghcLoc <- parseAbsFile ghcPathS
          let rootLoc = parent (parent ghcLoc)
              pkgDocLoc = rootLoc </> $(mkRelDir "doc/")
              ghcDocLoc = rootLoc </> $(mkRelDir "share/doc/ghc/html/")
          pkgDocExists <- doesDirectoryExist (FL.toFilePath pkgDocLoc)
          ghcDocExists <- doesDirectoryExist (FL.toFilePath ghcDocLoc)
          return (if pkgDocExists then Just pkgDocLoc else Nothing
                 ,if ghcDocExists then Just ghcDocLoc else Nothing)
