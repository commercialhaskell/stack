{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Utilities for built documentation, shared between @stackage-build@ and @fpdoc@.
module Stack.Build.Doc where

import           Control.Monad
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text as T
import           Path
import           Stack.Constants
import           Stack.Types
import           System.Directory
import           System.FilePath (takeFileName)
import           System.Environment (lookupEnv)

-- | Get all packages included in documentation from directory.
getDocPackages :: Path Abs Dir -> IO (Map PackageName [Version])
getDocPackages loc =
  do ls <- fmap (map (toFilePath loc ++)) (getDirectoryContents (toFilePath loc))
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
getUserDocPath :: IO (Path Abs Dir)
getUserDocPath = do
    homeDir <- parseAbsDir =<< getHomeDirectory
    return (userDocsDir homeDir)

-- | Get location of user-generated documentation if it exists.
getExistingUserDocPath :: IO (Maybe (Path Abs Dir))
getExistingUserDocPath = do
    docPath <- getUserDocPath
    docExists <- doesDirectoryExist (toFilePath docPath)
    if docExists
        then return (Just docPath)
        else return Nothing

-- | Get location of global package docs.
getGlobalDocPath :: IO (Maybe (Path Abs Dir))
getGlobalDocPath = do
    maybeRootEnv <- lookupEnv "STACKAGE_DOC_ROOT"
    case maybeRootEnv of
        Nothing -> return Nothing
        Just rootEnv -> do
            pkgDocPath <- parseAbsDir rootEnv
            pkgDocExists <- doesDirectoryExist (toFilePath pkgDocPath)
            return (if pkgDocExists then Just pkgDocPath else Nothing)

-- | Get location of GHC docs.
getGhcDocPath :: IO (Maybe (Path Abs Dir))
getGhcDocPath = do
    maybeGhcPathS <- findExecutable "ghc"
    case maybeGhcPathS of
        Nothing -> return Nothing
        Just ghcPathS -> do
            ghcPath <- parseAbsFile ghcPathS
            let ghcDocPath = parent (parent ghcPath) </> $(mkRelDir "share/doc/ghc/html/")
            ghcDocExists <- doesDirectoryExist (toFilePath ghcDocPath)
            return (if ghcDocExists then Just ghcDocPath else Nothing)
