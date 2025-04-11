{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Functions for the GHC package database.

module Stack.GhcPkg
  ( createDatabase
  , findGhcPkgField
  , getGlobalDB
  , ghcPkg
  , ghcPkgPathEnvVar
  , mkGhcPackagePath
  , unregisterGhcPkgIds
  ) where

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Utils.GhcPkg.Main.Compat ( ghcPkgUnregisterForce )
import           Path ( (</>), parent )
import           Path.Extra ( toFilePathNoTrailingSep )
import           Path.IO
                   ( doesDirExist, doesFileExist, ensureDir, resolveDir' )
import           RIO.Process ( HasProcessContext, proc, readProcess_ )
import           Stack.Constants ( relFilePackageCache )
import           Stack.Prelude
import           Stack.Types.Compiler ( WhichCompiler (..) )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), GhcPkgExe (..), HasCompiler
                   , compilerPathsL
                   )
import           Stack.Types.GhcPkgExe ( GhcPkgPrettyException (..) )
import           Stack.Types.GhcPkgId ( GhcPkgId, ghcPkgIdString )
import           System.FilePath ( searchPathSeparator )

-- | Get the global package database
getGlobalDB ::
     (HasProcessContext env, HasTerm env)
  => GhcPkgExe
  -> RIO env (Path Abs Dir)
getGlobalDB pkgexe = do
  logDebug "Getting global package database location"
  -- This seems like a strange way to get the global package database
  -- location, but I don't know of a better one
  bs <- ghcPkg pkgexe [] ["list", "--global"] >>= either throwIO pure
  let fp = S8.unpack $ stripTrailingColon $ firstLine bs
  liftIO $ resolveDir' fp
 where
  stripTrailingColon bs
    | S8.null bs = bs
    | S8.last bs == ':' = S8.init bs
    | otherwise = bs
  firstLine = S8.takeWhile (\c -> c /= '\r' && c /= '\n')

-- | Run the ghc-pkg executable
ghcPkg ::
     (HasProcessContext env, HasTerm env)
  => GhcPkgExe
  -> [Path Abs Dir]
  -> [String]
  -> RIO env (Either SomeException S8.ByteString)
ghcPkg pkgexe@(GhcPkgExe pkgPath) pkgDbs args = do
  eres <- go
  case eres of
    Left e -> do
      prettyDebug $
           fillSep
             [ flow "While using"
             , style Shell "ghc-pkg" <>","
             , flow "Stack encountered the following error:"
             ]
        <> blankLine
        <> string (displayException e)
        <> flow "Trying again after considering database creation..."
      mapM_ (createDatabase pkgexe) pkgDbs
      go
    Right _ -> pure eres
 where
  pkg = toFilePath pkgPath
  go = tryAny $ BL.toStrict . fst <$> proc pkg args' readProcess_
  args' = packageDbFlags pkgDbs ++ args

-- | Create a package database in the given directory, if it doesn't exist.
createDatabase ::
     (HasProcessContext env, HasTerm env)
  => GhcPkgExe
  -> Path Abs Dir
  -> RIO env ()
createDatabase (GhcPkgExe pkgPath) db = do
  exists <- doesFileExist (db </> relFilePackageCache)
  unless exists $ do
    -- ghc-pkg requires that the database directory does not exist
    -- yet. If the directory exists but the package.cache file
    -- does, we're in a corrupted state. Check for that state.
    dirExists <- doesDirExist db
    args <- if dirExists
      then do
        prettyWarnL
          [ flow "The package database located at"
          , pretty db
          , flow "is corrupted. It is missing its"
          , style File "package.cache"
          , flow "file. Stack is proceeding with a recache."
          ]
        pure ["--package-db", toFilePath db, "recache"]
      else do
        -- Creating the parent doesn't seem necessary, as ghc-pkg
        -- seems to be sufficiently smart. But I don't feel like
        -- finding out it isn't the hard way
        ensureDir (parent db)
        pure ["init", toFilePath db]
    void $ proc (toFilePath pkgPath) args $ \pc ->
      onException (readProcess_ pc) $
        logError $
          "Error: [S-9735]\n" <>
           "Unable to create package database at " <>
           fromString (toFilePath db)

-- | Get the environment variable to use for the package DB paths.
ghcPkgPathEnvVar :: WhichCompiler -> Text
ghcPkgPathEnvVar Ghc = "GHC_PACKAGE_PATH"

-- | Get the necessary ghc-pkg flags for setting up the given package database
packageDbFlags :: [Path Abs Dir] -> [String]
packageDbFlags pkgDbs =
    "--no-user-package-db"
  : map (\x -> "--package-db=" ++ toFilePath x) pkgDbs

-- | Get the value of a field of the package.
findGhcPkgField ::
     (HasProcessContext env, HasTerm env)
  => GhcPkgExe
  -> [Path Abs Dir] -- ^ package databases
  -> String -- ^ package identifier, or GhcPkgId
  -> Text
  -> RIO env (Maybe Text)
findGhcPkgField pkgexe pkgDbs name field = do
  result <-
    ghcPkg
      pkgexe
      pkgDbs
      ["field", "--simple-output", name, T.unpack field]
  pure $
    case result of
      Left{} -> Nothing
      Right bs ->
        fmap (stripCR . T.decodeUtf8) $ listToMaybe $ S8.lines bs

-- | unregister list of package ghcids, batching available from GHC 8.2.1,
-- see https://github.com/commercialhaskell/stack/issues/2662#issuecomment-460342402
-- using GHC package id where available (from GHC 7.9)
--
-- The version of the ghc-pkg executable supplied with GHCs published before
-- 28 August 2023 does not efficiently bulk unregister. Until an 'efficient'
-- ghc-pkg is available, this function no longer uses:
--
-- >   eres <- ghcPkg pkgexe [pkgDb] args
-- > where
-- >    args = "unregister" : "--user" : "--force" :
-- >      map packageIdentifierString idents ++
-- >      if null gids then [] else "--ipid" : map ghcPkgIdString gids
--
-- but uses:
--
-- >   globalDb <- view $ compilerPathsL.to cpGlobalDB
-- >   eres <- tryAny $ liftIO $
-- >     ghcPkgUnregisterUserForce globalDb pkgDb hasIpid pkgarg_strs
--
unregisterGhcPkgIds ::
     (HasCompiler env, HasProcessContext env, HasTerm env)
  => Bool
     -- ^ Report pretty exceptions as warnings?
  -> GhcPkgExe
  -> Path Abs Dir -- ^ package database
  -> NonEmpty (Either PackageIdentifier GhcPkgId)
  -> RIO env ()
unregisterGhcPkgIds isWarn pkgexe pkgDb epgids = do
  globalDb <- view $ compilerPathsL . to (.globalDB)
  eresUnregister <- try $ do
    ghcPkgUnregisterForce globalDb pkgDb hasIpid pkgarg_strs
  case eresUnregister of
    Left (PrettyException e) -> when isWarn $
      prettyWarn $
        "[S-8729]"
        <> line
        <> flow "While unregistering packages, Stack encountered the following \
                \error:"
        <> blankLine
        <> pretty e
    Right _ -> pure ()
  -- ghcPkgUnregisterForce does not perform an effective 'ghc-pkg recache', as
  -- that depends on a specific version of the Cabal package.
  eresRecache <- ghcPkg pkgexe [pkgDb] ["recache"]
  case eresRecache of
    Left err -> prettyThrowM $ CannotRecacheAfterUnregister pkgDb err
    Right _ -> pure ()
 where
  (idents, gids) = partitionEithers $ toList epgids
  hasIpid = not (null gids)
  pkgarg_strs = map packageIdentifierString idents <> map ghcPkgIdString gids

-- | Get the value for GHC_PACKAGE_PATH
mkGhcPackagePath :: Bool -> Path Abs Dir -> Path Abs Dir -> [Path Abs Dir] -> Path Abs Dir -> Text
mkGhcPackagePath locals localdb deps extras globaldb =
  T.pack $ L.intercalate [searchPathSeparator] $ concat
    [ [toFilePathNoTrailingSep localdb | locals]
    , [toFilePathNoTrailingSep deps]
    , [toFilePathNoTrailingSep db | db <- reverse extras]
    , [toFilePathNoTrailingSep globaldb]
    ]
