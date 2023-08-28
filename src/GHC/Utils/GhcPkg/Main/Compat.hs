{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

-- This module is based on GHC's utils\ghc-pkg\Main.hs at
-- commit f66fc15f2e6849125074bcfeb44334a663323ca6 (see GHC merge request
-- !11142), with redundant code deleted, Hlint applied and explicit import
-- lists.
--
-- The version of the ghc-pkg executable supplied with GHCs published before
-- 28 August 2023 does not efficiently bulk unregister. This module exports a
-- function that does efficiently bulk unregister.

module GHC.Utils.GhcPkg.Main.Compat
  ( ghcPkgUnregisterUserForce
  ) where

-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2004-2009.
--
-- Package management tool
--
-----------------------------------------------------------------------------

import qualified Control.Exception as Exception
import           Control.Monad ( ap, forM, forM_, liftM, unless, when )
import           Data.Bifunctor ( bimap )
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import           Data.List
                   ( foldl', isPrefixOf, isSuffixOf, nub, partition, sort
                   , stripPrefix
                   )
import qualified Data.Map as Map
import           Data.Maybe ( catMaybes, mapMaybe )
import qualified Data.Traversable as F
import qualified Data.Version as Version
import           Distribution.Backpack ( OpenModule (..), OpenUnitId (..) )
import           Distribution.InstalledPackageInfo as Cabal
import           Distribution.ModuleName (ModuleName)
import           Distribution.Package
                   ( PackageIdentifier, PackageName, UnitId, mkPackageName
                   , mkUnitId, mungedId, packageId, packageName, packageVersion
                   , pkgName, unAbiHash, unComponentId, unDefUnitId
                   , unPackageName
                   )
import qualified Distribution.Parsec as Cabal
import           Distribution.Pretty (Pretty (..))
import           Distribution.Simple.Utils (toUTF8BS, writeUTF8File)
import           Distribution.Text ( display )
import           Distribution.Types.UnqualComponentName
                   ( unUnqualComponentName )
import           Distribution.Types.LibraryName ( libraryNameString )
import           Distribution.Types.MungedPackageName ( MungedPackageName )
import           Distribution.Types.MungedPackageId ( MungedPackageId (..) )
import           Distribution.Version ( versionNumbers, nullVersion )
import qualified GHC.Data.ShortText as ST
import           GHC.IO ( catchException )
import           GHC.IO.Exception (IOErrorType(InappropriateType))
import           GHC.Platform.Host (hostPlatformArchOS)
import           GHC.Settings.Utils (getTargetArchOS, maybeReadFuzzy)
import           GHC.UniqueSubdir (uniqueSubdir)
import qualified GHC.Unit.Database as GhcPkg
import           GHC.Unit.Database
                   ( DbInstUnitId (..), DbMode (..), DbModule (..) )
import           Path ( Abs, Dir, Path, toFilePath )
import           Prelude
import           System.Directory
                   ( XdgDirectory (..), createDirectoryIfMissing
                   , doesDirectoryExist, doesFileExist, getAppUserDataDirectory
                   , getCurrentDirectory, getDirectoryContents
                   , getModificationTime, getXdgDirectory, removeFile
                   )
import           System.Exit ( exitWith, ExitCode(..) )
import           System.Environment ( getProgName, getEnv )
import           System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix
import           System.IO ( hFlush, hPutStrLn, stderr, stdout )
import           System.IO.Error
                   ( ioeGetErrorType, isDoesNotExistError, isPermissionError )

-- | Function equivalent to:
--
-- > ghc-pkg --no-user-package-db --package-db=<pkgDb> unregister --user --force [--ipid] <P>
--
ghcPkgUnregisterUserForce ::
     Path Abs Dir -- ^ Path to the global package database
  -> Path Abs Dir -- ^ package database
  -> Bool -- ^ Apply ghc-pkg's --ipid, --unit-id flag?
  -> [String] -- ^ Packages to unregister
  -> IO ()
ghcPkgUnregisterUserForce globalDb pkgDb hasIpid pkgarg_strs = do
  pkgargs <- forM pkgarg_strs $ readPackageArg as_arg
  unregisterPackages globalDb pkgargs verbosity cli force
 where
  verbosity = Normal
  cli = concat
    [ [FlagNoUserDb]
    , [FlagConfig $ toFilePath pkgDb]
    , [FlagUser]
    , [FlagForce]
    , [FlagUnitId | hasIpid]
    ]
  force = ForceAll
  as_arg | FlagUnitId `elem` cli = AsUnitId
         | otherwise             = AsDefault

-- | Short-circuit 'any' with a \"monadic predicate\".
anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = return False
anyM p (x:xs) = do
  b <- p x
  if b
    then return True
    else anyM p xs

-- -----------------------------------------------------------------------------
-- Command-line syntax

data Flag
  = FlagUser
  | FlagGlobal
  | FlagConfig FilePath
  | FlagGlobalConfig FilePath
  | FlagUserConfig FilePath
  | FlagForce
  | FlagNoUserDb
  | FlagVerbosity (Maybe String)
  | FlagUnitId
  deriving Eq

data Verbosity = Silent | Normal | Verbose
    deriving (Show, Eq, Ord)

-- -----------------------------------------------------------------------------
-- Do the business

data Force = ForceAll
  deriving (Eq,Ord)

-- | Enum flag representing argument type
data AsPackageArg
    = AsUnitId
    | AsDefault

-- | Represents how a package may be specified by a user on the command line.
data PackageArg
    -- | A package identifier foo-0.1, or a glob foo-*
    = Id GlobPackageIdentifier
    -- | An installed package ID foo-0.1-HASH.  This is guaranteed to uniquely
    -- match a single entry in the package database.
    | IUId UnitId
    -- | A glob against the package name.  The first string is the literal
    -- glob, the second is a function which returns @True@ if the argument
    -- matches.
    | Substring String (String->Bool)

parseCheck :: Cabal.Parsec a => String -> String -> IO a
parseCheck str what =
  case Cabal.eitherParsec str of
    Left e  -> die ("cannot parse \'" ++ str ++ "\' as a " ++ what ++ ": " ++ e)
    Right x -> pure x

-- | Either an exact 'PackageIdentifier', or a glob for all packages
-- matching 'PackageName'.
data GlobPackageIdentifier
    = ExactPackageIdentifier MungedPackageId
    | GlobPackageIdentifier  MungedPackageName

displayGlobPkgId :: GlobPackageIdentifier -> String
displayGlobPkgId (ExactPackageIdentifier pid) = display pid
displayGlobPkgId (GlobPackageIdentifier pn) = display pn ++ "-*"

readGlobPkgId :: String -> IO GlobPackageIdentifier
readGlobPkgId str
  | "-*" `isSuffixOf` str =
    GlobPackageIdentifier <$> parseCheck (init (init str)) "package identifier (glob)"
  | otherwise = ExactPackageIdentifier <$> parseCheck str "package identifier (exact)"

readPackageArg :: AsPackageArg -> String -> IO PackageArg
readPackageArg AsUnitId str = IUId <$> parseCheck str "installed package id"
readPackageArg AsDefault str = Id <$> readGlobPkgId str

-- -----------------------------------------------------------------------------
-- Package databases

-- Some commands operate on a single database:
--      register, unregister, expose, hide, trust, distrust
-- however these commands also check the union of the available databases
-- in order to check consistency.  For example, register will check that
-- dependencies exist before registering a package.
--
-- Some commands operate  on multiple databases, with overlapping semantics:
--      list, describe, field

data PackageDB (mode :: GhcPkg.DbMode)
  = PackageDB {
      location, locationAbsolute :: !FilePath,
      -- We need both possibly-relative and definitely-absolute package
      -- db locations. This is because the relative location is used as
      -- an identifier for the db, so it is important we do not modify it.
      -- On the other hand we need the absolute path in a few places
      -- particularly in relation to the ${pkgroot} stuff.

      packageDbLock :: !(GhcPkg.DbOpenMode mode GhcPkg.PackageDbLock),
      -- If package db is open in read write mode, we keep its lock around for
      -- transactional updates.

      packages :: [InstalledPackageInfo]
    }

type PackageDBStack = [PackageDB 'GhcPkg.DbReadOnly]
        -- A stack of package databases.  Convention: head is the topmost
        -- in the stack.

data PackageDBAndStack (mode :: GhcPkg.DbMode)
  = PackageDBAndStack (PackageDB mode) PackageDBStack

-- | Selector for picking the right package DB to modify as 'register' and
-- 'recache' operate on the database on top of the stack, whereas 'modify'
-- changes the first database that contains a specific package.
data DbModifySelector = TopOne | ContainsPkg PackageArg

allPackagesInStack :: PackageDBStack -> [InstalledPackageInfo]
allPackagesInStack = concatMap packages

-- | Retain only the part of the stack up to and including the given package
-- DB (where the global package DB is the bottom of the stack). The resulting
-- package DB stack contains exactly the packages that packages from the
-- specified package DB can depend on, since dependencies can only extend
-- down the stack, not up (e.g. global packages cannot depend on user
-- packages).
stackUpTo :: FilePath -> PackageDBStack -> PackageDBStack
stackUpTo to_modify = dropWhile ((/= to_modify) . location)

getPkgDatabases :: Path Abs Dir
                   -- ^ Path to the global package database.
                -> Verbosity
                -> GhcPkg.DbOpenMode mode DbModifySelector
                -> Bool    -- use the user db
                -> Bool    -- read caches, if available
                -> Bool    -- expand vars, like ${pkgroot} and $topdir
                -> [Flag]
                -> IO (PackageDBStack,
                          -- the real package DB stack: [global,user] ++
                          -- DBs specified on the command line with -f.
                       GhcPkg.DbOpenMode mode (PackageDB mode),
                          -- which one to modify, if any
                       PackageDBStack)
                          -- the package DBs specified on the command
                          -- line, or [global,user] otherwise.  This
                          -- is used as the list of package DBs for
                          -- commands that just read the DB, such as 'list'.

getPkgDatabases globalDb verbosity mode use_user use_cache expand_vars my_flags = do
  -- Second we determine the location of the global package config.  On Windows,
  -- this is found relative to the ghc-pkg.exe binary, whereas on Unix the
  -- location is passed to the binary using the --global-package-db flag by the
  -- wrapper script.
  let  global_conf = toFilePath globalDb

  -- The value of the $topdir variable used in some package descriptions
  -- Note that the way we calculate this is slightly different to how it
  -- is done in ghc itself. We rely on the convention that the global
  -- package db lives in ghc's libdir.
  top_dir <- absolutePath (takeDirectory global_conf)

  let no_user_db = FlagNoUserDb `elem` my_flags

  -- get the location of the user package database, and create it if necessary
  -- getXdgDirectory can fail (e.g. if $HOME isn't set)

  mb_user_conf <-
    case [ f | FlagUserConfig f <- my_flags ] of
      _ | no_user_db -> return Nothing
      [] -> do
        -- See Note [Settings file] about this file, and why we need GHC to share it with us.
        let settingsFile = top_dir </> "settings"
        exists_settings_file <- doesFileExist settingsFile
        targetArchOS <- if exists_settings_file
          then do
            settingsStr <- readFile settingsFile
            mySettings <- case maybeReadFuzzy settingsStr of
              Just s -> pure $ Map.fromList s
              -- It's excusable to not have a settings file (for now at
              -- least) but completely inexcusable to have a malformed one.
              Nothing -> die $ "Can't parse settings file " ++ show settingsFile
            case getTargetArchOS settingsFile mySettings of
              Right archOS -> pure archOS
              Left e -> die e
          else do
            warn $ "WARNING: settings file doesn't exist " ++ show settingsFile
            warn "cannot know target platform so guessing target == host (native compiler)."
            pure hostPlatformArchOS

        let subdir = uniqueSubdir targetArchOS

            getFirstSuccess :: [IO a] -> IO (Maybe a)
            getFirstSuccess [] = pure Nothing
            getFirstSuccess (a:as) = tryIO a >>= \case
              Left _ -> getFirstSuccess as
              Right d -> pure (Just d)
        -- The appdir used to be in ~/.ghc but to respect the XDG specification
        -- we want to move it under $XDG_DATA_HOME/
        -- However, old tooling (like cabal) might still write package environments
        -- to the old directory, so we prefer that if a subdirectory of ~/.ghc
        -- with the correct target and GHC version exists.
        --
        -- i.e. if ~/.ghc/$UNIQUE_SUBDIR exists we prefer that
        -- otherwise we use $XDG_DATA_HOME/$UNIQUE_SUBDIR
        --
        -- UNIQUE_SUBDIR is typically a combination of the target platform and GHC version
        m_appdir <- getFirstSuccess $ map (fmap (</> subdir))
          [ getAppUserDataDirectory "ghc"  -- this is ~/.ghc/
          , getXdgDirectory XdgData "ghc"  -- this is $XDG_DATA_HOME/
          ]
        case m_appdir of
          Nothing -> return Nothing
          Just dir -> do
            lookForPackageDBIn dir >>= \case
              Nothing -> return (Just (dir </> "package.conf.d", False))
              Just f  -> return (Just (f, True))
      fs -> return (Just (last fs, True))

  -- If the user database exists, and for "use_user" commands (which includes
  -- "ghc-pkg check" and all commands that modify the db) we will attempt to
  -- use the user db.
  let sys_databases
        | Just (user_conf,user_exists) <- mb_user_conf,
          use_user || user_exists = [user_conf, global_conf]
        | otherwise               = [global_conf]

  e_pkg_path <- tryIO (System.Environment.getEnv "GHC_PACKAGE_PATH")
  let env_stack =
        case e_pkg_path of
                Left  _ -> sys_databases
                Right path
                  | not (null path) && isSearchPathSeparator (last path)
                  -> splitSearchPath (init path) ++ sys_databases
                  | otherwise
                  -> splitSearchPath path

        -- The "global" database is always the one at the bottom of the stack.
        -- This is the database we modify by default.
      virt_global_conf = last env_stack

  let db_flags = mapMaybe is_db_flag my_flags
         where is_db_flag FlagUser
                      | Just (user_conf, _user_exists) <- mb_user_conf
                      = Just user_conf
               is_db_flag FlagGlobal     = Just virt_global_conf
               is_db_flag (FlagConfig f) = Just f
               is_db_flag _              = Nothing

  let flag_db_names | null db_flags = env_stack
                    | otherwise     = reverse (nub db_flags)

  -- For a "modify" command, treat all the databases as
  -- a stack, where we are modifying the top one, but it
  -- can refer to packages in databases further down the
  -- stack.

  -- -f flags on the command line add to the database
  -- stack, unless any of them are present in the stack
  -- already.
  let final_stack = filter (`notElem` env_stack)
                     [ f | FlagConfig f <- reverse my_flags ]
                     ++ env_stack

      top_db = if null db_flags
               then virt_global_conf
               else last db_flags

  (db_stack, db_to_operate_on) <- getDatabases top_dir mb_user_conf
                                               flag_db_names final_stack top_db

  let flag_db_stack = [ db | db_name <- flag_db_names,
                        db <- db_stack, location db == db_name ]

  when (verbosity > Normal) $ do
    infoLn ("db stack: " ++ show (map location db_stack))
    F.forM_ db_to_operate_on $ \db ->
      infoLn ("modifying: " ++ location db)
    infoLn ("flag db stack: " ++ show (map location flag_db_stack))

  return (db_stack, db_to_operate_on, flag_db_stack)
  where
    getDatabases top_dir mb_user_conf flag_db_names
                 final_stack top_db = case mode of
      -- When we open in read only mode, we simply read all of the databases/
      GhcPkg.DbOpenReadOnly -> do
        db_stack <- mapM readDatabase final_stack
        return (db_stack, GhcPkg.DbOpenReadOnly)

      -- The only package db we open in read write mode is the one on the top of
      -- the stack.
      GhcPkg.DbOpenReadWrite TopOne -> do
        (db_stack, mto_modify) <- stateSequence Nothing
          [ \case
              to_modify@(Just _) -> (, to_modify) <$> readDatabase db_path
              Nothing -> if db_path /= top_db
                then (, Nothing) <$> readDatabase db_path
                else do
                  db <- readParseDatabase verbosity mb_user_conf
                                          mode use_cache db_path
                    `catchException` couldntOpenDbForModification db_path
                  let ro_db = db { packageDbLock = GhcPkg.DbOpenReadOnly }
                  return (ro_db, Just db)
          | db_path <- final_stack ]

        to_modify <- case mto_modify of
          Just db -> return db
          Nothing -> die "no database selected for modification"

        return (db_stack, GhcPkg.DbOpenReadWrite to_modify)

      -- The package db we open in read write mode is the first one included in
      -- flag_db_names that contains specified package. Therefore we need to
      -- open each one in read/write mode first and decide whether it's for
      -- modification based on its contents.
      GhcPkg.DbOpenReadWrite (ContainsPkg pkgarg) -> do
        (db_stack, mto_modify) <- stateSequence Nothing
          [ \case
              to_modify@(Just _) -> (, to_modify) <$> readDatabase db_path
              Nothing -> if db_path `notElem` flag_db_names
                then (, Nothing) <$> readDatabase db_path
                else do
                  let hasPkg :: PackageDB mode -> Bool
                      hasPkg = not . null . findPackage pkgarg . packages

                      openRo (e::IOError) = do
                        db <- readDatabase db_path
                        if hasPkg db
                          then couldntOpenDbForModification db_path e
                          else return (db, Nothing)

                  -- If we fail to open the database in read/write mode, we need
                  -- to check if it's for modification first before throwing an
                  -- error, so we attempt to open it in read only mode.
                  Exception.handle openRo $ do
                    db <- readParseDatabase verbosity mb_user_conf
                                            mode use_cache db_path
                    let ro_db = db { packageDbLock = GhcPkg.DbOpenReadOnly }
                    if hasPkg db
                      then return (ro_db, Just db)
                      else do
                        -- If the database is not for modification after all,
                        -- drop the write lock as we are already finished with
                        -- the database.
                        case packageDbLock db of
                          GhcPkg.DbOpenReadWrite lock ->
                            GhcPkg.unlockPackageDb lock
                        return (ro_db, Nothing)
          | db_path <- final_stack ]

        to_modify <- case mto_modify of
          Just db -> return db
          Nothing -> cannotFindPackage pkgarg Nothing

        return (db_stack, GhcPkg.DbOpenReadWrite to_modify)
      where
        couldntOpenDbForModification :: FilePath -> IOError -> IO a
        couldntOpenDbForModification db_path e = die $ "Couldn't open database "
          ++ db_path ++ " for modification: " ++ show e

        -- Parse package db in read-only mode.
        readDatabase :: FilePath -> IO (PackageDB 'GhcPkg.DbReadOnly)
        readDatabase db_path = do
          db <- readParseDatabase verbosity mb_user_conf
                                  GhcPkg.DbOpenReadOnly use_cache db_path
          if expand_vars
            then return $ mungePackageDBPaths top_dir db
            else return db

    stateSequence :: Monad m => s -> [s -> m (a, s)] -> m ([a], s)
    stateSequence s []     = return ([], s)
    stateSequence s (m:ms) = do
      (a, s')   <- m s
      (as, s'') <- stateSequence s' ms
      return (a : as, s'')

lookForPackageDBIn :: FilePath -> IO (Maybe FilePath)
lookForPackageDBIn dir = do
  let path_dir = dir </> "package.conf.d"
  exists_dir <- doesDirectoryExist path_dir
  if exists_dir then return (Just path_dir) else do
    let path_file = dir </> "package.conf"
    exists_file <- doesFileExist path_file
    if exists_file then return (Just path_file) else return Nothing

readParseDatabase :: forall mode t. Verbosity
                  -> Maybe (FilePath,Bool)
                  -> GhcPkg.DbOpenMode mode t
                  -> Bool -- use cache
                  -> FilePath
                  -> IO (PackageDB mode)
readParseDatabase verbosity mb_user_conf mode use_cache path
  -- the user database (only) is allowed to be non-existent
  | Just (user_conf,False) <- mb_user_conf, path == user_conf
  = do lock <- F.forM mode $ \_ -> do
         createDirectoryIfMissing True path
         GhcPkg.lockPackageDb cache
       mkPackageDB [] lock
  | otherwise
  = do e <- tryIO $ getDirectoryContents path
       case e of
         Left err
           | ioeGetErrorType err == InappropriateType -> do
              -- We provide a limited degree of backwards compatibility for
              -- old single-file style db:
              mdb <- tryReadParseOldFileStyleDatabase verbosity
                       mb_user_conf mode use_cache path
              case mdb of
                Just db -> return db
                Nothing ->
                  die $ "ghc no longer supports single-file style package "
                     ++ "databases (" ++ path ++ ") use 'ghc-pkg init'"
                     ++ "to create the database with the correct format."

           | otherwise -> ioError err
         Right fs
           | not use_cache -> ignore_cache (const $ return ())
           | otherwise -> do
              e_tcache <- tryIO $ getModificationTime cache
              case e_tcache of
                Left ex -> do
                  whenReportCacheErrors $
                    if isDoesNotExistError ex
                      then
                        -- It's fine if the cache is not there as long as the
                        -- database is empty.
                        unless (null confs) $ do
                            warn ("WARNING: cache does not exist: " ++ cache)
                            warn ("ghc will fail to read this package db. " ++
                                  recacheAdvice)
                      else do
                        warn ("WARNING: cache cannot be read: " ++ show ex)
                        warn "ghc will fail to read this package db."
                  ignore_cache (const $ return ())
                Right tcache -> do
                  when (verbosity >= Verbose) $ do
                      warn ("Timestamp " ++ show tcache ++ " for " ++ cache)
                  -- If any of the .conf files is newer than package.cache, we
                  -- assume that cache is out of date.
                  cache_outdated <- (`anyM` confs)
                    (fmap (tcache <) . getModificationTime)
                  if not cache_outdated
                      then do
                          when (verbosity > Normal) $
                             infoLn ("using cache: " ++ cache)
                          GhcPkg.readPackageDbForGhcPkg cache mode
                            >>= uncurry mkPackageDB
                      else do
                          whenReportCacheErrors $ do
                              warn ("WARNING: cache is out of date: " ++ cache)
                              warn ("ghc will see an old view of this " ++
                                    "package db. " ++ recacheAdvice)
                          ignore_cache $ \file -> do
                            when (verbosity >= Verbose) $ do
                              tFile <- getModificationTime file
                              let rel = case tcache `compare` tFile of
                                    LT -> " (NEWER than cache)"
                                    GT -> " (older than cache)"
                                    EQ -> " (same as cache)"
                              warn ("Timestamp " ++ show tFile
                                ++ " for " ++ file ++ rel)
            where
                 confs = map (path </>) $ filter (".conf" `isSuffixOf`) fs

                 ignore_cache :: (FilePath -> IO ()) -> IO (PackageDB mode)
                 ignore_cache checkTime = do
                     -- If we're opening for modification, we need to acquire a
                     -- lock even if we don't open the cache now, because we are
                     -- going to modify it later.
                     lock <- F.mapM (const $ GhcPkg.lockPackageDb cache) mode
                     let doFile f = do checkTime f
                                       parseSingletonPackageConf verbosity f
                     pkgs <- mapM doFile confs
                     mkPackageDB pkgs lock

                 -- We normally report cache errors for read-only commands,
                 -- since modify commands will usually fix the cache.
                 whenReportCacheErrors = when $ verbosity > Normal
                   || verbosity >= Normal && GhcPkg.isDbOpenReadMode mode
  where
    cache = path </> cachefilename

    recacheAdvice
      | Just (user_conf, True) <- mb_user_conf, path == user_conf
      = "Use 'ghc-pkg recache --user' to fix."
      | otherwise
      = "Use 'ghc-pkg recache' to fix."

    mkPackageDB :: [InstalledPackageInfo]
                -> GhcPkg.DbOpenMode mode GhcPkg.PackageDbLock
                -> IO (PackageDB mode)
    mkPackageDB pkgs lock = do
      path_abs <- absolutePath path
      return $ PackageDB {
          location = path,
          locationAbsolute = path_abs,
          packageDbLock = lock,
          packages = pkgs
        }

parseSingletonPackageConf :: Verbosity -> FilePath -> IO InstalledPackageInfo
parseSingletonPackageConf verbosity file = do
  when (verbosity > Normal) $ infoLn ("reading package config: " ++ file)
  BS.readFile file >>= fmap fst . parsePackageInfo

cachefilename :: FilePath
cachefilename = "package.cache"

mungePackageDBPaths :: FilePath -> PackageDB mode -> PackageDB mode
mungePackageDBPaths top_dir db@PackageDB { packages = pkgs } =
    db { packages = map (mungePackagePaths top_dir pkgroot) pkgs }
  where
    pkgroot = takeDirectory $ dropTrailingPathSeparator (locationAbsolute db)
    -- It so happens that for both styles of package db ("package.conf"
    -- files and "package.conf.d" dirs) the pkgroot is the parent directory
    -- ${pkgroot}/package.conf  or  ${pkgroot}/package.conf.d/

-- | Perform path/URL variable substitution as per the Cabal ${pkgroot} spec
-- (http://www.haskell.org/pipermail/libraries/2009-May/011772.html)
-- Paths/URLs can be relative to ${pkgroot} or ${pkgrooturl}.
-- The "pkgroot" is the directory containing the package database.
--
-- Also perform a similar substitution for the older GHC-specific
-- "$topdir" variable. The "topdir" is the location of the ghc
-- installation (obtained from the -B option).
mungePackagePaths :: FilePath -> FilePath
                  -> InstalledPackageInfo -> InstalledPackageInfo
mungePackagePaths top_dir pkgroot pkg =
   -- TODO: similar code is duplicated in GHC.Unit.Database
    pkg {
      importDirs  = munge_paths (importDirs pkg),
      includeDirs = munge_paths (includeDirs pkg),
      libraryDirs = munge_paths (libraryDirs pkg),
      libraryDynDirs = munge_paths (libraryDynDirs pkg),
      frameworkDirs = munge_paths (frameworkDirs pkg),
      haddockInterfaces = munge_paths (haddockInterfaces pkg),
      -- haddock-html is allowed to be either a URL or a file
      haddockHTMLs = munge_paths (munge_urls (haddockHTMLs pkg))
    }
  where
    munge_paths = map munge_path
    munge_urls  = map munge_url
    (munge_path,munge_url) = mkMungePathUrl top_dir pkgroot

mkMungePathUrl :: FilePath -> FilePath -> (FilePath -> FilePath, FilePath -> FilePath)
mkMungePathUrl top_dir pkgroot = (munge_path, munge_url)
   where
    munge_path p
      | Just p' <- stripVarPrefix "${pkgroot}" p = pkgroot ++ p'
      | Just p' <- stripVarPrefix "$topdir"    p = top_dir ++ p'
      | otherwise                                = p

    munge_url p
      | Just p' <- stripVarPrefix "${pkgrooturl}" p = toUrlPath pkgroot p'
      | Just p' <- stripVarPrefix "$httptopdir"   p = toUrlPath top_dir p'
      | otherwise                                   = p

    toUrlPath r p = "file:///"
                 -- URLs always use posix style '/' separators:
                 ++ FilePath.Posix.joinPath
                        (r : -- We need to drop a leading "/" or "\\"
                             -- if there is one:
                             dropWhile (all isPathSeparator)
                                       (FilePath.splitDirectories p))

    -- We could drop the separator here, and then use </> above. However,
    -- by leaving it in and using ++ we keep the same path separator
    -- rather than letting FilePath change it to use \ as the separator
    stripVarPrefix var path = case stripPrefix var path of
                              Just [] -> Just []
                              Just cs@(c : _) | isPathSeparator c -> Just cs
                              _ -> Nothing

-- -----------------------------------------------------------------------------
-- Workaround for old single-file style package dbs

-- Single-file style package dbs have been deprecated for some time, but
-- it turns out that Cabal was using them in one place. So this code is for a
-- workaround to allow older Cabal versions to use this newer ghc.

-- We check if the file db contains just "[]" and if so, we look for a new
-- dir-style db in path.d/, ie in a dir next to the given file.
-- We cannot just replace the file with a new dir style since Cabal still
-- assumes it's a file and tries to overwrite with 'writeFile'.

-- ghc itself also cooperates in this workaround

tryReadParseOldFileStyleDatabase :: Verbosity -> Maybe (FilePath, Bool)
                                 -> GhcPkg.DbOpenMode mode t -> Bool -> FilePath
                                 -> IO (Maybe (PackageDB mode))
tryReadParseOldFileStyleDatabase verbosity mb_user_conf
                                 mode use_cache path = do
  -- assumes we've already established that path exists and is not a dir
  content <- readFile path `catchIO` \_ -> return ""
  if take 2 content == "[]"
    then do
      path_abs <- absolutePath path
      let path_dir = adjustOldDatabasePath path
      warn $ "Warning: ignoring old file-style db and trying " ++ path_dir
      direxists <- doesDirectoryExist path_dir
      if direxists
        then do
          db <- readParseDatabase verbosity mb_user_conf mode use_cache path_dir
          -- but pretend it was at the original location
          return $ Just db {
              location         = path,
              locationAbsolute = path_abs
            }
         else do
           lock <- F.forM mode $ \_ -> do
             createDirectoryIfMissing True path_dir
             GhcPkg.lockPackageDb $ path_dir </> cachefilename
           return $ Just PackageDB {
               location         = path,
               locationAbsolute = path_abs,
               packageDbLock    = lock,
               packages         = []
             }

    -- if the path is not a file, or is not an empty db then we fail
    else return Nothing

adjustOldFileStylePackageDB :: PackageDB mode -> IO (PackageDB mode)
adjustOldFileStylePackageDB db = do
  -- assumes we have not yet established if it's an old style or not
  mcontent <- fmap Just (readFile (location db)) `catchIO` \_ -> return Nothing
  case fmap (take 2) mcontent of
    -- it is an old style and empty db, so look for a dir kind in location.d/
    Just "[]" -> return db {
        location         = adjustOldDatabasePath $ location db,
        locationAbsolute = adjustOldDatabasePath $ locationAbsolute db
      }
    -- it is old style but not empty, we have to bail
    Just  _   -> die $ "ghc no longer supports single-file style package "
                    ++ "databases (" ++ location db ++ ") use 'ghc-pkg init'"
                    ++ "to create the database with the correct format."
    -- probably not old style, carry on as normal
    Nothing   -> return db

adjustOldDatabasePath :: FilePath -> FilePath
adjustOldDatabasePath = (<.> "d")

parsePackageInfo
        :: BS.ByteString
        -> IO (InstalledPackageInfo, [ValidateWarning])
parsePackageInfo str =
  case parseInstalledPackageInfo str of
    Right (warnings, ok) -> pure (mungePackageInfo ok, ws)
      where
        ws = [ msg | msg <- warnings
                   , not ("Unrecognized field pkgroot" `isPrefixOf` msg) ]
    Left err -> die (unlines (F.toList err))

mungePackageInfo :: InstalledPackageInfo -> InstalledPackageInfo
mungePackageInfo ipi = ipi

-- -----------------------------------------------------------------------------
-- Making changes to a package database

data DBOp = RemovePackage InstalledPackageInfo
          | AddPackage    InstalledPackageInfo
          | ModifyPackage InstalledPackageInfo

changeNewDB :: Verbosity
            -> [DBOp]
            -> PackageDB 'GhcPkg.DbReadWrite
            -> PackageDBStack
            -> IO ()
changeNewDB verbosity cmds new_db db_stack = do
  new_db' <- adjustOldFileStylePackageDB new_db
  createDirectoryIfMissing True (location new_db')
  changeDBDir verbosity cmds new_db' db_stack

changeDBDir :: Verbosity
            -> [DBOp]
            -> PackageDB 'GhcPkg.DbReadWrite
            -> PackageDBStack
            -> IO ()
changeDBDir verbosity cmds db db_stack = do
  mapM_ do_cmd cmds
  updateDBCache verbosity db db_stack
 where
  do_cmd (RemovePackage p) = do
    let file = location db </> display (installedUnitId p) <.> "conf"
    when (verbosity > Normal) $ infoLn ("removing " ++ file)
    removeFileSafe file
  do_cmd (AddPackage p) = do
    let file = location db </> display (installedUnitId p) <.> "conf"
    when (verbosity > Normal) $ infoLn ("writing " ++ file)
    writeUTF8File file (showInstalledPackageInfo p)
  do_cmd (ModifyPackage p) =
    do_cmd (AddPackage p)

updateDBCache :: Verbosity
              -> PackageDB 'GhcPkg.DbReadWrite
              -> PackageDBStack
              -> IO ()
updateDBCache verbosity db db_stack = do
  let filename = location db </> cachefilename
      db_stack_below = stackUpTo (location db) db_stack

      pkgsCabalFormat :: [InstalledPackageInfo]
      pkgsCabalFormat = packages db

      -- | All the packages we can legally depend on in this step.
      dependablePkgsCabalFormat :: [InstalledPackageInfo]
      dependablePkgsCabalFormat = allPackagesInStack db_stack_below

      pkgsGhcCacheFormat :: [(PackageCacheFormat, Bool)]
      pkgsGhcCacheFormat
        -- See Note [Recompute abi-depends]
        = map (recomputeValidAbiDeps dependablePkgsCabalFormat .
            convertPackageInfoToCacheFormat)
          pkgsCabalFormat

      hasAnyAbiDepends :: InstalledPackageInfo -> Bool
      hasAnyAbiDepends x = not (null (abiDepends x))

  -- warn when we find any (possibly-)bogus abi-depends fields;
  -- See Note [Recompute abi-depends]
  when (verbosity >= Normal) $ do
    let definitelyBrokenPackages =
          nub
            . sort
            . map (unPackageName . GhcPkg.unitPackageName . fst)
            . filter snd
            $ pkgsGhcCacheFormat
    when (definitelyBrokenPackages /= []) $ do
      warn "the following packages have broken abi-depends fields:"
      forM_ definitelyBrokenPackages $ \pkg ->
        warn $ "    " ++ pkg
    when (verbosity > Normal) $ do
      let possiblyBrokenPackages =
            nub
              . sort
              . filter (not . (`elem` definitelyBrokenPackages))
              . map (unPackageName . pkgName . packageId)
              . filter hasAnyAbiDepends
              $ pkgsCabalFormat
      when (possiblyBrokenPackages /= []) $ do
          warn $
            "the following packages have correct abi-depends, " ++
            "but may break in the future:"
          forM_ possiblyBrokenPackages $ \pkg ->
            warn $ "    " ++ pkg

  when (verbosity > Normal) $
      infoLn ("writing cache " ++ filename)

  let d = fmap (fromPackageCacheFormat . fst) pkgsGhcCacheFormat
  GhcPkg.writePackageDb filename d pkgsCabalFormat
    `catchIO` \e ->
      if isPermissionError e
      then die $ filename ++ ": you don't have permission to modify this file"
      else ioError e

  case packageDbLock db of
    GhcPkg.DbOpenReadWrite lock -> GhcPkg.unlockPackageDb lock

type PackageCacheFormat = GhcPkg.GenericUnitInfo
                            PackageIdentifier
                            PackageName
                            UnitId
                            ModuleName
                            OpenModule

{- Note [Recompute abi-depends]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Like most fields, `ghc-pkg` relies on who-ever is performing package
registration to fill in fields; this includes the `abi-depends` field present
for the package.

However, this was likely a mistake, and is not very robust; in certain cases,
versions of Cabal may use bogus abi-depends fields for a package when doing
builds. Why? Because package database information is aggressively cached; it is
possible to work Cabal into a situation where it uses a cached version of
`abi-depends`, rather than the one in the actual database after it has been
recomputed.

However, there is an easy fix: ghc-pkg /already/ knows the `abi-depends` of a
package, because they are the ABIs of the packages pointed at by the `depends`
field. So it can simply look up the abi from the dependencies in the original
database, and ignore whatever the system registering gave it.

So, instead, we do two things here:

  - We throw away the information for a registered package's `abi-depends` field.

  - We recompute it: we simply look up the unit ID of the package in the original
    database, and use *its* abi-depends.

See #14381, and Cabal issue #4728.

Additionally, because we are throwing away the original (declared) ABI deps, we
return a boolean that indicates whether any abi-depends were actually
overridden.

-}

recomputeValidAbiDeps :: [InstalledPackageInfo]
                      -> PackageCacheFormat
                      -> (PackageCacheFormat, Bool)
recomputeValidAbiDeps db pkg =
  (pkg { GhcPkg.unitAbiDepends = newAbiDeps }, abiDepsUpdated)
  where
    newAbiDeps =
      catMaybes . flip map (GhcPkg.unitAbiDepends pkg) $ \(k, _) ->
        case filter (\d -> installedUnitId d == k) db of
          [x] -> Just (k, ST.pack $ unAbiHash (abiHash x))
          _   -> Nothing
    abiDepsUpdated =
      GhcPkg.unitAbiDepends pkg /= newAbiDeps


-- | Convert from PackageCacheFormat to DbUnitInfo (the format used in
-- Ghc.PackageDb to store into the database)
fromPackageCacheFormat :: PackageCacheFormat -> GhcPkg.DbUnitInfo
fromPackageCacheFormat = GhcPkg.mapGenericUnitInfo
     mkUnitId' mkPackageIdentifier' mkPackageName' mkModuleName' mkModule'
   where
     displayBS :: Pretty a => a -> BS.ByteString
     displayBS            = toUTF8BS . display
     mkPackageIdentifier' = displayBS
     mkPackageName'       = displayBS
     mkComponentId'       = displayBS
     mkUnitId'            = displayBS
     mkModuleName'        = displayBS
     mkInstUnitId' i      = case i of
       IndefFullUnitId cid insts -> DbInstUnitId (mkComponentId' cid)
                                                 (fmap (bimap mkModuleName' mkModule') (Map.toList insts))
       DefiniteUnitId uid        -> DbUnitId (mkUnitId' (unDefUnitId uid))
     mkModule' m = case m of
       OpenModule uid n -> DbModule (mkInstUnitId' uid) (mkModuleName' n)
       OpenModuleVar n  -> DbModuleVar  (mkModuleName' n)

convertPackageInfoToCacheFormat :: InstalledPackageInfo -> PackageCacheFormat
convertPackageInfoToCacheFormat pkg =
    GhcPkg.GenericUnitInfo {
       GhcPkg.unitId             = installedUnitId pkg,
       GhcPkg.unitInstanceOf     = mkUnitId (unComponentId (installedComponentId pkg)),
       GhcPkg.unitInstantiations = instantiatedWith pkg,
       GhcPkg.unitPackageId      = sourcePackageId pkg,
       GhcPkg.unitPackageName    = packageName pkg,
       GhcPkg.unitPackageVersion = Version.Version (versionNumbers (packageVersion pkg)) [],
       GhcPkg.unitComponentName  =
         fmap (mkPackageName . unUnqualComponentName) (libraryNameString $ sourceLibName pkg),
       GhcPkg.unitDepends        = depends pkg,
       GhcPkg.unitAbiDepends     = map (\(AbiDependency k v) -> (k,ST.pack $ unAbiHash v)) (abiDepends pkg),
       GhcPkg.unitAbiHash        = ST.pack $ unAbiHash (abiHash pkg),
       GhcPkg.unitImportDirs     = map ST.pack $ importDirs pkg,
       GhcPkg.unitLibraries      = map ST.pack $ hsLibraries pkg,
       GhcPkg.unitExtDepLibsSys  = map ST.pack $ extraLibraries pkg,
       GhcPkg.unitExtDepLibsGhc  = map ST.pack $ extraGHCiLibraries pkg,
       GhcPkg.unitLibraryDirs    = map ST.pack $ libraryDirs pkg,
       GhcPkg.unitLibraryDynDirs = map ST.pack $ libraryDynDirs pkg,
       GhcPkg.unitExtDepFrameworks = map ST.pack $ frameworks pkg,
       GhcPkg.unitExtDepFrameworkDirs = map ST.pack $ frameworkDirs pkg,
       GhcPkg.unitLinkerOptions  = map ST.pack $ ldOptions pkg,
       GhcPkg.unitCcOptions      = map ST.pack $ ccOptions pkg,
       GhcPkg.unitIncludes       = map ST.pack $ includes pkg,
       GhcPkg.unitIncludeDirs    = map ST.pack $ includeDirs pkg,
       GhcPkg.unitHaddockInterfaces = map ST.pack $ haddockInterfaces pkg,
       GhcPkg.unitHaddockHTMLs   = map ST.pack $ haddockHTMLs pkg,
       GhcPkg.unitExposedModules = map convertExposed (exposedModules pkg),
       GhcPkg.unitHiddenModules  = hiddenModules pkg,
       GhcPkg.unitIsIndefinite   = indefinite pkg,
       GhcPkg.unitIsExposed      = exposed pkg,
       GhcPkg.unitIsTrusted      = trusted pkg
    }
  where
    convertExposed (ExposedModule n reexport) = (n, reexport)

-- -----------------------------------------------------------------------------
-- Exposing, Hiding, Trusting, Distrusting, Unregistering are all similar

unregisterPackages ::
     Path Abs Dir
     -- ^ Path to the global package database.
  -> [PackageArg]
  -> Verbosity
  -> [Flag]
  -> Force
  -> IO ()
unregisterPackages globalDb pkgargs verbosity my_flags force = do
  pkgsByPkgDBs <- F.foldlM (getPkgsByPkgDBs []) [] pkgargs
  forM_ pkgsByPkgDBs unregisterPackages'
 where
  -- Update a list of 'packages by package database (and database stack)' for a
  -- package. Assumes that a package to be unregistered is in no more than one
  -- database.
  getPkgsByPkgDBs :: [(PackageDBAndStack 'DbReadWrite, [UnitId])]
                  -- ^ List of considered 'packages by package database'
                  -> [(PackageDBAndStack 'DbReadWrite, [UnitId])]
                  -- ^ List of to be considered 'packages by package database'
                  -> PackageArg
                  -- Package to update
                  -> IO [(PackageDBAndStack 'DbReadWrite, [UnitId])]
  -- No more 'packages by package database' to consider? We need to try to get
  -- another package database.
  getPkgsByPkgDBs pkgsByPkgDBs [] pkgarg = do
    (db_stack, GhcPkg.DbOpenReadWrite db, _flag_dbs) <-
      getPkgDatabases globalDb verbosity (GhcPkg.DbOpenReadWrite $ ContainsPkg pkgarg)
        True{-use user-} True{-use cache-} False{-expand vars-} my_flags
    pks <- do
      let pkgs = packages db
          ps = findPackage pkgarg pkgs
      -- This shouldn't happen if getPkgsByPkgDBs picks the DB correctly.
      when (null ps) $ cannotFindPackage pkgarg $ Just db
      pure (map installedUnitId ps)
    let pkgsByPkgDB = (PackageDBAndStack db db_stack, pks)
    pure (pkgsByPkgDB : pkgsByPkgDBs)
  -- Consider the next 'packages by package database' in the list of ones to
  -- consider.
  getPkgsByPkgDBs pkgsByPkgDBs ( pkgsByPkgDB : pkgsByPkgDBs') pkgarg = do
    let (packageDBAndStack, pks') = pkgsByPkgDB
        PackageDBAndStack db _ = packageDBAndStack
        pkgs = packages db
        ps = findPackage pkgarg pkgs
        pks = map installedUnitId ps
        pkgByPkgDB' = (packageDBAndStack, pks <> pks')
    if null ps
      then
        -- Not found in the package database? Add the package database to those
        -- considered and try with the remaining package databases to consider.
        getPkgsByPkgDBs ( pkgsByPkgDB : pkgsByPkgDBs ) pkgsByPkgDBs' pkgarg
      else
        -- Found in the package database? Add to the list of packages to be
        -- unregistered from that package database. TO DO: Perhaps check not
        -- already in that list for better error messages when there are
        -- duplicated requests to unregister.
        pure (pkgsByPkgDBs <> (pkgByPkgDB' : pkgsByPkgDBs'))

  unregisterPackages' :: (PackageDBAndStack 'DbReadWrite, [UnitId]) -> IO ()
  unregisterPackages' (PackageDBAndStack db db_stack, pks) = do
    let db_name = location db
        pkgs = packages db
        cmds = [ RemovePackage pkg
               | pkg <- pkgs, installedUnitId pkg `elem` pks
               ]
        new_db = db{ packages = pkgs' }
         where
          deleteFirstsBy' :: (a -> b -> Bool) -> [a] -> [b] -> [a]
          deleteFirstsBy' eq = foldl' (deleteBy' eq)

          deleteBy' :: (a -> b -> Bool) -> [a] -> b -> [a]
          deleteBy' _ [] _ = []
          deleteBy' eq (y:ys) x = if y `eq` x then ys else y : deleteBy' eq ys x

          pkgs' = deleteFirstsBy'
            (\p1 p2 -> installedUnitId p1 == p2) pkgs pks
    -- ...but do consistency checks with regards to the full stack
    consistencyChecks new_db db_name db_stack force
    -- Use changeNewDB, rather than changeDB, to avoid duplicating
    -- updateInternalDB db cmds
    changeNewDB verbosity cmds new_db db_stack

-- Do consistency checks with regards to the full stack
consistencyChecks :: PackageDB mode -> FilePath -> PackageDBStack -> Force -> IO ()
consistencyChecks new_db db_name db_stack force = do
  let new_db_ro = new_db { packageDbLock = GhcPkg.DbOpenReadOnly }
      old_broken = brokenPackages (allPackagesInStack db_stack)
      rest_of_stack = filter ((/= db_name) . location) db_stack
      new_stack = new_db_ro : rest_of_stack
      new_broken = brokenPackages (allPackagesInStack new_stack)
      newly_broken = filter ((`notElem` map installedUnitId old_broken)
                            . installedUnitId) new_broken
      displayQualPkgId pkg
        | [_] <- filter ((== pkgid) . mungedId)
                        (allPackagesInStack db_stack)
            = display pkgid
        | otherwise = display pkgid ++ "@" ++ display (installedUnitId pkg)
        where pkgid = mungedId pkg
  unless (null newly_broken) $
      dieOrForceAll force ("unregistering would break the following packages: "
              ++ unwords (map displayQualPkgId newly_broken))

findPackage :: PackageArg -> [InstalledPackageInfo] -> [InstalledPackageInfo]
findPackage pkgarg = filter (pkgarg `matchesPkg`)

cannotFindPackage :: PackageArg -> Maybe (PackageDB mode) -> IO a
cannotFindPackage pkgarg mdb = die $ "cannot find package " ++ pkg_msg pkgarg
  ++ maybe "" (\db -> " in " ++ location db) mdb
  where
    pkg_msg (Id pkgid)           = displayGlobPkgId pkgid
    pkg_msg (IUId ipid)          = display ipid
    pkg_msg (Substring pkgpat _) = "matching " ++ pkgpat

matches :: GlobPackageIdentifier -> MungedPackageId -> Bool
GlobPackageIdentifier pn `matches` pid'
  = pn == mungedName pid'
ExactPackageIdentifier pid `matches` pid'
  = mungedName pid == mungedName pid' &&
    (mungedVersion pid == mungedVersion pid' || mungedVersion pid == nullVersion)

matchesPkg :: PackageArg -> InstalledPackageInfo -> Bool
(Id pid)        `matchesPkg` pkg = pid `matches` mungedId pkg
(IUId ipid)     `matchesPkg` pkg = ipid == installedUnitId pkg
(Substring _ m) `matchesPkg` pkg = m (display (mungedId pkg))

closure :: [InstalledPackageInfo] -> [InstalledPackageInfo]
        -> ([InstalledPackageInfo], [InstalledPackageInfo])
closure = go
 where
   go avail not_avail =
     case partition (depsAvailable avail) not_avail of
        ([],        not_avail') -> (avail, not_avail')
        (new_avail, not_avail') -> go (new_avail ++ avail) not_avail'

   depsAvailable :: [InstalledPackageInfo] -> InstalledPackageInfo
                 -> Bool
   depsAvailable pkgs_ok pkg = null dangling
        where dangling = filter (`notElem` pids) (depends pkg)
              pids = map installedUnitId pkgs_ok

        -- we want mutually recursive groups of package to show up
        -- as broken. (#1750)

brokenPackages :: [InstalledPackageInfo] -> [InstalledPackageInfo]
brokenPackages pkgs = snd (closure [] pkgs)

-----------------------------------------------------------------------------
-- Sanity-check a new package config, and automatically build GHCi libs
-- if requested.

type ValidateError   = (Force,String)
type ValidateWarning = String

newtype Validate a = V { runValidate :: IO (a, [ValidateError],[ValidateWarning]) }

instance Functor Validate where
    fmap = liftM

instance Applicative Validate where
    pure a = V $ pure (a, [], [])
    (<*>) = ap

instance Monad Validate where
   m >>= k = V $ do
      (a, es, ws) <- runValidate m
      (b, es', ws') <- runValidate (k a)
      return (b,es++es',ws++ws')

-----------------------------------------------------------------------------

getProgramName :: IO String
getProgramName = fmap (`withoutSuffix` ".bin") getProgName
   where str `withoutSuffix` suff
            | suff `isSuffixOf` str = take (length str - length suff) str
            | otherwise             = str

die :: String -> IO a
die = dieWith 1

dieWith :: Int -> String -> IO a
dieWith ec s = do
  prog <- getProgramName
  reportError (prog ++ ": " ++ s)
  exitWith (ExitFailure ec)

dieOrForceAll :: Force -> String -> IO ()
dieOrForceAll ForceAll = ignoreError

warn :: String -> IO ()
warn = reportError

-- send info messages to stdout
infoLn :: String -> IO ()
infoLn = putStrLn

ignoreError :: String -> IO ()
ignoreError s = reportError (s ++ " (ignoring)")

reportError :: String -> IO ()
reportError s = do hFlush stdout; hPutStrLn stderr s

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = catchException

tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO = Exception.try

-- removeFileSave doesn't throw an exceptions, if the file is already deleted
removeFileSafe :: FilePath -> IO ()
removeFileSafe fn =
  removeFile fn `catchIO` \ e ->
    unless (isDoesNotExistError e) $ ioError e

-- | Turn a path relative to the current directory into a (normalised)
-- absolute path.
absolutePath :: FilePath -> IO FilePath
absolutePath path = normalise . (</> path) <$> getCurrentDirectory
