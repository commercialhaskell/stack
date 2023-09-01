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
-- !11142), with:
-- * changeDBDir' does not perform an effective @ghc-pkg recache@,
-- * the cache is not used,
-- * consistency checks are not performed,
-- * redundant code deleted,
-- * Hlint applied, and
-- * explicit import lists.
--
-- The version of the ghc-pkg executable supplied with GHCs published before
-- 28 August 2023 does not efficiently bulk unregister. This module exports a
-- function that does efficiently bulk unregister.

module GHC.Utils.GhcPkg.Main.Compat
  ( ghcPkgUnregisterForce
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
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import           Data.List ( foldl', isPrefixOf, isSuffixOf, nub )
import           Data.Maybe ( mapMaybe )
import qualified Data.Traversable as F
import           Distribution.InstalledPackageInfo as Cabal
import           Distribution.Package ( UnitId, mungedId )
import qualified Distribution.Parsec as Cabal
import           Distribution.Text ( display )
import           Distribution.Types.MungedPackageName ( MungedPackageName )
import           Distribution.Types.MungedPackageId ( MungedPackageId (..) )
import           Distribution.Version ( nullVersion )
import           GHC.IO ( catchException )
import           GHC.IO.Exception (IOErrorType(InappropriateType))
import qualified GHC.Unit.Database as GhcPkg
import           Path ( Abs, Dir, Path, toFilePath )
import           Prelude
import           System.Directory
                   ( createDirectoryIfMissing, doesDirectoryExist
                   , getCurrentDirectory, getDirectoryContents, removeFile
                   )
import           System.Exit ( exitWith, ExitCode(..) )
import           System.Environment ( getProgName, getEnv )
import           System.FilePath as FilePath
import           System.IO ( hFlush, hPutStrLn, stderr, stdout )
import           System.IO.Error
                   ( ioeGetErrorType, isDoesNotExistError )

-- | Function equivalent to:
--
-- > ghc-pkg --no-user-package-db --package-db=<pkgDb> unregister [--ipid] <P>
--
ghcPkgUnregisterForce ::
     Path Abs Dir -- ^ Path to the global package database
  -> Path Abs Dir -- ^ Path to the package database
  -> Bool -- ^ Apply ghc-pkg's --ipid, --unit-id flag?
  -> [String] -- ^ Packages to unregister
  -> IO ()
ghcPkgUnregisterForce globalDb pkgDb hasIpid pkgarg_strs = do
  pkgargs <- forM pkgarg_strs $ readPackageArg as_arg
  unregisterPackages globalDb pkgargs verbosity cli
 where
  verbosity = Normal
  cli = [FlagConfig $ toFilePath pkgDb]
  as_arg = if hasIpid then AsUnitId else AsDefault

-- -----------------------------------------------------------------------------
-- Command-line syntax

newtype Flag
  = FlagConfig FilePath
  deriving Eq

data Verbosity = Silent | Normal | Verbose
    deriving (Show, Eq, Ord)

-- -----------------------------------------------------------------------------
-- Do the business

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

-- | Selector for picking the right package DB to modify as 'register' and
-- 'recache' operate on the database on top of the stack, whereas 'modify'
-- changes the first database that contains a specific package.
newtype DbModifySelector = ContainsPkg PackageArg

getPkgDatabases :: Path Abs Dir
                   -- ^ Path to the global package database.
                -> Verbosity
                -> PackageArg
                -> [Flag]
                -> IO (PackageDBStack,
                          -- the real package DB stack: [global,user] ++
                          -- DBs specified on the command line with -f.
                       GhcPkg.DbOpenMode GhcPkg.DbReadWrite (PackageDB GhcPkg.DbReadWrite),
                          -- which one to modify, if any
                       PackageDBStack)
                          -- the package DBs specified on the command
                          -- line, or [global,user] otherwise.  This
                          -- is used as the list of package DBs for
                          -- commands that just read the DB, such as 'list'.

getPkgDatabases globalDb verbosity pkgarg my_flags = do
  -- Second we determine the location of the global package config.  On Windows,
  -- this is found relative to the ghc-pkg.exe binary, whereas on Unix the
  -- location is passed to the binary using the --global-package-db flag by the
  -- wrapper script.
  let  global_conf = toFilePath globalDb

  let sys_databases = [global_conf]

  e_pkg_path <- tryIO (System.Environment.getEnv "GHC_PACKAGE_PATH")
  let env_stack =
        case e_pkg_path of
                Left  _ -> sys_databases
                Right path
                  | not (null path) && isSearchPathSeparator (last path)
                  -> splitSearchPath (init path) ++ sys_databases
                  | otherwise
                  -> splitSearchPath path

  let db_flags = mapMaybe is_db_flag my_flags
         where is_db_flag (FlagConfig f) = Just f

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

  (db_stack, db_to_operate_on) <- getDatabases flag_db_names final_stack

  let flag_db_stack = [ db | db_name <- flag_db_names,
                        db <- db_stack, location db == db_name ]

  when (verbosity > Normal) $ do
    infoLn ("db stack: " ++ show (map location db_stack))
    F.forM_ db_to_operate_on $ \db ->
      infoLn ("modifying: " ++ location db)
    infoLn ("flag db stack: " ++ show (map location flag_db_stack))

  return (db_stack, db_to_operate_on, flag_db_stack)
  where
    getDatabases flag_db_names final_stack = do
      -- The package db we open in read write mode is the first one included in
      -- flag_db_names that contains specified package. Therefore we need to
      -- open each one in read/write mode first and decide whether it's for
      -- modification based on its contents.
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
                    db <- readParseDatabase verbosity (GhcPkg.DbOpenReadWrite $ ContainsPkg pkgarg) db_path
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
        readDatabase = readParseDatabase verbosity GhcPkg.DbOpenReadOnly

    stateSequence :: Monad m => s -> [s -> m (a, s)] -> m ([a], s)
    stateSequence s []     = return ([], s)
    stateSequence s (m:ms) = do
      (a, s')   <- m s
      (as, s'') <- stateSequence s' ms
      return (a : as, s'')

readParseDatabase :: forall mode t. Verbosity
                  -> GhcPkg.DbOpenMode mode t
                  -> FilePath
                  -> IO (PackageDB mode)
readParseDatabase verbosity mode path = do
       e <- tryIO $ getDirectoryContents path
       case e of
         Left err
           | ioeGetErrorType err == InappropriateType -> do
              -- We provide a limited degree of backwards compatibility for
              -- old single-file style db:
              mdb <- tryReadParseOldFileStyleDatabase verbosity mode path
              case mdb of
                Just db -> return db
                Nothing ->
                  die $ "ghc no longer supports single-file style package "
                     ++ "databases (" ++ path ++ ") use 'ghc-pkg init'"
                     ++ "to create the database with the correct format."

           | otherwise -> ioError err
         Right fs -> ignore_cache (const $ return ())
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

  where
    cache = path </> cachefilename

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

tryReadParseOldFileStyleDatabase ::
     Verbosity
  -> GhcPkg.DbOpenMode mode t
  -> FilePath
  -> IO (Maybe (PackageDB mode))
tryReadParseOldFileStyleDatabase verbosity mode path = do
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
          db <- readParseDatabase verbosity mode path_dir
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

newtype DBOp = RemovePackage InstalledPackageInfo

changeNewDB :: Verbosity
            -> [DBOp]
            -> PackageDB 'GhcPkg.DbReadWrite
            -> IO ()
changeNewDB verbosity cmds new_db = do
  new_db' <- adjustOldFileStylePackageDB new_db
  createDirectoryIfMissing True (location new_db')
  changeDBDir' verbosity cmds new_db'

changeDBDir' :: Verbosity
             -> [DBOp]
             -> PackageDB 'GhcPkg.DbReadWrite
             -> IO ()
changeDBDir' verbosity cmds db = do
  mapM_ do_cmd cmds
  case packageDbLock db of
    GhcPkg.DbOpenReadWrite lock -> liftIO $ GhcPkg.unlockPackageDb lock
 where
  do_cmd (RemovePackage p) = do
    let file = location db </> display (installedUnitId p) <.> "conf"
    when (verbosity > Normal) $ infoLn ("removing " ++ file)
    removeFileSafe file

-- -----------------------------------------------------------------------------
-- Exposing, Hiding, Trusting, Distrusting, Unregistering are all similar

unregisterPackages ::
     Path Abs Dir
     -- ^ Path to the global package database.
  -> [PackageArg]
  -> Verbosity
  -> [Flag]
  -> IO ()
unregisterPackages globalDb pkgargs verbosity my_flags = do
  pkgsByPkgDBs <- F.foldlM (getPkgsByPkgDBs []) [] pkgargs
  forM_ pkgsByPkgDBs unregisterPackages'
 where
  -- Update a list of 'packages by package database' for a package. Assumes that
  -- a package to be unregistered is in no more than one database.
  getPkgsByPkgDBs :: [(PackageDB GhcPkg.DbReadWrite, [UnitId])]
                  -- ^ List of considered 'packages by package database'
                  -> [(PackageDB GhcPkg.DbReadWrite, [UnitId])]
                  -- ^ List of to be considered 'packages by package database'
                  -> PackageArg
                  -- Package to update
                  -> IO [(PackageDB GhcPkg.DbReadWrite, [UnitId])]
  -- No more 'packages by package database' to consider? We need to try to get
  -- another package database.
  getPkgsByPkgDBs pkgsByPkgDBs [] pkgarg = do
    (_, GhcPkg.DbOpenReadWrite db, _flag_dbs) <-
      getPkgDatabases globalDb verbosity pkgarg my_flags
    pks <- do
      let pkgs = packages db
          ps = findPackage pkgarg pkgs
      -- This shouldn't happen if getPkgsByPkgDBs picks the DB correctly.
      when (null ps) $ cannotFindPackage pkgarg $ Just db
      pure (map installedUnitId ps)
    let pkgsByPkgDB = (db, pks)
    pure (pkgsByPkgDB : pkgsByPkgDBs)
  -- Consider the next 'packages by package database' in the list of ones to
  -- consider.
  getPkgsByPkgDBs pkgsByPkgDBs ( pkgsByPkgDB : pkgsByPkgDBs') pkgarg = do
    let (db, pks') = pkgsByPkgDB
        pkgs = packages db
        ps = findPackage pkgarg pkgs
        pks = map installedUnitId ps
        pkgByPkgDB' = (db, pks <> pks')
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

  unregisterPackages' :: (PackageDB GhcPkg.DbReadWrite, [UnitId]) -> IO ()
  unregisterPackages' (db, pks) = do
    let pkgs = packages db
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
    -- Use changeNewDB, rather than changeDB, to avoid duplicating
    -- updateInternalDB db cmds
    changeNewDB verbosity cmds new_db

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

-----------------------------------------------------------------------------
-- Sanity-check a new package config, and automatically build GHCi libs
-- if requested.

type ValidateError   = String
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

warn :: String -> IO ()
warn = reportError

-- send info messages to stdout
infoLn :: String -> IO ()
infoLn = putStrLn

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
