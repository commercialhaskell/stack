{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : GHC.Utils.GhcPkg.Main.Compat
License     : BSD-3-Clause

This module is based on GHC's utils\ghc-pkg\Main.hs at
commit f66fc15f2e6849125074bcfeb44334a663323ca6 (see GHC merge request !11142),
with:

* changeDBDir' does not perform an effective @ghc-pkg recache@,
* the cache is not used,
* consistency checks are not performed,
* use Stack program name,
* use "Stack.Prelude" rather than "Prelude",
* use t'RIO' @env@ monad,
* use well-typed representations of paths from the @path@ package,
* add pretty messages and exceptions,
* redundant code deleted,
* Hlint applied, and
* explicit import lists.

The version of the ghc-pkg executable supplied with GHCs published before
28 August 2023 does not efficiently bulk unregister. This module exports a
function that does efficiently bulk unregister.
-}

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

import qualified Data.Foldable as F
import qualified Data.Traversable as F
import           Distribution.InstalledPackageInfo as Cabal
import           Distribution.Package ( UnitId, mungedId )
import qualified Distribution.Parsec as Cabal
import           Distribution.Text ( display )
import           Distribution.Version ( nullVersion )
import           GHC.IO.Exception (IOErrorType(InappropriateType))
import qualified GHC.Unit.Database as GhcPkg
import           Path
                   ( SomeBase (..), fileExtension, mapSomeBase, parseRelFile
                   , parseSomeDir, prjSomeBase
                   )
import qualified Path as P
import           Path.IO
                   ( createDirIfMissing, doesDirExist, listDir, removeFile )
import qualified RIO.ByteString as BS
import           RIO.List ( isPrefixOf, stripSuffix )
import           RIO.NonEmpty ( nonEmpty )
import qualified RIO.NonEmpty as NE
import           Stack.Constants ( relFilePackageCache )
import           Stack.Prelude hiding ( display )
import           Stack.Types.GhcPkgExe
                   ( GhcPkgPrettyException (..), GlobPackageIdentifier (..)
                   , PackageArg (..)
                   )
import           System.Environment ( getEnv )
import           System.FilePath as FilePath
import           System.IO ( readFile )
import           System.IO.Error
                   ( ioeGetErrorType, ioError, isDoesNotExistError )

-- | Function equivalent to:
--
-- > ghc-pkg --no-user-package-db --package-db=<pkgDb> unregister [--ipid] <P>
--
ghcPkgUnregisterForce ::
     HasTerm env
  => Path Abs Dir -- ^ Path to the global package database
  -> Path Abs Dir -- ^ Path to the package database
  -> Bool -- ^ Apply ghc-pkg's --ipid, --unit-id flag?
  -> [String] -- ^ Packages to unregister
  -> RIO env ()
ghcPkgUnregisterForce globalDb pkgDb hasIpid pkgarg_strs = do
  pkgargs <- forM pkgarg_strs $ readPackageArg as_arg
  prettyDebugL
    $ flow "Unregistering from"
    : (pretty pkgDb <> ":")
    : mkNarrativeList (Just Current) False
        (map (fromString . show) pkgargs :: [StyleDoc])
  unregisterPackages globalDb pkgargs pkgDb
 where
  as_arg = if hasIpid then AsUnitId else AsDefault

-- -----------------------------------------------------------------------------
-- Do the business

-- | Enum flag representing argument type
data AsPackageArg
  = AsUnitId
  | AsDefault

parseCheck :: Cabal.Parsec a => String -> String -> RIO env a
parseCheck str what =
  case Cabal.eitherParsec str of
    Left e  -> prettyThrowIO $ CannotParse str what e
    Right x -> pure x

readGlobPkgId :: String -> RIO env GlobPackageIdentifier
readGlobPkgId str = case stripSuffix "-*" str of
  Nothing ->
    ExactPackageIdentifier <$> parseCheck str "package identifier (exact)"
  Just str' ->
    GlobPackageIdentifier <$> parseCheck str' "package identifier (glob)"

readPackageArg :: AsPackageArg -> String -> RIO env PackageArg
readPackageArg AsUnitId str = IUId <$> parseCheck str "installed package id"
readPackageArg AsDefault str = Id <$> readGlobPkgId str

-- -----------------------------------------------------------------------------
-- Package databases

data PackageDB (mode :: GhcPkg.DbMode) = PackageDB
  { location :: !(SomeBase Dir)
    -- We only need possibly-relative package db location. The relative
    -- location is used as an identifier for the db, so it is important we do
    -- not modify it.
  , packageDbLock :: !(GhcPkg.DbOpenMode mode GhcPkg.PackageDbLock)
    -- If package db is open in read write mode, we keep its lock around for
    -- transactional updates.
  , packages :: [InstalledPackageInfo]
  }

-- | A stack of package databases. Convention: head is the topmost in the stack.
type PackageDBStack = [PackageDB 'GhcPkg.DbReadOnly]

-- | Selector for picking the right package DB to modify as \'modify\' changes
-- the first database that contains a specific package.
newtype DbModifySelector = ContainsPkg PackageArg

getPkgDatabases ::
     forall env. HasTerm env
  => Path Abs Dir
     -- ^ Path to the global package database.
  -> PackageArg
  -> Path Abs Dir
     -- ^ Path to the package database.
  -> RIO
       env
       ( PackageDBStack
          -- the real package DB stack: [global,user] ++ DBs specified on the
          -- command line with -f.
       , GhcPkg.DbOpenMode GhcPkg.DbReadWrite (PackageDB GhcPkg.DbReadWrite)
         -- which one to modify, if any
       , PackageDBStack
         -- the package DBs specified on the command line, or [global,user]
         -- otherwise. This is used as the list of package DBs for commands
         -- that just read the DB, such as 'list'.
       )
getPkgDatabases globalDb pkgarg pkgDb = do
  -- Second we determine the location of the global package config.  On Windows,
  -- this is found relative to the ghc-pkg.exe binary, whereas on Unix the
  -- location is passed to the binary using the --global-package-db flag by the
  -- wrapper script.
  let sys_databases = [Abs globalDb]
  e_pkg_path <- tryIO (liftIO $ System.Environment.getEnv "GHC_PACKAGE_PATH")
  let env_stack =
        case nonEmpty <$> e_pkg_path of
          Left _ -> sys_databases
          Right Nothing -> []
          Right (Just path)
            | isSearchPathSeparator (NE.last path)
            -> mapMaybe parseSomeDir (splitSearchPath (NE.init path)) <> sys_databases
            | otherwise
            -> mapMaybe parseSomeDir (splitSearchPath $ NE.toList path)

  -- -f flags on the command line add to the database stack, unless any of them
  -- are present in the stack already.
  let final_stack = [Abs pkgDb | Abs pkgDb `notElem` env_stack] <> env_stack

  (db_stack, db_to_operate_on) <- getDatabases pkgDb final_stack

  let flag_db_stack = [ db | db <- db_stack, db.location == Abs pkgDb ]

  prettyDebugL
    $ flow "Db stack:"
    : map (pretty . (.location)) db_stack
  F.forM_ db_to_operate_on $ \db ->
    prettyDebugL
      [ "Modifying:"
      , pretty db.location
      ]
  prettyDebugL
    $ flow "Flag db stack:"
    : map (pretty . (.location)) flag_db_stack

  pure (db_stack, db_to_operate_on, flag_db_stack)
 where
  getDatabases flag_db_name final_stack = do
    -- The package db we open in read write mode is the first one included in
    -- flag_db_names that contains specified package. Therefore we need to
    -- open each one in read/write mode first and decide whether it's for
    -- modification based on its contents.
      (db_stack, mto_modify) <- stateSequence Nothing
        [ \case
            to_modify@(Just _) -> (, to_modify) <$> readDatabase db_path
            Nothing -> if db_path /= Abs flag_db_name
              then (, Nothing) <$> readDatabase db_path
              else do
                let hasPkg :: PackageDB mode -> Bool
                    hasPkg = not . null . findPackage pkgarg . (.packages)

                    openRo (e::IOException) = do
                      db <- readDatabase db_path
                      if hasPkg db
                        then
                          prettyThrowIO $ CannotOpenDBForModification db_path e
                        else pure (db, Nothing)

                -- If we fail to open the database in read/write mode, we need
                -- to check if it's for modification first before throwing an
                -- error, so we attempt to open it in read only mode.
                handle openRo $ do
                  db <- readParseDatabase
                          (GhcPkg.DbOpenReadWrite $ ContainsPkg pkgarg) db_path
                  let ro_db = db { packageDbLock = GhcPkg.DbOpenReadOnly }
                  if hasPkg db
                    then pure (ro_db, Just db)
                    else do
                      -- If the database is not for modification after all,
                      -- drop the write lock as we are already finished with
                      -- the database.
                      case db.packageDbLock of
                        GhcPkg.DbOpenReadWrite lock ->
                          liftIO $ GhcPkg.unlockPackageDb lock
                      pure (ro_db, Nothing)
        | db_path <- final_stack ]

      to_modify <- case mto_modify of
        Just db -> pure db
        Nothing -> cannotFindPackage pkgarg Nothing

      pure (db_stack, GhcPkg.DbOpenReadWrite to_modify)
   where
    -- Parse package db in read-only mode.
    readDatabase :: SomeBase Dir -> RIO env (PackageDB 'GhcPkg.DbReadOnly)
    readDatabase = readParseDatabase GhcPkg.DbOpenReadOnly

  stateSequence :: Monad m => s -> [s -> m (a, s)] -> m ([a], s)
  stateSequence s []     = pure ([], s)
  stateSequence s (m:ms) = do
    (a, s')   <- m s
    (as, s'') <- stateSequence s' ms
    pure (a : as, s'')

readParseDatabase ::
     forall mode t env. HasTerm env
  => GhcPkg.DbOpenMode mode t
  -> SomeBase Dir
  -> RIO env (PackageDB mode)
readParseDatabase mode path = do
  e <- tryIO $ prjSomeBase listDir path
  case e of
    Left err
      | ioeGetErrorType err == InappropriateType -> do
         -- We provide a limited degree of backwards compatibility for
         -- old single-file style db:
         mdb <- tryReadParseOldFileStyleDatabase mode path
         case mdb of
           Just db -> pure db
           Nothing -> prettyThrowIO $ SingleFileDBUnsupported path

      | otherwise -> liftIO $ ioError err
    Right (_, fs) -> ignore_cache
     where
      confs = filter isConf fs

      isConf :: Path Abs File -> Bool
      isConf f = case fileExtension f of
        Nothing -> False
        Just ext -> ext == ".conf"

      ignore_cache :: RIO env (PackageDB mode)
      ignore_cache = do
        -- If we're opening for modification, we need to acquire a lock even if
        -- we don't open the cache now, because we are going to modify it later.
        lock <- liftIO $
          F.mapM (const $ GhcPkg.lockPackageDb (prjSomeBase toFilePath cache)) mode
        pkgs <- mapM parseSingletonPackageConf confs
        mkPackageDB pkgs lock
 where
  cache = mapSomeBase (P.</> relFilePackageCache) path

  mkPackageDB ::
       [InstalledPackageInfo]
    -> GhcPkg.DbOpenMode mode GhcPkg.PackageDbLock
    -> RIO env (PackageDB mode)
  mkPackageDB pkgs lock =
    pure PackageDB
      { location = path
      , packageDbLock = lock
      , packages = pkgs
      }

parseSingletonPackageConf ::
     HasTerm env
  => Path Abs File
  -> RIO env InstalledPackageInfo
parseSingletonPackageConf file = do
  prettyDebugL
    [ flow "Reading package config:"
    , pretty file
    ]
  BS.readFile (toFilePath file) >>= fmap fst . parsePackageInfo

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
     HasTerm env
  => GhcPkg.DbOpenMode mode t
  -> SomeBase Dir
  -> RIO env (Maybe (PackageDB mode))
tryReadParseOldFileStyleDatabase mode path = do
  -- assumes we've already established that path exists and is not a dir
  content <- liftIO $ readFile (prjSomeBase toFilePath path) `catchIO` \_ -> pure ""
  if take 2 content == "[]"
    then do
      path_dir <- adjustOldDatabasePath path
      prettyWarnL
        [ flow "Ignoring old file-style db and trying"
        , pretty path_dir
        ]
      direxists <- prjSomeBase doesDirExist path_dir
      if direxists
        then do
          db <- readParseDatabase mode path_dir
          -- but pretend it was at the original location
          pure $ Just db { location         = path }
         else do
           lock <- F.forM mode $ \_ -> do
             prjSomeBase (createDirIfMissing True) path_dir
             liftIO $ GhcPkg.lockPackageDb $
               prjSomeBase (toFilePath . (P.</> relFilePackageCache)) path_dir
           pure $ Just PackageDB
             { location         = path
             , packageDbLock    = lock
             , packages         = []
             }

    -- if the path is not a file, or is not an empty db then we fail
    else pure Nothing

adjustOldFileStylePackageDB :: PackageDB mode -> RIO env (PackageDB mode)
adjustOldFileStylePackageDB db = do
  -- assumes we have not yet established if it's an old style or not
  mcontent <- liftIO $
    fmap Just (readFile (prjSomeBase toFilePath db.location)) `catchIO` \_ -> pure Nothing
  case fmap (take 2) mcontent of
    -- it is an old style and empty db, so look for a dir kind in location.d/
    Just "[]" -> do
      adjustedDatabasePath <- adjustOldDatabasePath db.location
      pure db { location = adjustedDatabasePath }
    -- it is old style but not empty, we have to bail
    Just _ -> prettyThrowIO $ SingleFileDBUnsupported db.location
    -- probably not old style, carry on as normal
    Nothing -> pure db

adjustOldDatabasePath :: SomeBase Dir -> RIO env (SomeBase Dir)
adjustOldDatabasePath = prjSomeBase addDToDirName
 where
  addDToDirName dir = do
    let dirNameWithD = toFilePath dir <> ".d"
    maybe
      (prettyThrowIO $ CannotParseDirectoryWithDBug dirNameWithD)
      pure
      (parseSomeDir dirNameWithD)

parsePackageInfo :: BS.ByteString -> RIO env (InstalledPackageInfo, [String])
parsePackageInfo str =
  case parseInstalledPackageInfo str of
    Right (warnings, ok) -> pure (mungePackageInfo ok, ws)
     where
      ws = [ msg | msg <- warnings
                 , not ("Unrecognized field pkgroot" `isPrefixOf` msg) ]
    Left err -> prettyThrowIO $ ParsePackageInfoExceptions (unlines (F.toList err))

mungePackageInfo :: InstalledPackageInfo -> InstalledPackageInfo
mungePackageInfo ipi = ipi

-- -----------------------------------------------------------------------------
-- Making changes to a package database

newtype DBOp = RemovePackage InstalledPackageInfo

changeNewDB ::
     HasTerm env
  => [DBOp]
  -> PackageDB 'GhcPkg.DbReadWrite
  -> RIO env ()
changeNewDB cmds new_db = do
  new_db' <- adjustOldFileStylePackageDB new_db
  prjSomeBase (createDirIfMissing True) new_db'.location
  changeDBDir' cmds new_db'

changeDBDir' ::
     HasTerm env
  => [DBOp]
  -> PackageDB 'GhcPkg.DbReadWrite
  -> RIO env ()
changeDBDir' cmds db = do
  mapM_ do_cmd cmds
  case db.packageDbLock of
    GhcPkg.DbOpenReadWrite lock -> liftIO $ GhcPkg.unlockPackageDb lock
 where
  do_cmd (RemovePackage p) = do
    let relFileConfName = display (installedUnitId p) <> ".conf"
    relFileConf <- maybe
      (prettyThrowIO $ CannotParseRelFileBug relFileConfName)
      pure
      (parseRelFile relFileConfName)
    let file = mapSomeBase (P.</> relFileConf) db.location
    prettyDebugL
      [ "Removing"
      , pretty file
      ]
    removeFileSafe file

unregisterPackages ::
     forall env. HasTerm env
  => Path Abs Dir
     -- ^ Path to the global package database.
  -> [PackageArg]
  -> Path Abs Dir
     -- ^ Path to the package database.
  -> RIO env ()
unregisterPackages globalDb pkgargs pkgDb = do
  pkgsByPkgDBs <- F.foldlM (getPkgsByPkgDBs []) [] pkgargs
  forM_ pkgsByPkgDBs unregisterPackages'
 where
  -- Update a list of 'packages by package database' for a package. Assumes that
  -- a package to be unregistered is in no more than one database.
  getPkgsByPkgDBs ::
       [(PackageDB GhcPkg.DbReadWrite, [UnitId])]
       -- ^ List of considered 'packages by package database'
    -> [(PackageDB GhcPkg.DbReadWrite, [UnitId])]
       -- ^ List of to be considered 'packages by package database'
    -> PackageArg
       -- Package to update
    -> RIO env [(PackageDB GhcPkg.DbReadWrite, [UnitId])]
  -- No more 'packages by package database' to consider? We need to try to get
  -- another package database.
  getPkgsByPkgDBs pkgsByPkgDBs [] pkgarg =
    getPkgDatabases globalDb pkgarg pkgDb >>= \case
      (_, GhcPkg.DbOpenReadWrite (db :: PackageDB GhcPkg.DbReadWrite), _) -> do
        pks <- do
          let pkgs = db.packages
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
        pkgs = db.packages
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

  unregisterPackages' :: (PackageDB GhcPkg.DbReadWrite, [UnitId]) -> RIO env ()
  unregisterPackages' (db, pks) = do
    let pkgs = db.packages
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

          pkgs' = deleteFirstsBy' (\p1 p2 -> installedUnitId p1 == p2) pkgs pks
    -- Use changeNewDB, rather than changeDB, to avoid duplicating
    -- updateInternalDB db cmds
    changeNewDB cmds new_db

findPackage :: PackageArg -> [InstalledPackageInfo] -> [InstalledPackageInfo]
findPackage pkgarg = filter (pkgarg `matchesPkg`)

cannotFindPackage :: PackageArg -> Maybe (PackageDB mode) -> RIO env a
cannotFindPackage pkgarg mdb =
  prettyThrowIO $ CannotFindPackage pkgarg ((.location) <$> mdb)

matches :: GlobPackageIdentifier -> MungedPackageId -> Bool
GlobPackageIdentifier pn `matches` pid' = pn == mungedName pid'
ExactPackageIdentifier pid `matches` pid' =
     mungedName pid == mungedName pid'
  && (  mungedVersion pid == mungedVersion pid'
     || mungedVersion pid == nullVersion
     )

matchesPkg :: PackageArg -> InstalledPackageInfo -> Bool
(Id pid)        `matchesPkg` pkg = pid `matches` mungedId pkg
(IUId ipid)     `matchesPkg` pkg = ipid == installedUnitId pkg
(Substring _ m) `matchesPkg` pkg = m (display (mungedId pkg))

-- removeFileSave doesn't throw an exceptions, if the file is already deleted
removeFileSafe :: SomeBase File -> RIO env ()
removeFileSafe fn = do
  prjSomeBase removeFile fn `catchIO` \ e ->
    unless (isDoesNotExistError e) $ liftIO $ ioError e
