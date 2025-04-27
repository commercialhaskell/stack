
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Types.GhcPkgExe
License     : BSD-3-Clause
-}

module Stack.Types.GhcPkgExe
  ( GhcPkgPrettyException (..)
  , GlobPackageIdentifier (..)
  , PackageArg (..)
  ) where

import           Distribution.Package ( UnitId )
import           Distribution.Text ( display )
import           Path ( SomeBase (..) )
import           Stack.Prelude hiding ( display )

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "GHC.Utils.GhcPkg.Main.Compat" module or the "Stack.GhcPkg" module.
data GhcPkgPrettyException
  = CannotParse !String !String !String
  | CannotOpenDBForModification !(SomeBase Dir) !IOException
  | SingleFileDBUnsupported !(SomeBase Dir)
  | ParsePackageInfoExceptions !String
  | CannotFindPackage !PackageArg !(Maybe (SomeBase Dir))
  | CannotParseRelFileBug !String
  | CannotParseDirectoryWithDBug !String
  | CannotRecacheAfterUnregister !(Path Abs Dir) !SomeException
  deriving (Show, Typeable)

instance Pretty GhcPkgPrettyException where
  pretty (CannotParse str what e) =
    "[S-6512]"
    <> line
    <> fillSep
         [ flow "cannot parse"
         , style Current (fromString str)
         , flow "as a"
         , fromString what <> ":"
         ]
    <> blankLine
    <> fromString e
  pretty (CannotOpenDBForModification db_path e) =
    "[S-3384]"
    <> line
    <> fillSep
         [ flow "Couldn't open database"
         , pretty db_path
         , flow "for modification:"
         ]
    <> blankLine
    <> string (displayException e)
  pretty (SingleFileDBUnsupported path) =
    "[S-1430]"
    <> line
    <> fillSep
         [ flow "ghc no longer supports single-file style package databases"
         , parens (pretty path)
         , "use"
         , style Shell (flow "ghc-pkg init")
         , flow "to create the database with the correct format."
         ]
  pretty (ParsePackageInfoExceptions errs) =
    "[S-5996]"
    <> line
    <> flow errs
  pretty (CannotFindPackage pkgarg mdb_path) =
    "[S-3189]"
    <> line
    <> fillSep
         [ flow "cannot find package"
         , style Current (pkg_msg pkgarg)
         , maybe
             ""
             (\db_path -> fillSep ["in", pretty db_path])
             mdb_path
         ]
   where
    pkg_msg (Substring pkgpat _) = fillSep ["matching", fromString pkgpat]
    pkg_msg pkgarg' = fromString $ show pkgarg'
  pretty (CannotParseRelFileBug relFileName) = bugPrettyReport "[S-9323]" $
    fillSep
      [ flow "changeDBDir': Could not parse"
      , style File (fromString relFileName)
      , flow "as a relative path to a file."
      ]
  pretty (CannotParseDirectoryWithDBug dirName) = bugPrettyReport "[S-7651]" $
    fillSep
      [ flow "adjustOldDatabasePath: Could not parse"
      , style Dir (fromString dirName)
      , flow "as a directory."
      ]
  pretty (CannotRecacheAfterUnregister pkgDb e) =
    "[S-6590]"
    <> line
    <> fillSep
         [ flow "While recaching"
         , pretty pkgDb
         , flow "after unregistering packages, Stack encountered the following \
                \ error:"
         ]
    <> blankLine
    <> string (displayException e)

instance Exception GhcPkgPrettyException

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
  | Substring String (String -> Bool)

instance Show PackageArg where
  show (Id pkgid) = displayGlobPkgId pkgid
  show (IUId ipid) = display ipid
  show (Substring pkgpat _) = pkgpat

displayGlobPkgId :: GlobPackageIdentifier -> String
displayGlobPkgId (ExactPackageIdentifier pid) = display pid
displayGlobPkgId (GlobPackageIdentifier pn) = display pn ++ "-*"

-- | Either an exact t'PackageIdentifier', or a glob for all packages
-- matching 'PackageName'.
data GlobPackageIdentifier
  = ExactPackageIdentifier MungedPackageId
  | GlobPackageIdentifier  MungedPackageName
