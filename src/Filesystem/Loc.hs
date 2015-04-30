{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}

-- | A location on the file system.

module Filesystem.Loc
  (-- * Types
   Loc
  ,Base(..)
  ,Type(..)
  -- * Conversion
  ,parseAbsoluteDirLoc
  ,parseAbsoluteFileLoc
  ,parseRelativeDirLoc
  ,parseRelativeFileLoc
  ,toFilePath
  ,encodeString
  -- * Operations
  ,filename
  ,dirname
  ,parent
  ,getWorkingDir
  ,getHomeDir
  ,isParentOf
  ,appendLoc
  ,stripDir
  ,isHiddenDir
  -- * Template-Haskell helpers
  ,mkAbsoluteFile
  ,mkAbsoluteDir
  ,mkRelativeDir
  ,mkRelativeFile
  )
  where

import           Control.Exception
import           Control.Monad.Catch
import           Data.Data
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Filesystem
import           Filesystem.Path (FilePath)
import qualified Filesystem.Path.CurrentOS as FP
import           Language.Haskell.TH (Q,Exp(..),Lit(..))
import           Prelude hiding (FilePath)
import           System.FilePath hiding (combine,FilePath)

--------------------------------------------------------------------------------
-- Types

-- | Exception when parsing a location.
data LocException
  = InvalidAbsoluteDir FilePath
  | InvalidAbsoluteFile FilePath
  | AbsolutePathDoesn'tExist FilePath
  | InvalidRelativeDir FilePath
  | InvalidRelativeFile FilePath
  | RelativePathDoesn'tExist FilePath
  deriving (Show,Typeable)
instance Exception LocException

-- | Base of a location.
data Base
  = Absolute
  | Relative
  deriving (Data,Typeable)
deriving instance Typeable 'Absolute
deriving instance Typeable 'Relative

-- | Type of a location.
data Type
  = File
  | Dir
  deriving (Data,Typeable)
deriving instance Typeable 'File
deriving instance Typeable 'Dir

-- | Opaque location type.
newtype Loc (b :: Base) (t :: Type) = Loc FilePath
  deriving (Data,Typeable)
deriving instance Show (Loc b t)
deriving instance Eq (Loc b t)
deriving instance Ord (Loc b t)

--------------------------------------------------------------------------------
-- Conversion

-- | Get a location for an absolute directory. Produces a normalized
--  path which always ends in a path separator.
parseAbsoluteDirLoc :: MonadThrow m => FilePath -> m (Loc Absolute Dir)
parseAbsoluteDirLoc filepath =
  if isAbsolute fp
     then return (Loc (normalizeDir filepath))
     else throwM (InvalidAbsoluteDir filepath)
  where fp = FP.encodeString filepath

-- | Get a location for a relative directory. Produces a normalized
-- path which always ends in a path separator.
parseRelativeDirLoc :: MonadThrow m
                    => FilePath -- ^ Relative dir.
                    -> m (Loc Relative Dir)
parseRelativeDirLoc filepath =
  if not (isAbsolute fp) || not (null fp)
     then return (Loc (if null fp
                          then filepath
                          else normalizeDir filepath))
     else throwM (InvalidRelativeDir filepath)
  where fp = FP.encodeString filepath

-- | Get a location for an absolute file. Produces a normalized
--  path which always ends in a path separator.
parseAbsoluteFileLoc :: MonadThrow m
                     => FilePath -> m (Loc Absolute File)
parseAbsoluteFileLoc filepath =
  if isAbsolute fp && not (hasTrailingPathSeparator fp)
     then return (Loc (normalizeFile filepath))
     else throwM (InvalidAbsoluteFile filepath)
  where fp = FP.encodeString filepath

-- | Get a location for a relative file. Produces a normalized
-- path which always ends in a path separator.
parseRelativeFileLoc :: MonadThrow m
                     => FilePath -- ^ Relative file.
                     -> m (Loc Relative File)
parseRelativeFileLoc filepath =
  if not (isAbsolute fp || hasTrailingPathSeparator fp) && not (null fp)
     then return (Loc (normalizeFile filepath))
     else throwM (InvalidRelativeFile filepath)
  where fp = FP.encodeString filepath

-- | Internal use for normalizing a directory.
--
-- Can this be implemented without the overhead?
normalizeDir :: FilePath -> FilePath
normalizeDir =
  FP.decodeString . clean . addTrailingPathSeparator . normalise . FP.encodeString
  where clean "./" = ""
        clean x = x

-- | Internal use for normalizing a file.
--
-- Can this be implemented without the overhead?
normalizeFile :: FilePath -> FilePath
normalizeFile = FP.decodeString . normalise . FP.encodeString

-- | Convert to a file path.
toFilePath :: Loc a b -> FilePath
toFilePath (Loc l) = l

-- | Encode to a string.
encodeString :: Loc a b -> String
encodeString = FP.encodeString . toFilePath

--------------------------------------------------------------------------------
-- Template Haskell helpers

-- | Make a 'Loc Absolute File'.
mkAbsoluteFile :: String -> Q Exp
mkAbsoluteFile s =
  case parseAbsoluteFileLoc (FP.decodeString s) of
    Left err -> error (show err)
    Right (Loc (FP.encodeString -> str)) ->
      [|Loc $(return (LitE (StringL str))) :: Loc Absolute File|]

-- | Make a 'Loc Absolute Dir'.
mkAbsoluteDir :: String -> Q Exp
mkAbsoluteDir s =
  case parseAbsoluteDirLoc (FP.decodeString s) of
    Left err -> error (show err)
    Right (Loc (FP.encodeString -> str)) ->
      [|Loc $(return (LitE (StringL str))) :: Loc Absolute Dir|]

-- | Make a 'Loc Relative Dir'.
mkRelativeDir :: String -> Q Exp
mkRelativeDir s =
  case parseRelativeDirLoc (FP.decodeString s) of
    Left err -> error (show err)
    Right (Loc (FP.encodeString -> str)) ->
      [|Loc $(return (LitE (StringL str))) :: Loc Relative Dir|]

-- | Make a 'Loc Relative File'.
mkRelativeFile :: String -> Q Exp
mkRelativeFile s =
  case parseRelativeFileLoc (FP.decodeString s) of
    Left err -> error (show err)
    Right (Loc (FP.encodeString -> str)) ->
      [|Loc $(return (LitE (StringL str))) :: Loc Relative File|]

--------------------------------------------------------------------------------
-- Operations

-- | Get the working directory.
getWorkingDir :: IO (Loc Absolute Dir)
getWorkingDir =
  do pwd <- getWorkingDirectory
     parseAbsoluteDirLoc pwd

-- | Get the working directory.
getHomeDir :: IO (Loc Absolute Dir)
getHomeDir =
  do pwd <- getHomeDirectory
     parseAbsoluteDirLoc pwd

-- | Get the parent of the given location.
parent :: Loc Absolute t -> Loc Absolute Dir
parent (Loc fp) = Loc (normalizeDir (FP.parent fp))

-- | Is p a parent of the given location?
isParentOf :: Loc Absolute Dir -> Loc Absolute t -> Bool
isParentOf p l =
  isJust (stripDir p l)

-- | Extract the relative filename from a given location.
filename :: Loc b File -> Loc Relative File
filename (Loc l) = Loc (FP.filename l)

-- | Extract the relative directory name from a given location.
dirname :: Loc b Dir -> Loc Relative Dir
dirname (Loc l) = Loc (normalizeDir (FP.dirname l))

-- | Append two locations.
appendLoc :: Loc b Dir -> Loc Relative t -> Loc b t
appendLoc (Loc a) (Loc b) = Loc (a <> b)

-- | Strip directory from path, making it relative to that directory.
-- Returns 'Nothing' if directory is not a parent of the path.
stripDir :: Loc Absolute Dir -> Loc Absolute t -> Maybe (Loc Relative t)
stripDir (Loc p) (Loc l) =
  fmap Loc (FP.stripPrefix p l)

-- | Returns true for paths whose last directory component begins with ".".
isHiddenDir :: Loc b Dir -> Bool
isHiddenDir = isPrefixOf "." . encodeString . dirname
