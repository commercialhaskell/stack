{-# LANGUAGE OverloadedStrings #-}
-- | Exposed for testing, do not use!
module Pantry.Internal
  ( parseTree
  , renderTree
  , Tree (..)
  , TreeEntry (..)
  , mkSafeFilePath
  , pcHpackExecutable
  , normalizeParents
  ) where

import Pantry.Types
import qualified Data.Text as T

-- | Like @System.FilePath.normalise@, however:
--
-- * Only works on relative paths, absolute paths fail
--
-- * May not point to directories
--
-- * Only works on forward slashes, even on Windows
--
-- * Normalizes parent dirs @foo/../@ get stripped
--
-- * Spelled like an American, sorry
normalizeParents
  :: FilePath
  -> Either String FilePath
normalizeParents "" = Left "empty file path"
normalizeParents ('/':_) = Left "absolute path"
normalizeParents fp = do
  let t = T.pack fp
  case T.unsnoc t of
    Just (_, '/') -> Left "trailing slash"
    _ -> Right ()

  let c1 = T.split (== '/') t

  case reverse c1 of
    ".":_ -> Left "last component is a single dot"
    _ -> Right ()

  let c2 = filter (\x -> not (T.null x || x == ".")) c1

  let loop [] = []
      loop (_:"..":rest) = loop rest
      loop (x:xs) = x : loop xs

  case loop c2 of
    [] -> Left "no non-empty components"
    c' -> Right $ T.unpack $ T.intercalate "/" c'
