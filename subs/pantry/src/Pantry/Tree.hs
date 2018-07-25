{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.Tree
  ( unpackTree
  , findCabalFile
  ) where

import RIO
import qualified RIO.Map as Map
import qualified RIO.Text as T
import qualified RIO.ByteString as B
import Pantry.Storage
import Pantry.Types
import RIO.FilePath ((</>), takeDirectory)
import RIO.Directory (createDirectoryIfMissing)

#if !WINDOWS
import System.Posix.Files (setFileMode)
#endif

unpackTree
  :: (HasPantryConfig env, HasLogFunc env)
  => FilePath -- ^ dest dir, will be created if necessary
  -> Tree
  -> RIO env ()
unpackTree dir (TreeMap m) = do
  withStorage $ for_ (Map.toList m) $ \(sfp, TreeEntry blobKey ft) -> do
    let dest = dir </> T.unpack (unSafeFilePath sfp)
    createDirectoryIfMissing True $ takeDirectory dest
    mbs <- loadBlob blobKey
    case mbs of
      Nothing -> error $ "Missing blob: " ++ show blobKey
      Just bs -> do
        B.writeFile dest bs
#if !WINDOWS
        case ft of
          FTNormal -> pure ()
          FTExecutable -> liftIO $ setFileMode dest 0o755
#endif

findCabalFile
  :: MonadThrow m
  => PackageLocation -- ^ for exceptions
  -> Tree
  -> m (SafeFilePath, TreeEntry)
findCabalFile loc (TreeMap m) = do
  let isCabalFile (sfp, _) =
        let txt = unSafeFilePath sfp
         in not ("/" `T.isInfixOf` txt) && ".cabal" `T.isSuffixOf` txt
  case filter isCabalFile $ Map.toList m of
    [] -> throwM $ TreeWithoutCabalFile loc
    [(key, te)] -> pure (key, te)
    xs -> throwM $ TreeWithMultipleCabalFiles loc $ map fst xs
