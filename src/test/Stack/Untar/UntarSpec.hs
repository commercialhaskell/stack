{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stack.Untar.UntarSpec where

import Data.List (sort)
import Path
import Path.IO (removeDirRecur)
import qualified System.FilePath as FP
import Stack.Fetch (untar)
import Test.Hspec

spec :: Spec
spec = do
    describe "Untarring ignores strange entries" $
      mapM_ testTarFile tarFiles
  where
    -- XXX tests are run in the project root folder, but data files are next to
    -- this source data.
    currentFolder = $(mkRelDir $ "src" FP.</> "test" FP.</> "Stack" FP.</> "Untar")

    -- Pairs test tarball names + list of unexpected entries contained: for each
    -- entry, a tar pathname + description.
    tarFilesBase = [ ("test1", [])
                   , ("test2", [ ("bar", "named pipe")
                               , ("devB", "block device")
                               , ("devC", "character device")])]
    -- Prepend tarball name to tar pathnames:
    tarFiles =
      [ (name,
         [ (name FP.</> entryName, d)
         | (entryName, d) <- entries])
      | (name, entries) <- tarFilesBase ]

    testTarFile (name, expected) =
      it ("works on test " ++ name) $
        getEntries name `shouldReturn` sort expected

    getEntries name = do
      tarballName <- parseRelFile $ name ++ ".tar.gz"
      expectedTarFolder <- parseRelDir name

      entries <- untar (currentFolder </> tarballName) expectedTarFolder currentFolder
      removeDirRecur $ currentFolder </> expectedTarFolder
      return $ sort entries
