{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stack.Untar.UntarSpec where

import Data.List (sort)
import System.FilePath ((</>))
import System.Directory (removeDirectoryRecursive)
import Stack.Fetch (untar)
import Test.Hspec

spec :: Spec
spec = do
    describe "Untarring ignores strange entries" $
      mapM_ testTarFile tarFiles
  where
    -- XXX tests are run in the project root folder, but data files are next to
    -- this source data.
    currentFolder = "src" </> "test" </> "Stack" </> "Untar"

    -- Pairs test tarball names + list of unexpected entries contained: for each
    -- entry, a tar pathname + description.
    tarFiles = [ ("test1", [])
               , ("test2", [ ("test2" </> "bar", "named pipe")
                           , ("test2" </> "devB", "block device")
                           , ("test2" </> "devC", "character device")])]

    testTarFile (name, expected) =
      it ("works on test " ++ name) $
        getEntries name `shouldReturn` sort expected

    getEntries name = do
      let tarFP  = currentFolder </> name ++ ".tar.gz"
          expectedTarFolder = name
          dest = currentFolder

      entries <- untar tarFP expectedTarFolder dest
      removeDirectoryRecursive $ currentFolder </> expectedTarFolder
      return $ sort entries
