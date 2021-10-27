{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Stack.UploadSpec (spec) where

import RIO
import RIO.Directory
import RIO.FilePath ((</>))
import Stack.Upload
import Test.Hspec
import System.Permissions (osIsWindows)
import System.PosixCompat.Files (getFileStatus, fileMode)
import System.Environment (setEnv)
import Data.Bits ((.&.))

spec :: Spec
spec = do
  it "writeFilePrivate" $ example $ withSystemTempDirectory "writeFilePrivate" $ \dir -> replicateM_ 2 $ do
    let fp = dir </> "filename"
        contents :: IsString s => s
        contents = "These are the contents"
    writeFilePrivate fp contents
    actual <- readFileBinary fp
    actual `shouldBe` contents
    perms <- getPermissions fp
    perms `shouldBe` setOwnerWritable True (setOwnerReadable True emptyPermissions)

    unless osIsWindows $ do
      status <- getFileStatus fp
      (fileMode status .&. 0o777) `shouldBe` 0o600

  it "finds a HACKAGE_KEY env variable" $ do
    maybeGetHackageKey `shouldReturn` Nothing
    setEnv "HACKAGE_KEY" "api_key"
    maybeGetHackageKey `shouldReturn` Just "api_key"
