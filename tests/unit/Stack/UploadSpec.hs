{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.UploadSpec
  ( spec
  ) where

import           Data.Bits ((.&.))
import           RIO
                   ( Bool (..), IO, IsString, Maybe (..), String, ($), finally
                   , readFileBinary, replicateM_, runRIO, unless
                   , withSystemTempDirectory
                   )
import           RIO.Directory
                   ( emptyPermissions, getPermissions, setOwnerReadable
                   , setOwnerWritable
                   )
import           RIO.FilePath ( (</>) )
import           Stack.Upload
                   ( HackageKey (..), maybeGetHackageKey, writeFilePrivate )
import           System.Environment ( setEnv, unsetEnv )
import           System.Permissions ( osIsWindows )
import           System.PosixCompat.Files ( getFileStatus, fileMode )
import           Test.Hspec ( Spec, example, it, shouldBe, shouldReturn )

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
    runRIO () maybeGetHackageKey `shouldReturn` Nothing

    withEnv "HACKAGE_KEY" "api_key"
      $ runRIO () maybeGetHackageKey `shouldReturn` Just (HackageKey "api_key")

withEnv :: String -> String -> IO a -> IO a
withEnv k v f = do
  setEnv k v
  f `finally` unsetEnv k
