module Network.HTTP.Download.VerifiedSpec where

import Crypto.Hash
import Control.Exception
import Data.Maybe
import Network.HTTP.Client.Conduit
import Network.HTTP.Download.Verified
import Path
import System.Directory
import System.IO.Temp
import Test.Hspec

-- | An example path to download the exampleReq.
getExamplePath :: IO (Path Abs File)
getExamplePath = do
    curDir <- getCurrentDirectory >>= parseAbsDir
    file <- parseRelFile "cabal-install-1.22.4.0.tar.gz"
    return (curDir </> file)

-- | An example VerifiedRequest that uses a SHA1
exampleReq :: VerifiedRequest SHA1
exampleReq = fromMaybe (error "exampleReq") $ do
    req <- parseUrl "http://download.fpcomplete.com/stackage-cli/linux64/cabal-install-1.22.4.0.tar.gz"
    return VerifiedRequest
        { vrRequest = req
        , vrDownloadBytes = 302513
        , vrExpectedHexDigest = "b98eea96d321cdeed83a201c192dac116e786ec2"
        , vrHashAlgorithm  = SHA1
        }

exampleWrongContentLength :: Int
exampleWrongContentLength = 302512

exampleWrongDigest :: String
exampleWrongDigest = "b98eea96d321cdeed83a201c192dac116e786ec3"

isWrongContentLength :: VerifiedDownloadException -> Bool
isWrongContentLength WrongContentLength{} = True
isWrongContentLength _ = False

isWrongDigest :: VerifiedDownloadException -> Bool
isWrongDigest WrongDigest{} = True
isWrongDigest _ = False

spec :: Spec
spec = do
  -- TODO: not copy/paste this
  let inTempDir action = do
        currentDirectory <- getCurrentDirectory
        withSystemTempDirectory "NHD_VerifiedSpec" $ \tempDir -> do
          let enterDir = setCurrentDirectory tempDir
          let exitDir = setCurrentDirectory currentDirectory
          bracket_ enterDir exitDir action
  -- TODO: share manager across tests

  describe "verifiedDownload" $ do
    it "downloads the file correctly" $ inTempDir $ do
      examplePath <- getExamplePath
      let exampleFilePath = toFilePath examplePath
      doesFileExist exampleFilePath `shouldReturn` False
      withManager $ verifiedDownload exampleReq examplePath
      doesFileExist exampleFilePath `shouldReturn` True

    it "rejects incorrect content length" $ inTempDir $ do
      examplePath <- getExamplePath
      let exampleFilePath = toFilePath examplePath
      let wrongContentLengthReq = exampleReq
            { vrDownloadBytes = exampleWrongContentLength
            }
      let go = withManager $ verifiedDownload wrongContentLengthReq examplePath
      go `shouldThrow` isWrongContentLength
      doesFileExist exampleFilePath `shouldReturn` False

    it "rejects incorrect digest" $ inTempDir $ do
      examplePath <- getExamplePath
      let exampleFilePath = toFilePath examplePath
      let wrongDigestReq = exampleReq
            { vrExpectedHexDigest = exampleWrongDigest }
      let go = withManager $ verifiedDownload wrongDigestReq examplePath
      go `shouldThrow` isWrongDigest
      doesFileExist exampleFilePath `shouldReturn` False
