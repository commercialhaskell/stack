{-# LANGUAGE RecordWildCards #-}
module Network.HTTP.Download.VerifiedSpec where

import Crypto.Hash
import Control.Exception
import Control.Monad.Trans.Reader
import Data.Maybe
import Network.HTTP.Client.Conduit
import Network.HTTP.Download.Verified
import Path
import System.Directory
import System.IO (writeFile)
import System.IO.Temp
import Test.Hspec


-- TODO: share across test files
withTempDir :: (Path Abs Dir -> IO a) -> IO a
withTempDir f = withSystemTempDirectory "NHD_VerifiedSpec" $ \dirFp -> do
  dir <- parseAbsDir dirFp
  f dir


-- | An example path to download the exampleReq.
getExamplePath :: Path Abs Dir -> IO (Path Abs File)
getExamplePath dir = do
    file <- parseRelFile "cabal-install-1.22.4.0.tar.gz"
    return (dir </> file)

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

-- | The wrong ContentLength for exampleReq
exampleWrongContentLength :: Int
exampleWrongContentLength = 302512

-- | The wrong SHA1 digest for exampleReq
exampleWrongDigest :: String
exampleWrongDigest = "b98eea96d321cdeed83a201c192dac116e786ec3"

exampleWrongContent :: String
exampleWrongContent = "example wrong content"

isWrongContentLength :: VerifiedDownloadException -> Bool
isWrongContentLength WrongContentLength{} = True
isWrongContentLength _ = False

isWrongDigest :: VerifiedDownloadException -> Bool
isWrongDigest WrongDigest{} = True
isWrongDigest _ = False

data T = T
  { manager :: Manager
  }

runWith :: Manager -> ReaderT Manager m r -> m r
runWith = flip runReaderT

setup :: IO T
setup = do
  manager <- newManager
  return T{..}

teardown :: T -> IO ()
teardown _ = return ()

spec :: Spec
spec = beforeAll setup $ afterAll teardown $ do
  describe "verifiedDownload" $ do
    it "downloads the file correctly" $ \T{..} -> withTempDir $ \dir -> do
      examplePath <- getExamplePath dir
      let exampleFilePath = toFilePath examplePath
      doesFileExist exampleFilePath `shouldReturn` False
      let go = runWith manager $ verifiedDownload exampleReq examplePath
      go `shouldReturn` True
      doesFileExist exampleFilePath `shouldReturn` True

    it "is idempotent, and doesn't redownload unnecessarily" $ \T{..} -> withTempDir $ \dir -> do
      examplePath <- getExamplePath dir
      let exampleFilePath = toFilePath examplePath
      doesFileExist exampleFilePath `shouldReturn` False
      let go = runWith manager $ verifiedDownload exampleReq examplePath
      go `shouldReturn` True
      doesFileExist exampleFilePath `shouldReturn` True
      go `shouldReturn` False
      doesFileExist exampleFilePath `shouldReturn` True

    it "does redownload when the destination file is wrong" $ \T{..} -> withTempDir $ \dir -> do
      examplePath <- getExamplePath dir
      let exampleFilePath = toFilePath examplePath
      writeFile exampleFilePath exampleWrongContent
      doesFileExist exampleFilePath `shouldReturn` True
      let go = runWith manager $ verifiedDownload exampleReq examplePath
      go `shouldReturn` True
      doesFileExist exampleFilePath `shouldReturn` True

    it "rejects incorrect content length" $ \T{..} -> withTempDir $ \dir -> do
      examplePath <- getExamplePath dir
      let exampleFilePath = toFilePath examplePath
      let wrongContentLengthReq = exampleReq
            { vrDownloadBytes = exampleWrongContentLength
            }
      let go = runWith manager $ verifiedDownload wrongContentLengthReq examplePath
      go `shouldThrow` isWrongContentLength
      doesFileExist exampleFilePath `shouldReturn` False

    it "rejects incorrect digest" $ \T{..} -> withTempDir $ \dir -> do
      examplePath <- getExamplePath dir
      let exampleFilePath = toFilePath examplePath
      let wrongDigestReq = exampleReq
            { vrExpectedHexDigest = exampleWrongDigest }
      let go = runWith manager $ verifiedDownload wrongDigestReq examplePath
      go `shouldThrow` isWrongDigest
      doesFileExist exampleFilePath `shouldReturn` False
