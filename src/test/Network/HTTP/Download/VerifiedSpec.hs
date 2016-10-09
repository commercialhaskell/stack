{-# LANGUAGE RecordWildCards #-}
module Network.HTTP.Download.VerifiedSpec where

import           Control.Applicative
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Logger           (LoggingT, runStdoutLoggingT)
import           Control.Monad.Trans.Reader
import           Control.Retry                  (limitRetries)
import           Crypto.Hash
import           Data.Maybe
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Client.TLS        (getGlobalManager)
import           Network.HTTP.Download.Verified
import           Path
import           Path.IO
import           Prelude -- Fix redundant imports warnings
import           Test.Hspec

-- TODO: share across test files
withTempDir' :: (Path Abs Dir -> IO a) -> IO a
withTempDir' = withSystemTempDir "NHD_VerifiedSpec"

-- | An example path to download the exampleReq.
getExamplePath :: Path Abs Dir -> IO (Path Abs File)
getExamplePath dir = do
    file <- parseRelFile "cabal-install-1.22.4.0.tar.gz"
    return (dir </> file)

-- | An example DownloadRequest that uses a SHA1
exampleReq :: DownloadRequest
exampleReq = fromMaybe (error "exampleReq") $ do
    let req = parseRequest_ "http://download.fpcomplete.com/stackage-cli/linux64/cabal-install-1.22.4.0.tar.gz"
    return DownloadRequest
        { drRequest = req
        , drHashChecks = [exampleHashCheck]
        , drLengthCheck = Just exampleLengthCheck
        , drRetryPolicy = limitRetries 1
        }

exampleHashCheck :: HashCheck
exampleHashCheck = HashCheck
    { hashCheckAlgorithm = SHA1
    , hashCheckHexDigest = CheckHexDigestString "b98eea96d321cdeed83a201c192dac116e786ec2"
    }

exampleLengthCheck :: LengthCheck
exampleLengthCheck = 302513

-- | The wrong ContentLength for exampleReq
exampleWrongContentLength :: Int
exampleWrongContentLength = 302512

-- | The wrong SHA1 digest for exampleReq
exampleWrongDigest :: CheckHexDigest
exampleWrongDigest = CheckHexDigestString "b98eea96d321cdeed83a201c192dac116e786ec3"

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

runWith :: MonadIO m => Manager -> ReaderT Manager (LoggingT m) r -> m r
runWith manager = runStdoutLoggingT . flip runReaderT manager

setup :: IO T
setup = do
  manager <- getGlobalManager
  return T{..}

teardown :: T -> IO ()
teardown _ = return ()

spec :: Spec
spec = beforeAll setup $ afterAll teardown $ do
  let exampleProgressHook _ = return ()

  describe "verifiedDownload" $ do
    -- Preconditions:
    -- * the exampleReq server is running
    -- * the test runner has working internet access to it
    it "downloads the file correctly" $ \T{..} -> withTempDir' $ \dir -> do
      examplePath <- getExamplePath dir
      doesFileExist examplePath `shouldReturn` False
      let go = runWith manager $ verifiedDownload exampleReq examplePath exampleProgressHook
      go `shouldReturn` True
      doesFileExist examplePath `shouldReturn` True

    it "is idempotent, and doesn't redownload unnecessarily" $ \T{..} -> withTempDir' $ \dir -> do
      examplePath <- getExamplePath dir
      doesFileExist examplePath `shouldReturn` False
      let go = runWith manager $ verifiedDownload exampleReq examplePath exampleProgressHook
      go `shouldReturn` True
      doesFileExist examplePath `shouldReturn` True
      go `shouldReturn` False
      doesFileExist examplePath `shouldReturn` True

    -- https://github.com/commercialhaskell/stack/issues/372
    it "does redownload when the destination file is wrong" $ \T{..} -> withTempDir' $ \dir -> do
      examplePath <- getExamplePath dir
      let exampleFilePath = toFilePath examplePath
      writeFile exampleFilePath exampleWrongContent
      doesFileExist examplePath `shouldReturn` True
      readFile exampleFilePath `shouldReturn` exampleWrongContent
      let go = runWith manager $ verifiedDownload exampleReq examplePath exampleProgressHook
      go `shouldReturn` True
      doesFileExist examplePath `shouldReturn` True
      readFile exampleFilePath `shouldNotReturn` exampleWrongContent

    it "rejects incorrect content length" $ \T{..} -> withTempDir' $ \dir -> do
      examplePath <- getExamplePath dir
      let wrongContentLengthReq = exampleReq
            { drLengthCheck = Just exampleWrongContentLength
            }
      let go = runWith manager $ verifiedDownload wrongContentLengthReq examplePath exampleProgressHook
      go `shouldThrow` isWrongContentLength
      doesFileExist examplePath `shouldReturn` False

    it "rejects incorrect digest" $ \T{..} -> withTempDir' $ \dir -> do
      examplePath <- getExamplePath dir
      let wrongHashCheck = exampleHashCheck { hashCheckHexDigest = exampleWrongDigest }
      let wrongDigestReq = exampleReq { drHashChecks = [wrongHashCheck] }
      let go = runWith manager $ verifiedDownload wrongDigestReq examplePath exampleProgressHook
      go `shouldThrow` isWrongDigest
      doesFileExist examplePath `shouldReturn` False

    -- https://github.com/commercialhaskell/stack/issues/240
    it "can download hackage tarballs" $ \T{..} -> withTempDir' $ \dir -> do
      dest <- (dir </>) <$> parseRelFile "acme-missiles-0.3.tar.gz"
      let req = parseRequest_ "http://hackage.haskell.org/package/acme-missiles-0.3/acme-missiles-0.3.tar.gz"
      let dReq = DownloadRequest
            { drRequest = req
            , drHashChecks = []
            , drLengthCheck = Nothing
            , drRetryPolicy = limitRetries 1
            }
      let go = runWith manager $ verifiedDownload dReq dest exampleProgressHook
      doesFileExist dest `shouldReturn` False
      go `shouldReturn` True
      doesFileExist dest `shouldReturn` True
