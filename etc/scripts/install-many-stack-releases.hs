#!/usr/bin/env stack
{- stack script
    --resolver lts-11.2
    --package base
    --package directory
    --package filepath
    --package process
    --package safe
    --package temporary
-}

-- # Usage summary
--
-- This is a hacky script to install many stack releases to a target
-- directory. By default it installs all releases `>= 1.0` (this can be
-- changed by adjusting `minVersion` in the code). To use this on
-- standard 64 bit linux systems, do the following:
--
--     ./install-many-stack-releases.hs ~/.local/bin
--
-- It will then populate this folder with binaries like `stack-1.6.3`,
-- by downloading and unpacking stack releases to a temporary directory.
-- It will only download releases that do not already have binaries in
-- the target directory.
--
--
-- # 3rd party dependencies
--
-- * `stack >= 1.4`, in order to support the `script` command
--
-- * Fairly standard utilities `wget` / `tar`. `sudo apt install wget`
--
-- * [hub](https://github.com/github/hub) - install from github.
--
--
-- # Other platforms than 64 bit linux
--
-- To use this on other platforms, do something like:
--
--     ./install-many-stack-releases.hs ~/.local/bin linux-i386-gmp4
--
-- Note this has not been tested, but should work. The default platform
-- is `linux-x86_64`.

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Data.Version
import Safe
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process
import Text.ParserCombinators.ReadP (readP_to_S)

main :: IO ()
main = do
  args <- getArgs
  -- Parse directory from CLI args.
  let defaultPlatform = "linux-x86_64"
  targetDir <- case args of
    (dir:_) -> do
      exists <- doesDirectoryExist dir
      unless exists $ fail $ unwords [show dir, "is not a directory or does not exist."]
      return dir
    _ -> fail "Expected the first CLI argument to be the target directory to place stack binaries."
  -- Parse platform from CLI args, with default.
  platform <- case tail args of
    [] -> do
      putStrLn $ unwords
        [ "No argument specifying target platform provided, assuming"
        , show defaultPlatform
        , "\n"
        ]
      return defaultPlatform
    [x] -> return x
    _ -> fail "Expected at most two CLI argument, specifying target directory and target platform."
  -- Constants + common computation of urls / paths
  let minVersion = makeVersion [1, 0, 0]
      archiveExtension = "tar.gz"
      downloadUrl = "https://github.com/commercialhaskell/stack/releases/download"
      stackName = "stack"
      versionBinaryName version = stackName ++ "-" ++ version
      versionArchiveName version = stackName ++ "-" ++ version ++ "-" ++ platform
      targetFile version = targetDir </> versionBinaryName version
  -- Get releases from hub tool.
  releases <- lines <$> readProcess "hub" ["release"] ""
  putStrLn "Found the following releases from github:"
  print releases
  putStrLn ""
  -- Strip 'v' prefix.
  let releasesWithoutPrefix = mapMaybe (stripPrefix "v") releases
  -- Don't download super old versions
  let (newerVersions, olderVersions) =
        partition (>= minVersion) (mapMaybe readVersion releasesWithoutPrefix)
  putStrLn "The following releases look like stack releases that are older than minVersion:"
  print (map showVersion olderVersions)
  putStrLn ""
  putStrLn "The following releases look like recent enough stack releases:"
  print (map showVersion newerVersions)
  putStrLn ""
  -- Check which releases already exist.
  let releasesToCheck = map showVersion newerVersions
  releasesToDownload <- flip filterM releasesToCheck $ \version ->
    not <$> doesFileExist (targetFile version)
  putStrLn $ concat ["The following releases have not yet been placed in ", show targetDir, ":"]
  print releasesToDownload
  putStrLn ""
  -- Download / unpack / copy releases.
  putStrLn "So, attempting to download + unpack + copy these releases:"
  putStrLn ""
  forM_ releasesToDownload $ \version -> do
    putStrLn (replicate 80 '=')
    putStrLn $ unwords ["Attempting to download", stackName, "version", version]
    withSystemTempDirectory ("stack-" ++ version ++ "-download") $ \tmpDir ->
      withCurrentDirectory tmpDir $ do
        let archiveName = versionArchiveName version
            archiveFile = archiveName ++ "." ++ archiveExtension
            url = downloadUrl ++ "/v" ++ version ++ "/" ++ archiveFile
            unpackedBinPath = tmpDir </> archiveName </> "stack"
            targetBinPath = targetFile version
        wgetSuccess <- readProcessIsSuccess "wget" [url]
        case wgetSuccess of
          False ->
            putStrLn $ unwords ["Failed to download", show url, "so skipping it."]
          True -> do
            putStrLn $ unwords ["Success downloading", show url]
            unpackSuccess <- readProcessIsSuccess "tar" ["-xvf", archiveFile]
            case unpackSuccess of
              False ->
                putStrLn $ unwords ["Failed to unpack archive downloaded from", show url, "so skipping it."]
              True -> do
                putStrLn $ unwords ["Success unpacking", show archiveFile]
                eres <- try $ copyFile unpackedBinPath targetBinPath
                case eres of
                  Left exc -> do
                    putStrLn $ unwords ["Failed to copy", show unpackedBinPath, "exception was:"]
                    print (exc :: SomeException)
                  Right () ->
                    putStrLn $ unwords ["Success copying", show unpackedBinPath, "to", show targetBinPath]
    putStrLn ""

readProcessIsSuccess :: FilePath -> [String] -> IO Bool
readProcessIsSuccess name args = do
  (ec, out, err) <- readProcessWithExitCode name args ""
  case ec of
    ExitSuccess -> return True
    ExitFailure code -> do
      putStrLn $ unwords $ ["Running", name, "with args", show args, "failed with code", show code]
      putStrLn "stdout:"
      putStrLn out
      putStrLn "stderr:"
      putStrLn err
      return False

-- Damn, base has some ugly stuff... 'parseVersion' yields multiple
-- parses treating numeric portions as version tags.. WTF. Seems like
-- the last parse is always the correct one.
readVersion :: String -> Maybe Version
readVersion = fmap fst . lastMay . readP_to_S parseVersion
