{-# LANGUAGE OverloadedStrings #-}

import StackTest
import System.Environment (getEnv)
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import Control.Exception.Base (bracket, bracket_)
import Control.Monad (guard, when, unless, msum)
import Control.Concurrent (threadDelay)
import Data.List (isInfixOf, delete, repeat)

wrun :: FilePath -> String -> IO ()
wrun cmd args = run cmd $ words args

createDockerVolume :: Int -> IO String
createDockerVolume sizeInMB = do
  (ec, stdout, stderr) <- runEx "docker" $ "volume create"
    ++ " --driver local"
    ++ " --opt type=tmpfs"
    ++ " --opt device=tmpfs"
    ++ " --opt o=size=" ++ show sizeInMB ++ "m"
  unless (ec == ExitSuccess) $ error $ "Exited with exit code: " ++ show ec
  return $ delete '\n' stdout

removeDockerVolume :: Int -> String -> IO ()
removeDockerVolume attempts name | attempts <= 0 =
  error $ "Can't remove docker volume " ++ name
removeDockerVolume attempts name = do
  (ec, _, stderr) <- runEx "docker" $ "volume rm --force " ++ name
  let wasRemoved = (ec == ExitSuccess) || isInfixOf "No such volume" stderr
  unless wasRemoved $
    threadDelay 3000000 >> -- sometimes docker releases a volume slowly
    removeDockerVolume (attempts - 1) name

withDockerVolume :: Int -> (String -> IO a) -> IO a
withDockerVolume sizeInMB =
  bracket (createDockerVolume sizeInMB) (removeDockerVolume 5)

withSourceDirectory :: IO () -> IO ()
withSourceDirectory action = do
  stackSrc <- stackSrc
  currentDirectory <- getCurrentDirectory
  let enterDir = setCurrentDirectory stackSrc
      exitDir = setCurrentDirectory currentDirectory
  bracket_ enterDir exitDir action

buildDockerImageWithStackSourceInside :: String -> IO ()
buildDockerImageWithStackSourceInside tag = withSourceDirectory $
  testDir >>= (
    \testDir -> wrun
      "docker" $ "build"
      ++ " --file " ++ (testDir </> "Dockerfile")
      ++ " --tag " ++ tag
      ++ " --memory-swap -1"
      ++ " ."
      )

runDockerContainerWithVolume
  :: String
  -> String
  -> String
  -> String
  -> IO (ExitCode, String, String)
runDockerContainerWithVolume imageTag volumeName volumeLocation cmd =
  runEx "docker" $ "run"
    ++ " --rm"
    ++ " --workdir " ++ volumeLocation
    ++ " --mount type=volume,dst=" ++ volumeLocation ++ ",src=" ++ volumeName
    ++ " " ++ imageTag
    ++ " " ++ cmd

validateSrderr :: String -> Bool
validateSrderr = isInfixOf "No space left on device"

imageTag :: String
imageTag = "4085-fix"

spaceInMBJustEnoughToFailInTheExactMoment :: Int
spaceInMBJustEnoughToFailInTheExactMoment = 2000

main :: IO ()
main = do
  buildDockerImageWithStackSourceInside imageTag
  (ec, _, stderr) <- withDockerVolume
    spaceInMBJustEnoughToFailInTheExactMoment
    (\volumeName ->
        runDockerContainerWithVolume imageTag volumeName "/app" $
          "stack"
          ++ " --stack-root " ++ "/app"
          ++ " --resolver nightly-2018-06-05"
          ++ " --no-terminal"
          ++ " --install-ghc"
          ++ " test")
  unless (ec /= ExitSuccess) $
    error "stack process succeeded, but it shouldn't"
  unless (validateSrderr stderr) $
    error "stderr validation failed"
