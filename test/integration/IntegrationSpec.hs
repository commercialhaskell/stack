{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Conduit
import           Data.List                (stripPrefix)
import           Options.Generic
import           RIO
import           RIO.Char                 (toLower)
import           RIO.Directory            hiding (findExecutable)
import           RIO.FilePath
import           RIO.List                 (isInfixOf, partition)
import qualified RIO.Map                  as Map
import           RIO.Process
import qualified RIO.Set                  as Set
import qualified RIO.Text                 as T
import           System.Environment       (lookupEnv, getExecutablePath)
import           System.Info (os)
import           System.PosixCompat.Files

-- This code does not use a test framework so that we get direct
-- control of how the output is displayed.

main :: IO ()
main = runSimpleApp $ do
  logInfo "Initiating Stack integration test running"

  options <- getRecord "Stack integration tests"
  results <- runApp options $ do
    logInfo "Running with the following environment"
    proc "env" [] runProcess_
    tests <- asks appTestDirs
    let count = Set.size tests
        loop !idx rest !accum =
          case rest of
            [] -> pure accum
            next:rest' -> do
              logInfo $ "Running integration test "
                     <> display idx
                     <> "/"
                     <> display count
                     <> ": "
                     <> fromString (takeFileName next)
              res <- test next
              loop (idx + 1) rest' (res <> accum)

    loop (1 :: Int) (Set.toList tests) mempty

  let (successes, failures) = partition ((== ExitSuccess) . snd)
                            $ Map.toList results

  unless (null successes) $ do
    logInfo "Successful tests:"
    for_ successes $ \(x, _) -> logInfo $ "- " <> display x
    logInfo ""

  if null failures
    then logInfo "No failures!"
    else do
      logInfo "Failed tests:"
      for_ failures $ \(x, ec) -> logInfo $ "- " <> display x <> " - " <> displayShow ec
      exitFailure

data Options = Options
  { optSpeed :: Maybe Speed
  , optMatch :: Maybe String
  } deriving Generic

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers modifiers
    where
      optName = map toLower . drop 3
      modifiers = defaultModifiers { fieldNameModifier = optName
                                   , shortNameModifier = firstLetter . optName
                                   }

data Speed = Fast | Normal | Superslow
  deriving (Read, Generic)

instance ParseField Speed

exeExt :: String
exeExt = if isWindows then ".exe" else ""

isWindows :: Bool
isWindows = os == "mingw32"

runApp :: Options -> RIO App a -> RIO SimpleApp a
runApp options inner = do
  let speed = fromMaybe Normal $ optSpeed options
  simpleApp <- ask
  runghc <- findExecutable "runghc" >>= either throwIO pure
  srcDir <- canonicalizePath ""
  testsRoot <- canonicalizePath $ srcDir </> "test/integration"
  libdir <- canonicalizePath $ testsRoot </> "lib"
  myPath <- liftIO getExecutablePath

  stack <- canonicalizePath $ takeDirectory myPath </> "stack" ++ exeExt
  logInfo $ "Using stack located at " <> fromString stack
  proc stack ["--version"] runProcess_
  logInfo $ "Using runghc located at " <> fromString runghc
  proc runghc ["--version"] runProcess_

  let matchTest = case optMatch options of
        Nothing -> const True
        Just str -> (str `isInfixOf`)
  testDirs
    <- runConduitRes
     $ sourceDirectory (testsRoot </> "tests")
    .| filterMC (liftIO . hasTest)
    .| filterC matchTest
    .| foldMapC Set.singleton

  let modifyEnvCommon
        = Map.insert "SRC_DIR" (fromString srcDir)
        . Map.insert "STACK_EXE" (fromString stack)
        . Map.delete "GHC_PACKAGE_PATH"
        . Map.insert "STACK_TEST_SPEED"
            (case speed of
              Superslow -> "SUPERSLOW"
              _ -> "NORMAL")
        . Map.fromList
        . map (first T.toUpper)
        . Map.toList

  case speed of
    Fast -> do
      let app = App
            { appSimpleApp = simpleApp
            , appRunghc = runghc
            , appLibDir = libdir
            , appSetupHome = id
            , appTestDirs = testDirs
            }
      runRIO app $ withModifyEnvVars modifyEnvCommon inner
    _ -> do
      morigStackRoot <- liftIO $ lookupEnv "STACK_ROOT"
      origStackRoot <-
        case morigStackRoot of
          Nothing -> getAppUserDataDirectory "stack"
          Just x -> pure x

      logInfo "Initializing/updating the original Pantry store"
      proc stack ["update"] runProcess_

      pantryRoot <- canonicalizePath $ origStackRoot </> "pantry"
      let modifyEnv
               = Map.insert "PANTRY_ROOT" (fromString pantryRoot)
               . modifyEnvCommon

          app = App
            { appSimpleApp = simpleApp
            , appRunghc = runghc
            , appLibDir = libdir
            , appSetupHome = \inner' -> withSystemTempDirectory "home" $ \newHome -> do
                let newStackRoot = newHome </> ".stack"
                createDirectoryIfMissing True newStackRoot
                let modifyEnv'
                      = Map.insert "HOME" (fromString newHome)
                      . Map.insert "APPDATA" (fromString newHome)
                      . Map.insert "STACK_ROOT" (fromString newStackRoot)
                writeFileBinary (newStackRoot </> "config.yaml") "system-ghc: true\ninstall-ghc: false\n"
                withModifyEnvVars modifyEnv' inner'
            , appTestDirs = testDirs
            }

      runRIO app $ withModifyEnvVars modifyEnv inner


hasTest :: FilePath -> IO Bool
hasTest dir = doesFileExist $ dir </> "Main.hs"

data App = App
  { appRunghc :: !FilePath
  , appLibDir :: !FilePath
  , appSetupHome :: !(forall a. RIO App a -> RIO App a)
  , appSimpleApp :: !SimpleApp
  , appTestDirs :: !(Set FilePath)
  }
simpleAppL :: Lens' App SimpleApp
simpleAppL = lens appSimpleApp (\x y -> x { appSimpleApp = y })
instance HasLogFunc App where
  logFuncL = simpleAppL.logFuncL
instance HasProcessContext App where
  processContextL = simpleAppL.processContextL

-- | Call 'appSetupHome' on the inner action
withHome :: RIO App a -> RIO App a
withHome inner = do
  app <- ask
  appSetupHome app inner

test :: FilePath -- ^ test dir
     -> RIO App (Map Text ExitCode)
test testDir = withDir $ \dir -> withHome $ do
    runghc <- asks appRunghc
    libDir <- asks appLibDir
    let mainFile = testDir </> "Main.hs"

    copyTree (testDir </> "files") dir

    withSystemTempFile (name <.> "log") $ \logfp logh -> do
      ec <- withWorkingDir dir
          $ withModifyEnvVars (Map.insert "TEST_DIR" $ fromString testDir)
          $ proc runghc
              [ "-clear-package-db"
              , "-global-package-db"
              , "-i" ++ libDir
              , mainFile
              ]
           $ runProcess
           . setStdin closed
           . setStdout (useHandleOpen logh)
           . setStderr (useHandleOpen logh)
      hClose logh

      case ec of
        ExitSuccess -> logInfo "Success!"
        _ -> do
          logError "Failure, dumping log\n\n"
          withSourceFile logfp $ \src ->
            runConduit $ src .| stderrC
          logError $ "\n\nEnd of log for " <> fromString name
      pure $ Map.singleton (fromString name) ec
  where
    name = takeFileName testDir
    withDir = withSystemTempDirectory ("stack-integration-" ++ name)

copyTree :: MonadIO m => FilePath -> FilePath -> m ()
copyTree src dst =
    liftIO $
    runResourceT (sourceDirectoryDeep False src `connect` mapM_C go)
        `catch` \(_ :: IOException) -> return ()
  where
    go srcfp = liftIO $ do
        Just suffix <- return $ stripPrefix src srcfp
        let dstfp = dst </> stripHeadSeparator suffix
        createDirectoryIfMissing True $ takeDirectory dstfp
        -- copying yaml files so lock files won't get created in
        -- the source directory
        if takeFileName srcfp /= "package.yaml" &&
           (takeExtensions srcfp == ".yaml" || takeExtensions srcfp == ".yml")
          then
            copyFile srcfp dstfp
          else
            createSymbolicLink srcfp dstfp `catch` \(_ :: IOException) ->
                copyFile srcfp dstfp -- for Windows

    stripHeadSeparator :: FilePath -> FilePath
    stripHeadSeparator [] = []
    stripHeadSeparator fp@(x:xs) = if isPathSeparator x
                                   then xs
                                   else fp
