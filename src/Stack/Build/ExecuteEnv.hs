{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Provides all the necessary types and functions for running cabal Setup.hs
-- commands. Only used in the "Execute" and "ExecutePackage" modules
module Stack.Build.ExecuteEnv
  ( ExecuteEnv (..)
  , withExecuteEnv
  , withSingleContext
  , ExcludeTHLoading (..)
  , KeepOutputOpen (..)
  , OutputType (..)
  ) where

import           Control.Concurrent.Companion ( Companion, withCompanion )
import           Control.Concurrent.Execute
                   ( ActionContext (..), ActionId (..), Concurrency (..) )
import           Control.Monad.Extra ( whenJust )
import           Crypto.Hash ( SHA256 (..), hashWith )
import           Data.Attoparsec.Text ( char, choice, digit, parseOnly )
import qualified Data.Attoparsec.Text as P ( string )
import qualified Data.ByteArray as Mem ( convert )
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Builder ( toLazyByteString )
import qualified Data.ByteString.Char8 as S8
import           Data.Char ( isSpace )
import           Conduit
                   ( ConduitT, awaitForever, sinkHandle, withSinkFile
                   , withSourceFile, yield
                   )
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Encoding ( decodeUtf8 )
import           Data.Time
                   ( ZonedTime, defaultTimeLocale, formatTime, getZonedTime )
import qualified Distribution.PackageDescription as C
import qualified Distribution.Simple.Build.Macros as C
import           Distribution.System ( OS (..), Platform (..) )
import           Distribution.Types.PackageName ( mkPackageName )
import           Distribution.Verbosity ( showForCabal )
import           Distribution.Version ( mkVersion )
import           Path
                   ( PathException, (</>), parent, parseRelDir, parseRelFile )
import           Path.Extra ( forgivingResolveFile, toFilePathNoTrailingSep )
import           Path.IO
                   ( doesDirExist, doesFileExist, ensureDir, ignoringAbsence
                   , removeFile, renameDir, renameFile
                   )
import           RIO.Process
                   ( eceExitCode, proc, runProcess_, setStdout, useHandleOpen
                   , withWorkingDir
                   )
import           Stack.Config ( checkOwnership )
import           Stack.Constants
                   ( cabalPackageName, relDirDist, relDirSetup
                   , relDirSetupExeCache, relDirSetupExeSrc, relFileBuildLock
                   , relFileSetupHs, relFileSetupLhs, relFileSetupLower
                   , relFileSetupMacrosH, setupGhciShimCode, stackProgName
                   )
import           Stack.Constants.Config ( distDirFromDir, distRelativeDir )
import           Stack.Package ( buildLogPath )
import           Stack.Prelude
import           Stack.Types.ApplyGhcOptions ( ApplyGhcOptions (..) )
import           Stack.Types.Build
                   ( ConvertPathsToAbsolute (..), ExcludeTHLoading (..)
                   , KeepOutputOpen (..), TaskType (..), taskTypeLocation
                   , taskTypePackageIdentifier
                   )
import           Stack.Types.Build.Exception
                   ( BuildException (..), BuildPrettyException (..) )
import           Stack.Types.BuildOpts ( BuildOpts (..) )
import           Stack.Types.BuildOptsCLI ( BuildOptsCLI (..) )
import           Stack.Types.BuildOptsMonoid ( CabalVerbosity (..) )
import           Stack.Types.Compiler
                   ( ActualCompiler (..), WhichCompiler (..)
                   , compilerVersionString, getGhcVersion, whichCompilerL
                   )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), HasCompiler (..), cabalVersionL
                   , getCompilerPath
                   )
import           Stack.Types.Config
                   ( Config (..), HasConfig (..), stackRootL )
import           Stack.Types.ConfigureOpts ( BaseConfigOpts (..) )
import           Stack.Types.Dependency ( DepValue(..) )
import           Stack.Types.DumpLogs ( DumpLogs (..) )
import           Stack.Types.DumpPackage ( DumpPackage (..) )
import           Stack.Types.EnvConfig
                   ( HasEnvConfig (..), actualCompilerVersionL
                   , platformGhcRelDir, shouldForceGhcColorFlag
                   )
import           Stack.Types.EnvSettings ( EnvSettings (..) )
import           Stack.Types.GhcPkgId ( GhcPkgId, ghcPkgIdString )
import           Stack.Types.Installed ( InstallLocation (..), Installed (..) )
import           Stack.Types.Package
                   ( LocalPackage (..), Package (..), packageIdentifier )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Version ( withinRange )
import qualified System.Directory as D
import           System.Environment ( lookupEnv )
import           System.FileLock
                   ( SharedExclusive (..), withFileLock, withTryFileLock )

data ExecuteEnv = ExecuteEnv
  { installLock    :: !(MVar ())
  , buildOpts      :: !BuildOpts
  , buildOptsCLI   :: !BuildOptsCLI
  , baseConfigOpts :: !BaseConfigOpts
  , ghcPkgIds      :: !(TVar (Map PackageIdentifier Installed))
  , tempDir        :: !(Path Abs Dir)
  , setupHs        :: !(Path Abs File)
    -- ^ Temporary Setup.hs for simple builds
  , setupShimHs    :: !(Path Abs File)
    -- ^ Temporary SetupShim.hs, to provide access to initial-build-steps
  , setupExe       :: !(Maybe (Path Abs File))
    -- ^ Compiled version of eeSetupHs
  , cabalPkgVer    :: !Version
    -- ^ The version of the compiler's Cabal boot package.
  , totalWanted    :: !Int
  , locals         :: ![LocalPackage]
  , globalDB       :: !(Path Abs Dir)
  , globalDumpPkgs :: !(Map GhcPkgId DumpPackage)
  , snapshotDumpPkgs :: !(TVar (Map GhcPkgId DumpPackage))
  , localDumpPkgs  :: !(TVar (Map GhcPkgId DumpPackage))
  , logFiles       :: !(TChan (Path Abs Dir, Path Abs File))
  , customBuilt    :: !(IORef (Set PackageName))
    -- ^ Stores which packages with custom-setup have already had their
    -- Setup.hs built.
  , largestPackageName :: !(Maybe Int)
    -- ^ For nicer interleaved output: track the largest package name size
  , pathEnvVar :: !Text
    -- ^ Value of the PATH environment variable
  }

-- | Type representing setup executable circumstances.
data SetupExe
  = SimpleSetupExe !(Path Abs File)
    -- ^ The build type is Simple and there is a path to an existing setup
    -- executable.
  | OtherSetupHs !(Path Abs File)
    -- ^ Other circumstances with a path to the source code for the setup
    -- executable.

buildSetupArgs :: [String]
buildSetupArgs =
  [ "-rtsopts"
  , "-threaded"
  , "-clear-package-db"
  , "-global-package-db"
  , "-hide-all-packages"
  , "-package"
  , "base"
  , "-main-is"
  , "StackSetupShim.mainOverride"
  ]

simpleSetupCode :: Builder
simpleSetupCode = "import Distribution.Simple\nmain = defaultMain"

simpleSetupHash :: String
simpleSetupHash =
    T.unpack
  $ decodeUtf8
  $ S.take 8
  $ B64URL.encode
  $ Mem.convert
  $ hashWith SHA256
  $ toStrictBytes
  $ Data.ByteString.Builder.toLazyByteString
  $  encodeUtf8Builder (T.pack (unwords buildSetupArgs))
  <> setupGhciShimCode
  <> simpleSetupCode

-- | Get a compiled Setup exe
getSetupExe :: HasEnvConfig env
            => Path Abs File -- ^ Setup.hs input file
            -> Path Abs File -- ^ SetupShim.hs input file
            -> Path Abs Dir -- ^ temporary directory
            -> RIO env (Maybe (Path Abs File))
getSetupExe setupHs setupShimHs tmpdir = do
  wc <- view $ actualCompilerVersionL . whichCompilerL
  platformDir <- platformGhcRelDir
  config <- view configL
  cabalVersionString <- view $ cabalVersionL . to versionString
  actualCompilerVersionString <-
    view $ actualCompilerVersionL . to compilerVersionString
  platform <- view platformL
  let baseNameS = concat
        [ "Cabal-simple_"
        , simpleSetupHash
        , "_"
        , cabalVersionString
        , "_"
        , actualCompilerVersionString
        ]
      exeNameS = baseNameS ++
        case platform of
          Platform _ Windows -> ".exe"
          _ -> ""
      outputNameS =
        case wc of
            Ghc -> exeNameS
      setupDir =
        view stackRootL config </>
        relDirSetupExeCache </>
        platformDir

  exePath <- (setupDir </>) <$> parseRelFile exeNameS

  exists <- liftIO $ D.doesFileExist $ toFilePath exePath

  if exists
    then pure $ Just exePath
    else do
      tmpExePath <- fmap (setupDir </>) $ parseRelFile $ "tmp-" ++ exeNameS
      tmpOutputPath <-
        fmap (setupDir </>) $ parseRelFile $ "tmp-" ++ outputNameS
      ensureDir setupDir
      let args = buildSetupArgs ++
            [ "-package"
            , "Cabal-" ++ cabalVersionString
            , toFilePath setupHs
            , toFilePath setupShimHs
            , "-o"
            , toFilePath tmpOutputPath
            ]
      compilerPath <- getCompilerPath
      withWorkingDir (toFilePath tmpdir) $
        proc (toFilePath compilerPath) args (\pc0 -> do
          let pc = setStdout (useHandleOpen stderr) pc0
          runProcess_ pc)
            `catch` \ece ->
              prettyThrowM $ SetupHsBuildFailure
                (eceExitCode ece) Nothing compilerPath args Nothing []
      renameFile tmpExePath exePath
      pure $ Just exePath

-- | Execute a function that takes an 'ExecuteEnv'.
withExecuteEnv ::
     forall env a. HasEnvConfig env
  => BuildOpts
  -> BuildOptsCLI
  -> BaseConfigOpts
  -> [LocalPackage]
  -> [DumpPackage] -- ^ global packages
  -> [DumpPackage] -- ^ snapshot packages
  -> [DumpPackage] -- ^ project packages and local extra-deps
  -> Maybe Int -- ^ largest package name, for nicer interleaved output
  -> (ExecuteEnv -> RIO env a)
  -> RIO env a
withExecuteEnv
    buildOpts
    buildOptsCLI
    baseConfigOpts
    locals
    globalPackages
    snapshotPackages
    localPackages
    largestPackageName
    inner
  = createTempDirFunction stackProgName $ \tempDir -> do
      installLock <- liftIO $ newMVar ()
      ghcPkgIds <- liftIO $ newTVarIO Map.empty
      config <- view configL
      customBuilt <- newIORef Set.empty
      -- Create files for simple setup and setup shim, if necessary
      let setupSrcDir =
              view stackRootL config </>
              relDirSetupExeSrc
      ensureDir setupSrcDir
      let setupStub = "setup-" ++ simpleSetupHash
      setupFileName <- parseRelFile (setupStub ++ ".hs")
      setupHiName <- parseRelFile (setupStub ++ ".hi")
      setupOName <- parseRelFile (setupStub ++ ".o")
      let setupHs = setupSrcDir </> setupFileName
          setupHi = setupSrcDir </> setupHiName
          setupO =  setupSrcDir </> setupOName
      setupHsExists <- doesFileExist setupHs
      unless setupHsExists $ writeBinaryFileAtomic setupHs simpleSetupCode
      let setupShimStub = "setup-shim-" ++ simpleSetupHash
      setupShimFileName <- parseRelFile (setupShimStub ++ ".hs")
      setupShimHiName <- parseRelFile (setupShimStub ++ ".hi")
      setupShimOName <- parseRelFile (setupShimStub ++ ".o")
      let setupShimHs = setupSrcDir </> setupShimFileName
          setupShimHi = setupSrcDir </> setupShimHiName
          setupShimO = setupSrcDir </> setupShimOName
      setupShimHsExists <- doesFileExist setupShimHs
      unless setupShimHsExists $
        writeBinaryFileAtomic setupShimHs setupGhciShimCode
      setupExe <- getSetupExe setupHs setupShimHs tempDir
      -- See https://github.com/commercialhaskell/stack/issues/6267. Remove any
      -- historical *.hi or *.o files. This can be dropped when Stack drops
      -- support for the problematic versions of GHC.
      ignoringAbsence (removeFile setupHi)
      ignoringAbsence (removeFile setupO)
      ignoringAbsence (removeFile setupShimHi)
      ignoringAbsence (removeFile setupShimO)
      cabalPkgVer <- view cabalVersionL
      globalDB <- view $ compilerPathsL . to (.globalDB)
      let globalDumpPkgs = toDumpPackagesByGhcPkgId globalPackages
      snapshotDumpPkgs <-
        liftIO $ newTVarIO (toDumpPackagesByGhcPkgId snapshotPackages)
      localDumpPkgs <-
        liftIO $ newTVarIO (toDumpPackagesByGhcPkgId localPackages)
      logFiles <- liftIO $ atomically newTChan
      let totalWanted = length $ filter (.wanted) locals
      pathEnvVar <- liftIO $ maybe mempty T.pack <$> lookupEnv "PATH"
      inner ExecuteEnv
        { buildOpts
        , buildOptsCLI
          -- Uncertain as to why we cannot run configures in parallel. This
          -- appears to be a Cabal library bug. Original issue:
          -- https://github.com/commercialhaskell/stack/issues/84. Ideally
          -- we'd be able to remove this.
        , installLock
        , baseConfigOpts
        , ghcPkgIds
        , tempDir
        , setupHs
        , setupShimHs
        , setupExe
        , cabalPkgVer
        , totalWanted
        , locals
        , globalDB
        , globalDumpPkgs
        , snapshotDumpPkgs
        , localDumpPkgs
        , logFiles
        , customBuilt
        , largestPackageName
        , pathEnvVar
        } `finally` dumpLogs logFiles totalWanted
 where
  toDumpPackagesByGhcPkgId = Map.fromList . map (\dp -> (dp.ghcPkgId, dp))

  createTempDirFunction
    | buildOpts.keepTmpFiles = withKeepSystemTempDir
    | otherwise = withSystemTempDir

  dumpLogs :: TChan (Path Abs Dir, Path Abs File) -> Int -> RIO env ()
  dumpLogs chan totalWanted = do
    allLogs <- fmap reverse $ liftIO $ atomically drainChan
    case allLogs of
      -- No log files generated, nothing to dump
      [] -> pure ()
      firstLog:_ -> do
        toDump <- view $ configL . to (.dumpLogs)
        case toDump of
          DumpAllLogs -> mapM_ (dumpLog "") allLogs
          DumpWarningLogs -> mapM_ dumpLogIfWarning allLogs
          DumpNoLogs
              | totalWanted > 1 ->
                  prettyInfoL
                    [ flow "Build output has been captured to log files, use"
                    , style Shell "--dump-logs"
                    , flow "to see it on the console."
                    ]
              | otherwise -> pure ()
        prettyInfoL
          [ flow "Log files have been written to:"
          , pretty (parent (snd firstLog))
          ]

    -- We only strip the colors /after/ we've dumped logs, so that we get pretty
    -- colors in our dump output on the terminal.
    colors <- shouldForceGhcColorFlag
    when colors $ liftIO $ mapM_ (stripColors . snd) allLogs
   where
    drainChan :: STM [(Path Abs Dir, Path Abs File)]
    drainChan = do
      mx <- tryReadTChan chan
      case mx of
        Nothing -> pure []
        Just x -> do
          xs <- drainChan
          pure $ x:xs

  dumpLogIfWarning :: (Path Abs Dir, Path Abs File) -> RIO env ()
  dumpLogIfWarning (pkgDir, filepath) = do
    firstWarning <- withSourceFile (toFilePath filepath) $ \src ->
         runConduit
       $ src
      .| CT.decodeUtf8Lenient
      .| CT.lines
      .| CL.map stripCR
      .| CL.filter isWarning
      .| CL.take 1
    unless (null firstWarning) $ dumpLog " due to warnings" (pkgDir, filepath)

  isWarning :: Text -> Bool
  isWarning t = ": Warning:" `T.isSuffixOf` t -- prior to GHC 8
             || ": warning:" `T.isInfixOf` t -- GHC 8 is slightly different
             || "mwarning:" `T.isInfixOf` t -- colorized output

  dumpLog :: String -> (Path Abs Dir, Path Abs File) -> RIO env ()
  dumpLog msgSuffix (pkgDir, filepath) = do
    prettyNote $
         fillSep
           ( ( fillSep
                 ( flow "Dumping log file"
                 : [ flow msgSuffix | not (L.null msgSuffix) ]
                 )
             <> ":"
             )
           : [ pretty filepath <> "." ]
           )
      <> line
    compilerVer <- view actualCompilerVersionL
    withSourceFile (toFilePath filepath) $ \src ->
         runConduit
       $ src
      .| CT.decodeUtf8Lenient
      .| mungeBuildOutput ExcludeTHLoading ConvertPathsToAbsolute pkgDir compilerVer
      .| CL.mapM_ (logInfo . display)
    prettyNote $
         fillSep
           [ flow "End of log file:"
           , pretty filepath <> "."
           ]
      <> line

  stripColors :: Path Abs File -> IO ()
  stripColors fp = do
    let colorfp = toFilePath fp ++ "-color"
    withSourceFile (toFilePath fp) $ \src ->
      withSinkFile colorfp $ \sink ->
      runConduit $ src .| sink
    withSourceFile colorfp $ \src ->
      withSinkFile (toFilePath fp) $ \sink ->
      runConduit $ src .| noColors .| sink

   where
    noColors = do
      CB.takeWhile (/= 27) -- ESC
      mnext <- CB.head
      whenJust mnext $ \x -> assert (x == 27) $ do
        -- Color sequences always end with an m
        CB.dropWhile (/= 109) -- m
        CB.drop 1 -- drop the m itself
        noColors

-- | Make a padded prefix for log messages
packageNamePrefix :: ExecuteEnv -> PackageName -> String
packageNamePrefix ee name' =
  let name = packageNameString name'
      paddedName =
        case ee.largestPackageName of
          Nothing -> name
          Just len ->
            assert (len >= length name) $ take len $ name ++ L.repeat ' '
  in  paddedName <> "> "

announceTask ::
     HasLogFunc env
  => ExecuteEnv
  -> TaskType
  -> Utf8Builder
  -> RIO env ()
announceTask ee taskType action = logInfo $
     fromString
       (packageNamePrefix ee (pkgName (taskTypePackageIdentifier taskType)))
  <> action

prettyAnnounceTask ::
     HasTerm env
  => ExecuteEnv
  -> TaskType
  -> StyleDoc
  -> RIO env ()
prettyAnnounceTask ee taskType action = prettyInfo $
     fromString
       (packageNamePrefix ee (pkgName (taskTypePackageIdentifier taskType)))
  <> action

-- | Ensure we're the only action using the directory.  See
-- <https://github.com/commercialhaskell/stack/issues/2730>
withLockedDistDir ::
     forall env a. HasEnvConfig env
  => (StyleDoc -> RIO env ()) -- ^ A pretty announce function
  -> Path Abs Dir -- ^ root directory for package
  -> RIO env a
  -> RIO env a
withLockedDistDir announce root inner = do
  distDir <- distRelativeDir
  let lockFP = root </> distDir </> relFileBuildLock
  ensureDir $ parent lockFP

  mres <-
    withRunInIO $ \run ->
    withTryFileLock (toFilePath lockFP) Exclusive $ \_lock ->
    run inner

  case mres of
    Just res -> pure res
    Nothing -> do
      let complainer :: Companion (RIO env)
          complainer delay = do
            delay 5000000 -- 5 seconds
            announce $ fillSep
              [ flow "blocking for directory lock on"
              , pretty lockFP
              ]
            forever $ do
              delay 30000000 -- 30 seconds
              announce $ fillSep
                [ flow "still blocking for directory lock on"
                , pretty lockFP <> ";"
                , flow "maybe another Stack process is running?"
                ]
      withCompanion complainer $
        \stopComplaining ->
          withRunInIO $ \run ->
            withFileLock (toFilePath lockFP) Exclusive $ \_ ->
              run $ stopComplaining *> inner

-- | How we deal with output from GHC, either dumping to a log file or the
-- console (with some prefix).
data OutputType
  = OTLogFile !(Path Abs File) !Handle
  | OTConsole !(Maybe Utf8Builder)

-- | This sets up a context for executing build steps which need to run
-- Cabal (via a compiled Setup.hs).  In particular it does the following:
--
-- * Ensures the package exists in the file system, downloading if necessary.
--
-- * Opens a log file if the built output shouldn't go to stderr.
--
-- * Ensures that either a simple Setup.hs is built, or the package's
--   custom setup is built.
--
-- * Provides the user a function with which run the Cabal process.
withSingleContext ::
     forall env a. HasEnvConfig env
  => ActionContext
  -> ExecuteEnv
  -> TaskType
  -> Map PackageIdentifier GhcPkgId
     -- ^ All dependencies' package ids to provide to Setup.hs.
  -> Maybe String
  -> (  Package        -- Package info
     -> Path Abs File  -- Cabal file path
     -> Path Abs Dir   -- Package root directory file path
        -- Note that the `Path Abs Dir` argument is redundant with the
        -- `Path Abs File` argument, but we provide both to avoid recalculating
        -- `parent` of the `File`.
     -> (KeepOutputOpen -> ExcludeTHLoading -> [String] -> RIO env ())
        -- Function to run Cabal with args
     -> (Utf8Builder -> RIO env ())
        -- An plain 'announce' function, for different build phases
     -> OutputType
     -> RIO env a)
  -> RIO env a
withSingleContext
    ac
    ee
    taskType
    allDeps
    msuffix
    inner0
  = withPackage $ \package cabalFP pkgDir ->
      withOutputType pkgDir package $ \outputType ->
        withCabal package pkgDir outputType $ \cabal ->
          inner0 package cabalFP pkgDir cabal announce outputType
 where
  pkgId = taskTypePackageIdentifier taskType
  announce = announceTask ee taskType
  prettyAnnounce = prettyAnnounceTask ee taskType

  wanted =
    case taskType of
      TTLocalMutable lp -> lp.wanted
      TTRemotePackage{} -> False

  -- Output to the console if this is the last task, and the user asked to build
  -- it specifically. When the action is a 'ConcurrencyDisallowed' action
  -- (benchmarks), then we can also be sure to have exclusive access to the
  -- console, so output is also sent to the console in this case.
  --
  -- See the discussion on #426 for thoughts on sending output to the console
  --from concurrent tasks.
  console =
       (  wanted
       && all
            (\(ActionId ident _) -> ident == pkgId)
            (Set.toList ac.remaining)
       && ee.totalWanted == 1
       )
    || ac.concurrency == ConcurrencyDisallowed

  withPackage inner =
    case taskType of
      TTLocalMutable lp -> do
        let root = parent lp.cabalFP
        withLockedDistDir prettyAnnounce root $
          inner lp.package lp.cabalFP root
      TTRemotePackage _ package pkgloc -> do
        suffix <-
          parseRelDir $ packageIdentifierString $ packageIdentifier package
        let dir = ee.tempDir </> suffix
        unpackPackageLocation dir pkgloc

        -- See: https://github.com/commercialhaskell/stack/issues/157
        distDir <- distRelativeDir
        let oldDist = dir </> relDirDist
            newDist = dir </> distDir
        exists <- doesDirExist oldDist
        when exists $ do
          -- Previously used takeDirectory, but that got confused
          -- by trailing slashes, see:
          -- https://github.com/commercialhaskell/stack/issues/216
          --
          -- Instead, use Path which is a bit more resilient
          ensureDir $ parent newDist
          renameDir oldDist newDist

        let name = pkgName pkgId
        cabalfpRel <- parseRelFile $ packageNameString name ++ ".cabal"
        let cabalFP = dir </> cabalfpRel
        inner package cabalFP dir

  withOutputType pkgDir package inner
    -- Not in interleaved mode. When building a single wanted package, dump
    -- to the console with no prefix.
    | console = inner $ OTConsole Nothing

    -- If the user requested interleaved output, dump to the console with a
    -- prefix.
    | ee.buildOpts.interleavedOutput = inner $
        OTConsole $ Just $ fromString (packageNamePrefix ee package.name)

    -- Neither condition applies, dump to a file.
    | otherwise = do
        logPath <- buildLogPath package msuffix
        ensureDir (parent logPath)
        let fp = toFilePath logPath

        -- We only want to dump logs for local non-dependency packages
        case taskType of
          TTLocalMutable lp | lp.wanted ->
              liftIO $ atomically $ writeTChan ee.logFiles (pkgDir, logPath)
          _ -> pure ()

        withBinaryFile fp WriteMode $ \h -> inner $ OTLogFile logPath h

  withCabal ::
       Package
    -> Path Abs Dir
    -> OutputType
    -> (  (KeepOutputOpen -> ExcludeTHLoading -> [String] -> RIO env ())
       -> RIO env a
       )
    -> RIO env a
  withCabal package pkgDir outputType inner = do
    config <- view configL
    unless config.allowDifferentUser $
      checkOwnership (pkgDir </> config.workDir)
    let envSettings = EnvSettings
          { includeLocals = taskTypeLocation taskType == Local
          , includeGhcPackagePath = False
          , stackExe = False
          , localeUtf8 = True
          , keepGhcRts = False
          }
    menv <- liftIO $ config.processContextSettings envSettings
    distRelativeDir' <- distRelativeDir
    setupexehs <-
      -- Avoid broken Setup.hs files causing problems for simple build
      -- types, see:
      -- https://github.com/commercialhaskell/stack/issues/370
      case (package.buildType, ee.setupExe) of
        (C.Simple, Just setupExe) -> pure $ SimpleSetupExe setupExe
        _ -> liftIO $ OtherSetupHs <$> getSetupHs pkgDir
    inner $ \keepOutputOpen stripTHLoading args -> do
      let cabalPackageArg
            -- Omit cabal package dependency when building
            -- Cabal. See
            -- https://github.com/commercialhaskell/stack/issues/1356
            | package.name == mkPackageName "Cabal" = []
            | otherwise =
                ["-package=" ++ packageIdentifierString
                                    (PackageIdentifier cabalPackageName
                                                      ee.cabalPkgVer)]
          packageDBArgs =
            ( "-clear-package-db"
            : "-global-package-db"
            : map
                (("-package-db=" ++) . toFilePathNoTrailingSep)
                ee.baseConfigOpts.extraDBs
            ) ++
            ( (  "-package-db="
              ++ toFilePathNoTrailingSep ee.baseConfigOpts.snapDB
              )
            : (  "-package-db="
              ++ toFilePathNoTrailingSep ee.baseConfigOpts.localDB
              )
            : ["-hide-all-packages"]
            )

          warnCustomNoDeps :: RIO env ()
          warnCustomNoDeps =
            case (taskType, package.buildType) of
              (TTLocalMutable lp, C.Custom) | lp.wanted ->
                prettyWarnL
                  [ flow "Package"
                  , fromPackageName package.name
                  , flow "uses a custom Cabal build, but does not use a \
                         \custom-setup stanza"
                  ]
              _ -> pure ()

          getPackageArgs :: Path Abs Dir -> RIO env [String]
          getPackageArgs setupDir =
            case package.setupDeps of
              -- The package is using the Cabal custom-setup configuration
              -- introduced in Cabal 1.24. In this case, the package is
              -- providing an explicit list of dependencies, and we should
              -- simply use all of them.
              Just customSetupDeps -> do
                unless (Map.member (mkPackageName "Cabal") customSetupDeps) $
                  prettyWarnL
                    [ fromPackageName package.name
                    , flow "has a setup-depends field, but it does not mention \
                           \a Cabal dependency. This is likely to cause build \
                           \errors."
                    ]
                matchedDeps <-
                  forM (Map.toList customSetupDeps) $ \(name, depValue) -> do
                    let matches (PackageIdentifier name' version) =
                             name == name'
                          && version `withinRange` depValue.versionRange
                    case filter (matches . fst) (Map.toList allDeps) of
                      x:xs -> do
                        unless (null xs) $
                          prettyWarnL
                            [ flow "Found multiple installed packages for \
                                   \custom-setup dep:"
                            , style Current (fromPackageName name) <> "."
                            ]
                        pure ("-package-id=" ++ ghcPkgIdString (snd x), Just (fst x))
                      [] -> do
                        prettyWarnL
                          [ flow "Could not find custom-setup dep:"
                          , style Current (fromPackageName name) <> "."
                          ]
                        pure ("-package=" ++ packageNameString name, Nothing)
                let depsArgs = map fst matchedDeps
                -- Generate setup_macros.h and provide it to ghc
                let macroDeps = mapMaybe snd matchedDeps
                    cppMacrosFile = setupDir </> relFileSetupMacrosH
                    cppArgs =
                      ["-optP-include", "-optP" ++ toFilePath cppMacrosFile]
                writeBinaryFileAtomic
                  cppMacrosFile
                  ( encodeUtf8Builder
                      ( T.pack
                          ( C.generatePackageVersionMacros
                              package.version
                              macroDeps
                          )
                      )
                  )
                pure (packageDBArgs ++ depsArgs ++ cppArgs)

              -- This branch is usually taken for builds, and is always taken
              -- for `stack sdist`.
              --
              -- This approach is debatable. It adds access to the snapshot
              -- package database for Cabal. There are two possible objections:
              --
              -- 1. This doesn't isolate the build enough; arbitrary other
              -- packages available could cause the build to succeed or fail.
              --
              -- 2. This doesn't provide enough packages: we should also
              -- include the local database when building local packages.
              --
              -- Currently, this branch is only taken via `stack sdist` or when
              -- explicitly requested in the stack.yaml file.
              Nothing -> do
                warnCustomNoDeps
                let packageDBArgs' = case package.buildType of
                      -- The Configure build type is very similar to Simple. As
                      -- such, Stack builds the setup executable in much the
                      -- same way as it would in the case of Simple.
                      C.Configure ->
                        [ "-hide-all-packages"
                        , "-package base"
                        ]
                      -- NOTE: This is different from packageDBArgs above in
                      -- that it does not include the local database and does
                      -- not pass in the -hide-all-packages argument
                      _ ->
                           map
                             (("-package-db=" ++) . toFilePathNoTrailingSep)
                             ee.baseConfigOpts.extraDBs
                        <> [    "-package-db="
                             <> toFilePathNoTrailingSep ee.baseConfigOpts.snapDB
                           ]
                pure $
                     [ "-clear-package-db"
                     , "-global-package-db"
                     ]
                  <> packageDBArgs'
                  <> cabalPackageArg

          setupArgs =
            ("--builddir=" ++ toFilePathNoTrailingSep distRelativeDir') : args

          runExe :: Path Abs File -> [String] -> RIO env ()
          runExe exeName fullArgs = do
            compilerVer <- view actualCompilerVersionL
            runAndOutput compilerVer `catch` \ece -> do
              (mlogFile, bss) <-
                case outputType of
                  OTConsole _ -> pure (Nothing, [])
                  OTLogFile logFile h ->
                    if keepOutputOpen == KeepOpen
                    then
                      pure (Nothing, []) -- expected failure build continues further
                    else do
                      liftIO $ hClose h
                      fmap (Just logFile,) $ withSourceFile (toFilePath logFile) $
                        \src ->
                             runConduit
                           $ src
                          .| CT.decodeUtf8Lenient
                          .| mungeBuildOutput
                               stripTHLoading makeAbsolute pkgDir compilerVer
                          .| CL.consume
              prettyThrowM $ CabalExitedUnsuccessfully
                (eceExitCode ece) pkgId exeName fullArgs mlogFile bss
           where
            runAndOutput :: ActualCompiler -> RIO env ()
            runAndOutput compilerVer = withWorkingDir (toFilePath pkgDir) $
              withProcessContext menv $ case outputType of
                OTLogFile _ h -> do
                  let prefixWithTimestamps =
                        if config.prefixTimestamps
                          then PrefixWithTimestamps
                          else WithoutTimestamps
                  void $ sinkProcessStderrStdout (toFilePath exeName) fullArgs
                    (sinkWithTimestamps prefixWithTimestamps h)
                    (sinkWithTimestamps prefixWithTimestamps h)
                OTConsole mprefix ->
                  let prefix = fromMaybe mempty mprefix
                  in  void $ sinkProcessStderrStdout
                        (toFilePath exeName)
                        fullArgs
                        (outputSink KeepTHLoading LevelWarn compilerVer prefix)
                        (outputSink stripTHLoading LevelInfo compilerVer prefix)
            outputSink ::
                 HasCallStack
              => ExcludeTHLoading
              -> LogLevel
              -> ActualCompiler
              -> Utf8Builder
              -> ConduitM S.ByteString Void (RIO env) ()
            outputSink excludeTH level compilerVer prefix =
              CT.decodeUtf8Lenient
              .| mungeBuildOutput excludeTH makeAbsolute pkgDir compilerVer
              .| CL.mapM_ (logGeneric "" level . (prefix <>) . display)
            -- If users want control, we should add a config option for this
            makeAbsolute :: ConvertPathsToAbsolute
            makeAbsolute = case stripTHLoading of
              ExcludeTHLoading -> ConvertPathsToAbsolute
              KeepTHLoading    -> KeepPathsAsIs

      exeName <- case setupexehs of
        SimpleSetupExe setupExe -> pure setupExe
        OtherSetupHs setuphs -> do
          distDir <- distDirFromDir pkgDir
          let setupDir = distDir </> relDirSetup
              outputFile = setupDir </> relFileSetupLower
          customBuilt <- liftIO $ readIORef ee.customBuilt
          if Set.member package.name customBuilt
            then pure outputFile
            else do
              ensureDir setupDir
              compilerPath <- view $ compilerPathsL . to (.compiler)
              packageArgs <- getPackageArgs setupDir
              runExe compilerPath $
                   [ "--make"
                   , "-odir", toFilePathNoTrailingSep setupDir
                   , "-hidir", toFilePathNoTrailingSep setupDir
                   , "-i", "-i."
                   ]
                <> packageArgs
                <> [ toFilePath setuphs
                   , toFilePath ee.setupShimHs
                   , "-main-is"
                   , "StackSetupShim.mainOverride"
                   , "-o", toFilePath outputFile
                   , "-threaded"
                   ]
                -- Apply GHC options
                -- https://github.com/commercialhaskell/stack/issues/4526
                <> map
                     T.unpack
                     (  Map.findWithDefault
                          []
                          AGOEverything
                          config.ghcOptionsByCat
                     <> case config.applyGhcOptions of
                          AGOEverything -> ee.buildOptsCLI.ghcOptions
                          AGOTargets -> []
                          AGOLocals -> []
                     )

              liftIO $ atomicModifyIORef' ee.customBuilt $
                \oldCustomBuilt ->
                  (Set.insert package.name oldCustomBuilt, ())
              pure outputFile
      let cabalVerboseArg =
            let CabalVerbosity cv = ee.buildOpts.cabalVerbose
            in  "--verbose=" <> showForCabal cv
      runExe exeName $ cabalVerboseArg:setupArgs

-- | Strip Template Haskell "Loading package" lines and making paths absolute.
mungeBuildOutput ::
     forall m. (MonadIO m, MonadUnliftIO m)
  => ExcludeTHLoading       -- ^ exclude TH loading?
  -> ConvertPathsToAbsolute -- ^ convert paths to absolute?
  -> Path Abs Dir           -- ^ package's root directory
  -> ActualCompiler         -- ^ compiler we're building with
  -> ConduitM Text Text m ()
mungeBuildOutput excludeTHLoading makeAbsolute pkgDir compilerVer = void $
  CT.lines
  .| CL.map stripCR
  .| CL.filter (not . isTHLoading)
  .| filterLinkerWarnings
  .| toAbsolute
 where
  -- | Is this line a Template Haskell "Loading package" line
  -- ByteString
  isTHLoading :: Text -> Bool
  isTHLoading = case excludeTHLoading of
    KeepTHLoading    -> const False
    ExcludeTHLoading -> \bs ->
      "Loading package " `T.isPrefixOf` bs &&
      ("done." `T.isSuffixOf` bs || "done.\r" `T.isSuffixOf` bs)

  filterLinkerWarnings :: ConduitM Text Text m ()
  filterLinkerWarnings
    -- Check for ghc 7.8 since it's the only one prone to producing
    -- linker warnings on Windows x64
    | getGhcVersion compilerVer >= mkVersion [7, 8] = doNothing
    | otherwise = CL.filter (not . isLinkerWarning)

  isLinkerWarning :: Text -> Bool
  isLinkerWarning str =
       (  "ghc.exe: warning:" `T.isPrefixOf` str
       || "ghc.EXE: warning:" `T.isPrefixOf` str
       )
    && "is linked instead of __imp_" `T.isInfixOf` str

  -- | Convert GHC error lines with file paths to have absolute file paths
  toAbsolute :: ConduitM Text Text m ()
  toAbsolute = case makeAbsolute of
    KeepPathsAsIs          -> doNothing
    ConvertPathsToAbsolute -> CL.mapM toAbsolutePath

  toAbsolutePath :: Text -> m Text
  toAbsolutePath bs = do
    let (x, y) = T.break (== ':') bs
    mabs <-
      if isValidSuffix y
        then
          fmap (fmap ((T.takeWhile isSpace x <>) . T.pack . toFilePath)) $
            forgivingResolveFile pkgDir (T.unpack $ T.dropWhile isSpace x) `catch`
              \(_ :: PathException) -> pure Nothing
        else pure Nothing
    case mabs of
      Nothing -> pure bs
      Just fp -> pure $ fp `T.append` y

  doNothing :: ConduitM Text Text m ()
  doNothing = awaitForever yield

  -- | Match the error location format at the end of lines
  isValidSuffix = isRight . parseOnly lineCol
  lineCol = char ':'
    >> choice
         [ num >> char ':' >> num >> optional (char '-' >> num) >> pure ()
         , char '(' >> num >> char ',' >> num >> P.string ")-(" >> num >>
           char ',' >> num >> char ')' >> pure ()
         ]
    >> char ':'
    >> pure ()
   where
    num = some digit

-- | Whether to prefix log lines with timestamps.
data PrefixWithTimestamps
  = PrefixWithTimestamps
  | WithoutTimestamps

-- | Write stream of lines to handle, but adding timestamps.
sinkWithTimestamps ::
     MonadIO m
  => PrefixWithTimestamps
  -> Handle
  -> ConduitT ByteString Void m ()
sinkWithTimestamps prefixWithTimestamps h =
  case prefixWithTimestamps of
    PrefixWithTimestamps ->
      CB.lines .| CL.mapM addTimestamp .| CL.map (<> "\n") .| sinkHandle h
    WithoutTimestamps -> sinkHandle h
 where
  addTimestamp theLine = do
    now <- liftIO getZonedTime
    pure (formatZonedTimeForLog now <> " " <> theLine)

-- | Format a time in ISO8601 format. We choose ZonedTime over UTCTime
-- because a user expects to see logs in their local time, and would
-- be confused to see UTC time. Stack's debug logs also use the local
-- time zone.
formatZonedTimeForLog :: ZonedTime -> ByteString
formatZonedTimeForLog =
  S8.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6Q"

-- | Find the Setup.hs or Setup.lhs in the given directory. If none exists,
-- throw an exception.
getSetupHs :: Path Abs Dir -- ^ project directory
           -> IO (Path Abs File)
getSetupHs dir = do
  exists1 <- doesFileExist fp1
  if exists1
    then pure fp1
    else do
      exists2 <- doesFileExist fp2
      if exists2
        then pure fp2
        else throwM $ NoSetupHsFound dir
 where
  fp1 = dir </> relFileSetupHs
  fp2 = dir </> relFileSetupLhs
