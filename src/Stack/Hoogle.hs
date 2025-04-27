{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Stack.Hoogle
Description : A wrapper around hoogle.
License     : BSD-3-Clause

A wrapper around hoogle.
-}

module Stack.Hoogle
  ( hoogleCmd
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Char ( isSpace )
import           Data.Either.Extra ( eitherToMaybe )
import qualified Data.Text as T
import           Distribution.PackageDescription ( packageDescription, package )
import           Distribution.Types.PackageName ( mkPackageName )
import           Distribution.Version ( mkVersion )
import           Lens.Micro ( (?~) )
import           Path ( parseAbsFile )
import           Path.IO ( createDirIfMissing, doesFileExist )
import qualified RIO.Map as Map
import           RIO.Process ( findExecutable, proc, readProcess_, runProcess_)
import qualified Stack.Build ( build )
import           Stack.Build.Target ( NeedTargets (..) )
import           Stack.Constants ( stackProgName' )
import           Stack.Prelude
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withDefaultEnvConfig
                   , withEnvConfig
                   )
import           Stack.Types.BuildOptsCLI
                   ( BuildOptsCLI (..), defaultBuildOptsCLI )
import           Stack.Types.BuildOptsMonoid ( buildOptsMonoidHaddockL )
import           Stack.Types.Config
                   ( Config (..), HasConfig (..) )
import           Stack.Types.EnvConfig
                   ( EnvConfig, HasSourceMap (..), hoogleDatabasePath
                   , hoogleRoot
                   )
import           Stack.Types.EnvSettings ( EnvSettings (..) )
import           Stack.Types.GlobalOpts
                   ( GlobalOpts (..), globalOptsBuildOptsMonoidL )
import           Stack.Types.Runner ( Runner, globalOptsL )
import           Stack.Types.SourceMap ( DepPackage (..), SourceMap (..) )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Hoogle" module.
data HoogleException
  = HoogleOnPathNotFoundBug
  deriving (Show, Typeable)

instance Exception HoogleException where
  displayException HoogleOnPathNotFoundBug = bugReport "[S-9669]"
    "Cannot find Hoogle executable on PATH, after installing."

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.Hoogle" module.
data HooglePrettyException
  = HoogleNotFound StyleDoc
  | HoogleDatabaseNotFound
  deriving (Show, Typeable)

instance Pretty HooglePrettyException where
  pretty (HoogleNotFound e) =
    "[S-1329]"
    <> line
    <> e
    <> line
    <> fillSep
         [ flow "Not installing Hoogle due to"
         , style Shell "--no-setup" <> "."
         ]
  pretty HoogleDatabaseNotFound =
    "[S-3025]"
    <> line
    <> fillSep
         [ flow "No Hoogle database. Not building one due to"
         , style Shell "--no-setup" <> "."
         ]

instance Exception HooglePrettyException

-- | Helper type to duplicate log messages
data Muted = Muted | NotMuted

-- | Hoogle command.
hoogleCmd :: ([String], Bool, Bool, Bool) -> RIO Runner ()
hoogleCmd (args, setup, rebuild, startServer) =
  local (over globalOptsL modifyGO) $
    withConfig YesReexec $
      withDefaultEnvConfig $ do
        hooglePath <- ensureHoogleInPath
        generateDbIfNeeded hooglePath
        runHoogle hooglePath args'
 where
  modifyGO :: GlobalOpts -> GlobalOpts
  modifyGO = globalOptsBuildOptsMonoidL . buildOptsMonoidHaddockL ?~ True

  args' :: [String]
  args' = if startServer
    then ["server", "--local", "--port", "8080"] ++ args
    else args

  generateDbIfNeeded :: Path Abs File -> RIO EnvConfig ()
  generateDbIfNeeded hooglePath = do
    databaseExists <- checkDatabaseExists
    unless (databaseExists && not rebuild) $
      if setup || rebuild
        then do
          prettyWarnL $
            if rebuild
              then
                [ flow "Rebuilding database ..." ]
              else
                [ flow "No Hoogle database yet. Automatically building \
                       \Haddock documentation and Hoogle database (use"
                , style Shell "--no-setup"
                , flow "to disable) ..."
                ]
          buildHaddocks
          prettyInfoS "Built Haddock documentation."
          generateDb hooglePath
          prettyInfoS "Generated Hoogle database."
        else prettyThrowIO HoogleDatabaseNotFound

  generateDb :: Path Abs File -> RIO EnvConfig ()
  generateDb hooglePath = do
    dir <- hoogleRoot
    createDirIfMissing True dir
    runHoogle hooglePath ["generate", "--local"]

  buildHaddocks :: RIO EnvConfig ()
  buildHaddocks = do
    config <- view configL
    runRIO config $ -- a bit weird that we have to drop down like this
      catch (withDefaultEnvConfig $ Stack.Build.build Nothing)
            (\(_ :: ExitCode) -> pure ())

  hooglePackageName = mkPackageName "hoogle"
  hoogleMinVersion = mkVersion [5, 0]
  hoogleMinIdent =
    PackageIdentifier hooglePackageName hoogleMinVersion

  installHoogle :: RIO EnvConfig (Path Abs File)
  installHoogle = requiringHoogle Muted $ do
    Stack.Build.build Nothing
    mhooglePath' <- findExecutable "hoogle"
    case mhooglePath' of
      Right hooglePath -> parseAbsFile hooglePath
      Left _ -> throwIO HoogleOnPathNotFoundBug

  requiringHoogle :: Muted -> RIO EnvConfig x -> RIO EnvConfig x
  requiringHoogle muted f = do
    hoogleTarget <- do
      sourceMap <- view $ sourceMapL . to (.deps)
      case Map.lookup hooglePackageName sourceMap of
        Just hoogleDep ->
          case hoogleDep.location of
            PLImmutable pli ->
              T.pack . packageIdentifierString <$>
                  restrictMinHoogleVersion muted (packageLocationIdent pli)
            plm@(PLMutable _) ->
              T.pack . packageIdentifierString . package . packageDescription
                  <$> loadCabalFile (Just stackProgName') plm
        Nothing -> do
          -- not muted because this should happen only once
          prettyWarnS
            "No hoogle version was found, trying to install the latest version"
          mpir <- getLatestHackageVersion YesRequireHackageIndex hooglePackageName UsePreferredVersions
          let hoogleIdent = case mpir of
                  Nothing -> hoogleMinIdent
                  Just (PackageIdentifierRevision _ ver _) ->
                      PackageIdentifier hooglePackageName ver
          T.pack . packageIdentifierString <$>
              restrictMinHoogleVersion muted hoogleIdent
    config <- view configL
    let boptsCLI = defaultBuildOptsCLI
          { targetsCLI =  [hoogleTarget] }
    runRIO config $ withEnvConfig NeedTargets boptsCLI f

  restrictMinHoogleVersion ::
       HasLogFunc env
    => Muted
    -> PackageIdentifier
    -> RIO env PackageIdentifier
  restrictMinHoogleVersion muted ident =
    if ident < hoogleMinIdent
      then do
        muteableLog LevelWarn muted $
          "Minimum " <>
          fromString (packageIdentifierString hoogleMinIdent) <>
          " is not in your index. Installing the minimum version."
        pure hoogleMinIdent
      else do
        muteableLog LevelInfo muted $
          "Minimum version is " <>
          fromString (packageIdentifierString hoogleMinIdent) <>
          ". Found acceptable " <>
          fromString (packageIdentifierString ident) <>
          " in your index, requiring its installation."
        pure ident
  muteableLog ::
       HasLogFunc env
    => LogLevel
    -> Muted
    -> Utf8Builder
    -> RIO env ()
  muteableLog logLevel muted msg =
    case muted of
      Muted -> pure ()
      NotMuted -> logGeneric "" logLevel msg

  runHoogle :: Path Abs File -> [String] -> RIO EnvConfig ()
  runHoogle hooglePath hoogleArgs = do
    config <- view configL
    menv <- liftIO $ config.processContextSettings envSettings
    dbpath <- hoogleDatabasePath
    let databaseArg = ["--database=" ++ toFilePath dbpath]
    withProcessContext menv $ proc
      (toFilePath hooglePath)
      (hoogleArgs ++ databaseArg)
      runProcess_

  checkDatabaseExists = do
    path <- hoogleDatabasePath
    liftIO (doesFileExist path)

  ensureHoogleInPath :: RIO EnvConfig (Path Abs File)
  ensureHoogleInPath = do
    config <- view configL
    menv <- liftIO $ config.processContextSettings envSettings
    mHooglePath' <- eitherToMaybe <$> runRIO menv (findExecutable "hoogle")
    let mHooglePath'' =
          eitherToMaybe <$> requiringHoogle NotMuted (findExecutable "hoogle")
    mHooglePath <- maybe mHooglePath'' (pure . Just) mHooglePath'
    eres <- case mHooglePath of
      Nothing -> pure $ Left (flow "Hoogle isn't installed.")
      Just hooglePath -> do
        result <- withProcessContext menv
          $ proc hooglePath ["--numeric-version"]
          $ tryAny . fmap fst . readProcess_
        let unexpectedResult got = Left $
                 fillSep
                   [ style Shell (fromString hooglePath)
                   , style Shell "--numeric-version"
                   , flow "did not respond with expected value. Got:"
                   ]
              <> blankLine
              <> got
        pure $ case result of
          Left err -> unexpectedResult $ string (displayException err)
          Right bs ->
            case parseVersion (takeWhile (not . isSpace) (BL8.unpack bs)) of
              Nothing -> unexpectedResult $ fromString (BL8.unpack bs)
              Just ver
                | ver >= hoogleMinVersion -> Right hooglePath
                | otherwise -> Left $
                    fillSep
                      [ flow "Installed Hoogle is too old, "
                      , style Shell (fromString hooglePath)
                      , flow "is version"
                      , fromString (versionString ver)
                      , flow "but >= 5.0 is required."
                      ]
    case eres of
      Right hooglePath -> parseAbsFile hooglePath
      Left err
        | setup -> do
            prettyWarnL
              [ err
              , flow "Automatically installing (use"
              , style Shell "--no-setup"
              , flow "to disable) ..."
              ]
            installHoogle
        | otherwise -> prettyThrowIO $ HoogleNotFound err

  envSettings = EnvSettings
    { includeLocals = True
    , includeGhcPackagePath = True
    , stackExe = True
    , localeUtf8 = False
    , keepGhcRts = False
    }
