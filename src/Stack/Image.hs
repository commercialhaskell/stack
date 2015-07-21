{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module builds Docker (OpenContainer) images.
module Stack.Image
       (imageDocker, imgCmdName, imgDockerCmdName, imgOptsFromMonoid,
        imgDockerOptsFromMonoid, imgOptsParser, imgDockerOptsParser)
       where

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Catch hiding (bracket)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Char (toLower)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Typeable
import           Options.Applicative
import           Path
import           Path.IO
import           Stack.Build.Source
import           Stack.Package
import           Stack.Types
import           Stack.Types.Internal
import qualified System.Directory as SD
import           System.IO.Temp
import           System.FilePath (isPathSeparator)
import           System.Process

type M e m = (HasBuildConfig e, HasConfig e, HasEnvConfig e, HasTerminal e,
              MonadBaseControl IO m, MonadCatch m, MonadIO m, MonadLogger m,
              MonadReader e m)

-- | Builds a Docker (OpenContainer) image extending the `base` image
-- specified in the project's stack.yaml.  The new image will contain
-- all the executables from the packages in the project as well as any
-- other specified files to `add`.  Then new image will be extended
-- with an ENTRYPOINT specified for each `entrypoint` listed in the
-- config file.
imageDocker :: M e m => m ()
imageDocker = do
    tempDirFP <- liftIO SD.getTemporaryDirectory
    bracket
        (liftIO (createTempDirectory tempDirFP "stack-image-docker"))
        (liftIO . SD.removeDirectoryRecursive)
        (\dir ->
              do stageExesInDir dir
                 syncAddContentToDir dir
                 createDockerImage dir
                 extendDockerImageWithEntrypoint dir)

-- | Extract all the Package(s) from the stack.yaml config file &
-- project cabal files.
projectPkgs :: M e m => m [Package]
projectPkgs = do
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig
    forM
        (Map.toList
             (bcPackages bconfig))
        (\(dir,_wanted) ->
              do cabalfp <- getCabalFileName dir
                 name <- parsePackageNameFromFilePath cabalfp
                 let cfg = PackageConfig
                         { packageConfigEnableTests = True
                         , packageConfigEnableBenchmarks = True
                         , packageConfigFlags = localFlags mempty bconfig name
                         , packageConfigGhcVersion = envConfigGhcVersion econfig
                         , packageConfigPlatform = configPlatform
                               (getConfig bconfig)
                         }
                 readPackage cfg cabalfp)

-- | Stage all the Package executables in the usr/local/bin
-- subdirectory of a temp directory.
stageExesInDir :: M e m => FilePath -> m ()
stageExesInDir dir = do
    srcBinPath <- (</> $(mkRelDir "bin")) <$> installationRootLocal
    destBinPath <- (</> $(mkRelDir "usr/local/bin")) <$> parseAbsDir dir
    createTree destBinPath
    pkgs <- projectPkgs
    forM_
        (concatMap (Set.toList . packageExes) pkgs)
        (\exe ->
              do exePath <-
                     parseRelFile
                         (T.unpack exe)
                 copyFile
                     (srcBinPath </> exePath)
                     (destBinPath </> exePath))

-- | Add any additional files into the temp directory, respecting the
-- (Source, Destination) mapping.
syncAddContentToDir :: M e m => FilePath -> m ()
syncAddContentToDir dir = do
    config <- asks getConfig
    bconfig <- asks getBuildConfig
    dirPath <- parseAbsDir dir
    let imgAdd = maybe Map.empty imgDockerAdd (imgDocker (configImage config))
    forM_
        (Map.toList imgAdd)
        (\(source,dest) ->
              do sourcePath <- parseRelDir source
                 destPath <- parseAbsDir dest
                 let destFullPath = dirPath </> dropRoot destPath
                 createTree destFullPath
                 copyDirectoryRecursive
                     (bcRoot bconfig </> sourcePath)
                     destFullPath)

-- | Derive an image name from the project directory.
imageName :: BuildConfig -> String
imageName = map toLower . filter (not . isPathSeparator) . toFilePath . dirname . bcRoot

-- | Create a general purpose docker image from the temporary
-- directory of executables & static content.
createDockerImage :: M e m => FilePath -> m ()
createDockerImage dir = do
    config <- asks getConfig
    bconfig <- asks getBuildConfig
    let dockerConfig = imgDocker (configImage config)
    case imgDockerBase =<< dockerConfig of
        Nothing -> throwM StackImageDockerBaseUnspecifiedException
        Just base -> do
            dirPath <- parseAbsDir dir
            liftIO
                (do writeFile
                        (toFilePath
                             (dirPath </>
                              $(mkRelFile "Dockerfile")))
                        (unlines ["FROM " ++ base, "ADD ./ /"])
                    callProcess
                        "docker"
                        ["build"
                        ,"-t"
                        ,fromMaybe (imageName bconfig)
                                   (imgDockerImageName =<< dockerConfig)
                        ,dir])

-- | Extend the general purpose docker image with entrypoints (if
-- specified).
extendDockerImageWithEntrypoint :: M e m => FilePath -> m ()
extendDockerImageWithEntrypoint dir = do
    config <- asks getConfig
    bconfig <- asks getBuildConfig
    let dockerConfig = imgDocker (configImage config)
    let dockerImageName = fromMaybe (imageName bconfig) (imgDockerImageName =<< dockerConfig)
    let imgEntrypoints = maybe
                Nothing
                imgDockerEntrypoints
                dockerConfig
    case imgEntrypoints of
        Nothing -> return ()
        Just eps -> do
            dirPath <- parseAbsDir dir
            forM_
                eps
                (\ep ->
                      liftIO
                          (do writeFile
                                  (toFilePath
                                       (dirPath </>
                                        $(mkRelFile "Dockerfile")))
                                  (unlines
                                       [ "FROM " ++ dockerImageName
                                       , "ENTRYPOINT [\"/usr/local/bin/" ++
                                         ep ++ "\"]"
                                       , "CMD []"])
                              callProcess
                                  "docker"
                                  [ "build"
                                  , "-t"
                                  , dockerImageName ++ "-" ++ ep
                                  , dir]))

-- | The command name for dealing with images.
imgCmdName :: String
imgCmdName = "image"

-- | The command name for building a docker container.
imgDockerCmdName :: String
imgDockerCmdName = "container"

-- | A parser for ImageOptsMonoid.
imgOptsParser :: Parser ImageOptsMonoid
imgOptsParser = ImageOptsMonoid <$>
    optional
        (subparser
             (command
                  imgDockerCmdName
                  (info
                       imgDockerOptsParser
                       (progDesc "Create a container image (EXPERIMENTAL)"))))

-- | A parser for ImageDockerOptsMonoid.
imgDockerOptsParser :: Parser ImageDockerOptsMonoid
imgDockerOptsParser = ImageDockerOptsMonoid <$>
    optional
        (option
             str
             (long (imgDockerCmdName ++ "-" ++ T.unpack imgDockerBaseArgName) <>
              metavar "NAME" <>
              help "Docker base image name")) <*>
    pure Nothing <*>
    pure Nothing <*>
    pure Nothing

-- | Convert image opts monoid to image options.
imgOptsFromMonoid :: ImageOptsMonoid -> ImageOpts
imgOptsFromMonoid ImageOptsMonoid{..} = ImageOpts
    { imgDocker = imgDockerOptsFromMonoid <$> imgMonoidDocker
    }

-- | Convert Docker image opts monoid to Docker image options.
imgDockerOptsFromMonoid :: ImageDockerOptsMonoid -> ImageDockerOpts
imgDockerOptsFromMonoid ImageDockerOptsMonoid{..} = ImageDockerOpts
    { imgDockerBase = emptyToNothing imgDockerMonoidBase
    , imgDockerEntrypoints = emptyToNothing imgDockerMonoidEntrypoints
    , imgDockerAdd = fromMaybe Map.empty imgDockerMonoidAdd
    , imgDockerImageName = emptyToNothing imgDockerMonoidImageName
    }
    where emptyToNothing Nothing = Nothing
          emptyToNothing (Just s)
              | null s =
                  Nothing
              | otherwise =
                  Just s

-- | Stack image exceptions.
data StackImageException =
    StackImageDockerBaseUnspecifiedException
    deriving (Typeable)

instance Exception StackImageException

instance Show StackImageException where
    show StackImageDockerBaseUnspecifiedException = "You must specify a base docker image on which to place your haskell executables."
