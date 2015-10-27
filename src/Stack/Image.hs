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
       (stageContainerImageArtifacts, createContainerImageFromStage,
        imgCmdName, imgDockerCmdName, imgOptsFromMonoid,
        imgDockerOptsFromMonoid, imgOptsParser, imgDockerOptsParser)
       where

import           Control.Applicative
import           Control.Exception.Lifted hiding (finally)
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
import qualified Data.Text as T
import           Data.Typeable
import           Options.Applicative
import           Path
import           Path.Extra
import           Path.IO
import           Stack.Constants
import           Stack.Types
import           Stack.Types.Internal
import           System.Process

type Build e m = (HasBuildConfig e, HasConfig e, HasEnvConfig e, HasTerminal e, MonadBaseControl IO m, MonadCatch m, MonadIO m, MonadLogger m, MonadReader e m)

type Assemble e m = (HasConfig e, HasTerminal e, MonadBaseControl IO m, MonadCatch m, MonadIO m, MonadLogger m, MonadMask m, MonadReader e m)

-- | Stages the executables & additional content in a staging
-- directory under '.stack-work'
stageContainerImageArtifacts :: Build e m
                             => m ()
stageContainerImageArtifacts = do
    imageDir <- imageStagingDir <$> getWorkingDir
    removeTreeIfExists imageDir
    createTree imageDir
    stageExesInDir imageDir
    syncAddContentToDir imageDir

-- | Builds a Docker (OpenContainer) image extending the `base` image
-- specified in the project's stack.yaml.  Then new image will be
-- extended with an ENTRYPOINT specified for each `entrypoint` listed
-- in the config file.
createContainerImageFromStage :: Assemble e m
                              => m ()
createContainerImageFromStage = do
    imageDir <- imageStagingDir <$> getWorkingDir
    createDockerImage imageDir
    extendDockerImageWithEntrypoint imageDir

-- | Stage all the Package executables in the usr/local/bin
-- subdirectory of a temp directory.
stageExesInDir :: Build e m => Path Abs Dir -> m ()
stageExesInDir dir = do
    srcBinPath <-
        (</> $(mkRelDir "bin")) <$>
        installationRootLocal
    let destBinPath = dir </>
            $(mkRelDir "usr/local/bin")
    createTree destBinPath
    copyDirectoryRecursive srcBinPath destBinPath

-- | Add any additional files into the temp directory, respecting the
-- (Source, Destination) mapping.
syncAddContentToDir :: Build e m => Path Abs Dir -> m ()
syncAddContentToDir dir = do
    config <- asks getConfig
    bconfig <- asks getBuildConfig
    let imgAdd = maybe Map.empty imgDockerAdd (imgDocker (configImage config))
    forM_
        (Map.toList imgAdd)
        (\(source,dest) ->
              do sourcePath <- parseRelDir source
                 destPath <- parseAbsDir dest
                 let destFullPath = dir </> dropRoot destPath
                 createTree destFullPath
                 copyDirectoryRecursive
                     (bcRoot bconfig </> sourcePath)
                     destFullPath)

-- | Derive an image name from the project directory.
imageName :: Path Abs Dir -> String
imageName = map toLower . toFilePathNoTrailingSep . dirname

-- | Create a general purpose docker image from the temporary
-- directory of executables & static content.
createDockerImage :: Assemble e m => Path Abs Dir -> m ()
createDockerImage dir = do
    config <- asks getConfig
    let dockerConfig = imgDocker (configImage config)
    case imgDockerBase =<< dockerConfig of
        Nothing -> throwM StackImageDockerBaseUnspecifiedException
        Just base ->
            liftIO
                (do writeFile
                        (toFilePath
                             (dir </>
                              $(mkRelFile "Dockerfile")))
                        (unlines ["FROM " ++ base, "ADD ./ /"])
                    callProcess
                        "docker"
                        [ "build"
                        , "-t"
                        , fromMaybe
                              (imageName (parent (parent dir)))
                              (imgDockerImageName =<< dockerConfig)
                        , toFilePath dir])

-- | Extend the general purpose docker image with entrypoints (if
-- specified).
extendDockerImageWithEntrypoint :: Assemble e m => Path Abs Dir -> m ()
extendDockerImageWithEntrypoint dir = do
    config <- asks getConfig
    let dockerConfig = imgDocker (configImage config)
    let dockerImageName = fromMaybe
                (imageName (parent (parent dir)))
                (imgDockerImageName =<< dockerConfig)
    let imgEntrypoints = maybe Nothing imgDockerEntrypoints dockerConfig
    case imgEntrypoints of
        Nothing -> return ()
        Just eps ->
            forM_
                eps
                (\ep ->
                      liftIO
                          (do writeFile
                                  (toFilePath
                                       (dir </>
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
                                  , toFilePath dir]))

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
