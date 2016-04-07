{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | This module builds Docker (OpenContainer) images.
module Stack.Image
       (stageContainerImageArtifacts, createContainerImageFromStage,
        imgCmdName, imgDockerCmdName, imgOptsFromMonoid)
       where

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
import           Data.Typeable
import           Data.Text (Text)
import qualified Data.Text as T
import           Path
import           Path.Extra
import           Path.IO
import           Stack.Constants
import           Stack.Types
import           Stack.Types.Internal
import           System.Process.Run

type Build e m = (HasBuildConfig e, HasConfig e, HasEnvConfig e, HasTerminal e, MonadBaseControl IO m, MonadCatch m, MonadIO m, MonadLogger m, MonadReader e m)

type Assemble e m = (HasConfig e, HasTerminal e, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m, MonadReader e m)

-- | Stages the executables & additional content in a staging
-- directory under '.stack-work'
stageContainerImageArtifacts
    :: Build e m
    => Maybe (Path Abs Dir) -> m ()
stageContainerImageArtifacts mProjectRoot = do
    config <- asks getConfig
    forM_
        (zip [0 ..] (imgDockers $ configImage config))
        (\(idx,opts) ->
              do imageDir <-
                     imageStagingDir (fromMaybeProjectRoot mProjectRoot) idx
                 ignoringAbsence (removeDirRecur imageDir)
                 ensureDir imageDir
                 stageExesInDir opts imageDir
                 syncAddContentToDir opts imageDir)

-- | Builds a Docker (OpenContainer) image extending the `base` image
-- specified in the project's stack.yaml.  Then new image will be
-- extended with an ENTRYPOINT specified for each `entrypoint` listed
-- in the config file.
createContainerImageFromStage
    :: Assemble e m
    => Maybe (Path Abs Dir) -> [Text] -> m ()
createContainerImageFromStage mProjectRoot imageNames = do
    config <- asks getConfig
    forM_
        (zip
             [0 ..]
             (filterImages
                  (map T.unpack imageNames)
                  (imgDockers $ configImage config)))
        (\(idx,opts) ->
              do imageDir <-
                     imageStagingDir (fromMaybeProjectRoot mProjectRoot) idx
                 createDockerImage opts imageDir
                 extendDockerImageWithEntrypoint opts imageDir)
  where
    filterImages [] = id -- all: no filter
    filterImages names = filter (imageNameFound names . imgDockerImageName)
    imageNameFound names (Just name) = name `elem` names
    imageNameFound _ _ = False

-- | Stage all the Package executables in the usr/local/bin
-- subdirectory of a temp directory.
stageExesInDir
    :: Build e m
    => ImageDockerOpts -> Path Abs Dir -> m ()
stageExesInDir opts dir = do
    srcBinPath <- fmap (</> $(mkRelDir "bin")) installationRootLocal
    let destBinPath = dir </> $(mkRelDir "usr/local/bin")
    ensureDir destBinPath
    case imgDockerExecutables opts of
        Nothing -> copyDirRecur srcBinPath destBinPath
        Just exes ->
            forM_
                exes
                (\exe ->
                      do exeRelFile <- parseRelFile exe
                         copyFile
                             (srcBinPath </> exeRelFile)
                             (destBinPath </> exeRelFile))

-- | Add any additional files into the temp directory, respecting the
-- (Source, Destination) mapping.
syncAddContentToDir
    :: Build e m
    => ImageDockerOpts -> Path Abs Dir -> m ()
syncAddContentToDir opts dir = do
    bconfig <- asks getBuildConfig
    let imgAdd = imgDockerAdd opts
    forM_
        (Map.toList imgAdd)
        (\(source,dest) ->
              do sourcePath <- parseRelDir source
                 destPath <- parseAbsDir dest
                 let destFullPath = dir </> dropRoot destPath
                 ensureDir destFullPath
                 copyDirRecur (bcRoot bconfig </> sourcePath) destFullPath)

-- | Derive an image name from the project directory.
imageName
    :: Path Abs Dir -> String
imageName = map toLower . toFilePathNoTrailingSep . dirname

-- | Create a general purpose docker image from the temporary
-- directory of executables & static content.
createDockerImage
    :: Assemble e m
    => ImageDockerOpts -> Path Abs Dir -> m ()
createDockerImage dockerConfig dir = do
    menv <- getMinimalEnvOverride
    case imgDockerBase dockerConfig of
        Nothing -> throwM StackImageDockerBaseUnspecifiedException
        Just base -> do
            liftIO
                (writeFile
                     (toFilePath (dir </> $(mkRelFile "Dockerfile")))
                     (unlines ["FROM " ++ base, "ADD ./ /"]))
            let args =
                    [ "build"
                    , "-t"
                    , fromMaybe
                          (imageName (parent . parent . parent $ dir))
                          (imgDockerImageName dockerConfig)
                    , toFilePathNoTrailingSep dir]
            callProcess (Cmd Nothing "docker" menv args)

-- | Extend the general purpose docker image with entrypoints (if specified).
extendDockerImageWithEntrypoint
    :: Assemble e m
    => ImageDockerOpts -> Path Abs Dir -> m ()
extendDockerImageWithEntrypoint dockerConfig dir = do
    menv <- getMinimalEnvOverride
    let dockerImageName =
            fromMaybe
                (imageName (parent . parent . parent $ dir))
                (imgDockerImageName dockerConfig)
    let imgEntrypoints = imgDockerEntrypoints dockerConfig
    case imgEntrypoints of
        Nothing -> return ()
        Just eps ->
            forM_
                eps
                (\ep ->
                      do liftIO
                             (writeFile
                                  (toFilePath
                                       (dir </> $(mkRelFile "Dockerfile")))
                                  (unlines
                                       [ "FROM " ++ dockerImageName
                                       , "ENTRYPOINT [\"/usr/local/bin/" ++
                                         ep ++ "\"]"
                                       , "CMD []"]))
                         callProcess
                             (Cmd
                                  Nothing
                                  "docker"
                                  menv
                                  [ "build"
                                  , "-t"
                                  , dockerImageName ++ "-" ++ ep
                                  , toFilePathNoTrailingSep dir]))

-- | Fail with friendly error if project root not set.
fromMaybeProjectRoot :: Maybe (Path Abs Dir) -> Path Abs Dir
fromMaybeProjectRoot =
    fromMaybe (throw StackImageCannotDetermineProjectRootException)

-- | The command name for dealing with images.
imgCmdName
    :: String
imgCmdName = "image"

-- | The command name for building a docker container.
imgDockerCmdName
    :: String
imgDockerCmdName = "container"

-- | Convert image opts monoid to image options.
imgOptsFromMonoid
    :: ImageOptsMonoid -> ImageOpts
imgOptsFromMonoid ImageOptsMonoid{..} =
    ImageOpts
    { imgDockers = imgMonoidDockers
    }

-- | Stack image exceptions.
data StackImageException
    = StackImageDockerBaseUnspecifiedException  -- ^ Unspecified parent docker
                                                -- container makes building
                                                -- impossible
    | StackImageCannotDetermineProjectRootException  -- ^ Can't determine the
                                                     -- project root (where to
                                                     -- put image sandbox).
    deriving (Typeable)

instance Exception StackImageException

instance Show StackImageException where
    show StackImageDockerBaseUnspecifiedException =
        "You must specify a base docker image on which to place your haskell executables."
    show StackImageCannotDetermineProjectRootException =
        "Stack was unable to determine the project root in order to build a container."
