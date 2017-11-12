{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack.Types.Image where

import Data.Aeson.Extended
import qualified Data.Map as Map
import Generics.Deriving.Monoid (mappenddefault, memptydefault)
import Path
import Stack.Prelude

-- | Image options. Currently only Docker image options.
newtype ImageOpts = ImageOpts
    { imgDockers :: [ImageDockerOpts]
      -- ^ One or more stanzas for docker image settings.
    } deriving (Show)

data ImageDockerOpts = ImageDockerOpts
    { imgDockerBase :: !(Maybe String)
      -- ^ Maybe have a docker base image name. (Although we will not
      -- be able to create any Docker images without this.)
    , imgDockerEntrypoints :: !(Maybe [String])
      -- ^ Maybe have a specific ENTRYPOINT list that will be used to
      -- create images.
    , imgDockerAdd :: !(Map FilePath (Path Abs Dir))
      -- ^ Maybe have some static project content to include in a
      -- specific directory in all the images.
    , imgDockerImageName :: !(Maybe String)
      -- ^ Maybe have a name for the image we are creating
    , imgDockerExecutables :: !(Maybe [Path Rel File])
      -- ^ Filenames of executables to add (if Nothing, add them all)
    } deriving (Show)

newtype ImageOptsMonoid = ImageOptsMonoid
    { imgMonoidDockers :: [ImageDockerOpts]
    } deriving (Show, Generic)

instance FromJSON (WithJSONWarnings ImageOptsMonoid) where
    parseJSON = withObjectWarnings
            "ImageOptsMonoid"
            (\o ->
                  do (oldDocker :: Maybe ImageDockerOpts) <- jsonSubWarningsT (o ..:? imgDockerOldArgName)
                     (dockers :: [ImageDockerOpts]) <- jsonSubWarningsT (o ..:? imgDockersArgName ..!= [])
                     let imgMonoidDockers = dockers ++ maybeToList oldDocker
                     return
                         ImageOptsMonoid
                         { ..
                         })

instance Monoid ImageOptsMonoid where
    mempty = memptydefault
    mappend = mappenddefault

instance FromJSON (WithJSONWarnings ImageDockerOpts) where
    parseJSON = withObjectWarnings
            "ImageDockerOpts"
            (\o ->
                  do imgDockerBase <- o ..:? imgDockerBaseArgName
                     imgDockerEntrypoints <- o ..:? imgDockerEntrypointsArgName
                     imgDockerAdd <- o ..:? imgDockerAddArgName ..!= Map.empty
                     imgDockerImageName <- o ..:? imgDockerImageNameArgName
                     imgDockerExecutables <- o ..:? imgDockerExecutablesArgName
                     return
                         ImageDockerOpts
                         { ..
                         })

imgArgName :: Text
imgArgName = "image"

-- Kept for backward compatibility
imgDockerOldArgName :: Text
imgDockerOldArgName = "container"

imgDockersArgName :: Text
imgDockersArgName = "containers"

imgDockerBaseArgName :: Text
imgDockerBaseArgName = "base"

imgDockerAddArgName :: Text
imgDockerAddArgName = "add"

imgDockerEntrypointsArgName :: Text
imgDockerEntrypointsArgName = "entrypoints"

imgDockerImageNameArgName :: Text
imgDockerImageNameArgName = "name"

imgDockerExecutablesArgName :: Text
imgDockerExecutablesArgName = "executables"
