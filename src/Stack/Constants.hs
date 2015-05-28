{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Constants used throughout the project.

module Stack.Constants
    (haskellFileExts
    ,packageDownloadPrefix
    ,rawGithubUrl
    ,defaultShakeThreads)
    where

import Data.Text (Text)
import qualified Data.Text as T
import Path as FL
import Prelude

-- | Extensions used for Haskell files.
haskellFileExts :: [Text]
haskellFileExts = ["hs","hsc","lhs"]

-- | Default shake thread count for parallel builds.
defaultShakeThreads :: Int
defaultShakeThreads = 4

-- | URL prefix for downloading packages
packageDownloadPrefix :: Text
packageDownloadPrefix = "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"

-- | Get a URL for a raw file on Github
rawGithubUrl :: Text -- ^ user/org name
             -> Text -- ^ repo name
             -> Text -- ^ branch name
             -> Text -- ^ filename
             -> Text
rawGithubUrl org repo branch file = T.concat
    [ "https://raw.githubusercontent.com/"
    , org
    , "/"
    , repo
    , "/"
    , branch
    , "/"
    , file
    ]

-- -- | Hoogle database file.
-- hoogleDatabaseFile :: Path Abs Dir -> Path Abs File
-- hoogleDatabaseFile docLoc =
--   docLoc </>
--   $(mkRelFile "default.hoo")

-- -- | Extension for hoogle databases.
-- hoogleDbExtension :: String
-- hoogleDbExtension = "hoo"

-- -- | Extension of haddock files
-- haddockExtension :: String
-- haddockExtension = "haddock"
