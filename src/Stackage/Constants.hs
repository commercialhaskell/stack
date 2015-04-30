{-# LANGUAGE OverloadedStrings #-}

-- | Constants used throughout the project.

module Stackage.Constants where

import Data.Text (Text)

-- | Extensions used for Haskell files.
haskellFileExts :: [Text]
haskellFileExts = ["hs","hsc","lhs"]
