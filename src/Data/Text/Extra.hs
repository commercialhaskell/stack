{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Extra where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

-- | Strip trailing carriage return from Text
stripCR :: T.Text -> T.Text
stripCR t = fromMaybe t (T.stripSuffix "\r" t)
