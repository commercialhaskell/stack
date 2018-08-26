#!/usr/bin/env stack
-- stack --resolver lts-12.0 script
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import RIO
import qualified RIO.Map as Map
import Conduit
import Data.Yaml

main :: IO ()
main = runSimpleApp $ do
  m <- runConduitRes $ allFiles .| foldMC addFile mempty
  liftIO $ encodeFile "global-hints.yaml" m

allFiles =
  sourceDirectoryDeep True "stackage-snapshots/lts" *>
  sourceDirectoryDeep True "stackage-snapshots/nightly"

addFile m fp = do
  GlobalHints ghc packages <- liftIO $ decodeFileThrow fp
  evaluate $ Map.insert ghc
    (case Map.lookup ghc m of
      Nothing -> packages
      Just packages' -> Map.unionWith
        (\x y ->
           if x == y
             then x
             else error $ show (ghc, fp, x, y))
        packages
        packages') m

data GlobalHints = GlobalHints !Text !(Map Text Text)

instance FromJSON GlobalHints where
  parseJSON = withObject "GlobalHints" $ \o -> GlobalHints
    <$> o .: "compiler"
    <*> o .: "global-hints"
