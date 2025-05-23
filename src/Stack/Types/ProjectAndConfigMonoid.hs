{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Types.ProjectAndConfigMonoid
License     : BSD-3-Clause
-}

module Stack.Types.ProjectAndConfigMonoid
  ( ProjectAndConfigMonoid (..)
  , parseProjectAndConfigMonoid
  ) where

import           Data.Aeson.Types ( Value )
import           Data.Aeson.WarningParser
                   ( WithJSONWarnings, (...:), (..:?), (..!=), jsonSubWarnings
                   , jsonSubWarningsT, jsonSubWarningsTT, withObjectWarnings
                   )
import qualified Data.Set as Set
import qualified Data.Yaml as Yaml
import           Stack.Prelude
import           Stack.Types.ConfigMonoid
                   ( ConfigMonoid, parseConfigMonoidObject )
import           Stack.Types.Project ( Project (..) )

data ProjectAndConfigMonoid
  = ProjectAndConfigMonoid !Project !ConfigMonoid

parseProjectAndConfigMonoid ::
     Path Abs Dir
  -> Value
  -> Yaml.Parser (WithJSONWarnings (IO ProjectAndConfigMonoid))
parseProjectAndConfigMonoid rootDir =
  withObjectWarnings "ProjectAndConfigMonoid" $ \o -> do
    packages <- o ..:? "packages" ..!= [RelFilePath "."]
    deps <- jsonSubWarningsTT (o ..:? "extra-deps") ..!= []
    flags' <- o ..:? "flags" ..!= mempty
    let flagsByPkg = unCabalStringMap <$> unCabalStringMap
                (flags' :: Map (CabalString PackageName) (Map (CabalString FlagName) Bool))

    snapshot' <- jsonSubWarnings $ o ...: ["snapshot", "resolver"]
    compiler <- o ..:? "compiler"
    userMsg <- o ..:? "user-message"
    config <- parseConfigMonoidObject rootDir o
    extraPackageDBs <- o ..:? "extra-package-dbs" ..!= []
    curator <- jsonSubWarningsT (o ..:? "curator")
    drops <- o ..:? "drop-packages" ..!= mempty
    let dropPackages = Set.map unCabalString drops
    pure $ do
      deps' <- mapM (resolvePaths (Just rootDir)) deps
      let extraDeps =
            concatMap toList (deps' :: [NonEmpty RawPackageLocation])
      snapshot <- resolvePaths (Just rootDir) snapshot'
      let project = Project
            { userMsg
            , snapshot
            , compiler -- FIXME make sure snapshot' isn't SLCompiler
            , extraPackageDBs
            , packages
            , extraDeps
            , flagsByPkg
            , curator
            , dropPackages
            }
      pure $ ProjectAndConfigMonoid project config
