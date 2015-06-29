{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Stack.Dot where


import           Control.Monad (when)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger (MonadLogger, logInfo)
import           Control.Monad.Reader (MonadReader)
import qualified Data.Foldable as F
import           Data.Monoid ((<>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Stack.Build.Source
import           Stack.Build.Types
import           Stack.Package
import           Stack.Types

-- | Convert a package name to a graph node name.
nodeName :: PackageName -> T.Text
nodeName name = "\"" <> T.pack (packageNameString name) <> "\""

dot :: (MonadReader env m, HasBuildConfig env, MonadIO m, MonadLogger m, MonadCatch m,HasEnvConfig env)
    => m ()
dot = do
    (locals, _names, _idents) <- loadLocals
        BuildOpts
            { boptsTargets = []
            , boptsLibProfile = False
            , boptsExeProfile = False
            , boptsEnableOptimizations = Nothing
            , boptsHaddock = False
            , boptsHaddockDeps = Nothing
            , boptsFinalAction = DoNothing
            , boptsDryrun = False
            , boptsGhcOptions = []
            , boptsFlags = Map.empty
            , boptsInstallExes = False
            , boptsPreFetch = False
            , boptsTestArgs = []
            , boptsOnlySnapshot = False
            , boptsCoverage = False
            , boptsNoTests = False
            }
        Map.empty
    let localNames = Set.fromList $ map (packageName . lpPackage) locals

    $logInfo "digraph deps {"
    $logInfo "splines=polyline;"

    F.forM_ locals $ \lp -> do
        let deps = Set.intersection localNames $ packageAllDeps $ lpPackage lp
        F.forM_ deps $ \dep ->
            $logInfo $ T.concat
                [ nodeName $ packageName $ lpPackage lp
                , " -> "
                , nodeName dep
                , ";"
                ]
        when (Set.null deps) $
            $logInfo $ T.concat
                [ "{rank=max; "
                , nodeName $ packageName $ lpPackage lp
                , "}"
                ]

    $logInfo "}"
