{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Freeze
    ( freeze
    , FreezeOpts (..)
    , FreezeMode (..)
    ) where

import qualified Data.Yaml as Yaml
import qualified RIO.ByteString as B
import           Stack.Prelude
import           Stack.Types.BuildPlan
import           Stack.Types.Config

data FreezeMode = FreezeProject | FreezeSnapshot

data FreezeOpts = FreezeOpts
    { freezeMode :: FreezeMode
    }

freeze :: HasEnvConfig env => FreezeOpts -> RIO env ()
freeze (FreezeOpts FreezeProject) = do
  mproject <- view $ configL.to configMaybeProject
  case mproject of
    Just (p, _) -> do
      let deps = projectDependencies p
          resolver = projectResolver p
          completePackageLocation' pl =
            case pl of
              PLImmutable pli -> PLImmutable <$> completePackageLocation pli
              plm@(PLMutable _) -> pure plm
      resolver' <- completeSnapshotLocation resolver
      deps' <- mapM completePackageLocation' deps
      when (deps' /= deps || resolver' /= resolver) $
        liftIO $ B.putStr $ Yaml.encode p{ projectDependencies = deps'
                                         , projectResolver = resolver'
                                         }
    Nothing -> pure ()

freeze (FreezeOpts FreezeSnapshot) = do
  msnapshot <- view $ buildConfigL.to bcSnapshotDef.to sdSnapshot
  case msnapshot of
    Just (snap, _) -> do
      snap' <- completeSnapshot snap
      when (snap' /= snap) $
        liftIO $ B.putStr $ Yaml.encode snap'
    Nothing ->
      return ()
