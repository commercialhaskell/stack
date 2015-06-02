{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Stack.BuildPlanSpec where

import Stack.BuildPlan
import Control.Monad.Logger
import Control.Exception hiding (try)
import Control.Monad.Catch (try)
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import Network.HTTP.Conduit (Manager)
import System.Directory
import System.IO.Temp
import System.Environment
import Test.Hspec
import Stack.Config
import Stack.Types
import Stack.Types.StackT

data T = T
  { manager :: Manager
  }

setup :: IO T
setup = do
  manager <- newTLSManager
  unsetEnv "STACK_YAML"
  return T{..}

teardown :: T -> IO ()
teardown _ = return ()

spec :: Spec
spec = beforeAll setup $ afterAll teardown $ do
    let logLevel = LevelDebug
    let loadConfig' m = runStackLoggingT m logLevel (loadConfig mempty)
    let loadBuildConfigRest m = runStackLoggingT m logLevel
    let inTempDir action = do
            currentDirectory <- getCurrentDirectory
            withSystemTempDirectory "Stack_BuildPlanSpec" $ \tempDir -> do
                let enterDir = setCurrentDirectory tempDir
                let exitDir = setCurrentDirectory currentDirectory
                bracket_ enterDir exitDir action
    it "finds missing transitive dependencies #159" $ \T{..} -> inTempDir $ do
        -- Note: this test is somewhat fragile, depending on packages on
        -- Hackage remaining in a certain state. If it fails, confirm that
        -- github still depends on failure.
        writeFile "stack.yaml" "resolver: lts-2.9"
        LoadConfig{..} <- loadConfig' manager
        bconfig <- loadBuildConfigRest manager lcLoadBuildConfig
        runStackT manager logLevel bconfig $ do
            menv <- getMinimalEnvOverride
            mbp <- loadMiniBuildPlan $ LTS 2 9
            eres <- try $ resolveBuildPlan
                menv
                mbp
                (const False)
                (Map.fromList
                    [ ($(mkPackageName "github"), Set.empty)
                    ])
            case eres of
                Left (UnknownPackages _ unknown _) -> do
                    case Map.lookup $(mkPackageName "github") unknown of
                        Nothing -> error "doesn't list github as unknown"
                        Just _ -> return ()

                    {- Currently not implemented, see: https://github.com/fpco/stack/issues/159#issuecomment-107809418
                    case Map.lookup $(mkPackageName "failure") unknown of
                        Nothing -> error "failure not listed"
                        Just _ -> return ()
                    -}
                _ -> error $ "Unexpected result from resolveBuildPlan: " ++ show eres
            return ()
