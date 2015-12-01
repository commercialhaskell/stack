{-# LANGUAGE FlexibleContexts #-}
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
import Prelude -- Fix redundant import warnings
import System.Directory
import System.Environment
import System.IO.Temp (withSystemTempDirectory)
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

main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll setup $ afterAll teardown $ do
    let logLevel = LevelDebug
    let loadConfig' m = runStackLoggingT m logLevel False False (loadConfig mempty Nothing Nothing)
    let loadBuildConfigRest m = runStackLoggingT m logLevel False False
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
        bconfig <- loadBuildConfigRest manager (lcLoadBuildConfig Nothing)
        runStackT manager logLevel bconfig False False $ do
            mbp <- loadMiniBuildPlan $ LTS 2 9
            eres <- try $ resolveBuildPlan
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

    describe "shadowMiniBuildPlan" $ do
        let version = $(mkVersion "1.0.0") -- unimportant for this test
            pn = either throw id . parsePackageNameFromString
            mkMPI deps = MiniPackageInfo
                { mpiVersion = version
                , mpiFlags = Map.empty
                , mpiPackageDeps = Set.fromList $ map pn $ words deps
                , mpiToolDeps = Set.empty
                , mpiExes = Set.empty
                , mpiHasLibrary = True
                }
            go x y = (pn x, mkMPI y)
            resourcet = go "resourcet" ""
            conduit = go "conduit" "resourcet"
            conduitExtra = go "conduit-extra" "conduit"
            text = go "text" ""
            attoparsec = go "attoparsec" "text"
            aeson = go "aeson" "text attoparsec"
            mkMBP pkgs = MiniBuildPlan
                { mbpCompilerVersion = GhcVersion version
                , mbpPackages = Map.fromList pkgs
                }
            mbpAll = mkMBP [resourcet, conduit, conduitExtra, text, attoparsec, aeson]
            test name input shadowed output extra =
                it name $ const $
                    shadowMiniBuildPlan input (Set.fromList $ map pn $ words shadowed)
                    `shouldBe` (output, Map.fromList extra)
        test "no shadowing" mbpAll "" mbpAll []
        test "shadow something that isn't there" mbpAll "does-not-exist" mbpAll []
        test "shadow a leaf" mbpAll "conduit-extra"
                (mkMBP [resourcet, conduit, text, attoparsec, aeson])
                []
        test "shadow direct dep" mbpAll "conduit"
                (mkMBP [resourcet, text, attoparsec, aeson])
                [conduitExtra]
        test "shadow deep dep" mbpAll "resourcet"
                (mkMBP [text, attoparsec, aeson])
                [conduit, conduitExtra]
        test "shadow deep dep and leaf" mbpAll "resourcet aeson"
                (mkMBP [text, attoparsec])
                [conduit, conduitExtra]
        test "shadow deep dep and direct dep" mbpAll "resourcet conduit"
                (mkMBP [text, attoparsec, aeson])
                [conduitExtra]
