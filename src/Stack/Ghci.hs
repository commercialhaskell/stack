{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Run a GHCi configured with the user's project(s).

module Stack.Ghci where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Network.HTTP.Client.Conduit
import           Path
import           Path.IO
import           Stack.Build (build)
import           Stack.Build.Source
import           Stack.Exec
import           Stack.Package
import           Stack.Types
import           Stack.Build.Types
import           Stack.Types.Internal

-- | Launch a GHCi session for the given local project targets with the
-- given options and configure it with the load paths and extensions
-- of those targets.
ghci
    :: (HasConfig r, HasBuildConfig r, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m, MonadBaseControl IO m, HasTerminal r, HasHttpManager r, MonadMask m , HasLogLevel r)
    => [Text] -- ^ Targets.
    -> [String] -- ^ GHC options.
    -> FilePath
    -> Bool
    -> m ()
ghci targets useropts ghciPath noload = do
    pkgs <- ghciSetup targets
    let pkgopts = concatMap ghciPkgOpts pkgs
        srcfiles
          | noload = []
          | otherwise = concatMap (map toFilePath . ghciPkgModules) pkgs
    $logInfo
        ("Configuring GHCi with the following packages: " <>
         T.intercalate ", " (map packageNameText (map ghciPkgName pkgs)))
    exec
        defaultEnvSettings
        ghciPath
        ("--interactive" : pkgopts <> srcfiles <> useropts)

data GhciPkgInfo = GhciPkgInfo
  { ghciPkgName :: PackageName
  , ghciPkgOpts :: [String]
  , ghciPkgDir :: Path Abs Dir
  , ghciPkgModules :: [Path Abs File]
  }

ghciSetup :: (HasConfig r, HasBuildConfig r, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m, MonadBaseControl IO m, HasTerminal r, HasHttpManager r, MonadMask m, HasLogLevel r) => [Text] -> m [GhciPkgInfo]
ghciSetup targets = do
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig
    pwd <- getWorkingDir
    locals <-
        liftM catMaybes $
        forM (M.toList (envConfigPackages econfig)) $
        \(dir,validWanted) ->
             do cabalfp <- getCabalFileName dir
                name <- parsePackageNameFromFilePath cabalfp
                if validWanted && wanted pwd cabalfp name
                    then return (Just (name, cabalfp))
                    else return Nothing
    let findTarget x = find ((x ==) . packageNameText . fst) locals
        unmetTargets = filter (isNothing . findTarget) targets
    when (not (null unmetTargets)) $ throwM (TargetsNotFound unmetTargets)
    infos <-
        forM locals $
        \(name,cabalfp) ->
             do let config =
                        PackageConfig
                        { packageConfigEnableTests = True
                        , packageConfigEnableBenchmarks = True
                        , packageConfigFlags = localFlags mempty bconfig name
                        , packageConfigGhcVersion = envConfigGhcVersion econfig
                        , packageConfigPlatform = configPlatform
                              (getConfig bconfig)
                        }
                pkg <- readPackage config cabalfp
                pkgOpts <-
                    getPackageOpts (packageOpts pkg) (map fst locals) cabalfp
                srcfiles <- getPackageFiles (packageFiles pkg) Modules cabalfp
                return
                    GhciPkgInfo
                    { ghciPkgName = packageName pkg
                    , ghciPkgOpts = filter (not . badForGhci) pkgOpts
                    , ghciPkgDir = parent cabalfp
                    , ghciPkgModules = S.toList srcfiles
                    }
    let lock = Nothing -- I'm not sure this lock business is a good
                       -- idea, it complicates things.
    -- We build the necessary targets, including any dependencies,
    -- with tests and benchmarks enabled, before launching GHCi.
    build
        (const (return ()))
        lock
        (defaultBuildOpts
         { boptsTargets = targets
         , boptsEnableTests = True
         , boptsEnableBenchmarks = True
         } :: BuildOpts)
    return infos
  where
    wanted pwd cabalfp name = isInWantedList || targetsEmptyAndInDir
      where
        isInWantedList = elem (packageNameText name) targets
        targetsEmptyAndInDir = null targets || isParentOf (parent cabalfp) pwd
    badForGhci :: String -> Bool
    badForGhci x =
        isPrefixOf "-O" x || elem x (words "-debug -threaded -ticky")

data GhciSetupException =
    TargetsNotFound [Text]
    deriving Typeable

instance Exception GhciSetupException
instance Show GhciSetupException where
    show (TargetsNotFound targets) = unlines
        [ "Couldn't find targets: " ++ T.unpack (T.unwords targets)
        , "(expecting package names)"
        ]
