{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

-- | Run a GHCi configured with the user's package(s).

module Stack.Ghci (GhciOpts(..),GhciPkgInfo(..), ghciSetup, ghci) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Distribution.ModuleName (ModuleName)
import           Distribution.Text (display)
import           Network.HTTP.Client.Conduit
import           Path
import           Path.IO
import           Prelude
import           Stack.Build
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Build.Target
import           Stack.Constants
import           Stack.Exec
import           Stack.Package
import           Stack.Types
import           Stack.Types.Internal
import           System.Directory (getTemporaryDirectory)

-- | Command-line options for GHC.
data GhciOpts = GhciOpts
    {ghciTargets            :: ![Text]
    ,ghciArgs               :: ![String]
    ,ghciBuildGhcArgs       :: ![Text]
    ,ghciGhcCommand         :: !(Maybe FilePath)
    ,ghciNoLoadModules      :: !Bool
    ,ghciAdditionalPackages :: ![String]
    ,ghciMainIs             :: !(Maybe Text)
    } deriving (Show,Eq)

-- | Necessary information to load a package or its components.
data GhciPkgInfo = GhciPkgInfo
  { ghciPkgName :: PackageName
  , ghciPkgOpts :: [String]
  , ghciPkgDir :: Path Abs Dir
  , ghciPkgModules :: Set ModuleName
  , ghciPkgModFiles :: Set (Path Abs File) -- ^ Module file paths.
  , ghciPkgCFiles :: Set (Path Abs File) -- ^ C files.
  , ghciPkgMainIs :: Map NamedComponent (Set (Path Abs File))
  }

-- | Launch a GHCi session for the given local package targets with the
-- given options and configure it with the load paths and extensions
-- of those targets.
ghci
    :: (HasConfig r, HasBuildConfig r, HasHttpManager r, MonadMask m, HasLogLevel r, HasTerminal r, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m, MonadBaseControl IO m)
    => GhciOpts -> m ()
ghci GhciOpts{..} = do
    (targets,mainIsTargets,pkgs) <- ghciSetup ghciMainIs ghciTargets ghciBuildGhcArgs
    bconfig <- asks getBuildConfig
    mainFile <- figureOutMainFile mainIsTargets targets pkgs
    wc <- getWhichCompiler
    let pkgopts = concatMap ghciPkgOpts pkgs
        modulesToLoad
          | ghciNoLoadModules = []
          | otherwise =
              nub
                  (maybe [] (return . toFilePath) mainFile <>
                   concatMap (map display . S.toList . ghciPkgModules) pkgs)
        odir =
            [ "-odir=" <> toFilePath (objectInterfaceDir bconfig)
            , "-hidir=" <> toFilePath (objectInterfaceDir bconfig)]
    $logInfo
        ("Configuring GHCi with the following packages: " <>
         T.intercalate ", " (map (packageNameText . ghciPkgName) pkgs))
    tmp <- liftIO getTemporaryDirectory
    withCanonicalizedTempDirectory
        tmp
        "ghci-script"
        (\tmpDir ->
              do let scriptPath = tmpDir </> $(mkRelFile "ghci-script")
                     fp = toFilePath scriptPath
                     loadModules = ":l " <> unwords modulesToLoad
                     bringIntoScope = ":m + " <> unwords modulesToLoad
                 liftIO (writeFile fp (unlines [loadModules,bringIntoScope]))
                 finally (exec
                              defaultEnvSettings
                              (fromMaybe (compilerExeName wc) ghciGhcCommand)
                              ("--interactive" :
                               odir <> pkgopts <> ghciArgs <>
                               ["-ghci-script=" <> fp]))
                         (removeFile scriptPath))

-- | Figure out the main-is file to load based on the targets. Sometimes there
-- is none, sometimes it's unambiguous, sometimes it's
-- ambiguous. Warns and returns nothing if it's ambiguous.
figureOutMainFile
    :: (Monad m, MonadLogger m)
    => Maybe (Map PackageName SimpleTarget)
    -> Map PackageName SimpleTarget
    -> [GhciPkgInfo]
    -> m (Maybe (Path Abs File))
figureOutMainFile mainIsTargets targets0 packages =
    case candidates of
        [] -> return Nothing
        [c@(_,_,fp)] -> do $logInfo ("Using main module: " <> renderCandidate c)
                           return (Just fp)
        candidate:_ -> do
            let border = $logWarn "* * * * * * * *"
            border
            $logWarn "The main module to load is ambiguous. Candidates are: "
            forM_ (map renderCandidate candidates) $logWarn
            $logWarn
                "None will be loaded. You can specify which one to pick by: "
            $logWarn
                (" 1) Specifying targets to stack ghci e.g. stack ghci " <>
                 sampleTargetArg candidate)
            $logWarn
                (" 2) Specifying what the main is e.g. stack ghci " <>
                 sampleMainIsArg candidate)
            border
            return Nothing
  where
    targets = fromMaybe targets0 mainIsTargets
    candidates = do
        pkg <- packages
        case M.lookup (ghciPkgName pkg) targets of
            Nothing -> []
            Just target -> do
                (component,mains) <-
                    M.toList
                        (M.filterWithKey wantedComponent (ghciPkgMainIs pkg))
                main <- S.toList mains
                return (ghciPkgName pkg, component, main)
                where wantedComponent namedC _ =
                          case target of
                              STLocalAll -> True
                              STLocalComps cs -> S.member namedC cs
                              _ -> False
    renderCandidate (pkgName,namedComponent,mainIs) =
        "Package `" <> packageNameText pkgName <> "' component " <>
        renderComp namedComponent <>
        " with main-is file: " <>
        T.pack (toFilePath mainIs)
    renderComp c =
        case c of
            CLib -> "lib"
            CExe name -> "exe:" <> name
            CTest name -> "test:" <> name
            CBench name -> "bench:" <> name
    sampleTargetArg (pkg,comp,_) =
        packageNameText pkg <> ":" <> renderComp comp
    sampleMainIsArg (pkg,comp,_) =
        "--main-is " <> packageNameText pkg <> ":" <> renderComp comp

-- | Create a list of infos for each target containing necessary
-- information to load that package/components.
ghciSetup
    :: (HasConfig r, HasHttpManager r, HasBuildConfig r, MonadMask m, HasTerminal r, HasLogLevel r, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m, MonadBaseControl IO m)
    => Maybe Text
    -> [Text]
    -> [Text]
    -> m (Map PackageName SimpleTarget, Maybe (Map PackageName SimpleTarget), [GhciPkgInfo])
ghciSetup mainIs stringTargets buildGhcOpts = do
    (_,_,targets) <-
        parseTargetsFromBuildOpts
            AllowNoTargets
            defaultBuildOpts
            { boptsTargets = stringTargets
            , boptsGhcOptions = buildGhcOpts
            }
    mainIsTargets <-
        case mainIs of
            Nothing -> return Nothing
            Just target -> do
                (_,_,targets') <-
                    parseTargetsFromBuildOpts
                        AllowNoTargets
                        defaultBuildOpts
                        { boptsTargets = [target]
                        }
                return (Just targets')
    let bopts = makeBuildOpts targets
    econfig <- asks getEnvConfig
    (realTargets,_,_,_,sourceMap) <- loadSourceMap AllowNoTargets bopts
    menv <- getMinimalEnvOverride
    (installedMap, _, _, _) <- getInstalled
        menv
        GetInstalledOpts
            { getInstalledProfiling = False
            , getInstalledHaddock   = False
            }
        sourceMap
    locals <-
        liftM catMaybes $
        forM (M.toList (envConfigPackages econfig)) $
        \(dir,validWanted) ->
             do cabalfp <- getCabalFileName dir
                name <- parsePackageNameFromFilePath cabalfp
                if validWanted
                    then case M.lookup name targets of
                             Just simpleTargets ->
                                 return (Just (name, (cabalfp, simpleTargets)))
                             Nothing -> return Nothing
                    else return Nothing
    let localLibs = [name | (name, (_, target)) <- locals, hasLocalComp isCLib target]
    infos <-
        forM locals $
        \(name,(cabalfp,component)) ->
             makeGhciPkgInfo sourceMap installedMap localLibs name cabalfp component
    unless (M.null realTargets) (build (const (return ())) Nothing bopts)
    return (realTargets, mainIsTargets, infos)
  where
    makeBuildOpts targets =
        base
        { boptsTargets = stringTargets
        , boptsTests = any (hasLocalComp isCTest) elems
        , boptsBenchmarks = any (hasLocalComp isCBench) elems
        , boptsTestOpts = (boptsTestOpts base)
          { toDisableRun = True
          , toRerunTests = False
          }
        , boptsBenchmarkOpts = (boptsBenchmarkOpts base)
          { beoDisableRun = True
          }
        , boptsBuildSubset = BSOnlyDependencies
        , boptsGhcOptions = buildGhcOpts
        }
      where
        base = defaultBuildOpts
        elems = M.elems targets
    hasLocalComp p t =
        case t of
            STLocalComps s -> any p (S.toList s)
            STLocalAll -> True
            _ -> False
    isCLib nc =
        case nc of
            CLib{} -> True
            _ -> False
    isCTest nc =
        case nc of
            CTest{} -> True
            _ -> False
    isCBench nc =
        case nc of
            CBench{} -> True
            _ -> False

-- | Make information necessary to load the given package in GHCi.
makeGhciPkgInfo
    :: (MonadReader r m, HasEnvConfig r, MonadLogger m, MonadIO m, MonadCatch m)
    => SourceMap
    -> InstalledMap
    -> [PackageName]
    -> PackageName
    -> Path Abs File
    -> SimpleTarget
    -> m GhciPkgInfo
makeGhciPkgInfo sourceMap installedMap locals name cabalfp component = do
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig
    let config =
            PackageConfig
            { packageConfigEnableTests = True
            , packageConfigEnableBenchmarks = True
            , packageConfigFlags = localFlags mempty bconfig name
            , packageConfigCompilerVersion = envConfigCompilerVersion econfig
            , packageConfigPlatform = configPlatform (getConfig bconfig)
            }
    (warnings,pkg) <- readPackage config cabalfp
    mapM_ (printCabalFileWarning cabalfp) warnings
    (componentsModules,componentFiles,componentsOpts,generalOpts) <-
        getPackageOpts (packageOpts pkg) sourceMap installedMap locals cabalfp
    let filterWithinWantedComponents m =
            M.elems
                (M.filterWithKey
                     (\k _ ->
                           case component of
                               STLocalComps cs -> S.member k cs
                               _ -> True)
                     m)
        filteredOptions =
            nub (map
                     (\x ->
                           if badForGhci x
                               then Left x
                               else Right x)
                     (generalOpts <>
                      concat (filterWithinWantedComponents componentsOpts)))
    case lefts filteredOptions of
        [] -> return ()
        options ->
            $logWarn
                ("The following GHC options are incompatible with GHCi and have not been passed to it: " <>
                 T.unwords (map T.pack options))
    return
        GhciPkgInfo
        { ghciPkgName = packageName pkg
        , ghciPkgOpts = rights filteredOptions
        , ghciPkgDir = parent cabalfp
        , ghciPkgModules = mconcat
              (filterWithinWantedComponents componentsModules)
        , ghciPkgModFiles = mconcat
              (filterWithinWantedComponents
                   (M.map (setMapMaybe dotCabalModulePath) componentFiles))
        , ghciPkgMainIs = M.map (setMapMaybe dotCabalMainPath) componentFiles
        , ghciPkgCFiles = mconcat
              (filterWithinWantedComponents
                   (M.map (setMapMaybe dotCabalCFilePath) componentFiles))
        }
  where
    badForGhci :: String -> Bool
    badForGhci x =
        isPrefixOf "-O" x || elem x (words "-debug -threaded -ticky -static")
    setMapMaybe f = S.fromList . mapMaybe f . S.toList
