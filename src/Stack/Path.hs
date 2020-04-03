{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Handy path information.
module Stack.Path
    ( path
    , pathParser
    ) where

import           Stack.Prelude
import           Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Options.Applicative as OA
import           Path
import           Path.Extra
import           Stack.Constants
import           Stack.Constants.Config
import           Stack.GhcPkg as GhcPkg
import           Stack.Runners
import           Stack.Types.Config
import qualified System.FilePath as FP
import           RIO.PrettyPrint
import           RIO.Process (HasProcessContext (..), exeSearchPathL)

-- | Print out useful path information in a human-readable format (and
-- support others later).
path :: [Text] -> RIO Runner ()
path keys =
    do let deprecated = filter ((`elem` keys) . fst) deprecatedPathKeys
       forM_ deprecated $ \(oldOption, newOption) -> logWarn $
           "\n" <>
           "'--" <> display oldOption <> "' will be removed in a future release.\n" <>
           "Please use '--" <> display newOption <> "' instead.\n" <>
           "\n"
       let -- filter the chosen paths in flags (keys),
           -- or show all of them if no specific paths chosen.
           goodPaths = filter
                (\(_,key,_) ->
                      (null keys && key /= T.pack deprecatedStackRootOptionName) || elem key keys)
                paths
           singlePath = length goodPaths == 1
           toEither (_, k, UseHaddocks p) = Left (k, p)
           toEither (_, k, WithoutHaddocks p) = Right (k, p)
           (with, without) = partitionEithers $ map toEither goodPaths
           printKeys extractors single = do
             pathInfo <- fillPathInfo
             liftIO $ forM_ extractors $ \(key, extractPath) -> do
               let prefix = if single then "" else key <> ": "
               T.putStrLn $ prefix <> extractPath pathInfo
           runHaddock x = local
             (set (globalOptsL.globalOptsBuildOptsMonoidL.buildOptsMonoidHaddockL) (Just x)) .
             withConfig YesReexec . -- FIXME this matches previous behavior, but doesn't make a lot of sense
             withDefaultEnvConfig
       -- MSS 2019-03-17 Not a huge fan of rerunning withConfig and
       -- withDefaultEnvConfig each time, need to figure out what
       -- purpose is served and whether we can achieve it without two
       -- completely separate Config setups
       runHaddock True $ printKeys with singlePath
       runHaddock False $ printKeys without singlePath

fillPathInfo :: HasEnvConfig env => RIO env PathInfo
fillPathInfo = do
     -- We must use a BuildConfig from an EnvConfig to ensure that it contains the
     -- full environment info including GHC paths etc.
     piBuildConfig <- view $ envConfigL.buildConfigL
     -- This is the modified 'bin-path',
     -- including the local GHC or MSYS if not configured to operate on
     -- global GHC.
     -- It was set up in 'withBuildConfigAndLock -> withBuildConfigExt -> setupEnv'.
     -- So it's not the *minimal* override path.
     piSnapDb <- packageDatabaseDeps
     piLocalDb <- packageDatabaseLocal
     piExtraDbs <- packageDatabaseExtra
     piGlobalDb <- view $ compilerPathsL.to cpGlobalDB
     piSnapRoot <- installationRootDeps
     piLocalRoot <- installationRootLocal
     piToolsDir <- bindirCompilerTools
     piHoogleRoot <- hoogleRoot
     piDistDir <- distRelativeDir
     piHpcDir <- hpcReportDir
     piCompiler <- getCompilerPath
     return PathInfo {..}

pathParser :: OA.Parser [Text]
pathParser =
    mapMaybeA
        (\(desc,name,_) ->
             OA.flag Nothing
                     (Just name)
                     (OA.long (T.unpack name) <>
                      OA.help desc))
        paths

-- | Passed to all the path printers as a source of info.
data PathInfo = PathInfo
    { piBuildConfig  :: !BuildConfig
    , piSnapDb       :: !(Path Abs Dir)
    , piLocalDb      :: !(Path Abs Dir)
    , piGlobalDb     :: !(Path Abs Dir)
    , piSnapRoot     :: !(Path Abs Dir)
    , piLocalRoot    :: !(Path Abs Dir)
    , piToolsDir     :: !(Path Abs Dir)
    , piHoogleRoot   :: !(Path Abs Dir)
    , piDistDir      :: Path Rel Dir
    , piHpcDir       :: !(Path Abs Dir)
    , piExtraDbs     :: ![Path Abs Dir]
    , piCompiler     :: !(Path Abs File)
    }

instance HasPlatform PathInfo
instance HasLogFunc PathInfo where
    logFuncL = configL.logFuncL
instance HasRunner PathInfo where
    runnerL = configL.runnerL
instance HasStylesUpdate PathInfo where
  stylesUpdateL = runnerL.stylesUpdateL
instance HasTerm PathInfo where
  useColorL = runnerL.useColorL
  termWidthL = runnerL.termWidthL
instance HasGHCVariant PathInfo
instance HasConfig PathInfo
instance HasPantryConfig PathInfo where
    pantryConfigL = configL.pantryConfigL
instance HasProcessContext PathInfo where
    processContextL = configL.processContextL
instance HasBuildConfig PathInfo where
    buildConfigL = lens piBuildConfig (\x y -> x { piBuildConfig = y })
                 . buildConfigL

data UseHaddocks a = UseHaddocks a | WithoutHaddocks a

-- | The paths of interest to a user. The first tuple string is used
-- for a description that the optparse flag uses, and the second
-- string as a machine-readable key and also for @--foo@ flags. The user
-- can choose a specific path to list like @--stack-root@. But
-- really it's mainly for the documentation aspect.
--
-- When printing output we generate @PathInfo@ and pass it to the
-- function to generate an appropriate string.  Trailing slashes are
-- removed, see #506
paths :: [(String, Text, UseHaddocks (PathInfo -> Text))]
paths =
    [ ( "Global stack root directory"
      , T.pack stackRootOptionName
      , WithoutHaddocks $ view (stackRootL.to toFilePathNoTrailingSep.to T.pack))
    , ( "Project root (derived from stack.yaml file)"
      , "project-root"
      , WithoutHaddocks $ view (projectRootL.to toFilePathNoTrailingSep.to T.pack))
    , ( "Configuration location (where the stack.yaml file is)"
      , "config-location"
      , WithoutHaddocks $ view (stackYamlL.to toFilePath.to T.pack))
    , ( "PATH environment variable"
      , "bin-path"
      , WithoutHaddocks $ T.pack . intercalate [FP.searchPathSeparator] . view exeSearchPathL)
    , ( "Install location for GHC and other core tools"
      , "programs"
      , WithoutHaddocks $ view (configL.to configLocalPrograms.to toFilePathNoTrailingSep.to T.pack))
    , ( "Compiler binary (e.g. ghc)"
      , "compiler-exe"
      , WithoutHaddocks $ T.pack . toFilePath . piCompiler )
    , ( "Directory containing the compiler binary (e.g. ghc)"
      , "compiler-bin"
      , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . parent . piCompiler )
    , ( "Directory containing binaries specific to a particular compiler (e.g. intero)"
      , "compiler-tools-bin"
      , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . piToolsDir )
    , ( "Local bin dir where stack installs executables (e.g. ~/.local/bin)"
      , "local-bin"
      , WithoutHaddocks $ view $ configL.to configLocalBin.to toFilePathNoTrailingSep.to T.pack)
    , ( "Extra include directories"
      , "extra-include-dirs"
      , WithoutHaddocks $ T.intercalate ", " . map T.pack . configExtraIncludeDirs . view configL )
    , ( "Extra library directories"
      , "extra-library-dirs"
      , WithoutHaddocks $ T.intercalate ", " . map T.pack . configExtraLibDirs . view configL )
    , ( "Snapshot package database"
      , "snapshot-pkg-db"
      , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . piSnapDb )
    , ( "Local project package database"
      , "local-pkg-db"
      , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . piLocalDb )
    , ( "Global package database"
      , "global-pkg-db"
      , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . piGlobalDb )
    , ( "GHC_PACKAGE_PATH environment variable"
      , "ghc-package-path"
      , WithoutHaddocks $ \pi' -> mkGhcPackagePath True (piLocalDb pi') (piSnapDb pi') (piExtraDbs pi') (piGlobalDb pi'))
    , ( "Snapshot installation root"
      , "snapshot-install-root"
      , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . piSnapRoot )
    , ( "Local project installation root"
      , "local-install-root"
      , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . piLocalRoot )
    , ( "Snapshot documentation root"
      , "snapshot-doc-root"
      , UseHaddocks $ \pi' -> T.pack (toFilePathNoTrailingSep (piSnapRoot pi' </> docDirSuffix)))
    , ( "Local project documentation root"
      , "local-doc-root"
      , UseHaddocks $ \pi' -> T.pack (toFilePathNoTrailingSep (piLocalRoot pi' </> docDirSuffix)))
    , ( "Local project documentation root"
      , "local-hoogle-root"
      , UseHaddocks $ T.pack . toFilePathNoTrailingSep . piHoogleRoot)
    , ( "Dist work directory, relative to package directory"
      , "dist-dir"
      , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . piDistDir )
    , ( "Where HPC reports and tix files are stored"
      , "local-hpc-root"
      , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . piHpcDir )
    , ( "DEPRECATED: Use '--local-bin' instead"
      , "local-bin-path"
      , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . configLocalBin . view configL )
    , ( "DEPRECATED: Use '--programs' instead"
      , "ghc-paths"
      , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . configLocalPrograms . view configL )
    , ( "DEPRECATED: Use '--" <> stackRootOptionName <> "' instead"
      , T.pack deprecatedStackRootOptionName
      , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . view stackRootL )
    ]

deprecatedPathKeys :: [(Text, Text)]
deprecatedPathKeys =
    [ (T.pack deprecatedStackRootOptionName, T.pack stackRootOptionName)
    , ("ghc-paths", "programs")
    , ("local-bin-path", "local-bin")
    ]
