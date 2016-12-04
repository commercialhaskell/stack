{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handy path information.
module Stack.Path
    ( path
    , pathParser
    ) where

import           Control.Monad.Catch
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.List (intercalate)
import           Data.Maybe.Extra
import           Data.Monoid
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Lens.Micro (lens)
import qualified Options.Applicative as OA
import           Path
import           Path.Extra
import           Stack.Constants
import           Stack.GhcPkg as GhcPkg
import           Stack.Types.Config
import qualified System.FilePath as FP
import           System.IO (stderr)
import           System.Process.Read (EnvOverride(eoPath))

-- | Print out useful path information in a human-readable format (and
-- support others later).
path
    :: (MonadIO m, MonadBaseControl IO m, MonadReader env m, HasEnvConfig env,
        MonadCatch m, MonadLogger m)
    => [Text]
    -> m ()
path keys =
    do -- We must use a BuildConfig from an EnvConfig to ensure that it contains the
       -- full environment info including GHC paths etc.
       bcnl <- view $ envConfigL.buildConfigNoLocalL
       bcl <- view $ envConfigL.buildConfigLocalL
       -- This is the modified 'bin-path',
       -- including the local GHC or MSYS if not configured to operate on
       -- global GHC.
       -- It was set up in 'withBuildConfigAndLock -> withBuildConfigExt -> setupEnv'.
       -- So it's not the *minimal* override path.
       menv <- getMinimalEnvOverride
       snap <- packageDatabaseDeps
       plocal <- packageDatabaseLocal
       extra <- packageDatabaseExtra
       whichCompiler <- view $ actualCompilerVersionL.whichCompilerL
       global <- GhcPkg.getGlobalDB menv whichCompiler
       snaproot <- installationRootDeps
       localroot <- installationRootLocal
       distDir <- distRelativeDir
       hpcDir <- hpcReportDir
       compiler <- getCompilerPath whichCompiler
       let deprecated = filter ((`elem` keys) . fst) deprecatedPathKeys
       liftIO $ forM_ deprecated $ \(oldOption, newOption) -> T.hPutStrLn stderr $ T.unlines
           [ ""
           , "'--" <> oldOption <> "' will be removed in a future release."
           , "Please use '--" <> newOption <> "' instead."
           , ""
           ]
       forM_
           -- filter the chosen paths in flags (keys),
           -- or show all of them if no specific paths chosen.
           (filter
                (\(_,key,_) ->
                      (null keys && key /= T.pack deprecatedStackRootOptionName) || elem key keys)
                paths)
           (\(_,key,path') ->
                 liftIO $ T.putStrLn
                     -- If a single path type is requested, output it directly.
                     -- Otherwise, name all the paths.
                     ((if length keys == 1
                          then ""
                          else key <> ": ") <>
                      path'
                          (PathInfo
                               (BuildConfig bcnl bcl)
                               menv
                               snap
                               plocal
                               global
                               snaproot
                               localroot
                               distDir
                               hpcDir
                               extra
                               compiler)))

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
    { piBuildConfig  :: BuildConfig
    , piEnvOverride  :: EnvOverride
    , piSnapDb       :: Path Abs Dir
    , piLocalDb      :: Path Abs Dir
    , piGlobalDb     :: Path Abs Dir
    , piSnapRoot     :: Path Abs Dir
    , piLocalRoot    :: Path Abs Dir
    , piDistDir      :: Path Rel Dir
    , piHpcDir       :: Path Abs Dir
    , piExtraDbs     :: [Path Abs Dir]
    , piCompiler     :: Path Abs File
    }

instance HasPlatform PathInfo
instance HasConfig PathInfo
instance HasBuildConfigNoLocal PathInfo where
    buildConfigNoLocalL = lens piBuildConfig (\x y -> x { piBuildConfig = y })
                        . buildConfigNoLocalL
instance HasBuildConfig PathInfo where
    buildConfigLocalL = lens piBuildConfig (\x y -> x { piBuildConfig = y })
                      . buildConfigLocalL

-- | The paths of interest to a user. The first tuple string is used
-- for a description that the optparse flag uses, and the second
-- string as a machine-readable key and also for @--foo@ flags. The user
-- can choose a specific path to list like @--stack-root@. But
-- really it's mainly for the documentation aspect.
--
-- When printing output we generate @PathInfo@ and pass it to the
-- function to generate an appropriate string.  Trailing slashes are
-- removed, see #506
paths :: [(String, Text, PathInfo -> Text)]
paths =
    [ ( "Global stack root directory"
      , T.pack stackRootOptionName
      , view $ stackRootL.to toFilePathNoTrailingSep.to T.pack)
    , ( "Project root (derived from stack.yaml file)"
      , "project-root"
      , view $ projectRootL.to toFilePathNoTrailingSep.to T.pack)
    , ( "Configuration location (where the stack.yaml file is)"
      , "config-location"
      , view $ stackYamlL.to toFilePath.to T.pack)
    , ( "PATH environment variable"
      , "bin-path"
      , T.pack . intercalate [FP.searchPathSeparator] . eoPath . piEnvOverride )
    , ( "Install location for GHC and other core tools"
      , "programs"
      , view $ configL.to configLocalPrograms.to toFilePathNoTrailingSep.to T.pack)
    , ( "Compiler binary (e.g. ghc)"
      , "compiler-exe"
      , T.pack . toFilePath . piCompiler )
    , ( "Directory containing the compiler binary (e.g. ghc)"
      , "compiler-bin"
      , T.pack . toFilePathNoTrailingSep . parent . piCompiler )
    , ( "Local bin dir where stack installs executables (e.g. ~/.local/bin)"
      , "local-bin"
      , view $ configL.to configLocalBin.to toFilePathNoTrailingSep.to T.pack)
    , ( "Extra include directories"
      , "extra-include-dirs"
      , T.intercalate ", " . map (T.pack . toFilePathNoTrailingSep) . Set.elems . configExtraIncludeDirs . view configL )
    , ( "Extra library directories"
      , "extra-library-dirs"
      , T.intercalate ", " . map (T.pack . toFilePathNoTrailingSep) . Set.elems . configExtraLibDirs . view configL )
    , ( "Snapshot package database"
      , "snapshot-pkg-db"
      , T.pack . toFilePathNoTrailingSep . piSnapDb )
    , ( "Local project package database"
      , "local-pkg-db"
      , T.pack . toFilePathNoTrailingSep . piLocalDb )
    , ( "Global package database"
      , "global-pkg-db"
      , T.pack . toFilePathNoTrailingSep . piGlobalDb )
    , ( "GHC_PACKAGE_PATH environment variable"
      , "ghc-package-path"
      , \pi' -> mkGhcPackagePath True (piLocalDb pi') (piSnapDb pi') (piExtraDbs pi') (piGlobalDb pi'))
    , ( "Snapshot installation root"
      , "snapshot-install-root"
      , T.pack . toFilePathNoTrailingSep . piSnapRoot )
    , ( "Local project installation root"
      , "local-install-root"
      , T.pack . toFilePathNoTrailingSep . piLocalRoot )
    , ( "Snapshot documentation root"
      , "snapshot-doc-root"
      , \pi' -> T.pack (toFilePathNoTrailingSep (piSnapRoot pi' </> docDirSuffix)))
    , ( "Local project documentation root"
      , "local-doc-root"
      , \pi' -> T.pack (toFilePathNoTrailingSep (piLocalRoot pi' </> docDirSuffix)))
    , ( "Dist work directory, relative to package directory"
      , "dist-dir"
      , T.pack . toFilePathNoTrailingSep . piDistDir )
    , ( "Where HPC reports and tix files are stored"
      , "local-hpc-root"
      , T.pack . toFilePathNoTrailingSep . piHpcDir )
    , ( "DEPRECATED: Use '--local-bin' instead"
      , "local-bin-path"
      , T.pack . toFilePathNoTrailingSep . configLocalBin . view configL )
    , ( "DEPRECATED: Use '--programs' instead"
      , "ghc-paths"
      , T.pack . toFilePathNoTrailingSep . configLocalPrograms . view configL )
    , ( "DEPRECATED: Use '--" <> stackRootOptionName <> "' instead"
      , T.pack deprecatedStackRootOptionName
      , T.pack . toFilePathNoTrailingSep . view stackRootL )
    ]

deprecatedPathKeys :: [(Text, Text)]
deprecatedPathKeys =
    [ (T.pack deprecatedStackRootOptionName, T.pack stackRootOptionName)
    , ("ghc-paths", "programs")
    , ("local-bin-path", "local-bin")
    ]
