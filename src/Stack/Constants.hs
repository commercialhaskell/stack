{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Constants used throughout the project.

module Stack.Constants
    (buildPlanDir
    ,distDirFromDir
    ,workDirFromDir
    ,distRelativeDir
    ,haskellModuleExts
    ,imageStagingDir
    ,projectDockerSandboxDir
    ,stackDotYaml
    ,stackRootEnvVar
    ,stackRootOptionName
    ,deprecatedStackRootOptionName
    ,inContainerEnvVar
    ,configCacheFile
    ,configCabalMod
    ,buildCacheFile
    ,testSuccessFile
    ,testBuiltFile
    ,stackProgName
    ,stackProgNameUpper
    ,wiredInPackages
    ,ghcjsBootPackages
    ,cabalPackageName
    ,implicitGlobalProjectDirDeprecated
    ,implicitGlobalProjectDir
    ,hpcRelativeDir
    ,hpcDirFromDir
    ,objectInterfaceDir
    ,templatesDir
    ,defaultUserConfigPathDeprecated
    ,defaultUserConfigPath
    ,defaultGlobalConfigPathDeprecated
    ,defaultGlobalConfigPath
    ,platformVariantEnvVar
    )
    where

import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.Reader
import           Data.Char (toUpper)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Text (Text)
import           Path as FL
import           Prelude
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName

-- | Extensions for anything that can be a Haskell module.
haskellModuleExts :: [Text]
haskellModuleExts = haskellFileExts ++ haskellPreprocessorExts

-- | Extensions used for Haskell modules. Excludes preprocessor ones.
haskellFileExts :: [Text]
haskellFileExts = ["hs", "hsc", "lhs"]

-- | Extensions for modules that are preprocessed by common preprocessors.
haskellPreprocessorExts :: [Text]
haskellPreprocessorExts = ["gc", "chs", "hsc", "x", "y", "ly", "cpphs"]

-- | Output .o/.hi directory.
objectInterfaceDir :: (MonadReader env m, HasConfig env)
  => BuildConfig -> m (Path Abs Dir)
objectInterfaceDir bconfig = do
  bcwd <- bcWorkDir bconfig
  return (bcwd </> $(mkRelDir "odir/"))

-- | The filename used for dirtiness check of source files.
buildCacheFile :: (MonadThrow m, MonadReader env m, HasPlatform env,HasEnvConfig env)
               => Path Abs Dir      -- ^ Package directory.
               -> m (Path Abs File)
buildCacheFile dir =
    liftM
        (</> $(mkRelFile "stack-build-cache"))
        (distDirFromDir dir)

-- | The filename used to mark tests as having succeeded
testSuccessFile :: (MonadThrow m, MonadReader env m, HasPlatform env,HasEnvConfig env)
                => Path Abs Dir -- ^ Package directory
                -> m (Path Abs File)
testSuccessFile dir =
    liftM
        (</> $(mkRelFile "stack-test-success"))
        (distDirFromDir dir)

-- | The filename used to mark tests as having built
testBuiltFile :: (MonadThrow m, MonadReader env m, HasPlatform env,HasEnvConfig env)
              => Path Abs Dir -- ^ Package directory
              -> m (Path Abs File)
testBuiltFile dir =
    liftM
        (</> $(mkRelFile "stack-test-built"))
        (distDirFromDir dir)

-- | The filename used for dirtiness check of config.
configCacheFile :: (MonadThrow m, MonadReader env m, HasPlatform env,HasEnvConfig env)
                => Path Abs Dir      -- ^ Package directory.
                -> m (Path Abs File)
configCacheFile dir =
    liftM
        (</> $(mkRelFile "stack-config-cache"))
        (distDirFromDir dir)

-- | The filename used for modification check of .cabal
configCabalMod :: (MonadThrow m, MonadReader env m, HasPlatform env,HasEnvConfig env)
               => Path Abs Dir      -- ^ Package directory.
               -> m (Path Abs File)
configCabalMod dir =
    liftM
        (</> $(mkRelFile "stack-cabal-mod"))
        (distDirFromDir dir)

-- | Directory for HPC work.
hpcDirFromDir
    :: (MonadThrow m, MonadReader env m, HasPlatform env, HasEnvConfig env)
    => Path Abs Dir  -- ^ Package directory.
    -> m (Path Abs Dir)
hpcDirFromDir fp =
    liftM (fp </>) hpcRelativeDir

-- | Relative location of directory for HPC work.
hpcRelativeDir :: (MonadThrow m, MonadReader env m, HasPlatform env, HasEnvConfig env)
               => m (Path Rel Dir)
hpcRelativeDir =
    liftM (</> $(mkRelDir "hpc")) distRelativeDir

-- | Package's build artifacts directory.
distDirFromDir :: (MonadThrow m, MonadReader env m, HasPlatform env, HasEnvConfig env)
               => Path Abs Dir
               -> m (Path Abs Dir)
distDirFromDir fp =
    liftM (fp </>) distRelativeDir

-- | Package's working directory.
workDirFromDir :: (MonadThrow m, MonadReader env m, HasPlatform env, HasEnvConfig env)
               => Path Abs Dir
               -> m (Path Abs Dir)
workDirFromDir fp =
    liftM (fp </>) getWorkDir

-- | Directory for project templates.
templatesDir :: Config -> Path Abs Dir
templatesDir config = configStackRoot config </> $(mkRelDir "templates")

-- | Relative location of build artifacts.
distRelativeDir :: (MonadThrow m, MonadReader env m, HasPlatform env, HasEnvConfig env)
                => m (Path Rel Dir)
distRelativeDir = do
    cabalPkgVer <- asks (envConfigCabalVersion . getEnvConfig)
    platform <- platformGhcRelDir
    wc <- getWhichCompiler
    -- Cabal version, suffixed with "_ghcjs" if we're using GHCJS.
    envDir <-
        parseRelDir $
        (if wc == Ghcjs then (++ "_ghcjs") else id) $
        packageIdentifierString $
        PackageIdentifier cabalPackageName cabalPkgVer
    platformAndCabal <- useShaPathOnWindows (platform </> envDir)
    workDir <- getWorkDir
    return $
        workDir </>
        $(mkRelDir "dist") </>
        platformAndCabal

-- | Docker sandbox from project root.
projectDockerSandboxDir :: (MonadReader env m, HasConfig env)
  => Path Abs Dir      -- ^ Project root
  -> m (Path Abs Dir)  -- ^ Docker sandbox
projectDockerSandboxDir projectRoot = do
  workDir <- getWorkDir
  return $ projectRoot </> workDir </> $(mkRelDir "docker/")

-- | Image staging dir from project root.
imageStagingDir :: (MonadReader env m, HasConfig env, MonadThrow m)
  => Path Abs Dir      -- ^ Project root
  -> Int               -- ^ Index of image
  -> m (Path Abs Dir)  -- ^ Docker sandbox
imageStagingDir projectRoot imageIdx = do
  workDir <- getWorkDir
  idxRelDir <- parseRelDir (show imageIdx)
  return $ projectRoot </> workDir </> $(mkRelDir "image") </> idxRelDir

-- | Name of the 'stack' program, uppercased
stackProgNameUpper :: String
stackProgNameUpper = map toUpper stackProgName

-- | Name of the 'stack' program.
stackProgName :: String
stackProgName = "stack"

-- | The filename used for the stack config file.
stackDotYaml :: Path Rel File
stackDotYaml = $(mkRelFile "stack.yaml")

-- | Environment variable used to override the '~/.stack' location.
stackRootEnvVar :: String
stackRootEnvVar = "STACK_ROOT"

-- | Option name for the global stack root.
stackRootOptionName :: String
stackRootOptionName = "stack-root"

-- | Deprecated option name for the global stack root.
--
-- Deprecated since stack-1.0.5.
--
-- TODO: Remove occurences of this variable and use 'stackRootOptionName' only
-- after an appropriate deprecation period.
deprecatedStackRootOptionName :: String
deprecatedStackRootOptionName = "global-stack-root"

-- | Environment variable used to indicate stack is running in container.
inContainerEnvVar :: String
inContainerEnvVar = stackProgNameUpper ++ "_IN_CONTAINER"

-- See https://downloads.haskell.org/~ghc/7.10.1/docs/html/libraries/ghc/src/Module.html#integerPackageKey
wiredInPackages :: HashSet PackageName
wiredInPackages =
    maybe (error "Parse error in wiredInPackages") HashSet.fromList mparsed
  where
    mparsed = mapM parsePackageName
      [ "ghc-prim"
      , "integer-gmp"
      , "integer-simple"
      , "base"
      , "rts"
      , "template-haskell"
      , "dph-seq"
      , "dph-par"
      , "ghc"
      , "interactive"
      ]

-- TODO: Get this unwieldy list out of here and into a datafile
-- generated by GHCJS! See https://github.com/ghcjs/ghcjs/issues/434
ghcjsBootPackages :: HashSet PackageName
ghcjsBootPackages =
    maybe (error "Parse error in ghcjsBootPackages") HashSet.fromList mparsed
  where
    mparsed = mapM parsePackageName
      -- stage1a
      [ "array"
      , "base"
      , "binary"
      , "bytestring"
      , "containers"
      , "deepseq"
      , "integer-gmp"
      , "pretty"
      , "primitive"
      , "integer-gmp"
      , "pretty"
      , "primitive"
      , "template-haskell"
      , "transformers"
      -- stage1b
      , "directory"
      , "filepath"
      , "old-locale"
      , "process"
      , "time"
      -- stage2
      , "async"
      , "aeson"
      , "attoparsec"
      , "case-insensitive"
      , "dlist"
      , "extensible-exceptions"
      , "hashable"
      , "mtl"
      , "old-time"
      , "parallel"
      , "scientific"
      , "stm"
      , "syb"
      , "text"
      , "unordered-containers"
      , "vector"
      ]

-- | Just to avoid repetition and magic strings.
cabalPackageName :: PackageName
cabalPackageName =
    $(mkPackageName "Cabal")

-- | Deprecated implicit global project directory used when outside of a project.
implicitGlobalProjectDirDeprecated :: Path Abs Dir -- ^ Stack root.
                                   -> Path Abs Dir
implicitGlobalProjectDirDeprecated p =
    p </>
    $(mkRelDir "global")

-- | Implicit global project directory used when outside of a project.
-- Normally, @getImplicitGlobalProjectDir@ should be used instead.
implicitGlobalProjectDir :: Path Abs Dir -- ^ Stack root.
                         -> Path Abs Dir
implicitGlobalProjectDir p =
    p </>
    $(mkRelDir "global-project")

-- | Deprecated default global config path.
defaultUserConfigPathDeprecated :: Path Abs Dir -> Path Abs File
defaultUserConfigPathDeprecated = (</> $(mkRelFile "stack.yaml"))

-- | Default global config path.
-- Normally, @getDefaultUserConfigPath@ should be used instead.
defaultUserConfigPath :: Path Abs Dir -> Path Abs File
defaultUserConfigPath = (</> $(mkRelFile "config.yaml"))

-- | Deprecated default global config path.
-- Note that this will be @Nothing@ on Windows, which is by design.
defaultGlobalConfigPathDeprecated :: Maybe (Path Abs File)
defaultGlobalConfigPathDeprecated = parseAbsFile "/etc/stack/config"

-- | Default global config path.
-- Normally, @getDefaultGlobalConfigPath@ should be used instead.
-- Note that this will be @Nothing@ on Windows, which is by design.
defaultGlobalConfigPath :: Maybe (Path Abs File)
defaultGlobalConfigPath = parseAbsFile "/etc/stack/config.yaml"

-- | Path where build plans are stored.
buildPlanDir :: Path Abs Dir -- ^ Stack root
             -> Path Abs Dir
buildPlanDir = (</> $(mkRelDir "build-plan"))

-- | Environment variable that stores a variant to append to platform-specific directory
-- names.  Used to ensure incompatible binaries aren't shared between Docker builds and host
platformVariantEnvVar :: String
platformVariantEnvVar = stackProgNameUpper ++ "_PLATFORM_VARIANT"
