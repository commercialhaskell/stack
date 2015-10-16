{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Constants used throughout the project.

module Stack.Constants
    (builtConfigFileFromDir
    ,builtFileFromDir
    ,configuredFileFromDir
    ,defaultShakeThreads
    ,distDirFromDir
    ,distRelativeDir
    ,haskellModuleExts
    ,imageStagingDir
    ,projectDockerSandboxDir
    ,rawGithubUrl
    ,stackDotYaml
    ,stackRootEnvVar
    ,userDocsDir
    ,configCacheFile
    ,configCabalMod
    ,buildCacheFile
    ,testSuccessFile
    ,testBuiltFile
    ,benchBuiltFile
    ,stackProgName
    ,wiredInPackages
    ,cabalPackageName
    ,implicitGlobalProjectDirDeprecated
    ,implicitGlobalProjectDir
    ,hpcRelativeDir
    ,hpcDirFromDir
    ,dotHpc
    ,objectInterfaceDir
    ,templatesDir
    ,defaultUserConfigPathDeprecated
    ,defaultUserConfigPath
    ,defaultGlobalConfigPathDeprecated
    ,defaultGlobalConfigPath
    )
    where

import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.Reader
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Text (Text)
import qualified Data.Text as T
import           Path as FL
import           Prelude
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

-- | The filename used for completed build indicators.
builtFileFromDir :: (MonadThrow m, MonadReader env m, HasPlatform env,HasEnvConfig env)
                 => Path Abs Dir
                 -> m (Path Abs File)
builtFileFromDir fp = do
  dist <- distDirFromDir fp
  return (dist </> $(mkRelFile "stack.gen"))

-- | The filename used for completed configure indicators.
configuredFileFromDir :: (MonadThrow m, MonadReader env m, HasPlatform env,HasEnvConfig env)
                      => Path Abs Dir
                      -> m (Path Abs File)
configuredFileFromDir fp = do
  dist <- distDirFromDir fp
  return (dist </> $(mkRelFile "setup-config"))

-- | The filename used for completed build indicators.
builtConfigFileFromDir :: (MonadThrow m, MonadReader env m, HasPlatform env,HasEnvConfig env)
                       => Path Abs Dir
                       -> m (Path Abs File)
builtConfigFileFromDir fp =
    liftM (fp </>) builtConfigRelativeFile

-- | Relative location of completed build indicators.
builtConfigRelativeFile :: (MonadThrow m, MonadReader env m, HasPlatform env,HasEnvConfig env)
                        => m (Path Rel File)
builtConfigRelativeFile = do
  dist <- distRelativeDir
  return (dist </> $(mkRelFile "stack.config"))

-- | Default shake thread count for parallel builds.
defaultShakeThreads :: Int
defaultShakeThreads = 4

-- -- | Hoogle database file.
-- hoogleDatabaseFile :: Path Abs Dir -> Path Abs File
-- hoogleDatabaseFile docLoc =
--   docLoc </>
--   $(mkRelFile "default.hoo")

-- -- | Extension for hoogle databases.
-- hoogleDbExtension :: String
-- hoogleDbExtension = "hoo"

-- -- | Extension of haddock files
-- haddockExtension :: String
-- haddockExtension = "haddock"

-- | User documentation directory.
userDocsDir :: Config -> Path Abs Dir
userDocsDir config = configStackRoot config </> $(mkRelDir "doc/")

-- | Output .o/.hi directory.
objectInterfaceDir :: BuildConfig -> Path Abs Dir
objectInterfaceDir bconfig = bcWorkDir bconfig </> $(mkRelDir "odir/")

-- | The filename used for dirtiness check of source files.
buildCacheFile :: (MonadThrow m, MonadReader env m, HasPlatform env,HasEnvConfig env)
               => Path Abs Dir      -- ^ Package directory.
               -> m (Path Abs File)
buildCacheFile dir = do
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

-- | The filename used to mark benchmarks as having built
benchBuiltFile :: (MonadThrow m, MonadReader env m, HasPlatform env,HasEnvConfig env)
               => Path Abs Dir -- ^ Package directory
               -> m (Path Abs File)
benchBuiltFile dir =
    liftM
        (</> $(mkRelFile "stack-bench-built"))
        (distDirFromDir dir)

-- | The filename used for dirtiness check of config.
configCacheFile :: (MonadThrow m, MonadReader env m, HasPlatform env,HasEnvConfig env)
                => Path Abs Dir      -- ^ Package directory.
                -> m (Path Abs File)
configCacheFile dir = do
    liftM
        (</> $(mkRelFile "stack-config-cache"))
        (distDirFromDir dir)

-- | The filename used for modification check of .cabal
configCabalMod :: (MonadThrow m, MonadReader env m, HasPlatform env,HasEnvConfig env)
               => Path Abs Dir      -- ^ Package directory.
               -> m (Path Abs File)
configCabalMod dir = do
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

-- | Directory for project templates.
templatesDir :: Config -> Path Abs Dir
templatesDir config = configStackRoot config </> $(mkRelDir "templates")

-- | Relative location of build artifacts.
distRelativeDir :: (MonadThrow m, MonadReader env m, HasPlatform env, HasEnvConfig env)
                => m (Path Rel Dir)
distRelativeDir = do
    cabalPkgVer <- asks (envConfigCabalVersion . getEnvConfig)
    platform <- platformVariantRelDir
    cabal <-
        parseRelDir $
        packageIdentifierString
            (PackageIdentifier cabalPackageName cabalPkgVer)
    platformAndCabal <- useShaPathOnWindows (platform </> cabal)
    return $
        workDirRel </>
        $(mkRelDir "dist") </>
        platformAndCabal

-- | Get a URL for a raw file on Github
rawGithubUrl :: Text -- ^ user/org name
             -> Text -- ^ repo name
             -> Text -- ^ branch name
             -> Text -- ^ filename
             -> Text
rawGithubUrl org repo branch file = T.concat
    [ "https://raw.githubusercontent.com/"
    , org
    , "/"
    , repo
    , "/"
    , branch
    , "/"
    , file
    ]

-- -- | Hoogle database file.
-- hoogleDatabaseFile :: Path Abs Dir -> Path Abs File
-- hoogleDatabaseFile docLoc =
--   docLoc </>
--   $(mkRelFile "default.hoo")

-- -- | Extension for hoogle databases.
-- hoogleDbExtension :: String
-- hoogleDbExtension = "hoo"

-- -- | Extension of haddock files
-- haddockExtension :: String
-- haddockExtension = "haddock"

-- | Docker sandbox from project root.
projectDockerSandboxDir :: Path Abs Dir -> Path Abs Dir
projectDockerSandboxDir projectRoot = projectRoot </> workDirRel </> $(mkRelDir "docker/")

-- | Image staging dir from project root.
imageStagingDir :: Path Abs Dir -> Path Abs Dir
imageStagingDir p = p </> workDirRel </> $(mkRelDir "image/")

-- | Name of the 'stack' program.
stackProgName :: String
stackProgName = "stack"

-- | The filename used for the stack config file.
stackDotYaml :: Path Rel File
stackDotYaml = $(mkRelFile "stack.yaml")

-- | Environment variable used to override the '~/.stack' location.
stackRootEnvVar :: String
stackRootEnvVar = "STACK_ROOT"

-- See https://downloads.haskell.org/~ghc/7.10.1/docs/html/libraries/ghc/src/Module.html#integerPackageKey
wiredInPackages :: HashSet PackageName
wiredInPackages =
    maybe (error "Parse error in wiredInPackages") HashSet.fromList mparsed
  where
    mparsed = sequence $ map parsePackageName
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

-- | Where .mix files go.
dotHpc :: Path Rel Dir
dotHpc = $(mkRelDir ".hpc")

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
