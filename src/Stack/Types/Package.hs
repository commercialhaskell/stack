{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
-- |

module Stack.Types.Package where

import Control.Exception hiding (try,catch)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader
import Data.Data
import Data.Function
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Distribution.InstalledPackageInfo (PError)
import Distribution.Package hiding (Package,PackageName,packageName,packageVersion,PackageIdentifier)
import Distribution.System (Platform (..))
import Path as FL
import Prelude hiding (FilePath)
import Stack.Types

-- | All exceptions thrown by the library.
data PackageException
  = PackageInvalidCabalFile (Maybe (Path Abs File)) PError
  | PackageNoCabalFileFound (Path Abs Dir)
  | PackageMultipleCabalFilesFound (Path Abs Dir) [Path Abs File]
  | MismatchedCabalName (Path Abs File) PackageName
  deriving Typeable
instance Exception PackageException
instance Show PackageException where
    show (PackageInvalidCabalFile mfile err) =
        "Unable to parse cabal file" ++
        (case mfile of
            Nothing -> ""
            Just file -> ' ' : toFilePath file) ++
        ": " ++
        show err
    show (PackageNoCabalFileFound dir) =
        "No .cabal file found in directory " ++
        toFilePath dir
    show (PackageMultipleCabalFilesFound dir files) =
        "Multiple .cabal files found in directory " ++
        toFilePath dir ++
        ": " ++
        intercalate ", " (map (toFilePath . filename) files)
    show (MismatchedCabalName fp name) = concat
        [ "cabal file "
        , toFilePath fp
        , " has a mismatched package name: "
        , packageNameString name
        ]

-- | Some package info.
data Package =
  Package {packageName :: !PackageName                    -- ^ Name of the package.
          ,packageVersion :: !Version                     -- ^ Version of the package
          ,packageFiles :: !GetPackageFiles
          ,packageDeps :: !(Map PackageName VersionRange) -- ^ Packages that the package depends on.
          ,packageTools :: ![Dependency]                  -- ^ A build tool name.
          ,packageAllDeps :: !(Set PackageName)           -- ^ Original dependencies (not sieved).
          ,packageFlags :: !(Map FlagName Bool)           -- ^ Flags used on package.
          ,packageHasLibrary :: !Bool                     -- ^ does the package have a buildable library stanza?
          ,packageTests :: !(Set Text)                    -- ^ names of test suites
          ,packageBenchmarks :: !(Set Text)               -- ^ names of benchmarks
          ,packageExes :: !(Set Text)                     -- ^ names of executables
          ,packageOpts :: !GetPackageOpts                 -- ^ Args to pass to GHC.
          ,packageHasExposedModules :: !Bool              -- ^ Does the package have exposed modules?
          ,packageSimpleType :: !Bool                     -- ^ Does the package of build-type: Simple
          ,packageDefinedFlags :: !(Set FlagName)         -- ^ All flags defined in the .cabal file
          }
 deriving (Show,Typeable)

-- | Files that the package depends on, relative to package directory.
-- Argument is the location of the .cabal file
newtype GetPackageOpts = GetPackageOpts
    { getPackageOpts :: forall env m. (MonadIO m,HasEnvConfig env, HasPlatform env, MonadThrow m, MonadReader env m)
                     => [PackageName]
                     -> Path Abs File
                     -> m [String]
    }
instance Show GetPackageOpts where
    show _ = "<GetPackageOpts>"

-- | Files that the package depends on, relative to package directory.
-- Argument is the location of the .cabal file
newtype GetPackageFiles = GetPackageFiles
    { getPackageFiles :: forall m. (MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m)
                      => CabalFileType
                      -> Path Abs File
                      -> m (Set (Path Abs File))
    }
instance Show GetPackageFiles where
    show _ = "<GetPackageFiles>"

-- | Package build configuration
data PackageConfig =
  PackageConfig {packageConfigEnableTests :: !Bool        -- ^ Are tests enabled?
                ,packageConfigEnableBenchmarks :: !Bool   -- ^ Are benchmarks enabled?
                ,packageConfigFlags :: !(Map FlagName Bool)   -- ^ Package config flags.
                ,packageConfigGhcVersion :: !Version      -- ^ GHC version
                ,packageConfigPlatform :: !Platform       -- ^ host platform
                }
 deriving (Show,Typeable)

-- | Files to get for a cabal package.
data CabalFileType
    = AllFiles
    | Modules

-- | Compares the package name.
instance Ord Package where
  compare = on compare packageName

-- | Compares the package name.
instance Eq Package where
  (==) = on (==) packageName
