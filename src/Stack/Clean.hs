{-# LANGUAGE DeriveDataTypeable #-}

-- | Clean a project.
module Stack.Clean
    (clean
    ,CleanOpts(..)
    ,StackCleanException(..)
    ) where

import           Control.Exception (Exception)
import           Control.Monad.Catch (MonadThrow,throwM)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger (MonadLogger)
import           Control.Monad.Reader (MonadReader)
import           Data.Foldable (forM_)
import           Data.List ((\\),intercalate)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Typeable (Typeable)
import           Path.IO (removeTreeIfExists)
import           Stack.Build.Source (getLocalPackageViews)
import           Stack.Build.Target (LocalPackageView(..))
import           Stack.Constants (distDirFromDir)
import           Stack.Types (HasEnvConfig,PackageName)


-- | Reset the build, i.e. remove the @dist@ directory
-- (for example @.stack-work\/dist\/x84_64-linux\/Cabal-1.22.4.0@)
-- for all targets.
--
-- Throws 'StackCleanException'.
clean
    :: (MonadThrow m, MonadIO m, MonadReader env m, HasEnvConfig env, MonadLogger m)
    => CleanOpts
    -> m ()
clean (CleanOpts targets) = do
    locals <- getLocalPackageViews
    case targets \\ Map.keys locals of
        [] -> do
            let lpvs =
                    if null targets
                        then Map.elems locals -- default to cleaning all local packages
                        else mapMaybe (`Map.lookup` locals) targets
            forM_ lpvs $ \(LocalPackageView{lpvRoot = pkgDir},_) -> do
                distDir <- distDirFromDir pkgDir
                removeTreeIfExists distDir
        pkgs -> throwM (NonLocalPackages pkgs)

-- | Options for cleaning a project.
newtype CleanOpts = CleanOpts
    { cleanOptsTargets :: [PackageName]
    -- ^ Names of the packages to clean.
    -- If the list is empty, every local package should be cleaned.
    }

-- | Exceptions during cleanup.
newtype StackCleanException
    = NonLocalPackages [PackageName]
    deriving (Typeable)

instance Show StackCleanException where
    show (NonLocalPackages pkgs) =
        "The following packages are not part of this project: " ++
        intercalate ", " (map show pkgs)

instance Exception StackCleanException
