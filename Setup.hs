module Main
  ( main
  ) where

import           Data.List ( nub, sortOn )
import           Distribution.InstalledPackageInfo
                   ( installedUnitId, sourcePackageId )
import           Distribution.Package ( UnitId, packageName, packageVersion )
import           Distribution.PackageDescription
                   ( Executable (..), PackageDescription )
import           Distribution.Pretty ( prettyShow )
import           Distribution.Simple
                   ( UserHooks(..), defaultMainWithHooks, simpleUserHooks )
import           Distribution.Simple.BuildPaths ( autogenPackageModulesDir )
import           Distribution.Simple.LocalBuildInfo
                   ( ComponentLocalBuildInfo (..), LocalBuildInfo, installedPkgs
                   , withExeLBI, withLibLBI
                   )
import           Distribution.Simple.PackageIndex
                   ( allPackages, dependencyClosure )
import           Distribution.Simple.Setup
                   ( BuildFlags (..), ReplFlags (..), fromFlag )
import           Distribution.Simple.Utils
                   ( createDirectoryIfMissingVerbose, rewriteFileEx )
import           Distribution.Types.PackageName ( unPackageName )
import           Distribution.Types.UnqualComponentName
                   ( unUnqualComponentName )
import           Distribution.Utils.Path ( interpretSymbolicPathCWD )
import           Distribution.Verbosity ( Verbosity, normal )
import           System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
      generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
      buildHook simpleUserHooks pkg lbi hooks flags
    -- The 'cabal repl' hook corresponds to the 'cabal build' hook and is added
    -- because, with a Cabal-based cradle, Haskell Language Server makes use of
    -- 'cabal repl'.
  , replHook = \pkg lbi hooks flags args -> do
      generateBuildModule (fromFlag (replVerbosity flags)) pkg lbi
      replHook simpleUserHooks pkg lbi hooks flags args
  }

generateBuildModule ::
     Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = interpretSymbolicPathCWD (autogenPackageModulesDir lbi)
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    withExeLBI pkg lbi $ \exe clbi ->
      rewriteFileEx normal (dir </> "Build_" ++ exeName' exe ++ ".hs") $ unlines
        [ "module Build_" ++ exeName' exe
        , "  ( deps"
        , "  ) where"
        , ""
        , "deps :: [String]"
        , "deps = " ++ show (formatdeps (transDeps libcfg clbi))
        ]
  where
    exeName' = unUnqualComponentName . exeName
    formatdeps = map formatone . sortOn unPackageName'
    formatone p = unPackageName' p ++ "-" ++ prettyShow (packageVersion p)
    unPackageName' = unPackageName . packageName
    transDeps xs ys = either
      (map sourcePackageId . allPackages)
      handleDepClosureFailure $ dependencyClosure allInstPkgsIdx availInstPkgIds
     where
      allInstPkgsIdx = installedPkgs lbi
      allInstPkgIds = map installedUnitId $ allPackages allInstPkgsIdx
      -- instPkgIds includes `stack-X.X.X`, which is not a dependency hence is
      -- missing from allInstPkgsIdx. Filter that out.
      availInstPkgIds = filter (`elem` allInstPkgIds) $ testDeps xs ys
      handleDepClosureFailure unsatisfied =
        error $
             "Computation of transitive dependencies failed."
          ++ if null unsatisfied
               then ""
               else " Unresolved dependencies: " ++ show unsatisfied

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [UnitId]
testDeps xs ys =
  map fst $ nub $ componentPackageDeps xs ++ componentPackageDeps ys
