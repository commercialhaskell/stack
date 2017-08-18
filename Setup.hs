{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Main (main) where

import Data.List ( nub, sortBy )
import Data.Ord ( comparing )
import Distribution.Package ( PackageId, InstalledPackageId, packageVersion, packageName )
import Distribution.PackageDescription ( PackageDescription(), Executable(..) )
import Distribution.InstalledPackageInfo (sourcePackageId, installedPackageId)
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.PackageIndex (allPackages, dependencyClosure)
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), fromFlag )
import Distribution.Simple.LocalBuildInfo ( installedPkgs, withLibLBI, withExeLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps) )
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Verbosity ( Verbosity )
import Distribution.Version ( showVersion )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    withExeLBI pkg lbi $ \exe clbi ->
      rewriteFile (dir </> "Build_" ++ exeName' exe ++ ".hs") $ unlines
        [ "module Build_" ++ exeName' exe ++ " where"
        , ""
        , "deps :: [String]"
        , "deps = " ++ (show $ formatdeps (transDeps libcfg clbi))
        ]
  where
    exeName' = unUnqualComponentName . exeName
    formatdeps = map formatone . sortBy (comparing unPackageName')
    formatone p = unPackageName' p ++ "-" ++ showVersion (packageVersion p)
    unPackageName' = unPackageName . packageName
    transDeps xs ys =
      either (map sourcePackageId . allPackages) handleDepClosureFailure $ dependencyClosure allInstPkgsIdx availInstPkgIds
      where
        allInstPkgsIdx = installedPkgs lbi
        allInstPkgIds = map installedPackageId $ allPackages allInstPkgsIdx
        -- instPkgIds includes `stack-X.X.X`, which is not a depedency hence is missing from allInstPkgsIdx. Filter that out.
        availInstPkgIds = filter (`elem` allInstPkgIds) $ testDeps xs ys
        handleDepClosureFailure unsatisfied =
          error $
            "Computation of transitive dependencies failed." ++
            if null unsatisfied then "" else " Unresolved dependencies: " ++ show unsatisfied

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [InstalledPackageId]
testDeps xs ys = map fst $ nub $ componentPackageDeps xs ++ componentPackageDeps ys
