{-# LANGUAGE OverloadedStrings #-}

-- | See https://github.com/well-typed/hooks-build-type. As part of their work,
-- Well-Typed reviewed stack-2.13.1 and identified that it used a pre-build hook
-- to generate, for the stack main library component, a module that lists all
-- the dependencies of stack (both library and executable), which is used in
-- `Stack.BuildInfo` to be listed. They also wrote an experimental patch, the
-- source code of which is below (with some reformatting).
--
-- This would be used if Stack's build type was 'Hooks' rather than 'Custom'.

module SetupHooks
  ( setupHooks
  ) where

import           Data.List ( nub, sortBy )
import           Data.Ord ( comparing )
import           Distribution.InstalledPackageInfo
                   ( installedUnitId, sourcePackageId )
import           Distribution.Package
                   ( PackageId, UnitId, packageName, packageVersion )
import           Distribution.PackageDescription
                   ( PackageDescription (..), Executable (..), componentNameRaw
                   )
import           Distribution.Pretty ( prettyShow )
import           Distribution.Simple
                   ( UserHooks(..), defaultMainWithHooks, simpleUserHooks )
import           Distribution.Simple.BuildPaths ( autogenComponentModulesDir )
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PackageIndex
                   ( allPackages, dependencyClosure )
import           Distribution.Simple.Setup ( BuildFlags (..), fromFlag )
import           Distribution.Simple.SetupHooks
import           Distribution.Simple.Utils
                   ( createDirectoryIfMissingVerbose, rewriteFileEx )
import           Distribution.Types.PackageName ( PackageName, unPackageName )
import           Distribution.Types.UnqualComponentName
                   ( unUnqualComponentName )
import           Distribution.Verbosity ( Verbosity, normal )
import           System.FilePath ( (</>) )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
       noBuildHooks
         { preBuildComponentHook = Just preBuildHook }
    }

preBuildHook :: BuildingWhat -> LocalBuildInfo -> TargetInfo -> IO ()
preBuildHook flags lbi tgt
  | CLibName LMainLibName <- componentName $ targetComponent tgt =
      generateBuildModule (buildingWhatVerbosity flags) (localPkgDescr lbi)
        lbi tgt
  | otherwise = pure ()

generateBuildModule ::
     Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> TargetInfo
  -> IO ()
generateBuildModule verbosity pkg lbi mainLibTargetInfo = do
  -- Generate a module in the stack library component that lists all the
  -- dependencies of stack (both the library and the executable).
  createDirectoryIfMissingVerbose verbosity True autogenDir
  withExeLBI pkg lbi $ \ _ exeCLBI -> do
    rewriteFileEx normal buildModulePath $ unlines
      [ "module Build_" ++ pkgNm
      , "  ( deps"
      , "  ) where"
      , ""
      , "deps :: [String]"
      , "deps = " ++ (show $ formatdeps (transDeps mainLibCLBI exeCLBI))
      ]
  where
    mainLibCLBI = targetCLBI mainLibTargetInfo
    autogenDir = autogenComponentModulesDir lbi mainLibCLBI
    pkgNm :: String
    pkgNm = unPackageName' $ package pkg
    buildModulePath = autogenDir </> "Build_" ++ pkgNm ++ ".hs"
    formatdeps = map formatone . sortBy (comparing unPackageName')
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
