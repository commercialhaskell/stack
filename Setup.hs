{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Main (main) where

import Data.List ( nub, sortBy )
import Data.Ord ( comparing )
import Data.Version ( showVersion )
import Distribution.Package ( PackageName(PackageName), PackageId, InstalledPackageId, packageVersion, packageName )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..), Executable(..) )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), fromFlag )
import Distribution.Simple.LocalBuildInfo ( installedPkgs, withLibLBI, withTestLBI, withExeLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps) )
import Distribution.Verbosity ( Verbosity )

import qualified Distribution.InstalledPackageInfo  as InstalledPackageInfo
import qualified Distribution.Simple.PackageIndex   as Cabal

import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }

{-
-- From https://github.com/jaspervdj/cabal-dependency-licenses/blob/a7571b29b35af79fbd936d8436b75e2ab25a1e07/src/Main.hs#L40
#if MIN_VERSION_Cabal(1,24,0)
type PackageIndex a = Cabal.PackageIndex InstalledPackageInfo.InstalledPackageInfo
#elif MIN_VERSION_Cabal(1,22,0)
type PackageIndex a = Cabal.PackageIndex (InstalledPackageInfo.InstalledPackageInfo_ a)
#else
type PackageIndex a = Cabal.PackageIndex
#endif

findTransitiveDependencies
    :: PackageIndex a
    -> Set Cabal.InstalledPackageId
    -> Set Cabal.InstalledPackageId
findTransitiveDependencies pkgIdx set0 = go Set.empty (Set.toList set0)
  where
    go set []  = set
    go set (q : queue)
        | q `Set.member` set = go set queue
        | otherwise          =
            case Cabal.lookupInstalledPackageId pkgIdx q of
                Nothing  ->
                    -- Not found can mean that the package still needs to be
                    -- installed (e.g. a component of the target cabal package).
                    -- We can ignore those.
                    go set queue
                Just ipi ->
                    go (Set.insert q set)
                        (InstalledPackageInfo.depends ipi ++ queue)
-}

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    withTestLBI pkg lbi $ \suite clbi ->
      rewriteFile (dir </> "Build_" ++ testName suite ++ ".hs") $ unlines
        [ "module Build_" ++ testName suite ++ " where"
        , ""
        , "autogen_dir :: String"
        , "autogen_dir = " ++ show dir
        , ""
        , "deps :: [String]"
        , "deps = " ++ (show $ formatdeps (transDeps libcfg clbi))
        ]
    withExeLBI pkg lbi $ \exe clbi ->
      rewriteFile (dir </> "Build_" ++ exeName exe ++ ".hs") $ unlines
        [ "module Build_" ++ exeName exe ++ " where"
        , ""
        , "deps :: [String]"
        , "deps = " ++ (show $ formatdeps (transDeps libcfg clbi))
        ]
  where
    formatdeps = map formatone . sortBy (comparing unPackageName')
    formatone p = unPackageName' p ++ "-" ++ showVersion (packageVersion p)
    unPackageName' p = case packageName p of PackageName n -> n
    transDeps xs ys =
      --let res1 = map InstalledPackageInfo.sourcePackageId . Cabal.allPackages . installedPkgs $ lbi in
      either (map InstalledPackageInfo.sourcePackageId . Cabal.allPackages) handleDepClosureFailure $ Cabal.dependencyClosure allInstPkgsIdx $ availInstPkgIds
      where
        allInstPkgsIdx = installedPkgs lbi
        allInstPkgIds = map InstalledPackageInfo.installedPackageId . Cabal.allPackages $ allInstPkgsIdx
        -- instPkgIds includes `stack-X.X.X`, which is not a depedency hence is missing from allInstPkgsIdx. Filter that out.
        availInstPkgIds = filter (`elem` allInstPkgIds) instPkgIds
        (instPkgIds, _pkgIds) = unzip $ testDeps xs ys
        handleDepClosureFailure unsatisfied =
          error $
            "Computation of transitive dependencies failed." ++
            if null unsatisfied then "" else " Unresolved dependencies: " ++ show unsatisfied

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys
