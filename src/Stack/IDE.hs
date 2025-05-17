{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Stack.IDE
Description : Types and functions related to Stack's @ide@ command.
License     : BSD-3-Clause

Types and functions related to Stack's @ide@ command.
-}

module Stack.IDE
  ( idePackagesCmd
  , ideTargetsCmd
  , ideGhcOptionsCmd
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Tuple ( swap )
import           Stack.Build.FileTargets
                   ( findFileTargets, getAllLocalTargets, getAllNonLocalTargets
                   , getGhciPkgInfos, loadGhciPkgDescs, optsAndMacros
                   )
import           Stack.Build.Installed ( toInstallMap )
import           Stack.Build.Source ( localDependencies, projectLocalPackages )
import           Stack.Build.Target ( NeedTargets (..) )
import           Stack.Package ( topSortPackageComponent )
import           Path.Extra ( forgivingResolveFile' )
import           Stack.Prelude
import           Stack.Runners
                   ( ShouldReexec (..), withBuildConfig, withConfig
                   , withEnvConfig
                   )
import           Stack.Types.Build.FileTargets ( FileTarget (..), toTarget )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..) )
import           Stack.Types.BuildOpts ( BuildOpts (..) )
import qualified Stack.Types.BuildOpts as BenchmarkOpts ( BenchmarkOpts (..) )
import qualified Stack.Types.BuildOpts as TestOpts ( TestOpts (..) )
import           Stack.Types.BuildOptsCLI
                   ( BuildOptsCLI (..), defaultBuildOptsCLI )
import           Stack.Types.Config ( buildOptsL )
import           Stack.Types.EnvConfig ( EnvConfig (..), HasEnvConfig (..) )
import           Stack.Types.IdeOpts ( ListPackagesCmd (..), OutputStream (..) )
import           Stack.Types.NamedComponent
                   ( NamedComponent, isCBench, isCExe, isCSubLib, isCTest
                   , renderPkgComponent
                   )
import           Stack.Types.Package ( LocalPackage (..), Package (..) )
import           Stack.Types.Runner ( Runner )
import           Stack.Types.SourceMap
                   ( ProjectPackage (..), SMWanted (..), ppComponentsMaybe )
import           System.IO ( print, putStrLn )

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.IDE" module.
newtype IdePrettyException
  = FileTargetIsInvalidAbsFile String
  deriving (Show, Typeable)

instance Pretty IdePrettyException where
  pretty (FileTargetIsInvalidAbsFile name) =
       "[S-9208]"
    <> line
    <> fillSep
         [ flow "Cannot work out a valid path for file target"
         , style File (fromString name) <> "."
         ]

instance Exception IdePrettyException

-- | Function underlying the @stack ide packages@ command. List packages in the
-- project.
idePackagesCmd :: (OutputStream, ListPackagesCmd) -> RIO Runner ()
idePackagesCmd =
  withConfig NoReexec . withBuildConfig . uncurry listPackages

compTypes :: (Bool, Bool, Bool) -> NamedComponent -> Bool
compTypes (False, False, False) = const True
compTypes (exe, test, bench) =
  \x -> (exe && isCExe x) || (test && isCTest x) || (bench && isCBench x)

-- | Function underlying the @stack ide targets@ command. List targets in the
-- project.
ideTargetsCmd :: ((Bool, Bool, Bool), OutputStream)  -> RIO Runner ()
ideTargetsCmd = withConfig NoReexec .
  withBuildConfig . uncurry listTargets . fmap compTypes . swap

outputFunc :: HasTerm env => OutputStream -> String -> RIO env ()
outputFunc OutputLogInfo = prettyInfo . fromString
outputFunc OutputStdout  = liftIO . putStrLn

-- | List the packages inside the current project.
listPackages ::
     HasBuildConfig env
  => OutputStream
  -> ListPackagesCmd
  -> RIO env ()
listPackages stream flag = do
  packages <- view $ buildConfigL . to (.smWanted.project)
  let strs = case flag of
        ListPackageNames ->
          map packageNameString (Map.keys packages)
        ListPackageCabalFiles ->
          map (toFilePath . (.cabalFP)) (Map.elems packages)
  mapM_ (outputFunc stream) strs

-- | List the targets in the current project.
listTargets ::
     forall env. HasBuildConfig env
  => OutputStream
  -> (NamedComponent -> Bool)
  -> RIO env ()
listTargets stream isCompType = do
  packages <- view $ buildConfigL . to (.smWanted.project)
  pairs <- concat <$> Map.traverseWithKey toNameAndComponent packages
  outputFunc stream $ T.unpack $ T.intercalate "\n" $
    map renderPkgComponent pairs
 where
  toNameAndComponent ::
       PackageName
    -> ProjectPackage
    -> RIO env [(PackageName, NamedComponent)]
  toNameAndComponent pkgName' =
    fmap (map (pkgName',) . Set.toList) . ppComponentsMaybe (\x ->
      if isCompType x then Just x else Nothing)

-- | Function underlying the @stack ide ghc-options@ command.
ideGhcOptionsCmd :: Text -> RIO Runner ()
ideGhcOptionsCmd rawTarget =
  let boptsCLI = defaultBuildOptsCLI { initialBuildSteps = True }
  in  withConfig YesReexec $ withEnvConfig AllowNoTargets boptsCLI $ do
        bopts <- view buildOptsL
        -- override env so running of tests and benchmarks is disabled
        let boptsLocal = bopts
              { testOpts = bopts.testOpts { TestOpts.runTests = False }
              , benchmarkOpts =
                  bopts.benchmarkOpts { BenchmarkOpts.runBenchmarks = False }
              }
        local (set buildOptsL boptsLocal) (ideGhcOptions rawTarget)

ideGhcOptions :: HasEnvConfig env => Text -> RIO env ()
ideGhcOptions rawTarget = do
  sourceMap <- view $ envConfigL . to (.sourceMap)
  installMap <- toInstallMap sourceMap
  locals <- projectLocalPackages
  depLocals <- localDependencies
  let localMap = M.fromList [(lp.package.name, lp) | lp <- locals ++ depLocals]
  -- Parse to either file targets or build targets
  (inputTargets', mfileTargets) <- processRawTarget rawTarget >>= maybe
    (pure (mempty, Nothing))
    -- Figure out targets based on file target
    (findFileTargets locals . pure)
  let inputTargets = Map.map toTarget inputTargets'
  -- Get a list of all the local target packages.
  (directlyWanted, extraLoadDeps) <-
    getAllLocalTargets True inputTargets Nothing localMap
  -- Get a list of all the non-local target packages.
  nonLocalTargets <- getAllNonLocalTargets inputTargets
  let localTargets = directlyWanted <> extraLoadDeps
      getInternalDependencies target localPackage =
        topSortPackageComponent localPackage.package target False
      internalDependencies =
        M.intersectionWith getInternalDependencies inputTargets localMap
      relevantDependencies = M.filter (any isCSubLib) internalDependencies
  -- Load package descriptions.
  pkgDescs <- loadGhciPkgDescs mempty localTargets
  pkgs <- getGhciPkgInfos installMap [] (fmap fst mfileTargets) pkgDescs
  (omittedOpts, pkgopts, macros) <-
    optsAndMacros
      Nothing
      localTargets
      pkgs
      nonLocalTargets
      relevantDependencies
  let outputDivider = liftIO $ putStrLn "---"
  outputDivider
  mapM_ (liftIO . print) $
    concatMap (\(FileTarget t) -> concat $ Map.elems t) (Map.elems inputTargets')
  outputDivider
  mapM_ (liftIO . putStrLn) pkgopts
  outputDivider
  liftIO $ BS.putStr macros
  outputDivider
  mapM_ (liftIO . putStrLn) omittedOpts
  outputDivider

processRawTarget :: HasEnvConfig env => Text -> RIO env (Maybe (Path Abs File))
processRawTarget rawTarget =
  if ".hs" `T.isSuffixOf` rawTarget || ".lhs" `T.isSuffixOf` rawTarget
    then
      forgivingResolveFile' rawTarget' >>= maybe
          (prettyThrowM $ FileTargetIsInvalidAbsFile rawTarget')
          (pure . Just)
    else pure Nothing
 where
  rawTarget' = T.unpack rawTarget
