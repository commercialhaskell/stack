{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Types and functions related to Stack's @ide@ command.
module Stack.IDE
  ( OutputStream (..)
  , ListPackagesCmd (..)
  , idePackagesCmd
  , ideTargetsCmd
  , listPackages
  , listTargets
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Stack.Prelude
import           Stack.Runners
                   ( ShouldReexec (..), withBuildConfig, withConfig )
import           Stack.Types.Config
                   ( BuildConfig (..), HasBuildConfig (..), ppComponents )
import           Stack.Types.NamedComponent
                   ( NamedComponent, renderPkgComponent )
import           Stack.Types.Runner ( Runner )
import           Stack.Types.SourceMap ( ProjectPackage (..), SMWanted (..) )
import           System.IO ( putStrLn )

-- Type representing output channel choices for the @stack ide packages@ and
-- @stack ide targets@ commands.
data OutputStream
  = OutputLogInfo
    -- ^ To the same output channel as other log information.
  | OutputStdout
    -- ^ To the standard output channel.

-- Type representing output choices for the @stack ide packages@ command.
data ListPackagesCmd
  = ListPackageNames
    -- ^ Package names.
  | ListPackageCabalFiles
    -- ^ Paths to Cabal files.

-- | Function underlying the @stack ide packages@ command. List packages in the
-- project.
idePackagesCmd :: (OutputStream, ListPackagesCmd) -> RIO Runner ()
idePackagesCmd =
  withConfig NoReexec . withBuildConfig . uncurry listPackages

-- | Function underlying the @stack ide targets@ command. List targets in the
-- project.
ideTargetsCmd :: OutputStream -> RIO Runner ()
ideTargetsCmd = withConfig NoReexec . withBuildConfig . listTargets

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
  packages <- view $ buildConfigL.to (smwProject . bcSMWanted)
  let strs = case flag of
        ListPackageNames ->
          map packageNameString (Map.keys packages)
        ListPackageCabalFiles ->
          map (toFilePath . ppCabalFP) (Map.elems packages)
  mapM_ (outputFunc stream) strs

-- | List the targets in the current project.
listTargets :: forall env. HasBuildConfig env => OutputStream -> RIO env ()
listTargets stream = do
  packages <- view $ buildConfigL.to (smwProject . bcSMWanted)
  pairs <- concat <$> Map.traverseWithKey toNameAndComponent packages
  outputFunc stream $ T.unpack $ T.intercalate "\n" $
    map renderPkgComponent pairs
 where
  toNameAndComponent ::
       PackageName
    -> ProjectPackage
    -> RIO env [(PackageName, NamedComponent)]
  toNameAndComponent pkgName' =
    fmap (map (pkgName', ) . Set.toList) . ppComponents
