{-# LANGUAGE DeriveGeneric,RankNTypes #-}
module Main where-- Stack.Hack

import qualified Data.Text as Text
import Stack.Dot -- (listDependencies)
import Stack.Types.Config.Build(defaultBuildOptsCLI)
import Stack.Types.Config
import Stack.Types.PackageName
import Stack.Types.Version
import Stack.Config (getLocalPackages)
import Stack.Options.GlobalParser (globalOptsFromMonoid)
import Stack.PackageIndex (getPackageCaches, getPackageVersions)
import Stack.Runners
import Stack.Setup
import RIO.Process
import RIO.Prelude
import RIO.Logger
import Data.Map (Map(..))
import qualified Data.Map            as Map
import qualified Options.Applicative as OA
import qualified Data.Text           as Text

doOpts = DotOpts True True Nothing mempty mempty mempty True True
ldoOpts = ListDepsOpts doOpts (Text.pack "*--*") False


data CacheInfoMonoid = CacheInfoMonoid {
     cimWantCacheStatus :: !Any
} deriving (Generic)

data CacheInfo = CacheInfo {
     ciWantCacheStatus :: Bool
} deriving Show

instance Monoid CacheInfoMonoid where
         mempty = CacheInfoMonoid (Any False)
         mappend (CacheInfoMonoid a)
                 (CacheInfoMonoid b) = CacheInfoMonoid (mappend a b)

-- mapMaybeA :: Applicative f => (a -> f (Maybe b)) -> [a] -> f [b]
-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]

hackParser :: OA.Parser CacheInfoMonoid
hackParser =   fmap (\flags -> CacheInfoMonoid (Any (any (=="cache-status") flags))) $
                mapMaybeA
                 (\(short,long) -> OA.flag Nothing
                                     (Just short)
                                     (OA.long long <> OA.help "opt"))
                 options

options = [("cache-status","cache-status")]

-- Unsafe internal helper to create a package name
pkgName :: Text -> PackageName
pkgName = fromMaybe failure . parsePackageName
  where
   failure = error "Internal error during package name creation in DotSpec.pkgName"

piInfo = OA.info hackParser mempty

stackPkg = pkgName (Text.pack "stack")

main = do
     flags <- OA.execParser piInfo

     let go = (globalOptsFromMonoid False mempty)
                    { globalLogLevel = LevelDebug }

     void $ Stack.Runners.loadConfigWithOpts go $ \lc -> do
          Stack.Runners.withUserFileLock go (view stackRootL lc) $ \lk -> do
             munlockFile lk
             liftIO (print "YOOO")
             stackVersions <- runRIO (lcConfig lc) (getPackageVersions stackPkg)
             liftIO $ withBuildConfig go $
                    getLocalPackages & fmap lpProject & fmap Map.elems >>= \localPkgs ->
                    logDebug (Text.pack $ "cache space") *>
                    logDebug (Text.pack $ show $ length localPkgs) *>
                    logDebug (Text.pack $ show (lpvGPD <$> localPkgs))

-- compilation: stack ghc -- --make -isrc -isubs/rio/src -hide-package=cryptohash-0.11.9 -hide-package=rio Hack.hs
-- note: https://github.com/commercialhaskell/stack/issues/1220
