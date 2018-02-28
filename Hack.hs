{-# LANGUAGE DeriveGeneric #-}
module Main where-- Stack.Hack

import qualified Data.Text as Text
import Stack.Dot -- (listDependencies)
import Stack.Types.Config.Build(defaultBuildOptsCLI)
import Stack.Types.Config
import Stack.Options.GlobalParser (globalOptsFromMonoid)
import Stack.PackageIndex (getPackageCaches)
import Stack.Runners
import Stack.Setup
import RIO.Process
import RIO.Prelude
import RIO.Logger
import Data.Map (Map(..))
import qualified Options.Applicative as OA
import qualified Data.Text           as Text

doOpts = DotOpts True True Nothing mempty mempty mempty True True
ldoOpts = ListDepsOpts doOpts (Text.pack ",") False


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

hackParser :: OA.Parser CacheInfoMonoid
hackParser =   fmap (\flags -> CacheInfoMonoid (Any (any (=="cache-status") flags))) $
                mapMaybeA
                 (\(short,long) -> OA.flag Nothing
                                     (Just short)
                                     (OA.long long <> OA.help "opt"))
                 options

options = [("cache-status","cache-status")]

piInfo = OA.info hackParser mempty

main = do
     flags <- OA.execParser piInfo

     let go = (globalOptsFromMonoid False mempty)
                    { globalLogLevel = LevelDebug }
     void $ Stack.Runners.loadConfigWithOpts go $ \lc -> do
          Stack.Runners.withUserFileLock go (view stackRootL lc) $ \lk -> do
             munlockFile lk
             let getCompilerVersion = loadCompilerVersion go lc
             runRIO (lcConfig lc) $ do
                 pkgs <- getPackageCaches
                 liftIO (print pkgstus)
             liftIO (withBuildConfigDot (DotOpts True True Nothing mempty [] mempty  False False) go (listDependencies ldoOpts))
