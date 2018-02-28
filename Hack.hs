{-# LANGUAGE DeriveGeneric #-}
module Main where-- Stack.Hack

import qualified Data.Text as Text
import Stack.Dot -- (listDependencies)
import Stack.Types.Config.Build(defaultBuildOptsCLI)
import Stack.Runners
import RIO.Process
import RIO.Prelude
import Data.Map (Map(..))
import qualified Options.Applicative as OA

doOpts = DotOpts True True Nothing mempty mempty mempty True True
ldoOpts = ListDepsOpts doOpts (Text.pack ",") False


data CacheInfoMonoid = CacheInfoMonoid {
     cimWantCacheStatus :: Any
} deriving (Generic,Show)

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
     -- deps <- runRIO $ listDependencies ldoOpts
     void $ withMiniConfigAndLock undefined $ do
          liftIO (return ())
