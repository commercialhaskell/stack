module Stack.Prelude
  ( mapLeft
  , readMaybe
  , runConduitRes
  , withSystemTempDir
  , module X
  ) where

import Text.Read (readMaybe)
import UnliftIO as X
import Control.Monad.Catch as X (MonadThrow (..)) -- future consideration: move to explicit Either
import Data.Conduit as X (runConduit, (.|), ConduitM)
import Data.Void as X (Void, absurd)
import Path as X (Path, Abs, Rel, Dir, File)

import qualified Path.IO

mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft f (Left a1) = Left (f a1)
mapLeft _ (Right b) = Right b

runConduitRes :: MonadUnliftIO m => ConduitM () Void (ResourceT m) r -> m r
runConduitRes = runResourceT . runConduit

-- | Path version
withSystemTempDir :: MonadUnliftIO m => String -> (Path Abs Dir -> m a) -> m a
withSystemTempDir str inner = withRunInIO $ \run -> Path.IO.withSystemTempDir str $ run . inner
