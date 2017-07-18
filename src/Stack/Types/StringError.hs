module Stack.Types.StringError where

import Control.Monad.IO.Unlift
import Data.Typeable

newtype StringError = StringError String
    deriving (Typeable)

instance Exception StringError
instance Show StringError where show (StringError str) = str

throwString :: MonadThrow m => String -> m a
throwString = throwM . StringError

errorString :: String -> a
errorString = impureThrow . StringError
