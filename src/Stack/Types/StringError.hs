{-# LANGUAGE MagicHash #-}

module Stack.Types.StringError where

import Control.Exception
import Control.Monad.Catch
import Data.Typeable
import GHC.Prim

newtype StringError = StringError String
    deriving (Typeable)

instance Exception StringError
instance Show StringError where show (StringError str) = str

throwString :: MonadThrow m => String -> m a
throwString = throwM . StringError

-- FIXME: actually need this?
stringError :: String -> a
stringError = raise# . toException . StringError
