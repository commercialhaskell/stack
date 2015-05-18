-- | Handling of environment variables, such as the PATH,
-- GHC_PACKAGE_SANDBOX, etc.
module Stackage.Environment where

import Control.Monad.Catch
import Control.Monad.Logger hiding (Loc)
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Text (Text)

import Stackage.Config


getEnvironmentVariables :: (MonadLogger m,MonadIO m,MonadThrow m)
                        => Config -> m (Map Text Text)
getEnvironmentVariables _ = throwM $ NotYetImplemented "getEnvironmentVariables"

getPATH :: (MonadLogger m,MonadIO m,MonadThrow m)
        => Config -> m Text
getPATH _ = throwM $ NotYetImplemented "getPATH"

withConfiguredEnvironment :: (MonadLogger m,MonadIO m,MonadThrow m)
                          => Config -> (m a -> m a) -> m a
withConfiguredEnvironment _ _ = throwM $ NotYetImplemented "withConfiguredEnvironment"
