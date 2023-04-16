{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP               #-}

#ifdef USE_GIT_INFO
{-# LANGUAGE TemplateHaskell   #-}
#endif

module Stack.Internal.BuildInfo
  ( maybeGitHash
  ) where

#ifdef USE_GIT_INFO
import           GitHash ( giHash, tGitInfoCwdTry )
#endif
import           Stack.Prelude

-- | If USE_GIT_INFO is enabled, the Git hash in the build directory, otherwise
-- Nothing.
maybeGitHash :: Maybe String
maybeGitHash =
#ifdef USE_GIT_INFO
  (either (const Nothing) (Just . giHash) $$tGitInfoCwdTry)
#else
  Nothing
#endif
