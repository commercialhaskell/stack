{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions related to Stack's @docker pull@ and @docker reset@ commands.
module Stack.DockerCmd
  ( dockerPullCmd
  , dockerResetCmd
  ) where

import           Stack.Docker ( preventInContainer, pull, reset )
import           Stack.Prelude
import           Stack.Runners ( ShouldReexec (..), withConfig )
import           Stack.Types.Config ( Runner )

-- | Function underlying the @stack docker pull@ command. Pull the current
-- Docker image.
dockerPullCmd :: () -> RIO Runner ()
dockerPullCmd () = withConfig NoReexec $ preventInContainer pull

-- | Function underlying the @stack docker reset@ command. Reset the Docker
-- sandbox.
dockerResetCmd ::
     Bool
     -- ^ Delete the sandbox's home directory?
  -> RIO Runner ()
dockerResetCmd = withConfig NoReexec . preventInContainer . reset
