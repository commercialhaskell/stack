{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions related to Stack's @update@ command.
module Stack.Update
  ( updateCmd
  ) where

import          Stack.Prelude
import          Stack.Runners ( ShouldReexec (..), withConfig )
import          Stack.Types.Runner ( Runner )

-- | Function underlying the @stack update@ command. Update the package index.
updateCmd :: () -> RIO Runner ()
updateCmd () = withConfig NoReexec (void (updateHackageIndex Nothing))
