{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.AddCommand
  ( AddCommand
  ) where

import           Control.Monad.Trans.Except ( ExceptT )
import           Control.Monad.Writer ( Writer )
import qualified Options.Applicative as OA
import           Stack.Prelude
import           Stack.Types.GlobalOptsMonoid ( GlobalOptsMonoid )
import           Stack.Types.Runner ( Runner )

type AddCommand =
  ExceptT (RIO Runner ())
          (Writer (OA.Mod OA.CommandFields (RIO Runner (), GlobalOptsMonoid)))
          ()
