{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Types.AddCommand
License     : BSD-3-Clause
-}

module Stack.Types.AddCommand
  ( AddCommand
  ) where

import           Control.Monad.Trans.Except ( ExceptT )
import           Control.Monad.Writer ( Writer )
import qualified Options.Applicative as OA
import           Stack.Prelude
import           Stack.Types.GlobalOptsMonoid ( GlobalOptsMonoid )
import           Stack.Types.Runner ( Runner )

-- | A type synonym for the monad used to add command line commands to Stack.
-- The monad is a stack of an 'ExceptT' @(@t'RIO' 'Runner' @())@ monad on top of
-- a 'Writer' @f@ monad, where @f@ is
-- 'Options.Applicative.Mod' 'Options.Applicative.CommandFields' @(@t'RIO' 'Runner' @(),@ 'GlobalOptsMonoid'@)@ - that
-- is, an option modifier for command options that have return type
-- @(@t'RIO' 'Runner' @(),@ 'GlobalOptsMonoid'@)@.
type AddCommand =
  ExceptT (RIO Runner ())
          (Writer (OA.Mod OA.CommandFields (RIO Runner (), GlobalOptsMonoid)))
          ()
