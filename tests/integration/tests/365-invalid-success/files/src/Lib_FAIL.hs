{-# LANGUAGE CPP #-}

-- | Version of Lib that does not compile.
module Lib where

-- Avoid problems with CPP and HLint
#ifndef __HLINT__

#error Not going to compile, sorry

#endif
