{-# LANGUAGE CPP #-}

module Lib where

-- Avoid problems with CPP and HLint
#ifndef __HLINT__

#if !WORK
#error Not going to work, sorry
#endif

#endif
