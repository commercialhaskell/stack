{-# LANGUAGE CPP #-}

module Lib where

-- Avoid problems with CPP and HLint
#ifndef __HLINT__

#ifndef VARIABLE_A
#error VARIABLE_A isn't defined
#endif

#ifndef VARIABLE_B
#error VARIABLE_B isn't defined
#endif

#endif
