{-# LANGUAGE CPP #-}

module Lib where

-- Avoid problems with CPP and HLint
#ifndef __HLINT__

#ifndef BAR
#error BAR isn't defined
#endif

#ifndef BAZ
#error BAZ isn't defined
#endif

#endif
