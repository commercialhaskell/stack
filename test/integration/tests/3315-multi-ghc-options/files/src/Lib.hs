{-# LANGUAGE CPP #-}

module Lib where

#ifndef BAR
#error BAR isn't defined
#endif

#ifndef BAZ
#error BAZ isn't defined
#endif
