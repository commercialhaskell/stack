{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#endif
{- |
Module      :  System.FilePath
Copyright   :  (c) Neil Mitchell 2005-2014
License     :  BSD3

Maintainer  :  ndmitchell@gmail.com
Stability   :  stable
Portability :  portable

A library for 'FilePath' manipulations, using Posix or Windows filepaths
depending on the platform.

Both "System.FilePath.Posix" and "System.FilePath.Windows" provide the
same interface. See either for examples and a list of the available
functions.
-}


#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
module System.FilePath(module System.FilePath.Windows) where
import System.FilePath.Windows
#else
module System.FilePath(module System.FilePath.Posix) where
import System.FilePath.Posix
#endif
