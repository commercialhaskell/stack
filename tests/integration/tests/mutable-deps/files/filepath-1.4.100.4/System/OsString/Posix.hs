{- HLINT ignore -}
{-# LANGUAGE CPP #-}
#undef WINDOWS
#define MODULE_NAME     Posix
#define PLATFORM_STRING PosixString
#define PLATFORM_WORD   PosixChar
#define IS_WINDOWS      False
#include "Common.hs"
