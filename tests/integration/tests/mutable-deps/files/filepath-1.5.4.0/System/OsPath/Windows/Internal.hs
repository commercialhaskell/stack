{-# LANGUAGE CPP #-}

#undef  POSIX
#define WINDOWS
#define OS_PATH
#define IS_WINDOWS True
#define MODULE_NAME Windows

#include "../../FilePath/Internal.hs"
