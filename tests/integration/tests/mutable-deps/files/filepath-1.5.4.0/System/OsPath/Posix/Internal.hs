{-# LANGUAGE CPP #-}

#undef WINDOWS
#define OS_PATH
#define IS_WINDOWS False
#define MODULE_NAME Posix

#include "../../FilePath/Internal.hs"
