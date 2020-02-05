{-# LANGUAGE CPP #-}

module Path.Extended 
 ( fileExtension
 , addExtension
 , replaceExtension
 ) where

import           Control.Monad.Catch
import qualified Path
import           Path (Path, File)

fileExtension :: MonadThrow m => Path b File -> m String
fileExtension = 
#if MIN_VERSION_path(0,7,0)
    Path.fileExtension
#else
    pure . Path.fileExtension
#endif

addExtension :: MonadThrow m
  => String
  -> Path b File
  -> m (Path b File)
addExtension =
#if MIN_VERSION_path(0,7,0)
    Path.addExtension
#else
    Path.addFileExtension
#endif

replaceExtension :: MonadThrow m
  => String
  -> Path b File
  -> m (Path b File)
replaceExtension =
#if MIN_VERSION_path(0,7,0)
    Path.replaceExtension
#else
    flip (Path.-<.>)
#endif
