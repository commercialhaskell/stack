module Path.Extended
  ( fileExtension
  , addExtension
  , replaceExtension
  ) where

import           Control.Monad.Catch
import qualified Path
import           Path (Path, File)

fileExtension :: MonadThrow m => Path b File -> m String
fileExtension = Path.fileExtension

addExtension ::
     MonadThrow m
  => String
  -> Path b File
  -> m (Path b File)
addExtension = Path.addExtension

replaceExtension ::
     MonadThrow m
  => String
  -> Path b File
  -> m (Path b File)
replaceExtension ext = Path.replaceExtension ('.' : ext)
