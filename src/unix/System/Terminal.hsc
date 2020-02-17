module System.Terminal
( fixCodePage
, getTerminalWidth
, hIsTerminalDeviceOrMinTTY
) where

import           RIO (MonadIO, Handle, hIsTerminalDevice)
import           Foreign
-- import           Foreign.C.Error
import           Foreign.C.Types
-- import           GHC.IO.FD (FD(FD, fdFD))
-- import           GHC.IO.Handle.Internals (withHandle_)
-- import           GHC.IO.Handle.Types (Handle, Handle__(Handle__, haDevice))
-- import           System.Posix.Types (Fd(Fd))

#include <sys/ioctl.h>
#include <unistd.h>


-- #let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
newtype WindowWidth = WindowWidth CUShort
    deriving (Eq, Ord, Show)

instance Storable WindowWidth where
  sizeOf _ = (#size struct winsize)
  alignment _ = (#alignment struct winsize)
  peek p = WindowWidth <$> (#peek struct winsize, ws_col) p
  poke p (WindowWidth w) = do
    (#poke struct winsize, ws_col) p w


-- fdSize :: Integral n => Fd -> IO (Maybe (Window n))
-- fdSize (Fd fd) = with (CWin 0 0) $ \ws -> do
--   throwErrnoIfMinus1 "ioctl" $
--     ioctl fd (#const TIOCGWINSZ) ws
--   CWin row col <- peek ws
--   return . Just $ Window (fromIntegral row) (fromIntegral col)
--  `catch`
--   handler
--  where
--   handler :: IOError -> IO (Maybe (Window h))
--   handler _ = return Nothing

foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr WindowWidth -> IO CInt

-- size :: Integral n => IO (Maybe (Window n))
-- size = fdSize (Fd (#const STDOUT_FILENO))

-- hSize :: Integral n => Handle -> IO (Maybe (Window n))
-- hSize h = withHandle_ "hSize" h $ \Handle__ { haDevice = dev } ->
--   case cast dev of
--     Nothing -> return Nothing
--     Just FD { fdFD = fd } -> fdSize (Fd fd)

getTerminalWidth :: IO (Maybe Int)
getTerminalWidth =
    alloca $ \p -> do	
        errno <- ioctl (#const STDOUT_FILENO) (#const TIOCGWINSZ) p	
        if errno < 0	
        then return Nothing	
        else do	
            WindowWidth w <- peek p	
            return . Just . fromIntegral $ w

fixCodePage :: x -> y -> a -> a
fixCodePage _ _ = id

-- | hIsTerminaDevice does not recognise handles to mintty terminals as terminal
-- devices, but isMinTTYHandle does.
hIsTerminalDeviceOrMinTTY :: MonadIO m => Handle -> m Bool
hIsTerminalDeviceOrMinTTY = hIsTerminalDevice
