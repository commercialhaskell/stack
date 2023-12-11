module Codec.Archive.Tar.Utf8
  ( module Codec.Archive.Tar
  , entryPath
  , unpack
  ) where

-- | A module that is equivalent to "Codec.Archive.Tar" from the @tar@ package,
-- except that @unpack@ assumes that the file paths in an archive are UTF8
-- encoded.

import           Codec.Archive.Tar hiding ( entryPath, unpack )
import           Codec.Archive.Tar.Check ( checkSecurity )
import           Codec.Archive.Tar.Entry ( Entry (..), TarPath, fromLinkTarget )
import qualified Codec.Archive.Tar.Entry as Tar
import           Control.Exception ( Exception, catch, throwIO )
import           Data.Bits ( (.|.), (.&.), shiftL )
import qualified Data.ByteString.Lazy as LBS
import           Data.Char ( chr, ord )
import           Data.Int ( Int64 )
import           Data.Maybe ( fromMaybe )
import           Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import           System.Directory
                   ( copyFile, createDirectoryIfMissing, setModificationTime )
import           System.FilePath ( (</>) )
import qualified System.FilePath as FP
import           System.IO.Error ( isPermissionError )

type EpochTime = Int64

-- | Native 'FilePath' of the file or directory within the archive.
--
-- Assumes that the 'TarPath' of an 'Entry' is UTF8 encoded.
entryPath :: Entry -> FilePath
entryPath = fromTarPath . entryTarPath

-- | Convert a 'TarPath' to a native 'FilePath'.
--
-- The native 'FilePath' will use the native directory separator but it is not
-- otherwise checked for validity or sanity. In particular:
--
-- * The tar path may be invalid as a native path, eg the file name @\"nul\"@
--   is not valid on Windows.
--
-- * The tar path may be an absolute path or may contain @\"..\"@ components.
--   For security reasons this should not usually be allowed, but it is your
--   responsibility to check for these conditions (eg using 'checkSecurity').
--
-- Assumes that the 'TarPath' is UTF8 encoded.
fromTarPath :: TarPath -> FilePath
fromTarPath tp = decodeIfUtf8Encoded $ Tar.fromTarPath tp

-- | Create local files and directories based on the entries of a tar archive.
--
-- This is a portable implementation of unpacking suitable for portable
-- archives. It handles 'NormalFile' and 'Directory' entries and has simulated
-- support for 'SymbolicLink' and 'HardLink' entries. Links are implemented by
-- copying the target file. This therefore works on Windows as well as Unix.
-- All other entry types are ignored, that is they are not unpacked and no
-- exception is raised.
--
-- If the 'Entries' ends in an error then it is raised an an exception. Any
-- files or directories that have been unpacked before the error was
-- encountered will not be deleted. For this reason you may want to unpack
-- into an empty directory so that you can easily clean up if unpacking fails
-- part-way.
--
-- On its own, this function only checks for security (using 'checkSecurity').
-- You can do other checks by applying checking functions to the 'Entries' that
-- you pass to this function. For example:
--
-- > unpack dir (checkTarbomb expectedDir entries)
--
-- If you care about the priority of the reported errors then you may want to
-- use 'checkSecurity' before 'checkTarbomb' or other checks.
--
-- Assumes that the 'TarPath' of an `Entry` is UTF8 encoded.
unpack :: Exception e => FilePath -> Entries e -> IO ()
unpack baseDir entries = unpackEntries [] (checkSecurity entries)
                     >>= emulateLinks

  where
    -- We're relying here on 'checkSecurity' to make sure we're not scribbling
    -- files all over the place.

    unpackEntries _     (Fail err)      = either throwIO throwIO err
    unpackEntries links Done            = return links
    unpackEntries links (Next entry es) = case entryContent entry of
      NormalFile file _ -> extractFile path file mtime
                        >> unpackEntries links es
      Directory         -> extractDir path mtime
                        >> unpackEntries links es
      HardLink     link -> (unpackEntries $! saveLink path link links) es
      SymbolicLink link -> (unpackEntries $! saveLink path link links) es
      _                 -> unpackEntries links es --ignore other file types
      where
        path  = entryPath entry
        mtime = entryTime entry

    extractFile path content mtime = do
      -- Note that tar archives do not make sure each directory is created
      -- before files they contain, indeed we may have to create several
      -- levels of directory.
      createDirectoryIfMissing True absDir
      LBS.writeFile absPath content
      setModTime absPath mtime
      where
        absDir  = baseDir </> FP.takeDirectory path
        absPath = baseDir </> path

    extractDir path mtime = do
      createDirectoryIfMissing True absPath
      setModTime absPath mtime
      where
        absPath = baseDir </> path

    saveLink path link links = seq (length path)
                             $ seq (length link')
                             $ (path, link'):links
      where link' = fromLinkTarget link

    emulateLinks = mapM_ $ \(relPath, relLinkTarget) ->
      let absPath   = baseDir </> relPath
          absTarget = FP.takeDirectory absPath </> relLinkTarget
       in copyFile absTarget absPath

setModTime :: FilePath -> EpochTime -> IO ()
setModTime path t =
    setModificationTime path (posixSecondsToUTCTime (fromIntegral t))
      `catch` \e ->
        if isPermissionError e then return () else throwIO e

-- | If the given 'String' can be interpreted as a string of bytes that encodes
-- a string using UTF8, then yields the string decoded, otherwise yields the
-- given 'String'.

-- Inspired by the utf8-string package.
decodeIfUtf8Encoded :: String -> String
decodeIfUtf8Encoded s = fromMaybe s $ decode s
 where
  decode :: String -> Maybe String
  decode [] = Just ""
  decode (c:cs)
    | c' < 0x80  = decode' c cs
    | c' < 0xc0  = Nothing
    | c' < 0xe0  = multi1
    | c' < 0xf0  = multiByte 2 0b1111 0x00000800
    | c' < 0xf8  = multiByte 3 0b0111 0x00010000
    | c' < 0xfc  = multiByte 4 0b0011 0x00200000
    | c' < 0xfe  = multiByte 5 0b0001 0x04000000
    | otherwise = Nothing
   where
    c' = ord c
    isValidByte b = b <= 0xff && b .&. 0b11000000 == 0b10000000
    combine b1 b2 = (b1 `shiftL` 6) .|. (b2 .&. 0b00111111)
    multi1 = case cs of
      c1:ds | isValidByte c1' ->
        let d = combine (c' .&. 0b00011111) c1'
        in  if d >= 0x80
              then decode' (chr d) ds
              else Nothing
       where
        c1' = ord c1
      _ -> Nothing
    multiByte :: Int -> Int -> Int -> Maybe String
    multiByte i mask overlong = aux i cs (c' .&. mask)
      where
        aux 0 rs acc
          | isValidAcc = decode' (chr acc) rs
          | otherwise = Nothing
         where
          isValidAcc =  overlong <= acc
                     && acc <= 0x10ffff
                     && (acc < 0xd800 || 0xdfff < acc)
                     && (acc < 0xfffe || 0xffff < acc)
        aux n (r : rs) acc | isValidByte r' = aux (n - 1) rs $ combine acc r'
         where
          r' = ord r
        aux _ _ _ = Nothing
  decode' :: Char -> String -> Maybe String
  decode' x xs = do
    xs' <- decode xs
    pure $ x : xs'
