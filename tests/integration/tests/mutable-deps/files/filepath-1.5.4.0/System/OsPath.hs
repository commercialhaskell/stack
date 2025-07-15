{-# LANGUAGE CPP #-}

#define FILEPATH_NAME OsPath
#define OSSTRING_NAME OsString
#define WORD_NAME OsChar

-- |
-- Module      :  System.OsPath
-- Copyright   :  © 2021 Julian Ospald
-- License     :  MIT
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of the <https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/abstract-file-path Abstract FilePath Proposal>,
-- which aims to supersede @type FilePath = String@ for various reasons:
--
-- 1. it is more efficient and avoids memory fragmentation (uses unpinned 'ShortByteString' under the hood)
-- 2. it is more type-safe (newtype over 'ShortByteString')
-- 3. avoids round-tripping issues by not converting to String (which is not total and loses the encoding)
-- 4. abstracts over unix and windows while keeping the original bytes
--
-- It is important to know that filenames\/filepaths have different representations across platforms:
--
-- - On /Windows/, filepaths are expected to be encoded as UTF16-LE <https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-dtyp/76f10dd8-699d-45e6-a53c-5aefc586da20 as per the documentation>, but
--   may also include invalid surrogate pairs, in which case UCS-2 can be used. They are passed as @wchar_t*@ to syscalls.
--   'OsPath' only maintains the wide character invariant.
-- - On /Unix/, filepaths don't have a predefined encoding (although they
--   are often interpreted as UTF8) as per the
--   <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_170 POSIX specification>
--   and are passed as @char[]@ to syscalls. 'OsPath' maintains no invariant
--   here.
--
-- Apart from encoding, filepaths have additional restrictions per platform:
--
-- - On /Windows/ the <https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#naming-conventions naming convention> may apply
-- - On /Unix/, only @NUL@ bytes are disallowed as per the <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_170 POSIX specification>
--
-- Use 'isValid' to check for these restrictions ('OsPath' doesn't
-- maintain this invariant).
--
-- Also note that these restrictions are
-- not exhaustive and further filesystem specific restrictions may apply on
-- all platforms. This library makes no attempt at satisfying these.
-- Library users may need to account for that, depending
-- on what filesystems they want to support.
--
-- It is advised to follow these principles when dealing with filepaths\/filenames:
--
-- 1. Avoid interpreting filenames that the OS returns, unless absolutely necessary.
--    For example, the filepath separator is usually a predefined 'Word8'/'Word16', regardless of encoding.
--    So even if we need to split filepaths, it might still not be necessary to understand the encoding
--    of the filename.
-- 2. When interpreting OS returned filenames consider that these might not be UTF8 on /unix/
--    or at worst don't have an ASCII compatible encoding. The are 3 available strategies fer decoding/encoding:
--    a) pick the best UTF (UTF-8 on unix, UTF-16LE on windows), b) decode with an explicitly defined 'TextEncoding',
--    c) mimic the behavior of the @base@ library (permissive UTF16 on windows, current filesystem encoding on unix).
-- 3. Avoid comparing @String@ based filepaths, because filenames of different encodings
--    may have the same @String@ representation, although they're not the same byte-wise.


#include "OsPath/Common.hs"
