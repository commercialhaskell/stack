{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import System.OsPath.Types
import System.OsPath.Encoding ( ucs2le )
import qualified System.OsString.Internal.Types as OST
import qualified Data.ByteString.Short as SBS

import Test.Tasty.Bench

import qualified System.FilePath.Posix as PF
import qualified System.FilePath.Posix as WF
import qualified System.OsString.Posix as OSP
import qualified System.OsString.Windows as WSP
import qualified System.OsPath.Posix as APF
import qualified System.OsPath.Windows as AWF

main :: IO ()
main = defaultMain
    [ bgroup "filepath (string)" $ map (uncurry bench)
      [("splitExtension (posix)"      , nf PF.splitExtension posixPath)
      ,("splitExtension (windows)"    , nf WF.splitExtension windowsPath)
      ,("takeExtension (posix)"       , nf PF.takeExtension posixPath)
      ,("takeExtension (windows)"     , nf WF.takeExtension windowsPath)
      ,("replaceExtension (posix)"    , nf (PF.replaceExtension ".lol") posixPath)
      ,("replaceExtension (windows)"  , nf (WF.replaceExtension ".lol") windowsPath)
      ,("dropExtension (posix)"       , nf PF.dropExtension posixPath)
      ,("dropExtension (windows)"     , nf WF.dropExtension windowsPath)
      ,("addExtension (posix)"        , nf (PF.addExtension ".lol") posixPath)
      ,("addExtension (windows)"      , nf (WF.addExtension ".lol") windowsPath)
      ,("hasExtension (posix)"        , nf PF.hasExtension posixPath)
      ,("hasExtension (windows)"      , nf WF.hasExtension windowsPath)
      ,("splitExtensions (posix)"     , nf PF.splitExtensions posixPath)
      ,("splitExtensions (windows)"   , nf WF.splitExtensions windowsPath)
      ,("dropExtensions (posix)"      , nf PF.dropExtensions posixPath)
      ,("dropExtensions (windows)"    , nf WF.dropExtensions windowsPath)
      ,("takeExtensions (posix)"      , nf PF.takeExtensions posixPath)
      ,("takeExtensions (windows)"    , nf WF.takeExtensions windowsPath)
      ,("replaceExtensions (posix)"   , nf (PF.replaceExtensions ".lol") posixPath)
      ,("replaceExtensions (windows)" , nf (WF.replaceExtensions ".lol") windowsPath)
      ,("isExtensionOf (posix)"       , nf (PF.isExtensionOf ".lol") posixPath)
      ,("isExtensionOf (windows)"     , nf (WF.isExtensionOf ".lol") windowsPath)
      ,("stripExtension (posix)"      , nf (PF.stripExtension ".lol") posixPath)
      ,("stripExtension (windows)"    , nf (WF.stripExtension ".lol") windowsPath)

      ,("splitFileName (posix)"       , nf PF.splitFileName posixPath)
      ,("splitFileName (windows)"     , nf WF.splitFileName windowsPath)
      ,("takeFileName (posix)"        , nf PF.takeFileName posixPath)
      ,("takeFileName (windows)"      , nf WF.takeFileName windowsPath)
      ,("replaceFileName (posix)"     , nf (PF.replaceFileName "lol") posixPath)
      ,("replaceFileName (windows)"   , nf (WF.replaceFileName "lol") windowsPath)
      ,("dropFileName (posix)"        , nf PF.dropFileName posixPath)
      ,("dropFileName (windows)"      , nf WF.dropFileName windowsPath)
      ,("takeBaseName (posix)"        , nf PF.takeBaseName posixPath)
      ,("takeBaseName (windows)"      , nf WF.takeBaseName windowsPath)
      ,("replaceBaseName (posix)"     , nf (PF.replaceBaseName "lol") posixPath)
      ,("replaceBaseName (windows)"   , nf (WF.replaceBaseName "lol") windowsPath)
      ,("takeDirectory (posix)"       , nf PF.takeDirectory posixPath)
      ,("takeDirectory (windows)"     , nf WF.takeDirectory windowsPath)
      ,("replaceDirectory (posix)"    , nf (PF.replaceDirectory "lol") posixPath)
      ,("replaceDirectory (windows)"  , nf (WF.replaceDirectory "lol") windowsPath)
      ,("combine (posix)"             , nf (PF.combine "lol") posixPath)
      ,("combine (windows)"           , nf (WF.combine "lol") windowsPath)
      ,("splitPath (posix)"           , nf PF.splitPath    posixPath)
      ,("splitPath (windows)"         , nf WF.splitPath    windowsPath)
      ,("joinPath (posix)"            , nf PF.joinPath     (PF.splitPath posixPath))
      ,("joinPath (windows)"          , nf WF.joinPath     (WF.splitPath windowsPath))
      ,("splitDirectories (posix)"    , nf PF.splitDirectories    posixPath)
      ,("splitDirectories (windows)"  , nf WF.splitDirectories    windowsPath)

      ,("splitDrive (posix)"          , nf PF.splitDrive    posixPath)
      ,("splitDrive (windows)"        , nf WF.splitDrive    windowsPath)
      ,("joinDrive (posix)"           , nf (PF.joinDrive "/")    posixPath)
      ,("joinDrive (windows)"         , nf (WF.joinDrive "C:\\")  windowsPath)
      ,("takeDrive (posix)"           , nf PF.takeDrive    posixPath)
      ,("takeDrive (windows)"         , nf WF.takeDrive    windowsPath)
      ,("hasDrive (posix)"            , nf PF.hasDrive    posixPath)
      ,("hasDrive (windows)"          , nf WF.hasDrive    windowsPath)
      ,("dropDrive (posix)"           , nf PF.dropDrive    posixPath)
      ,("dropDrive (windows)"         , nf WF.dropDrive    windowsPath)
      ,("isDrive (posix)"             , nf PF.isDrive    posixPath)
      ,("isDrive (windows)"           , nf WF.isDrive    windowsPath)

      ,("hasTrailingPathSeparator (posix)"    , nf PF.hasTrailingPathSeparator    posixPath)
      ,("hasTrailingPathSeparator (windows)"  , nf WF.hasTrailingPathSeparator    windowsPath)
      ,("addTrailingPathSeparator (posix)"    , nf PF.addTrailingPathSeparator    posixPath)
      ,("addTrailingPathSeparator (windows)"  , nf WF.addTrailingPathSeparator    windowsPath)
      ,("dropTrailingPathSeparator (posix)"   , nf PF.addTrailingPathSeparator    posixPath)
      ,("dropTrailingPathSeparator (windows)" , nf WF.addTrailingPathSeparator    windowsPath)

      ,("normalise (posix)"           , nf PF.normalise    posixPath)
      ,("normalise (windows)"         , nf WF.normalise    windowsPath)
      ,("equalFilePath (posix)"       , nf (PF.equalFilePath "abc/def/zs")   posixPath)
      ,("equalFilePath (windows)"     , nf (WF.equalFilePath "abc/def/zs")   windowsPath)
      ,("makeRelative (posix)"        , nf (PF.makeRelative "abc/def/zs")   posixPath)
      ,("makeRelative (windows)"      , nf (WF.makeRelative "abc/def/zs")   windowsPath)
      ,("isRelative (posix)"          , nf PF.isRelative    posixPath)
      ,("isRelative (windows)"        , nf WF.isRelative    windowsPath)
      ,("isAbsolute (posix)"          , nf PF.isAbsolute    posixPath)
      ,("isAbsolute (windows)"        , nf WF.isAbsolute    windowsPath)
      ,("isValid (posix)"             , nf PF.isValid    posixPath)
      ,("isValid (windows)"           , nf WF.isValid    windowsPath)
      ,("makeValid (posix)"           , nf PF.makeValid    posixPath)
      ,("makeValid (windows)"         , nf WF.makeValid    windowsPath)

      ,("splitSearchPath (posix)"    , nf PF.splitSearchPath posixSearchPath)
      ,("splitSearchPath (windows)"  , nf WF.splitSearchPath windowsSearchPath)
      ]

    , bgroup "filepath (AFPP)" $ map (uncurry bench)
      [ ("splitExtension (posix)"      , nf APF.splitExtension posixPathAFPP)
      , ("splitExtension (windows)"    , nf AWF.splitExtension windowsPathAFPP)
      , ("takeExtension (posix)"       , nf APF.takeExtension posixPathAFPP)
      , ("takeExtension (windows)"     , nf AWF.takeExtension windowsPathAFPP)
      , ("replaceExtension (posix)"    , nf (APF.replaceExtension [OSP.pstr|.lol|]) posixPathAFPP)
      , ("replaceExtension (windows)"  , nf (AWF.replaceExtension [WSP.pstr|.lol|]) windowsPathAFPP)
      , ("dropExtension (posix)"       , nf APF.dropExtension posixPathAFPP)
      , ("dropExtension (windows)"     , nf AWF.dropExtension windowsPathAFPP)
      , ("addExtension (posix)"        , nf (APF.addExtension [OSP.pstr|.lol|]) posixPathAFPP)
      , ("addExtension (windows)"      , nf (AWF.addExtension [WSP.pstr|.lol|]) windowsPathAFPP)
      , ("hasExtension (posix)"        , nf APF.hasExtension posixPathAFPP)
      , ("hasExtension (windows)"      , nf AWF.hasExtension windowsPathAFPP)
      , ("splitExtensions (posix)"     , nf APF.splitExtensions posixPathAFPP)
      , ("splitExtensions (windows)"   , nf AWF.splitExtensions windowsPathAFPP)
      , ("dropExtensions (posix)"      , nf APF.dropExtensions posixPathAFPP)
      , ("dropExtensions (windows)"    , nf AWF.dropExtensions windowsPathAFPP)
      , ("takeExtensions (posix)"      , nf APF.takeExtensions posixPathAFPP)
      , ("takeExtensions (windows)"    , nf AWF.takeExtensions windowsPathAFPP)
      , ("replaceExtensions (posix)"   , nf (APF.replaceExtensions [OSP.pstr|.lol|]) posixPathAFPP)
      , ("replaceExtensions (windows)" , nf (AWF.replaceExtensions [WSP.pstr|.lol|]) windowsPathAFPP)
      , ("isExtensionOf (posix)"       , nf (APF.isExtensionOf [OSP.pstr|.lol|]) posixPathAFPP)
      , ("isExtensionOf (windows)"     , nf (AWF.isExtensionOf [WSP.pstr|.lol|]) windowsPathAFPP)
      , ("stripExtension (posix)"      , nf (APF.stripExtension [OSP.pstr|.lol|]) posixPathAFPP)
      , ("stripExtension (windows)"    , nf (AWF.stripExtension [WSP.pstr|.lol|]) windowsPathAFPP)

      , ("splitFileName (posix)"       , nf APF.splitFileName posixPathAFPP)
      , ("splitFileName (windows)"     , nf AWF.splitFileName windowsPathAFPP)
      , ("takeFileName (posix)"        , nf APF.takeFileName posixPathAFPP)
      , ("takeFileName (windows)"      , nf AWF.takeFileName windowsPathAFPP)
      , ("replaceFileName (posix)"     , nf (APF.replaceFileName [OSP.pstr|lol|]) posixPathAFPP)
      , ("replaceFileName (windows)"   , nf (AWF.replaceFileName [WSP.pstr|lol|]) windowsPathAFPP)
      , ("dropFileName (posix)"        , nf APF.dropFileName posixPathAFPP)
      , ("dropFileName (windows)"      , nf AWF.dropFileName windowsPathAFPP)
      , ("takeBaseName (posix)"        , nf APF.takeBaseName posixPathAFPP)
      , ("takeBaseName (windows)"      , nf AWF.takeBaseName windowsPathAFPP)
      , ("replaceBaseName (posix)"     , nf (APF.replaceBaseName [OSP.pstr|lol|]) posixPathAFPP)
      , ("replaceBaseName (windows)"   , nf (AWF.replaceBaseName [WSP.pstr|lol|]) windowsPathAFPP)
      , ("takeDirectory (posix)"       , nf APF.takeDirectory posixPathAFPP)
      , ("takeDirectory (windows)"     , nf AWF.takeDirectory windowsPathAFPP)
      , ("replaceDirectory (posix)"    , nf (APF.replaceDirectory [OSP.pstr|lol|]) posixPathAFPP)
      , ("replaceDirectory (windows)"  , nf (AWF.replaceDirectory [WSP.pstr|lol|]) windowsPathAFPP)
      , ("combine (posix)"             , nf (APF.combine [OSP.pstr|lol|]) posixPathAFPP)
      , ("combine (windows)"           , nf (AWF.combine [WSP.pstr|lol|]) windowsPathAFPP)
      , ("splitPath (posix)"           , nf APF.splitPath    posixPathAFPP)
      , ("splitPath (windows)"         , nf AWF.splitPath    windowsPathAFPP)
      , ("joinPath (posix)"            , nf APF.joinPath     (APF.splitPath posixPathAFPP))
      , ("joinPath (windows)"          , nf AWF.joinPath     (AWF.splitPath windowsPathAFPP))
      , ("splitDirectories (posix)"    , nf APF.splitDirectories    posixPathAFPP)
      , ("splitDirectories (windows)"  , nf AWF.splitDirectories    windowsPathAFPP)

      , ("splitDrive (posix)"          , nf APF.splitDrive    posixPathAFPP)
      , ("splitDrive (windows)"        , nf AWF.splitDrive    windowsPathAFPP)
      , ("joinDrive (posix)"           , nf (APF.joinDrive [OSP.pstr|/|])    posixPathAFPP)
      , ("joinDrive (windows)"         , nf (AWF.joinDrive [WSP.pstr|C:\|])    windowsPathAFPP)
      , ("takeDrive (posix)"           , nf APF.takeDrive    posixPathAFPP)
      , ("takeDrive (windows)"         , nf AWF.takeDrive    windowsPathAFPP)
      , ("hasDrive (posix)"            , nf APF.hasDrive    posixPathAFPP)
      , ("hasDrive (windows)"          , nf AWF.hasDrive    windowsPathAFPP)
      , ("dropDrive (posix)"           , nf APF.dropDrive    posixPathAFPP)
      , ("dropDrive (windows)"         , nf AWF.dropDrive    windowsPathAFPP)
      , ("isDrive (posix)"             , nf APF.isDrive    posixPathAFPP)
      , ("isDrive (windows)"           , nf AWF.isDrive    windowsPathAFPP)

      , ("hasTrailingPathSeparator (posix)"    , nf APF.hasTrailingPathSeparator    posixPathAFPP)
      , ("hasTrailingPathSeparator (windows)"  , nf AWF.hasTrailingPathSeparator    windowsPathAFPP)
      , ("addTrailingPathSeparator (posix)"    , nf APF.addTrailingPathSeparator    posixPathAFPP)
      , ("addTrailingPathSeparator (windows)"  , nf AWF.addTrailingPathSeparator    windowsPathAFPP)
      , ("dropTrailingPathSeparator (posix)"   , nf APF.addTrailingPathSeparator    posixPathAFPP)
      , ("dropTrailingPathSeparator (windows)" , nf AWF.addTrailingPathSeparator    windowsPathAFPP)

      , ("normalise (posix)"           , nf APF.normalise    posixPathAFPP)
      , ("normalise (windows)"         , nf AWF.normalise    windowsPathAFPP)
      , ("equalFilePath (posix)"       , nf (APF.equalFilePath [OSP.pstr|abc/def/zs|])   posixPathAFPP)
      , ("equalFilePath (windows)"     , nf (AWF.equalFilePath [WSP.pstr|abc/def/zs|])   windowsPathAFPP)
      , ("makeRelative (posix)"        , nf (APF.makeRelative [OSP.pstr|abc/def/zs|])   posixPathAFPP)
      , ("makeRelative (windows)"      , nf (AWF.makeRelative [WSP.pstr|abc/def/zs|])   windowsPathAFPP)
      , ("isRelative (posix)"          , nf APF.isRelative    posixPathAFPP)
      , ("isRelative (windows)"        , nf AWF.isRelative    windowsPathAFPP)
      , ("isAbsolute (posix)"          , nf APF.isAbsolute    posixPathAFPP)
      , ("isAbsolute (windows)"        , nf AWF.isAbsolute    windowsPathAFPP)
      , ("isValid (posix)"             , nf APF.isValid    posixPathAFPP)
      , ("isValid (windows)"           , nf AWF.isValid    windowsPathAFPP)
      , ("makeValid (posix)"           , nf APF.makeValid    posixPathAFPP)
      , ("makeValid (windows)"         , nf AWF.makeValid    windowsPathAFPP)

      , ("splitSearchPath (posix)"     , nf APF.splitSearchPath posixSearchPathAFPP)
      , ("splitSearchPath (windows)"   , nf AWF.splitSearchPath windowsSearchPathAFPP)
      ]

    , bgroup "encoding/decoding" $ map (uncurry bench)
      [ ("decodeUtf (posix)"                  , nf (APF.decodeUtf @Maybe) posixPathAFPP)
      , ("decodeUtf (windows)"                , nf (AWF.decodeUtf @Maybe) windowsPathAFPP)
      , ("decodeWith (windows)"               , nf (AWF.decodeWith ucs2le) windowsPathAFPP)

      , ("encodeUtf (posix)"                  , nf (APF.encodeUtf @Maybe) posixPath)
      , ("encodeUtf (windows)"                , nf (AWF.encodeUtf @Maybe) windowsPath)
      , ("encodeWith (windows)"               , nf (AWF.encodeWith ucs2le) windowsPath)

      , ("unpack PlatformString (posix)"       , nf APF.unpack posixPathAFPP)
      , ("unpack PlatformString (windows)"     , nf AWF.unpack windowsPathAFPP)
      , ("pack PlatformString (posix)"         , nf APF.pack (APF.unpack posixPathAFPP))
      , ("pack PlatformString (windows)"       , nf AWF.pack (AWF.unpack windowsPathAFPP))

      , ("fromBytes (posix)"                  , nf (OSP.fromBytes @Maybe) (SBS.fromShort . OST.getPosixString $ posixPathAFPP))
      , ("fromBytes (windows)"                , nf (WSP.fromBytes @Maybe) (SBS.fromShort . OST.getWindowsString $ windowsPathAFPP))
      ]
    ]


posixPath :: FilePath
posixPath = "/foo/bar/bath/baz/baz/tz/fooooooooooooooo/laaaaaaaaaaaaaaa/baaaaaaaaaaaaar/zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz/zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz/kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk/kkkkkkkkkkkkkkkkkk/h/h/h/a/s/r/a/h/gt/r/r/r/s/s.txt"

windowsPath :: FilePath
windowsPath = "C:\\foo\\bar\\bath\\baz\\baz\\tz\\fooooooooooooooo\\laaaaaaaaaaaaaaa\\baaaaaaaaaaaaar\\zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz\\zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz\\kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk\\kkkkkkkkkkkkkkkkkk\\h\\h\\h\\a\\s\\r\\a\\h\\gt\\r\\r\\r\\s\\s.txt"

posixPathAFPP :: PosixPath
posixPathAFPP = [OSP.pstr|/foo/bar/bath/baz/baz/tz/fooooooooooooooo/laaaaaaaaaaaaaaa/baaaaaaaaaaaaar/zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz/zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz/kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk/kkkkkkkkkkkkkkkkkk/h/h/h/a/s/r/a/h/gt/r/r/r/s/s.txt|]

windowsPathAFPP :: WindowsPath
windowsPathAFPP = [WSP.pstr|C:\\foo\\bar\\bath\\baz\\baz\\tz\\fooooooooooooooo\\laaaaaaaaaaaaaaa\\baaaaaaaaaaaaar\\zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz\\zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz\\kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk\\kkkkkkkkkkkkkkkkkk\\h\\h\\h\\a\\s\\r\\a\\h\\gt\\r\\r\\r\\s\\s.txt|]

posixSearchPath :: FilePath
posixSearchPath = ":foo:bar:bath:baz:baz:tz:fooooooooooooooo:laaaaaaaaaaaaaaa:baaaaaaaaaaaaar:zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz:zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz:kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk:kkkkkkkkkkkkkkkkkk:h:h:h:a:s:r:a:h:gt:r:r:r:s:s.txt"

windowsSearchPath :: FilePath
windowsSearchPath = "foo;bar;bath;baz;baz;tz;fooooooooooooooo;laaaaaaaaaaaaaaa;baaaaaaaaaaaaar;zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz;zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz;kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk;kkkkkkkkkkkkkkkkkk;h;h;h;a;s;r;a;h;gt;r;r;r;s;s.txt"

posixSearchPathAFPP :: PosixString
posixSearchPathAFPP = [OSP.pstr|:foo:bar:bath:baz:baz:tz:fooooooooooooooo:laaaaaaaaaaaaaaa:baaaaaaaaaaaaar:zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz:zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz:kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk:kkkkkkkkkkkkkkkkkk:h:h:h:a:s:r:a:h:gt:r:r:r:s:s.txt|]

windowsSearchPathAFPP :: WindowsString
windowsSearchPathAFPP = [WSP.pstr|foo;bar;bath;baz;baz;tz;fooooooooooooooo;laaaaaaaaaaaaaaa;baaaaaaaaaaaaar;zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz;zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz;kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk;kkkkkkkkkkkkkkkkkk;h;h;h;a;s;r;a;h;gt;r;r;r;s;s.txt|]
