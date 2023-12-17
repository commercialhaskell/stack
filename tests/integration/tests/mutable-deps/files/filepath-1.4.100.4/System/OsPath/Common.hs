{- HLINT ignore -}
{-# LANGUAGE TypeApplications #-}
-- This template expects CPP definitions for:
--
--     WINDOWS defined? = no            | yes              | no
--     POSIX   defined? = yes           | no               | no
--
--     FILEPATH_NAME    = PosixPath     | WindowsPath  | OsPath
--     OSSTRING_NAME    = PosixString   | WindowsString    | OsString
--     WORD_NAME        = PosixChar     | WindowsChar      | OsChar

-- For (native) abstract file paths we document both platforms, so people can
-- understand how their code is compiled no matter what. But for the
-- platform-specific types we only want to document the behavior on that
-- platform.
#if defined(WINDOWS)
#define WINDOWS_DOC
#elif defined(POSIX)
#define POSIX_DOC
#endif

#ifdef WINDOWS
module System.OsPath.Windows
#elif defined(POSIX)
module System.OsPath.Posix
#else
module System.OsPath
#endif
  (
  -- * Types
#ifdef WINDOWS
    WindowsString
  , WindowsChar
  , WindowsPath
#elif defined(POSIX)
    PosixString
  , PosixChar
  , PosixPath
#else
    OsPath
  , OsString
  , OsChar
#endif
  -- * Filepath construction
  , PS.encodeUtf
  , PS.encodeWith
  , PS.encodeFS
#if defined(WINDOWS) || defined(POSIX)
  , pstr
#else
  , osp
#endif
  , PS.pack

  -- * Filepath deconstruction
  , PS.decodeUtf
  , PS.decodeWith
  , PS.decodeFS
  , PS.unpack

  -- * Word construction
  , unsafeFromChar

  -- * Word deconstruction
  , toChar

  -- * Separator predicates
  , pathSeparator
  , pathSeparators
  , isPathSeparator
  , searchPathSeparator
  , isSearchPathSeparator
  , extSeparator
  , isExtSeparator

  -- * $PATH methods
  , splitSearchPath,

  -- * Extension functions
    splitExtension,
    takeExtension, replaceExtension, (-<.>), dropExtension, addExtension, hasExtension, (<.>),
    splitExtensions, dropExtensions, takeExtensions, replaceExtensions, isExtensionOf,
    stripExtension,

    -- * Filename\/directory functions
    splitFileName,
    takeFileName, replaceFileName, dropFileName,
    takeBaseName, replaceBaseName,
    takeDirectory, replaceDirectory,
    combine, (</>),
    splitPath, joinPath, splitDirectories,

    -- * Drive functions
    splitDrive, joinDrive,
    takeDrive, hasDrive, dropDrive, isDrive,

    -- * Trailing slash functions
    hasTrailingPathSeparator,
    addTrailingPathSeparator,
    dropTrailingPathSeparator,

    -- * File name manipulations
    normalise, equalFilePath,
    makeRelative,
    isRelative, isAbsolute,
    isValid, makeValid
  )
where


#ifdef WINDOWS
import System.OsPath.Types
import System.OsString.Windows as PS
    ( unsafeFromChar
    , toChar
    , decodeUtf
    , decodeWith
    , decodeFS
    , pack
    , encodeUtf
    , encodeWith
    , encodeFS
    , unpack
    )
import Data.Bifunctor ( bimap )
import qualified System.OsPath.Windows.Internal as C
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
import Control.Monad ( when )

#elif defined(POSIX)
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
import Control.Monad ( when )
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )

import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import System.OsPath.Types
import System.OsString.Posix as PS
    ( unsafeFromChar
    , toChar
    , decodeUtf
    , decodeWith
    , decodeFS
    , pack
    , encodeUtf
    , encodeWith
    , encodeFS
    , unpack
    )
import Data.Bifunctor ( bimap )
import qualified System.OsPath.Posix.Internal as C

#else

import System.OsPath.Internal as PS
    ( osp
    , decodeUtf
    , decodeWith
    , decodeFS
    , pack
    , encodeUtf
    , encodeWith
    , encodeFS
    , unpack
    )
import System.OsPath.Types
    ( OsPath )
import System.OsString ( unsafeFromChar, toChar )

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified System.OsPath.Windows as C
#else
import qualified System.OsPath.Posix as C
#endif

import Data.Bifunctor
    ( bimap )
#endif
import System.OsString.Internal.Types


------------------------
-- Separator predicates


#ifdef WINDOWS_DOC
-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- > pathSeparator == '\\'S
#elif defined(POSIX_DOC)
-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- > pathSeparator ==  '/'
#else
-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- > Windows: pathSeparator == '\\'S
-- > Posix:   pathSeparator ==  '/'
#endif
pathSeparator :: WORD_NAME
pathSeparator = WORD_NAME C.pathSeparator

#ifdef WINDOWS_DOC
-- | The list of all possible separators.
--
-- > pathSeparators == ['\\', '/']
-- > pathSeparator `elem` pathSeparators
#elif defined(POSIX_DOC)
-- | The list of all possible separators.
--
-- > pathSeparators == ['/']
-- > pathSeparator `elem` pathSeparators
#else
-- | The list of all possible separators.
--
-- > Windows: pathSeparators == ['\\', '/']
-- > Posix:   pathSeparators == ['/']
-- > pathSeparator `elem` pathSeparators
#endif
pathSeparators :: [WORD_NAME]
pathSeparators = WORD_NAME <$> C.pathSeparators

-- | Rather than using @(== 'pathSeparator')@, use this. Test if something
--   is a path separator.
--
-- > isPathSeparator a == (a `elem` pathSeparators)
isPathSeparator :: WORD_NAME -> Bool
isPathSeparator (WORD_NAME w) = C.isPathSeparator w

#ifdef WINDOWS_DOC
-- | The character that is used to separate the entries in the $PATH environment variable.
--
-- > searchPathSeparator == ';'
#elif defined(POSIX_DOC)
-- | The character that is used to separate the entries in the $PATH environment variable.
--
-- > searchPathSeparator == ':'
#else
-- | The character that is used to separate the entries in the $PATH environment variable.
--
-- > Posix:   searchPathSeparator == ':'
-- > Windows: searchPathSeparator == ';'
#endif
searchPathSeparator :: WORD_NAME
searchPathSeparator = WORD_NAME C.searchPathSeparator

-- | Is the character a file separator?
--
-- > isSearchPathSeparator a == (a == searchPathSeparator)
isSearchPathSeparator :: WORD_NAME -> Bool
isSearchPathSeparator (WORD_NAME w) = C.isSearchPathSeparator w


-- | File extension character
--
-- > extSeparator == '.'
extSeparator :: WORD_NAME
extSeparator = WORD_NAME C.extSeparator


-- | Is the character an extension character?
--
-- > isExtSeparator a == (a == extSeparator)
isExtSeparator :: WORD_NAME -> Bool
isExtSeparator (WORD_NAME w) = C.isExtSeparator w


---------------------------------------------------------------------
-- Path methods (environment $PATH)

#ifdef WINDOWS_DOC
-- | Take a string, split it on the 'searchPathSeparator' character.
--
--   Blank items are ignored and path elements are stripped of quotes.
--
-- > splitSearchPath "File1;File2;File3"  == ["File1","File2","File3"]
-- > splitSearchPath "File1;;File2;File3" == ["File1","File2","File3"]
-- > splitSearchPath "File1;\"File2\";File3" == ["File1","File2","File3"]
#elif defined(POSIX_DOC)
-- | Take a string, split it on the 'searchPathSeparator' character.
--
--   Blank items are converted to @.@ on , and quotes are not
--   treated specially.
--
--   Follows the recommendations in
--   <http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap08.html>
--
-- > splitSearchPath "File1:File2:File3"  == ["File1","File2","File3"]
-- > splitSearchPath "File1::File2:File3" == ["File1",".","File2","File3"]
#else
-- | Take a string, split it on the 'searchPathSeparator' character.
--
--   On Windows, blank items are ignored on Windows, and path elements are
--   stripped of quotes.
--
--   On Posix, blank items are converted to @.@ on Posix, and quotes are not
--   treated specially.
--
--   Follows the recommendations in
--   <http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap08.html>
--
-- > Windows: splitSearchPath "File1;File2;File3"  == ["File1","File2","File3"]
-- > Windows: splitSearchPath "File1;;File2;File3" == ["File1","File2","File3"]
-- > Windows: splitSearchPath "File1;\"File2\";File3" == ["File1","File2","File3"]
-- > Posix:   splitSearchPath "File1:File2:File3"  == ["File1","File2","File3"]
-- > Posix:   splitSearchPath "File1::File2:File3" == ["File1",".","File2","File3"]
#endif
splitSearchPath :: OSSTRING_NAME -> [FILEPATH_NAME]
splitSearchPath (OSSTRING_NAME x) = fmap OSSTRING_NAME . C.splitSearchPath $ x



------------------------
-- Extension functions

-- | Split on the extension. 'addExtension' is the inverse.
--
-- > splitExtension "/directory/path.ext" == ("/directory/path",".ext")
-- > uncurry (<>) (splitExtension x) == x
-- > Valid x => uncurry addExtension (splitExtension x) == x
-- > splitExtension "file.txt" == ("file",".txt")
-- > splitExtension "file" == ("file","")
-- > splitExtension "file/file.txt" == ("file/file",".txt")
-- > splitExtension "file.txt/boris" == ("file.txt/boris","")
-- > splitExtension "file.txt/boris.ext" == ("file.txt/boris",".ext")
-- > splitExtension "file/path.txt.bob.fred" == ("file/path.txt.bob",".fred")
-- > splitExtension "file/path.txt/" == ("file/path.txt/","")
splitExtension :: FILEPATH_NAME -> (FILEPATH_NAME, OSSTRING_NAME)
splitExtension (OSSTRING_NAME x) = bimap OSSTRING_NAME OSSTRING_NAME $ C.splitExtension x


-- | Get the extension of a file, returns @\"\"@ for no extension, @.ext@ otherwise.
--
-- > takeExtension "/directory/path.ext" == ".ext"
-- > takeExtension x == snd (splitExtension x)
-- > Valid x => takeExtension (addExtension x "ext") == ".ext"
-- > Valid x => takeExtension (replaceExtension x "ext") == ".ext"
takeExtension :: FILEPATH_NAME -> OSSTRING_NAME
takeExtension (OSSTRING_NAME x) = OSSTRING_NAME $ C.takeExtension x


-- | Remove the current extension and add another, equivalent to 'replaceExtension'.
--
-- > "/directory/path.txt" -<.> "ext" == "/directory/path.ext"
-- > "/directory/path.txt" -<.> ".ext" == "/directory/path.ext"
-- > "foo.o" -<.> "c" == "foo.c"
(-<.>) :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
(-<.>) = replaceExtension

-- | Set the extension of a file, overwriting one if already present, equivalent to '-<.>'.
--
-- > replaceExtension "/directory/path.txt" "ext" == "/directory/path.ext"
-- > replaceExtension "/directory/path.txt" ".ext" == "/directory/path.ext"
-- > replaceExtension "file.txt" ".bob" == "file.bob"
-- > replaceExtension "file.txt" "bob" == "file.bob"
-- > replaceExtension "file" ".bob" == "file.bob"
-- > replaceExtension "file.txt" "" == "file"
-- > replaceExtension "file.fred.bob" "txt" == "file.fred.txt"
-- > replaceExtension x y == addExtension (dropExtension x) y
replaceExtension :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
replaceExtension (OSSTRING_NAME path) (OSSTRING_NAME ext) = OSSTRING_NAME (C.replaceExtension path ext)


-- | Add an extension, even if there is already one there, equivalent to 'addExtension'.
--
-- > "/directory/path" <.> "ext" == "/directory/path.ext"
-- > "/directory/path" <.> ".ext" == "/directory/path.ext"
(<.>) :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
(<.>) = addExtension

-- | Remove last extension, and the \".\" preceding it.
--
-- > dropExtension "/directory/path.ext" == "/directory/path"
-- > dropExtension x == fst (splitExtension x)
dropExtension :: FILEPATH_NAME -> FILEPATH_NAME
dropExtension (OSSTRING_NAME x) = OSSTRING_NAME $ C.dropExtension x


-- | Add an extension, even if there is already one there, equivalent to '<.>'.
--
-- > addExtension "/directory/path" "ext" == "/directory/path.ext"
-- > addExtension "file.txt" "bib" == "file.txt.bib"
-- > addExtension "file." ".bib" == "file..bib"
-- > addExtension "file" ".bib" == "file.bib"
-- > addExtension "/" "x" == "/.x"
-- > addExtension x "" == x
-- > Valid x => takeFileName (addExtension (addTrailingPathSeparator x) "ext") == ".ext"
-- > Windows: addExtension "\\\\share" ".txt" == "\\\\share\\.txt"

#ifdef WINDOWS_DOC
-- | Add an extension, even if there is already one there, equivalent to '<.>'.
--
-- > addExtension "/directory/path" "ext" == "/directory/path.ext"
-- > addExtension "file.txt" "bib" == "file.txt.bib"
-- > addExtension "file." ".bib" == "file..bib"
-- > addExtension "file" ".bib" == "file.bib"
-- > addExtension "/" "x" == "/.x"
-- > addExtension x "" == x
-- > Valid x => takeFileName (addExtension (addTrailingPathSeparator x) "ext") == ".ext"
-- > addExtension "\\\\share" ".txt" == "\\\\share\\.txt"
#elif defined(POSIX_DOC)
-- | Add an extension, even if there is already one there, equivalent to '<.>'.
--
-- > addExtension "/directory/path" "ext" == "/directory/path.ext"
-- > addExtension "file.txt" "bib" == "file.txt.bib"
-- > addExtension "file." ".bib" == "file..bib"
-- > addExtension "file" ".bib" == "file.bib"
-- > addExtension "/" "x" == "/.x"
-- > addExtension x "" == x
-- > Valid x => takeFileName (addExtension (addTrailingPathSeparator x) "ext") == ".ext"
#else
-- | Add an extension, even if there is already one there, equivalent to '<.>'.
--
-- > addExtension "/directory/path" "ext" == "/directory/path.ext"
-- > addExtension "file.txt" "bib" == "file.txt.bib"
-- > addExtension "file." ".bib" == "file..bib"
-- > addExtension "file" ".bib" == "file.bib"
-- > addExtension "/" "x" == "/.x"
-- > addExtension x "" == x
-- > Valid x => takeFileName (addExtension (addTrailingPathSeparator x) "ext") == ".ext"
-- > Windows: addExtension "\\\\share" ".txt" == "\\\\share\\.txt"
#endif
addExtension :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
addExtension (OSSTRING_NAME bs) (OSSTRING_NAME ext) = OSSTRING_NAME $ C.addExtension bs ext


-- | Does the given filename have an extension?
--
-- > hasExtension "/directory/path.ext" == True
-- > hasExtension "/directory/path" == False
-- > null (takeExtension x) == not (hasExtension x)
hasExtension :: FILEPATH_NAME -> Bool
hasExtension (OSSTRING_NAME x) = C.hasExtension x

-- | Does the given filename have the specified extension?
--
-- > "png" `isExtensionOf` "/directory/file.png" == True
-- > ".png" `isExtensionOf` "/directory/file.png" == True
-- > ".tar.gz" `isExtensionOf` "bar/foo.tar.gz" == True
-- > "ar.gz" `isExtensionOf` "bar/foo.tar.gz" == False
-- > "png" `isExtensionOf` "/directory/file.png.jpg" == False
-- > "csv/table.csv" `isExtensionOf` "/data/csv/table.csv" == False
isExtensionOf :: OSSTRING_NAME -> FILEPATH_NAME -> Bool
isExtensionOf (OSSTRING_NAME x) (OSSTRING_NAME y) = C.isExtensionOf x y

-- | Drop the given extension from a filepath, and the @\".\"@ preceding it.
--   Returns 'Nothing' if the filepath does not have the given extension, or
--   'Just' and the part before the extension if it does.
--
--   This function can be more predictable than 'dropExtensions', especially if the filename
--   might itself contain @.@ characters.
--
-- > stripExtension "hs.o" "foo.x.hs.o" == Just "foo.x"
-- > stripExtension "hi.o" "foo.x.hs.o" == Nothing
-- > dropExtension x == fromJust (stripExtension (takeExtension x) x)
-- > dropExtensions x == fromJust (stripExtension (takeExtensions x) x)
-- > stripExtension ".c.d" "a.b.c.d"  == Just "a.b"
-- > stripExtension ".c.d" "a.b..c.d" == Just "a.b."
-- > stripExtension "baz"  "foo.bar"  == Nothing
-- > stripExtension "bar"  "foobar"   == Nothing
-- > stripExtension ""     x          == Just x
stripExtension :: OSSTRING_NAME -> FILEPATH_NAME -> Maybe FILEPATH_NAME
stripExtension (OSSTRING_NAME bs) (OSSTRING_NAME x) = OSSTRING_NAME <$> C.stripExtension bs x

-- | Split on all extensions.
--
-- > splitExtensions "/directory/path.ext" == ("/directory/path",".ext")
-- > splitExtensions "file.tar.gz" == ("file",".tar.gz")
-- > uncurry (<>) (splitExtensions x) == x
-- > Valid x => uncurry addExtension (splitExtensions x) == x
-- > splitExtensions "file.tar.gz" == ("file",".tar.gz")
splitExtensions :: FILEPATH_NAME -> (FILEPATH_NAME, OSSTRING_NAME)
splitExtensions (OSSTRING_NAME x) = bimap OSSTRING_NAME OSSTRING_NAME $ C.splitExtensions x


-- | Drop all extensions.
--
-- > dropExtensions "/directory/path.ext" == "/directory/path"
-- > dropExtensions "file.tar.gz" == "file"
-- > not $ hasExtension $ dropExtensions x
-- > not $ any isExtSeparator $ takeFileName $ dropExtensions x
dropExtensions :: FILEPATH_NAME -> FILEPATH_NAME
dropExtensions (OSSTRING_NAME x) = OSSTRING_NAME $ C.dropExtensions x


-- | Get all extensions.
--
-- > takeExtensions "/directory/path.ext" == ".ext"
-- > takeExtensions "file.tar.gz" == ".tar.gz"
takeExtensions :: FILEPATH_NAME -> OSSTRING_NAME
takeExtensions (OSSTRING_NAME x) = OSSTRING_NAME $ C.takeExtensions x

-- | Replace all extensions of a file with a new extension. Note
--   that 'replaceExtension' and 'addExtension' both work for adding
--   multiple extensions, so only required when you need to drop
--   all extensions first.
--
-- > replaceExtensions "file.fred.bob" "txt" == "file.txt"
-- > replaceExtensions "file.fred.bob" "tar.gz" == "file.tar.gz"
replaceExtensions :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
replaceExtensions (OSSTRING_NAME x) (OSSTRING_NAME y) = OSSTRING_NAME $ C.replaceExtensions x y


------------------------
-- Drive functions


#ifdef WINDOWS_DOC
-- | Split a path into a drive and a path.
--
-- > uncurry (<>) (splitDrive x) == x
-- > splitDrive "file" == ("","file")
-- > splitDrive "c:/file" == ("c:/","file")
-- > splitDrive "c:\\file" == ("c:\\","file")
-- > splitDrive "\\\\shared\\test" == ("\\\\shared\\","test")
-- > splitDrive "\\\\shared" == ("\\\\shared","")
-- > splitDrive "\\\\?\\UNC\\shared\\file" == ("\\\\?\\UNC\\shared\\","file")
-- > splitDrive "\\\\?\\UNCshared\\file" == ("\\\\?\\","UNCshared\\file")
-- > splitDrive "\\\\?\\d:\\file" == ("\\\\?\\d:\\","file")
-- > splitDrive "/d" == ("","/d")
#elif defined(POSIX_DOC)
-- | Split a path into a drive and a path.
--   \/ is a Drive.
--
-- > uncurry (<>) (splitDrive x) == x
-- > splitDrive "/test" == ("/","test")
-- > splitDrive "//test" == ("//","test")
-- > splitDrive "test/file" == ("","test/file")
-- > splitDrive "file" == ("","file")
#else
-- | Split a path into a drive and a path.
--   On Posix, \/ is a Drive.
--
-- > uncurry (<>) (splitDrive x) == x
-- > Windows: splitDrive "file" == ("","file")
-- > Windows: splitDrive "c:/file" == ("c:/","file")
-- > Windows: splitDrive "c:\\file" == ("c:\\","file")
-- > Windows: splitDrive "\\\\shared\\test" == ("\\\\shared\\","test")
-- > Windows: splitDrive "\\\\shared" == ("\\\\shared","")
-- > Windows: splitDrive "\\\\?\\UNC\\shared\\file" == ("\\\\?\\UNC\\shared\\","file")
-- > Windows: splitDrive "\\\\?\\UNCshared\\file" == ("\\\\?\\","UNCshared\\file")
-- > Windows: splitDrive "\\\\?\\d:\\file" == ("\\\\?\\d:\\","file")
-- > Windows: splitDrive "/d" == ("","/d")
-- > Posix:   splitDrive "/test" == ("/","test")
-- > Posix:   splitDrive "//test" == ("//","test")
-- > Posix:   splitDrive "test/file" == ("","test/file")
-- > Posix:   splitDrive "file" == ("","file")
#endif
splitDrive :: FILEPATH_NAME -> (FILEPATH_NAME, FILEPATH_NAME)
splitDrive (OSSTRING_NAME p) = bimap OSSTRING_NAME OSSTRING_NAME $ C.splitDrive p


-- | Join a drive and the rest of the path.
--
-- > Valid x => uncurry joinDrive (splitDrive x) == x
-- > Windows: joinDrive "C:" "foo" == "C:foo"
-- > Windows: joinDrive "C:\\" "bar" == "C:\\bar"
-- > Windows: joinDrive "\\\\share" "foo" == "\\\\share\\foo"
-- > Windows: joinDrive "/:" "foo" == "/:\\foo"

#ifdef WINDOWS_DOC
-- | Join a drive and the rest of the path.
--
-- > Valid x => uncurry joinDrive (splitDrive x) == x
-- > joinDrive "C:" "foo" == "C:foo"
-- > joinDrive "C:\\" "bar" == "C:\\bar"
-- > joinDrive "\\\\share" "foo" == "\\\\share\\foo"
-- > joinDrive "/:" "foo" == "/:\\foo"
#elif defined(POSIX_DOC)
-- | Join a drive and the rest of the path.
--
-- > Valid x => uncurry joinDrive (splitDrive x) == x
#else
-- | Join a drive and the rest of the path.
--
-- > Valid x => uncurry joinDrive (splitDrive x) == x
-- > Windows: joinDrive "C:" "foo" == "C:foo"
-- > Windows: joinDrive "C:\\" "bar" == "C:\\bar"
-- > Windows: joinDrive "\\\\share" "foo" == "\\\\share\\foo"
-- > Windows: joinDrive "/:" "foo" == "/:\\foo"
#endif
joinDrive :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
joinDrive (OSSTRING_NAME a) (OSSTRING_NAME b) = OSSTRING_NAME $ C.joinDrive a b


-- | Get the drive from a filepath.
--
-- > takeDrive x == fst (splitDrive x)
takeDrive :: FILEPATH_NAME -> FILEPATH_NAME
takeDrive (OSSTRING_NAME x) = OSSTRING_NAME $ C.takeDrive x


-- | Delete the drive, if it exists.
--
-- > dropDrive x == snd (splitDrive x)
dropDrive :: FILEPATH_NAME -> FILEPATH_NAME
dropDrive (OSSTRING_NAME x) = OSSTRING_NAME $ C.dropDrive x


#ifdef WINDOWS_DOC
-- | Does a path have a drive.
--
-- > not (hasDrive x) == null (takeDrive x)
-- > hasDrive "C:\\foo" == True
-- > hasDrive "C:foo" == True
-- > hasDrive "foo" == False
-- > hasDrive "" == False
--
#elif defined(POSIX_DOC)
-- | Does a path have a drive.
--
-- > not (hasDrive x) == null (takeDrive x)
-- > hasDrive "/foo" == True
-- > hasDrive "foo" == False
-- > hasDrive "" == False
--
#else
-- | Does a path have a drive.
--
-- > not (hasDrive x) == null (takeDrive x)
-- > Posix:   hasDrive "/foo" == True
-- > Windows: hasDrive "C:\\foo" == True
-- > Windows: hasDrive "C:foo" == True
-- >          hasDrive "foo" == False
-- >          hasDrive "" == False
--
#endif
hasDrive :: FILEPATH_NAME -> Bool
hasDrive (OSSTRING_NAME x) = C.hasDrive x


#ifdef WINDOWS_DOC
-- | Is an element a drive
--
-- > isDrive "C:\\" == True
-- > isDrive "C:\\foo" == False
-- > isDrive "" == False
#elif defined(POSIX_DOC)
-- | Is an element a drive
--
-- > isDrive "/" == True
-- > isDrive "/foo" == False
-- > isDrive "" == False
#else
-- | Is an element a drive
--
-- > Posix:   isDrive "/" == True
-- > Posix:   isDrive "/foo" == False
-- > Windows: isDrive "C:\\" == True
-- > Windows: isDrive "C:\\foo" == False
-- >          isDrive "" == False
#endif
isDrive :: FILEPATH_NAME -> Bool
isDrive (OSSTRING_NAME x) = C.isDrive x


---------------------------------------------------------------------
-- Operations on a filepath, as a list of directories

#ifdef WINDOWS_DOC
-- | Split a filename into directory and file. '</>' is the inverse.
--   The first component will often end with a trailing slash.
--
-- > splitFileName "/directory/file.ext" == ("/directory/","file.ext")
-- > Valid x => uncurry (</>) (splitFileName x) == x || fst (splitFileName x) == "./"
-- > Valid x => isValid (fst (splitFileName x))
-- > splitFileName "file/bob.txt" == ("file/", "bob.txt")
-- > splitFileName "file/" == ("file/", "")
-- > splitFileName "bob" == ("./", "bob")
-- > splitFileName "c:" == ("c:","")
#elif defined(POSIX_DOC)
-- | Split a filename into directory and file. '</>' is the inverse.
--   The first component will often end with a trailing slash.
--
-- > splitFileName "/directory/file.ext" == ("/directory/","file.ext")
-- > Valid x => uncurry (</>) (splitFileName x) == x || fst (splitFileName x) == "./"
-- > Valid x => isValid (fst (splitFileName x))
-- > splitFileName "file/bob.txt" == ("file/", "bob.txt")
-- > splitFileName "file/" == ("file/", "")
-- > splitFileName "bob" == ("./", "bob")
-- > splitFileName "/" == ("/","")
#else
-- | Split a filename into directory and file. '</>' is the inverse.
--   The first component will often end with a trailing slash.
--
-- > splitFileName "/directory/file.ext" == ("/directory/","file.ext")
-- > Valid x => uncurry (</>) (splitFileName x) == x || fst (splitFileName x) == "./"
-- > Valid x => isValid (fst (splitFileName x))
-- > splitFileName "file/bob.txt" == ("file/", "bob.txt")
-- > splitFileName "file/" == ("file/", "")
-- > splitFileName "bob" == ("./", "bob")
-- > Posix:   splitFileName "/" == ("/","")
-- > Windows: splitFileName "c:" == ("c:","")
#endif
splitFileName :: FILEPATH_NAME -> (FILEPATH_NAME, FILEPATH_NAME)
splitFileName (OSSTRING_NAME x) = bimap OSSTRING_NAME OSSTRING_NAME $ C.splitFileName x


-- | Set the filename.
--
-- > replaceFileName "/directory/other.txt" "file.ext" == "/directory/file.ext"
-- > Valid x => replaceFileName x (takeFileName x) == x
replaceFileName :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
replaceFileName (OSSTRING_NAME x) (OSSTRING_NAME y) = OSSTRING_NAME $ C.replaceFileName x y


-- | Drop the filename. Unlike 'takeDirectory', this function will leave
--   a trailing path separator on the directory.
--
-- > dropFileName "/directory/file.ext" == "/directory/"
-- > dropFileName x == fst (splitFileName x)
dropFileName :: FILEPATH_NAME -> FILEPATH_NAME
dropFileName (OSSTRING_NAME x) = OSSTRING_NAME $ C.dropFileName x


-- | Get the file name.
--
-- > takeFileName "/directory/file.ext" == "file.ext"
-- > takeFileName "test/" == ""
-- > takeFileName x `isSuffixOf` x
-- > takeFileName x == snd (splitFileName x)
-- > Valid x => takeFileName (replaceFileName x "fred") == "fred"
-- > Valid x => takeFileName (x </> "fred") == "fred"
-- > Valid x => isRelative (takeFileName x)
takeFileName :: FILEPATH_NAME -> FILEPATH_NAME
takeFileName (OSSTRING_NAME x) = OSSTRING_NAME $ C.takeFileName x


-- | Get the base name, without an extension or path.
--
-- > takeBaseName "/directory/file.ext" == "file"
-- > takeBaseName "file/test.txt" == "test"
-- > takeBaseName "dave.ext" == "dave"
-- > takeBaseName "" == ""
-- > takeBaseName "test" == "test"
-- > takeBaseName (addTrailingPathSeparator x) == ""
-- > takeBaseName "file/file.tar.gz" == "file.tar"
takeBaseName :: FILEPATH_NAME -> FILEPATH_NAME
takeBaseName (OSSTRING_NAME x) = OSSTRING_NAME $ C.takeBaseName x


-- | Set the base name.
--
-- > replaceBaseName "/directory/other.ext" "file" == "/directory/file.ext"
-- > replaceBaseName "file/test.txt" "bob" == "file/bob.txt"
-- > replaceBaseName "fred" "bill" == "bill"
-- > replaceBaseName "/dave/fred/bob.gz.tar" "new" == "/dave/fred/new.tar"
-- > Valid x => replaceBaseName x (takeBaseName x) == x
replaceBaseName :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
replaceBaseName (OSSTRING_NAME path) (OSSTRING_NAME name) = OSSTRING_NAME $ C.replaceBaseName path name


-- | Is an item either a directory or the last character a path separator?
--
-- > hasTrailingPathSeparator "test" == False
-- > hasTrailingPathSeparator "test/" == True
hasTrailingPathSeparator :: FILEPATH_NAME -> Bool
hasTrailingPathSeparator (OSSTRING_NAME x) = C.hasTrailingPathSeparator x


#ifdef WINDOWS_DOC
-- | Add a trailing file path separator if one is not already present.
--
-- > hasTrailingPathSeparator (addTrailingPathSeparator x)
-- > hasTrailingPathSeparator x ==> addTrailingPathSeparator x == x
#elif defined(POSIX_DOC)
-- | Add a trailing file path separator if one is not already present.
--
-- > hasTrailingPathSeparator (addTrailingPathSeparator x)
-- > hasTrailingPathSeparator x ==> addTrailingPathSeparator x == x
-- > addTrailingPathSeparator "test/rest" == "test/rest/"
#else
-- | Add a trailing file path separator if one is not already present.
--
-- > hasTrailingPathSeparator (addTrailingPathSeparator x)
-- > hasTrailingPathSeparator x ==> addTrailingPathSeparator x == x
-- > Posix:    addTrailingPathSeparator "test/rest" == "test/rest/"
#endif
addTrailingPathSeparator :: FILEPATH_NAME -> FILEPATH_NAME
addTrailingPathSeparator (OSSTRING_NAME bs) = OSSTRING_NAME $ C.addTrailingPathSeparator bs


#ifdef WINDOWS_DOC
-- | Remove any trailing path separators
--
-- > dropTrailingPathSeparator "file/test/" == "file/test"
-- > dropTrailingPathSeparator "/" == "/"
-- > dropTrailingPathSeparator "\\" == "\\"
#elif defined(POSIX_DOC)
-- | Remove any trailing path separators
--
-- > dropTrailingPathSeparator "file/test/" == "file/test"
-- > dropTrailingPathSeparator "/" == "/"
-- > not (hasTrailingPathSeparator (dropTrailingPathSeparator x)) || isDrive x
#else
-- | Remove any trailing path separators
--
-- > dropTrailingPathSeparator "file/test/" == "file/test"
-- >           dropTrailingPathSeparator "/" == "/"
-- > Windows:  dropTrailingPathSeparator "\\" == "\\"
-- > Posix:    not (hasTrailingPathSeparator (dropTrailingPathSeparator x)) || isDrive x
#endif
dropTrailingPathSeparator :: FILEPATH_NAME -> FILEPATH_NAME
dropTrailingPathSeparator (OSSTRING_NAME x) = OSSTRING_NAME $ C.dropTrailingPathSeparator x


#ifdef WINDOWS_DOC
-- | Get the directory name, move up one level.
--
-- > takeDirectory "/directory/other.ext" == "/directory"
-- > takeDirectory x `isPrefixOf` x || takeDirectory x == "."
-- > takeDirectory "foo" == "."
-- > takeDirectory "/" == "/"
-- > takeDirectory "/foo" == "/"
-- > takeDirectory "/foo/bar/baz" == "/foo/bar"
-- > takeDirectory "/foo/bar/baz/" == "/foo/bar/baz"
-- > takeDirectory "foo/bar/baz" == "foo/bar"
-- > takeDirectory "foo\\bar" == "foo"
-- > takeDirectory "foo\\bar\\\\" == "foo\\bar"
-- > takeDirectory "C:\\" == "C:\\"
#elif defined(POSIX_DOC)
-- | Get the directory name, move up one level.
--
-- >           takeDirectory "/directory/other.ext" == "/directory"
-- >           takeDirectory x `isPrefixOf` x || takeDirectory x == "."
-- >           takeDirectory "foo" == "."
-- >           takeDirectory "/" == "/"
-- >           takeDirectory "/foo" == "/"
-- >           takeDirectory "/foo/bar/baz" == "/foo/bar"
-- >           takeDirectory "/foo/bar/baz/" == "/foo/bar/baz"
-- >           takeDirectory "foo/bar/baz" == "foo/bar"
#else
-- | Get the directory name, move up one level.
--
-- >           takeDirectory "/directory/other.ext" == "/directory"
-- >           takeDirectory x `isPrefixOf` x || takeDirectory x == "."
-- >           takeDirectory "foo" == "."
-- >           takeDirectory "/" == "/"
-- >           takeDirectory "/foo" == "/"
-- >           takeDirectory "/foo/bar/baz" == "/foo/bar"
-- >           takeDirectory "/foo/bar/baz/" == "/foo/bar/baz"
-- >           takeDirectory "foo/bar/baz" == "foo/bar"
-- > Windows:  takeDirectory "foo\\bar" == "foo"
-- > Windows:  takeDirectory "foo\\bar\\\\" == "foo\\bar"
-- > Windows:  takeDirectory "C:\\" == "C:\\"
#endif
takeDirectory :: FILEPATH_NAME -> FILEPATH_NAME
takeDirectory (OSSTRING_NAME x) = OSSTRING_NAME $ C.takeDirectory x


-- | Set the directory, keeping the filename the same.
--
-- > replaceDirectory "root/file.ext" "/directory/" == "/directory/file.ext"
-- > Valid x => replaceDirectory x (takeDirectory x) `equalFilePath` x
replaceDirectory :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
replaceDirectory (OSSTRING_NAME file) (OSSTRING_NAME dir) = OSSTRING_NAME $ C.replaceDirectory file dir


-- | An alias for '</>'.
combine :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
combine (OSSTRING_NAME a) (OSSTRING_NAME b) = OSSTRING_NAME $ C.combine a b

#ifdef WINDOWS_DOC
-- | Combine two paths with a path separator.
--   If the second path starts with a path separator or a drive letter, then it returns the second.
--   The intention is that @readFile (dir '</>' file)@ will access the same file as
--   @setCurrentDirectory dir; readFile file@.
--
-- > "/directory" </> "file.ext" == "/directory\\file.ext"
-- > "directory" </> "/file.ext" == "/file.ext"
-- > Valid x => (takeDirectory x </> takeFileName x) `equalFilePath` x
--
--   Combined:
--
-- > "C:\\foo" </> "bar" == "C:\\foo\\bar"
-- > "home" </> "bob" == "home\\bob"
--
--   Not combined:
--
-- > "home" </> "C:\\bob" == "C:\\bob"
--
--   Not combined (tricky):
--
--   If a filepath starts with a single slash, it is relative to the
--   root of the current drive. In [1], this is (confusingly) referred to as an
--   absolute path.
--   The current behavior of '</>' is to never combine these forms.
--
-- > "home" </> "/bob" == "/bob"
-- > "home" </> "\\bob" == "\\bob"
-- > "C:\\home" </> "\\bob" == "\\bob"
--
--   From [1]: "If a file name begins with only a disk designator
--   but not the backslash after the colon, it is interpreted as a relative path
--   to the current directory on the drive with the specified letter."
--   The current behavior of '</>' is to never combine these forms.
--
-- > "D:\\foo" </> "C:bar" == "C:bar"
-- > "C:\\foo" </> "C:bar" == "C:bar"
#elif defined(POSIX_DOC)
-- | Combine two paths with a path separator.
--   If the second path starts with a path separator or a drive letter, then it returns the second.
--   The intention is that @readFile (dir '</>' file)@ will access the same file as
--   @setCurrentDirectory dir; readFile file@.
--
-- > "/directory" </> "file.ext" == "/directory/file.ext"
-- > Valid x => (takeDirectory x </> takeFileName x) `equalFilePath` x
--
--   Combined:
--
-- > "/" </> "test" == "/test"
-- > "home" </> "bob" == "home/bob"
-- > "x:" </> "foo" == "x:/foo"
--
--   Not combined:
--
-- > "home" </> "/bob" == "/bob"
#else
-- | Combine two paths with a path separator.
--   If the second path starts with a path separator or a drive letter, then it returns the second.
--   The intention is that @readFile (dir '</>' file)@ will access the same file as
--   @setCurrentDirectory dir; readFile file@.
--
-- > Posix:   "/directory" </> "file.ext" == "/directory/file.ext"
-- > Windows: "/directory" </> "file.ext" == "/directory\\file.ext"
-- >          "directory" </> "/file.ext" == "/file.ext"
-- > Valid x => (takeDirectory x </> takeFileName x) `equalFilePath` x
--
--   Combined:
--
-- > Posix:   "/" </> "test" == "/test"
-- > Posix:   "home" </> "bob" == "home/bob"
-- > Posix:   "x:" </> "foo" == "x:/foo"
-- > Windows: "C:\\foo" </> "bar" == "C:\\foo\\bar"
-- > Windows: "home" </> "bob" == "home\\bob"
--
--   Not combined:
--
-- > Posix:   "home" </> "/bob" == "/bob"
-- > Windows: "home" </> "C:\\bob" == "C:\\bob"
--
--   Not combined (tricky):
--
--   On Windows, if a filepath starts with a single slash, it is relative to the
--   root of the current drive. In [1], this is (confusingly) referred to as an
--   absolute path.
--   The current behavior of '</>' is to never combine these forms.
--
-- > Windows: "home" </> "/bob" == "/bob"
-- > Windows: "home" </> "\\bob" == "\\bob"
-- > Windows: "C:\\home" </> "\\bob" == "\\bob"
--
--   On Windows, from [1]: "If a file name begins with only a disk designator
--   but not the backslash after the colon, it is interpreted as a relative path
--   to the current directory on the drive with the specified letter."
--   The current behavior of '</>' is to never combine these forms.
--
-- > Windows: "D:\\foo" </> "C:bar" == "C:bar"
-- > Windows: "C:\\foo" </> "C:bar" == "C:bar"
#endif
(</>) :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
(</>) = combine


#ifdef WINDOWS_DOC
-- | Split a path by the directory separator.
--
-- > splitPath "/directory/file.ext" == ["/","directory/","file.ext"]
-- > concat (splitPath x) == x
-- > splitPath "test//item/" == ["test//","item/"]
-- > splitPath "test/item/file" == ["test/","item/","file"]
-- > splitPath "" == []
-- > splitPath "c:\\test\\path" == ["c:\\","test\\","path"]
#elif defined(POSIX_DOC)
-- | Split a path by the directory separator.
--
-- > splitPath "/directory/file.ext" == ["/","directory/","file.ext"]
-- > concat (splitPath x) == x
-- > splitPath "test//item/" == ["test//","item/"]
-- > splitPath "test/item/file" == ["test/","item/","file"]
-- > splitPath "" == []
-- > splitPath "/file/test" == ["/","file/","test"]
#else
-- | Split a path by the directory separator.
--
-- > splitPath "/directory/file.ext" == ["/","directory/","file.ext"]
-- > concat (splitPath x) == x
-- > splitPath "test//item/" == ["test//","item/"]
-- > splitPath "test/item/file" == ["test/","item/","file"]
-- > splitPath "" == []
-- > Windows: splitPath "c:\\test\\path" == ["c:\\","test\\","path"]
-- > Posix:   splitPath "/file/test" == ["/","file/","test"]
#endif
splitPath :: FILEPATH_NAME -> [FILEPATH_NAME]
splitPath (OSSTRING_NAME bs) = OSSTRING_NAME <$> C.splitPath bs

#ifdef WINDOWS_DOC
-- | Just as 'splitPath', but don't add the trailing slashes to each element.
--
-- > splitDirectories "/directory/file.ext" == ["/","directory","file.ext"]
-- > splitDirectories "test/file" == ["test","file"]
-- > splitDirectories "/test/file" == ["/","test","file"]
-- > splitDirectories "C:\\test\\file" == ["C:\\", "test", "file"]
-- > Valid x => joinPath (splitDirectories x) `equalFilePath` x
-- > splitDirectories "" == []
-- > splitDirectories "C:\\test\\\\\\file" == ["C:\\", "test", "file"]
-- > splitDirectories "/test///file" == ["/","test","file"]
#elif defined(POSIX_DOC)
-- | Just as 'splitPath', but don't add the trailing slashes to each element.
--
-- > splitDirectories "/directory/file.ext" == ["/","directory","file.ext"]
-- > splitDirectories "test/file" == ["test","file"]
-- > splitDirectories "/test/file" == ["/","test","file"]
-- > Valid x => joinPath (splitDirectories x) `equalFilePath` x
-- > splitDirectories "" == []
-- > splitDirectories "/test///file" == ["/","test","file"]
#else
-- | Just as 'splitPath', but don't add the trailing slashes to each element.
--
-- >          splitDirectories "/directory/file.ext" == ["/","directory","file.ext"]
-- >          splitDirectories "test/file" == ["test","file"]
-- >          splitDirectories "/test/file" == ["/","test","file"]
-- > Windows: splitDirectories "C:\\test\\file" == ["C:\\", "test", "file"]
-- >          Valid x => joinPath (splitDirectories x) `equalFilePath` x
-- >          splitDirectories "" == []
-- > Windows: splitDirectories "C:\\test\\\\\\file" == ["C:\\", "test", "file"]
-- >          splitDirectories "/test///file" == ["/","test","file"]
#endif
splitDirectories :: FILEPATH_NAME -> [FILEPATH_NAME]
splitDirectories (OSSTRING_NAME x) = OSSTRING_NAME <$> C.splitDirectories x

#ifdef WINDOWS_DOC
-- | Join path elements back together.
--
-- > joinPath z == foldr (</>) "" z
-- > joinPath ["/","directory/","file.ext"] == "/directory/file.ext"
-- > Valid x => joinPath (splitPath x) == x
-- > joinPath [] == ""
#elif defined(POSIX_DOC)
-- | Join path elements back together.
--
-- > joinPath z == foldr (</>) "" z
-- > joinPath ["/","directory/","file.ext"] == "/directory/file.ext"
-- > Valid x => joinPath (splitPath x) == x
-- > joinPath [] == ""
-- > joinPath ["test","file","path"] == "test/file/path"
#else
-- | Join path elements back together.
--
-- > joinPath z == foldr (</>) "" z
-- > joinPath ["/","directory/","file.ext"] == "/directory/file.ext"
-- > Valid x => joinPath (splitPath x) == x
-- > joinPath [] == ""
-- > Posix: joinPath ["test","file","path"] == "test/file/path"
#endif
joinPath :: [FILEPATH_NAME] -> FILEPATH_NAME
joinPath = foldr (</>) (OSSTRING_NAME mempty)









------------------------
-- File name manipulations


#ifdef WINDOWS_DOC
-- | Equality of two filepaths.
--   If you call @System.Directory.canonicalizePath@
--   first this has a much better chance of working.
--   Note that this doesn't follow symlinks or DOSNAM~1s.
--
-- Similar to 'normalise', this does not expand @".."@, because of symlinks.
--
-- > x == y ==> equalFilePath x y
-- > normalise x == normalise y ==> equalFilePath x y
-- > equalFilePath "foo" "foo/"
-- > not (equalFilePath "/a/../c" "/c")
-- > not (equalFilePath "foo" "/foo")
-- > equalFilePath "foo" "FOO"
-- > not (equalFilePath "C:" "C:/")
#elif defined(POSIX_DOC)
-- | Equality of two filepaths.
--   If you call @System.Directory.canonicalizePath@
--   first this has a much better chance of working.
--   Note that this doesn't follow symlinks or DOSNAM~1s.
--
-- Similar to 'normalise', this does not expand @".."@, because of symlinks.
--
-- > x == y ==> equalFilePath x y
-- > normalise x == normalise y ==> equalFilePath x y
-- > equalFilePath "foo" "foo/"
-- > not (equalFilePath "/a/../c" "/c")
-- > not (equalFilePath "foo" "/foo")
-- > not (equalFilePath "foo" "FOO")
#else
-- | Equality of two filepaths.
--   If you call @System.Directory.canonicalizePath@
--   first this has a much better chance of working.
--   Note that this doesn't follow symlinks or DOSNAM~1s.
--
-- Similar to 'normalise', this does not expand @".."@, because of symlinks.
--
-- >          x == y ==> equalFilePath x y
-- >          normalise x == normalise y ==> equalFilePath x y
-- >          equalFilePath "foo" "foo/"
-- >          not (equalFilePath "/a/../c" "/c")
-- >          not (equalFilePath "foo" "/foo")
-- > Posix:   not (equalFilePath "foo" "FOO")
-- > Windows: equalFilePath "foo" "FOO"
-- > Windows: not (equalFilePath "C:" "C:/")
#endif
equalFilePath :: FILEPATH_NAME -> FILEPATH_NAME -> Bool
equalFilePath (OSSTRING_NAME p1) (OSSTRING_NAME p2) = C.equalFilePath p1 p2

#ifdef WINDOWS_DOC
-- | Contract a filename, based on a relative path. Note that the resulting path
--   will never introduce @..@ paths, as the presence of symlinks means @..\/b@
--   may not reach @a\/b@ if it starts from @a\/c@. For a worked example see
--   <http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html this blog post>.
--
--   The corresponding @makeAbsolute@ function can be found in
--   @System.Directory@.
--
-- > makeRelative "/directory" "/directory/file.ext" == "file.ext"
-- > Valid x => makeRelative (takeDirectory x) x `equalFilePath` takeFileName x
-- > makeRelative x x == "."
-- > Valid x y => equalFilePath x y || (isRelative x && makeRelative y x == x) || equalFilePath (y </> makeRelative y x) x
-- > makeRelative "C:\\Home" "c:\\home\\bob" == "bob"
-- > makeRelative "C:\\Home" "c:/home/bob" == "bob"
-- > makeRelative "C:\\Home" "D:\\Home\\Bob" == "D:\\Home\\Bob"
-- > makeRelative "C:\\Home" "C:Home\\Bob" == "C:Home\\Bob"
-- > makeRelative "/Home" "/home/bob" == "bob"
-- > makeRelative "/" "//" == "//"
#elif defined(POSIX_DOC)
-- | Contract a filename, based on a relative path. Note that the resulting path
--   will never introduce @..@ paths, as the presence of symlinks means @..\/b@
--   may not reach @a\/b@ if it starts from @a\/c@. For a worked example see
--   <http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html this blog post>.
--
--   The corresponding @makeAbsolute@ function can be found in
--   @System.Directory@.
--
-- > makeRelative "/directory" "/directory/file.ext" == "file.ext"
-- > Valid x => makeRelative (takeDirectory x) x `equalFilePath` takeFileName x
-- > makeRelative x x == "."
-- > Valid x y => equalFilePath x y || (isRelative x && makeRelative y x == x) || equalFilePath (y </> makeRelative y x) x
-- > makeRelative "/Home" "/home/bob" == "/home/bob"
-- > makeRelative "/home/" "/home/bob/foo/bar" == "bob/foo/bar"
-- > makeRelative "/fred" "bob" == "bob"
-- > makeRelative "/file/test" "/file/test/fred" == "fred"
-- > makeRelative "/file/test" "/file/test/fred/" == "fred/"
-- > makeRelative "some/path" "some/path/a/b/c" == "a/b/c"
#else
-- | Contract a filename, based on a relative path. Note that the resulting path
--   will never introduce @..@ paths, as the presence of symlinks means @..\/b@
--   may not reach @a\/b@ if it starts from @a\/c@. For a worked example see
--   <http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html this blog post>.
--
--   The corresponding @makeAbsolute@ function can be found in
--   @System.Directory@.
--
-- >          makeRelative "/directory" "/directory/file.ext" == "file.ext"
-- >          Valid x => makeRelative (takeDirectory x) x `equalFilePath` takeFileName x
-- >          makeRelative x x == "."
-- >          Valid x y => equalFilePath x y || (isRelative x && makeRelative y x == x) || equalFilePath (y </> makeRelative y x) x
-- > Windows: makeRelative "C:\\Home" "c:\\home\\bob" == "bob"
-- > Windows: makeRelative "C:\\Home" "c:/home/bob" == "bob"
-- > Windows: makeRelative "C:\\Home" "D:\\Home\\Bob" == "D:\\Home\\Bob"
-- > Windows: makeRelative "C:\\Home" "C:Home\\Bob" == "C:Home\\Bob"
-- > Windows: makeRelative "/Home" "/home/bob" == "bob"
-- > Windows: makeRelative "/" "//" == "//"
-- > Posix:   makeRelative "/Home" "/home/bob" == "/home/bob"
-- > Posix:   makeRelative "/home/" "/home/bob/foo/bar" == "bob/foo/bar"
-- > Posix:   makeRelative "/fred" "bob" == "bob"
-- > Posix:   makeRelative "/file/test" "/file/test/fred" == "fred"
-- > Posix:   makeRelative "/file/test" "/file/test/fred/" == "fred/"
-- > Posix:   makeRelative "some/path" "some/path/a/b/c" == "a/b/c"
#endif
makeRelative :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
makeRelative (OSSTRING_NAME root) (OSSTRING_NAME path) = OSSTRING_NAME $ C.makeRelative root path

#ifdef WINDOWS_DOC
-- | Normalise a file
--
-- * \/\/ outside of the drive can be made blank
--
-- * \/ -> 'pathSeparator'
--
-- * .\/ -> \"\"
--
-- Does not remove @".."@, because of symlinks.
--
-- > normalise "c:\\file/bob\\" == "C:\\file\\bob\\"
-- > normalise "c:\\" == "C:\\"
-- > normalise "C:.\\" == "C:"
-- > normalise "\\\\server\\test" == "\\\\server\\test"
-- > normalise "//server/test" == "\\\\server\\test"
-- > normalise "c:/file" == "C:\\file"
-- > normalise "/file" == "\\file"
-- > normalise "\\" == "\\"
-- > normalise "/./" == "\\"
-- > normalise "." == "."
#elif defined(POSIX_DOC)
-- | Normalise a file
--
-- * \/\/ outside of the drive can be made blank
--
-- * \/ -> 'pathSeparator'
--
-- * .\/ -> \"\"
--
-- Does not remove @".."@, because of symlinks.
--
-- > normalise "/file/\\test////" == "/file/\\test/"
-- > normalise "/file/./test" == "/file/test"
-- > normalise "/test/file/../bob/fred/" == "/test/file/../bob/fred/"
-- > normalise "../bob/fred/" == "../bob/fred/"
-- > normalise "/a/../c" == "/a/../c"
-- > normalise "./bob/fred/" == "bob/fred/"
-- > normalise "." == "."
-- > normalise "./" == "./"
-- > normalise "./." == "./"
-- > normalise "/./" == "/"
-- > normalise "/" == "/"
-- > normalise "bob/fred/." == "bob/fred/"
-- > normalise "//home" == "/home"
#else
-- | Normalise a file
--
-- * \/\/ outside of the drive can be made blank
--
-- * \/ -> 'pathSeparator'
--
-- * .\/ -> \"\"
--
-- Does not remove @".."@, because of symlinks.
--
-- > Posix:   normalise "/file/\\test////" == "/file/\\test/"
-- > Posix:   normalise "/file/./test" == "/file/test"
-- > Posix:   normalise "/test/file/../bob/fred/" == "/test/file/../bob/fred/"
-- > Posix:   normalise "../bob/fred/" == "../bob/fred/"
-- > Posix:   normalise "/a/../c" == "/a/../c"
-- > Posix:   normalise "./bob/fred/" == "bob/fred/"
-- > Windows: normalise "c:\\file/bob\\" == "C:\\file\\bob\\"
-- > Windows: normalise "c:\\" == "C:\\"
-- > Windows: normalise "C:.\\" == "C:"
-- > Windows: normalise "\\\\server\\test" == "\\\\server\\test"
-- > Windows: normalise "//server/test" == "\\\\server\\test"
-- > Windows: normalise "c:/file" == "C:\\file"
-- > Windows: normalise "/file" == "\\file"
-- > Windows: normalise "\\" == "\\"
-- > Windows: normalise "/./" == "\\"
-- >          normalise "." == "."
-- > Posix:   normalise "./" == "./"
-- > Posix:   normalise "./." == "./"
-- > Posix:   normalise "/./" == "/"
-- > Posix:   normalise "/" == "/"
-- > Posix:   normalise "bob/fred/." == "bob/fred/"
-- > Posix:   normalise "//home" == "/home"
#endif
normalise :: FILEPATH_NAME -> FILEPATH_NAME
normalise (OSSTRING_NAME filepath) = OSSTRING_NAME $ C.normalise filepath


#ifdef WINDOWS_DOC
-- | Is a filepath valid, i.e. could you create a file like it? This function checks for invalid names,
--   and invalid characters, but does not check if length limits are exceeded, as these are typically
--   filesystem dependent.
--
-- > isValid "" == False
-- > isValid "\0" == False
-- > isValid "c:\\test" == True
-- > isValid "c:\\test:of_test" == False
-- > isValid "test*" == False
-- > isValid "c:\\test\\nul" == False
-- > isValid "c:\\test\\prn.txt" == False
-- > isValid "c:\\nul\\file" == False
-- > isValid "\\\\" == False
-- > isValid "\\\\\\foo" == False
-- > isValid "\\\\?\\D:file" == False
-- > isValid "foo\tbar" == False
-- > isValid "nul .txt" == False
-- > isValid " nul.txt" == True
#elif defined(POSIX_DOC)
-- | Is a filepath valid, i.e. could you create a file like it? This function checks for invalid names,
--   and invalid characters, but does not check if length limits are exceeded, as these are typically
--   filesystem dependent.
--
-- > isValid "" == False
-- > isValid "\0" == False
-- > isValid "/random_ path:*" == True
-- > isValid x == not (null x)
#else
-- | Is a filepath valid, i.e. could you create a file like it? This function checks for invalid names,
--   and invalid characters, but does not check if length limits are exceeded, as these are typically
--   filesystem dependent.
--
-- >          isValid "" == False
-- >          isValid "\0" == False
-- > Posix:   isValid "/random_ path:*" == True
-- > Posix:   isValid x == not (null x)
-- > Windows: isValid "c:\\test" == True
-- > Windows: isValid "c:\\test:of_test" == False
-- > Windows: isValid "test*" == False
-- > Windows: isValid "c:\\test\\nul" == False
-- > Windows: isValid "c:\\test\\prn.txt" == False
-- > Windows: isValid "c:\\nul\\file" == False
-- > Windows: isValid "\\\\" == False
-- > Windows: isValid "\\\\\\foo" == False
-- > Windows: isValid "\\\\?\\D:file" == False
-- > Windows: isValid "foo\tbar" == False
-- > Windows: isValid "nul .txt" == False
-- > Windows: isValid " nul.txt" == True
#endif
isValid :: FILEPATH_NAME -> Bool
isValid (OSSTRING_NAME filepath) = C.isValid filepath


#ifdef WINDOWS_DOC
-- | Take a filepath and make it valid; does not change already valid filepaths.
--
-- > isValid (makeValid x)
-- > isValid x ==> makeValid x == x
-- > makeValid "" == "_"
-- > makeValid "file\0name" == "file_name"
-- > makeValid "c:\\already\\/valid" == "c:\\already\\/valid"
-- > makeValid "c:\\test:of_test" == "c:\\test_of_test"
-- > makeValid "test*" == "test_"
-- > makeValid "c:\\test\\nul" == "c:\\test\\nul_"
-- > makeValid "c:\\test\\prn.txt" == "c:\\test\\prn_.txt"
-- > makeValid "c:\\test/prn.txt" == "c:\\test/prn_.txt"
-- > makeValid "c:\\nul\\file" == "c:\\nul_\\file"
-- > makeValid "\\\\\\foo" == "\\\\drive"
-- > makeValid "\\\\?\\D:file" == "\\\\?\\D:\\file"
-- > makeValid "nul .txt" == "nul _.txt"
#elif defined(POSIX_DOC)
-- | Take a filepath and make it valid; does not change already valid filepaths.
--
-- > isValid (makeValid x)
-- > isValid x ==> makeValid x == x
-- > makeValid "" == "_"
-- > makeValid "file\0name" == "file_name"
#else
-- | Take a filepath and make it valid; does not change already valid filepaths.
--
-- > isValid (makeValid x)
-- > isValid x ==> makeValid x == x
-- > makeValid "" == "_"
-- > makeValid "file\0name" == "file_name"
-- > Windows: makeValid "c:\\already\\/valid" == "c:\\already\\/valid"
-- > Windows: makeValid "c:\\test:of_test" == "c:\\test_of_test"
-- > Windows: makeValid "test*" == "test_"
-- > Windows: makeValid "c:\\test\\nul" == "c:\\test\\nul_"
-- > Windows: makeValid "c:\\test\\prn.txt" == "c:\\test\\prn_.txt"
-- > Windows: makeValid "c:\\test/prn.txt" == "c:\\test/prn_.txt"
-- > Windows: makeValid "c:\\nul\\file" == "c:\\nul_\\file"
-- > Windows: makeValid "\\\\\\foo" == "\\\\drive"
-- > Windows: makeValid "\\\\?\\D:file" == "\\\\?\\D:\\file"
-- > Windows: makeValid "nul .txt" == "nul _.txt"
#endif
makeValid :: FILEPATH_NAME -> FILEPATH_NAME
makeValid (OSSTRING_NAME path) = OSSTRING_NAME $ C.makeValid path


#ifdef WINDOWS_DOC
-- | Is a path relative, or is it fixed to the root?
--
-- > isRelative "path\\test" == True
-- > isRelative "c:\\test" == False
-- > isRelative "c:test" == True
-- > isRelative "c:\\" == False
-- > isRelative "c:/" == False
-- > isRelative "c:" == True
-- > isRelative "\\\\foo" == False
-- > isRelative "\\\\?\\foo" == False
-- > isRelative "\\\\?\\UNC\\foo" == False
-- > isRelative "/foo" == True
-- > isRelative "\\foo" == True
--
-- According to [1]:
--
-- * "A UNC name of any format [is never relative]."
--
-- * "You cannot use the "\\?\" prefix with a relative path."
#elif defined(POSIX_DOC)
-- | Is a path relative, or is it fixed to the root?
--
-- > isRelative "test/path" == True
-- > isRelative "/test" == False
-- > isRelative "/" == False
#else
-- | Is a path relative, or is it fixed to the root?
--
-- > Windows: isRelative "path\\test" == True
-- > Windows: isRelative "c:\\test" == False
-- > Windows: isRelative "c:test" == True
-- > Windows: isRelative "c:\\" == False
-- > Windows: isRelative "c:/" == False
-- > Windows: isRelative "c:" == True
-- > Windows: isRelative "\\\\foo" == False
-- > Windows: isRelative "\\\\?\\foo" == False
-- > Windows: isRelative "\\\\?\\UNC\\foo" == False
-- > Windows: isRelative "/foo" == True
-- > Windows: isRelative "\\foo" == True
-- > Posix:   isRelative "test/path" == True
-- > Posix:   isRelative "/test" == False
-- > Posix:   isRelative "/" == False
--
-- According to [1]:
--
-- * "A UNC name of any format [is never relative]."
--
-- * "You cannot use the "\\?\" prefix with a relative path."
#endif
isRelative :: FILEPATH_NAME -> Bool
isRelative (OSSTRING_NAME x) = C.isRelative x


-- | @not . 'isRelative'@
--
-- > isAbsolute x == not (isRelative x)
isAbsolute :: FILEPATH_NAME -> Bool
isAbsolute (OSSTRING_NAME x) = C.isAbsolute x
