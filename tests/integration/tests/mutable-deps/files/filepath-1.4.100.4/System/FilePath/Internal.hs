{- HLINT ignore -}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeApplications #-}

-- This template expects CPP definitions for:
--     MODULE_NAME = Posix | Windows
--     IS_WINDOWS  = False | True

-- |
-- Module      :  System.FilePath.MODULE_NAME
-- Copyright   :  (c) Neil Mitchell 2005-2014
-- License     :  BSD3
--
-- Maintainer  :  ndmitchell@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- A library for 'FilePath' manipulations, using MODULE_NAME style paths on
-- all platforms. Importing "System.FilePath" is usually better.
--
-- Given the example 'FilePath': @\/directory\/file.ext@
--
-- We can use the following functions to extract pieces.
--
-- * 'takeFileName' gives @\"file.ext\"@
--
-- * 'takeDirectory' gives @\"\/directory\"@
--
-- * 'takeExtension' gives @\".ext\"@
--
-- * 'dropExtension' gives @\"\/directory\/file\"@
--
-- * 'takeBaseName' gives @\"file\"@
--
-- And we could have built an equivalent path with the following expressions:
--
-- * @\"\/directory\" '</>' \"file.ext\"@.
--
-- * @\"\/directory\/file" '<.>' \"ext\"@.
--
-- * @\"\/directory\/file.txt" '-<.>' \"ext\"@.
--
-- Each function in this module is documented with several examples,
-- which are also used as tests.
--
-- Here are a few examples of using the @filepath@ functions together:
--
-- /Example 1:/ Find the possible locations of a Haskell module @Test@ imported from module @Main@:
--
-- @['replaceFileName' path_to_main \"Test\" '<.>' ext | ext <- [\"hs\",\"lhs\"] ]@
--
-- /Example 2:/ Download a file from @url@ and save it to disk:
--
-- @do let file = 'makeValid' url
--   System.Directory.createDirectoryIfMissing True ('takeDirectory' file)@
--
-- /Example 3:/ Compile a Haskell file, putting the @.hi@ file under @interface@:
--
-- @'takeDirectory' file '</>' \"interface\" '</>' ('takeFileName' file '-<.>' \"hi\")@
--
-- References:
-- [1] <http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247.aspx Naming Files, Paths and Namespaces> (Microsoft MSDN)
#ifndef OS_PATH
module System.FilePath.MODULE_NAME
#else
module System.OsPath.MODULE_NAME.Internal
#endif
    (
    -- * Separator predicates
#ifndef OS_PATH
    FilePath,
#endif
    pathSeparator, pathSeparators, isPathSeparator,
    searchPathSeparator, isSearchPathSeparator,
    extSeparator, isExtSeparator,

    -- * @$PATH@ methods
    splitSearchPath,
#ifndef OS_PATH
    getSearchPath,
#endif

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

{- HLINT ignore "Use fewer imports" -}
import Prelude (Char, Bool(..), Maybe(..), (.), (&&), (<=), not, fst, maybe, (||), (==), ($), otherwise, fmap, mempty, (>=), (/=), (++), snd)
import Data.Bifunctor (first)
import Data.Semigroup ((<>))
import qualified Prelude as P
import Data.Maybe(fromMaybe, isJust)
import qualified Data.List as L

#ifndef OS_PATH
import Data.String (fromString)
import System.Environment(getEnv)
import Prelude (String, map, FilePath, Eq, IO, id, last, init, reverse, dropWhile, null, break, take, all, elem, any, span)
import Data.Char(toLower, toUpper, isAsciiLower, isAsciiUpper)
import Data.List(stripPrefix, isSuffixOf, uncons, dropWhileEnd)
#define CHAR Char
#define STRING String
#define FILEPATH FilePath
#else
import Prelude (fromIntegral)
import Control.Exception ( SomeException, evaluate, try, displayException )
import Control.DeepSeq (force)
import GHC.IO (unsafePerformIO)
import qualified Data.Char as C
#ifdef WINDOWS
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import qualified GHC.Foreign as GHC
import Data.Word ( Word16 )
import System.OsPath.Data.ByteString.Short.Word16
import System.OsPath.Data.ByteString.Short ( packCStringLen )
#define CHAR Word16
#define STRING ShortByteString
#define FILEPATH ShortByteString
#else
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import Data.Word ( Word8 )
import System.OsPath.Data.ByteString.Short
#define CHAR Word8
#define STRING ShortByteString
#define FILEPATH ShortByteString
#endif
#endif


infixr 7  <.>, -<.>
infixr 5  </>





---------------------------------------------------------------------
-- Platform Abstraction Methods (private)

-- | Is the operating system Unix or Linux like
isPosix :: Bool
isPosix = not isWindows

-- | Is the operating system Windows like
isWindows :: Bool
isWindows = IS_WINDOWS


---------------------------------------------------------------------
-- The basic functions

-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- > Windows: pathSeparator == '\\'
-- > Posix:   pathSeparator ==  '/'
-- > isPathSeparator pathSeparator
pathSeparator :: CHAR
pathSeparator = if isWindows then _backslash else _slash

-- | The list of all possible separators.
--
-- > Windows: pathSeparators == ['\\', '/']
-- > Posix:   pathSeparators == ['/']
-- > pathSeparator `elem` pathSeparators
pathSeparators :: [CHAR]
pathSeparators = if isWindows then [_backslash, _slash] else [_slash]

-- | Rather than using @(== 'pathSeparator')@, use this. Test if something
--   is a path separator.
--
-- > isPathSeparator a == (a `elem` pathSeparators)
isPathSeparator :: CHAR -> Bool
isPathSeparator c
  | c == _slash = True
  | c == _backslash = isWindows
  | otherwise = False


-- | The character that is used to separate the entries in the $PATH environment variable.
--
-- > Windows: searchPathSeparator == ';'
-- > Posix:   searchPathSeparator == ':'
searchPathSeparator :: CHAR
searchPathSeparator = if isWindows then _semicolon else _colon

-- | Is the character a file separator?
--
-- > isSearchPathSeparator a == (a == searchPathSeparator)
isSearchPathSeparator :: CHAR -> Bool
isSearchPathSeparator = (== searchPathSeparator)


-- | File extension character
--
-- > extSeparator == '.'
extSeparator :: CHAR
extSeparator = _period

-- | Is the character an extension character?
--
-- > isExtSeparator a == (a == extSeparator)
isExtSeparator :: CHAR -> Bool
isExtSeparator = (== extSeparator)


---------------------------------------------------------------------
-- Path methods (environment $PATH)

-- | Take a string, split it on the 'searchPathSeparator' character.
--   Blank items are ignored on Windows, and converted to @.@ on Posix.
--   On Windows path elements are stripped of quotes.
--
--   Follows the recommendations in
--   <http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap08.html>
--
-- > Posix:   splitSearchPath "File1:File2:File3"  == ["File1","File2","File3"]
-- > Posix:   splitSearchPath "File1::File2:File3" == ["File1",".","File2","File3"]
-- > Windows: splitSearchPath "File1;File2;File3"  == ["File1","File2","File3"]
-- > Windows: splitSearchPath "File1;;File2;File3" == ["File1","File2","File3"]
-- > Windows: splitSearchPath "File1;\"File2\";File3" == ["File1","File2","File3"]
splitSearchPath :: STRING -> [FILEPATH]
splitSearchPath = f
    where
    f xs = let (pre, post) = break isSearchPathSeparator xs
           in case uncons post of
             Nothing     -> g pre
             Just (_, t) -> g pre ++ f t

    g x = case uncons x of
      Nothing -> [singleton _period | isPosix]
      Just (h, t)
        | h == _quotedbl
        , (Just _) <- uncons t -- >= 2
        , isWindows
        , (Just (i, l)) <- unsnoc t
        , l == _quotedbl -> [i]
        | otherwise -> [x]


-- TODO for AFPP
#ifndef OS_PATH
-- | Get a list of 'FILEPATH's in the $PATH variable.
getSearchPath :: IO [FILEPATH]
getSearchPath = fmap splitSearchPath (getEnv "PATH")
#endif


---------------------------------------------------------------------
-- Extension methods

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

-- A naive implementation would be to use @splitFileName_@ first,
-- then break filename into basename and extension, then recombine dir and basename.
-- This is way too expensive, see @splitFileName_@ comment for discussion.
--
-- Instead we speculatively split on the extension separator first, then check
-- whether results are well-formed.
splitExtension :: FILEPATH -> (STRING, STRING)
splitExtension x
  -- Imagine x = "no-dots", then nameDot = ""
  | null nameDot = (x, mempty)
  -- Imagine x = "\\shared.with.dots\no-dots"
  | isWindows && null (dropDrive nameDot) = (x, mempty)
  -- Imagine x = "dir.with.dots/no-dots"
  | any isPathSeparator ext = (x, mempty)
  | otherwise = (init nameDot, extSeparator `cons` ext)
  where
    (nameDot, ext) = breakEnd isExtSeparator x

-- | Get the extension of a file, returns @\"\"@ for no extension, @.ext@ otherwise.
--
-- > takeExtension "/directory/path.ext" == ".ext"
-- > takeExtension x == snd (splitExtension x)
-- > Valid x => takeExtension (addExtension x "ext") == ".ext"
-- > Valid x => takeExtension (replaceExtension x "ext") == ".ext"
takeExtension :: FILEPATH -> STRING
takeExtension = snd . splitExtension

-- | Remove the current extension and add another, equivalent to 'replaceExtension'.
--
-- > "/directory/path.txt" -<.> "ext" == "/directory/path.ext"
-- > "/directory/path.txt" -<.> ".ext" == "/directory/path.ext"
-- > "foo.o" -<.> "c" == "foo.c"
(-<.>) :: FILEPATH -> STRING -> FILEPATH
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
replaceExtension :: FILEPATH -> STRING -> FILEPATH
replaceExtension x y = dropExtension x <.> y

-- | Add an extension, even if there is already one there, equivalent to 'addExtension'.
--
-- > "/directory/path" <.> "ext" == "/directory/path.ext"
-- > "/directory/path" <.> ".ext" == "/directory/path.ext"
(<.>) :: FILEPATH -> STRING -> FILEPATH
(<.>) = addExtension

-- | Remove last extension, and the \".\" preceding it.
--
-- > dropExtension "/directory/path.ext" == "/directory/path"
-- > dropExtension x == fst (splitExtension x)
dropExtension :: FILEPATH -> FILEPATH
dropExtension = fst . splitExtension

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
addExtension :: FILEPATH -> STRING -> FILEPATH
addExtension file xs = case uncons xs of
  Nothing -> file
  Just (x, _) -> joinDrive a res
    where
        res = if isExtSeparator x then b <> xs
              else b <> (extSeparator `cons` xs)

        (a,b) = splitDrive file

-- | Does the given filename have an extension?
--
-- > hasExtension "/directory/path.ext" == True
-- > hasExtension "/directory/path" == False
-- > null (takeExtension x) == not (hasExtension x)
hasExtension :: FILEPATH -> Bool
hasExtension = any isExtSeparator . takeFileName


-- | Does the given filename have the specified extension?
--
-- > "png" `isExtensionOf` "/directory/file.png" == True
-- > ".png" `isExtensionOf` "/directory/file.png" == True
-- > ".tar.gz" `isExtensionOf` "bar/foo.tar.gz" == True
-- > "ar.gz" `isExtensionOf` "bar/foo.tar.gz" == False
-- > "png" `isExtensionOf` "/directory/file.png.jpg" == False
-- > "csv/table.csv" `isExtensionOf` "/data/csv/table.csv" == False
isExtensionOf :: STRING -> FILEPATH -> Bool
isExtensionOf ext = \fp -> case uncons ext of
  Just (x, _)
    | x == _period -> isSuffixOf ext . takeExtensions $ fp
  _ -> isSuffixOf (_period `cons` ext) . takeExtensions $ fp

-- | Drop the given extension from a FILEPATH, and the @\".\"@ preceding it.
--   Returns 'Nothing' if the FILEPATH does not have the given extension, or
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
stripExtension :: STRING -> FILEPATH -> Maybe FILEPATH
stripExtension ext path = case uncons ext of
  Just (x, _) -> let dotExt = if isExtSeparator x then ext else _period `cons` ext
                 in stripSuffix dotExt path
  Nothing -> Just path


-- | Split on all extensions.
--
-- > splitExtensions "/directory/path.ext" == ("/directory/path",".ext")
-- > splitExtensions "file.tar.gz" == ("file",".tar.gz")
-- > uncurry (<>) (splitExtensions x) == x
-- > Valid x => uncurry addExtension (splitExtensions x) == x
-- > splitExtensions "file.tar.gz" == ("file",".tar.gz")
splitExtensions :: FILEPATH -> (FILEPATH, STRING)
splitExtensions x = (a <> c, d)
    where
        (a,b) = splitFileName_ x
        (c,d) = break isExtSeparator b

-- | Drop all extensions.
--
-- > dropExtensions "/directory/path.ext" == "/directory/path"
-- > dropExtensions "file.tar.gz" == "file"
-- > not $ hasExtension $ dropExtensions x
-- > not $ any isExtSeparator $ takeFileName $ dropExtensions x
dropExtensions :: FILEPATH -> FILEPATH
dropExtensions = fst . splitExtensions

-- | Get all extensions.
--
-- > takeExtensions "/directory/path.ext" == ".ext"
-- > takeExtensions "file.tar.gz" == ".tar.gz"
takeExtensions :: FILEPATH -> STRING
takeExtensions = snd . splitExtensions


-- | Replace all extensions of a file with a new extension. Note
--   that 'replaceExtension' and 'addExtension' both work for adding
--   multiple extensions, so only required when you need to drop
--   all extensions first.
--
-- > replaceExtensions "file.fred.bob" "txt" == "file.txt"
-- > replaceExtensions "file.fred.bob" "tar.gz" == "file.tar.gz"
replaceExtensions :: FILEPATH -> STRING -> FILEPATH
replaceExtensions x y = dropExtensions x <.> y



---------------------------------------------------------------------
-- Drive methods

-- | Is the given character a valid drive letter?
-- only a-z and A-Z are letters, not isAlpha which is more unicodey
isLetter :: CHAR -> Bool
isLetter x = isAsciiLower x || isAsciiUpper x


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
splitDrive :: FILEPATH -> (FILEPATH, FILEPATH)
splitDrive x | isPosix = span (== _slash) x
splitDrive x | Just y <- readDriveLetter x = y
splitDrive x | Just y <- readDriveUNC x = y
splitDrive x | Just y <- readDriveShare x = y
splitDrive x = (mempty, x)

addSlash :: FILEPATH -> FILEPATH -> (FILEPATH, FILEPATH)
addSlash a xs = (a <> c, d)
    where (c, d) = span isPathSeparator xs

-- See [1].
-- "\\?\D:\<path>" or "\\?\UNC\<server>\<share>"
readDriveUNC :: FILEPATH -> Maybe (FILEPATH, FILEPATH)
readDriveUNC bs = case unpack bs of
  (s1:s2:q:s3:xs)
    | q == _question && L.all isPathSeparator [s1,s2,s3] ->
      case L.map toUpper xs of
          (u:n:c:s4:_)
            | u == _U && n == _N && c == _C && isPathSeparator s4 ->
              let (a,b) = readDriveShareName (pack (L.drop 4 xs))
              in Just (pack (s1:s2:_question:s3:L.take 4 xs) <> a, b)
          _ -> case readDriveLetter (pack xs) of
                   -- Extended-length path.
                   Just (a,b) -> Just (pack [s1,s2,_question,s3] <> a, b)
                   Nothing -> Nothing
  _ -> Nothing

{- c:\ -}
readDriveLetter :: STRING -> Maybe (FILEPATH, FILEPATH)
readDriveLetter bs = case uncons2 bs of
  Nothing -> Nothing
  Just (x, c, ys)
    | isLetter x, c == _colon -> Just $ case uncons ys of
      Just (y, _)
        | isPathSeparator y -> addSlash (pack [x,_colon]) ys
      _ -> (pack [x,_colon], ys)
    | otherwise -> Nothing

{- \\sharename\ -}
readDriveShare :: STRING -> Maybe (FILEPATH, FILEPATH)
readDriveShare bs = case unpack bs of
  (s1:s2:xs) | isPathSeparator s1 && isPathSeparator s2 ->
    let (a, b) = readDriveShareName (pack xs)
    in Just (s1 `cons` (s2 `cons` a), b)
  _ -> Nothing

{- assume you have already seen \\ -}
{- share\bob -> "share\", "bob" -}
readDriveShareName :: STRING -> (FILEPATH, FILEPATH)
readDriveShareName name = addSlash a b
    where (a,b) = break isPathSeparator name



-- | Join a drive and the rest of the path.
--
-- > Valid x => uncurry joinDrive (splitDrive x) == x
-- > Windows: joinDrive "C:" "foo" == "C:foo"
-- > Windows: joinDrive "C:\\" "bar" == "C:\\bar"
-- > Windows: joinDrive "\\\\share" "foo" == "\\\\share\\foo"
-- > Windows: joinDrive "/:" "foo" == "/:\\foo"
joinDrive :: FILEPATH -> FILEPATH -> FILEPATH
joinDrive = combineAlways

-- | Get the drive from a filepath.
--
-- > takeDrive x == fst (splitDrive x)
takeDrive :: FILEPATH -> FILEPATH
takeDrive = fst . splitDrive

-- | Delete the drive, if it exists.
--
-- > dropDrive x == snd (splitDrive x)
dropDrive :: FILEPATH -> FILEPATH
dropDrive = snd . splitDrive

-- | Does a path have a drive.
--
-- > not (hasDrive x) == null (takeDrive x)
-- > Posix:   hasDrive "/foo" == True
-- > Windows: hasDrive "C:\\foo" == True
-- > Windows: hasDrive "C:foo" == True
-- >          hasDrive "foo" == False
-- >          hasDrive "" == False
hasDrive :: FILEPATH -> Bool
hasDrive = not . null . takeDrive


-- | Is an element a drive
--
-- > Posix:   isDrive "/" == True
-- > Posix:   isDrive "/foo" == False
-- > Windows: isDrive "C:\\" == True
-- > Windows: isDrive "C:\\foo" == False
-- >          isDrive "" == False
isDrive :: FILEPATH -> Bool
isDrive x = not (null x) && null (dropDrive x)


---------------------------------------------------------------------
-- Operations on a filepath, as a list of directories

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
-- > Windows: splitFileName "\\\\?\\A:\\fred" == ("\\\\?\\A:\\","fred")
splitFileName :: FILEPATH -> (STRING, STRING)
splitFileName x = if null path
    then (dotSlash, file)
    else (path, file)
  where
    (path, file) = splitFileName_ x
    dotSlash = _period `cons` singleton _slash

-- version of splitFileName where, if the FILEPATH has no directory
-- component, the returned directory is "" rather than "./".  This
-- is used in cases where we are going to combine the returned
-- directory to make a valid FILEPATH, and having a "./" appear would
-- look strange and upset simple equality properties.  See
-- e.g. replaceFileName.
--
-- A naive implementation is
--
-- splitFileName_ fp = (drv <> dir, file)
--   where
--     (drv, pth) = splitDrive fp
--     (dir, file) = breakEnd isPathSeparator pth
--
-- but it is undesirable for two reasons:
-- * splitDrive is very slow on Windows,
-- * we unconditionally allocate 5 FilePath objects where only 2 would normally suffice.
--
-- In the implementation below we first speculatively split the input by the last path
-- separator. In the vast majority of cases this is already the answer, except
-- two exceptional cases explained below.
--
splitFileName_ :: FILEPATH -> (STRING, STRING)
splitFileName_ fp
  -- If dirSlash is empty, @fp@ is either a genuine filename without any dir,
  -- or just a Windows drive name without slash like "c:".
  -- Run readDriveLetter to figure out.
  | isWindows
  , null dirSlash
  = fromMaybe (mempty, fp) (readDriveLetter fp)
  -- Another Windows quirk is that @fp@ could have been a shared drive "\\share"
  -- or UNC location "\\?\UNC\foo", where path separator is a part of the drive name.
  -- We can test this by trying dropDrive and falling back to splitDrive.
  | isWindows
  , Just (s1, _s2, bs') <- uncons2 dirSlash
  , isPathSeparator s1
  -- If bs' is empty, then s2 as the last character of dirSlash must be a path separator,
  -- so we are in the middle of shared drive.
  -- Otherwise, since s1 is a path separator, we might be in the middle of UNC path.
  , null bs' || maybe False isIncompleteUNC (readDriveUNC dirSlash)
  = (fp, mempty)
  | otherwise
  = (dirSlash, file)
  where
    (dirSlash, file) = breakEnd isPathSeparator fp

    isIncompleteUNC (pref, suff) = null suff && not (hasPenultimateColon pref)
    hasPenultimateColon = maybe False (maybe False ((== _colon) . snd) . unsnoc . fst) . unsnoc

-- | Set the filename.
--
-- > replaceFileName "/directory/other.txt" "file.ext" == "/directory/file.ext"
-- > Valid x => replaceFileName x (takeFileName x) == x
replaceFileName :: FILEPATH -> STRING -> FILEPATH
replaceFileName x y = a </> y where (a,_) = splitFileName_ x

-- | Drop the filename. Unlike 'takeDirectory', this function will leave
--   a trailing path separator on the directory.
--
-- > dropFileName "/directory/file.ext" == "/directory/"
-- > dropFileName x == fst (splitFileName x)
dropFileName :: FILEPATH -> FILEPATH
dropFileName = fst . splitFileName


-- | Get the file name.
--
-- > takeFileName "/directory/file.ext" == "file.ext"
-- > takeFileName "test/" == ""
-- > isSuffixOf (takeFileName x) x
-- > takeFileName x == snd (splitFileName x)
-- > Valid x => takeFileName (replaceFileName x "fred") == "fred"
-- > Valid x => takeFileName (x </> "fred") == "fred"
-- > Valid x => isRelative (takeFileName x)
takeFileName :: FILEPATH -> FILEPATH
takeFileName = snd . splitFileName

-- | Get the base name, without an extension or path.
--
-- > takeBaseName "/directory/file.ext" == "file"
-- > takeBaseName "file/test.txt" == "test"
-- > takeBaseName "dave.ext" == "dave"
-- > takeBaseName "" == ""
-- > takeBaseName "test" == "test"
-- > takeBaseName (addTrailingPathSeparator x) == ""
-- > takeBaseName "file/file.tar.gz" == "file.tar"
takeBaseName :: FILEPATH -> STRING
takeBaseName = dropExtension . takeFileName

-- | Set the base name.
--
-- > replaceBaseName "/directory/other.ext" "file" == "/directory/file.ext"
-- > replaceBaseName "file/test.txt" "bob" == "file/bob.txt"
-- > replaceBaseName "fred" "bill" == "bill"
-- > replaceBaseName "/dave/fred/bob.gz.tar" "new" == "/dave/fred/new.tar"
-- > Valid x => replaceBaseName x (takeBaseName x) == x
replaceBaseName :: FILEPATH -> STRING -> FILEPATH
replaceBaseName pth nam = combineAlways a (nam <.> ext)
    where
        (a,b) = splitFileName_ pth
        ext = takeExtension b

-- | Is an item either a directory or the last character a path separator?
--
-- > hasTrailingPathSeparator "test" == False
-- > hasTrailingPathSeparator "test/" == True
hasTrailingPathSeparator :: FILEPATH -> Bool
hasTrailingPathSeparator x
  | null x = False
  | otherwise = isPathSeparator $ last x


hasLeadingPathSeparator :: FILEPATH -> Bool
hasLeadingPathSeparator = maybe False (isPathSeparator . fst) . uncons


-- | Add a trailing file path separator if one is not already present.
--
-- > hasTrailingPathSeparator (addTrailingPathSeparator x)
-- > hasTrailingPathSeparator x ==> addTrailingPathSeparator x == x
-- > Posix:    addTrailingPathSeparator "test/rest" == "test/rest/"
addTrailingPathSeparator :: FILEPATH -> FILEPATH
addTrailingPathSeparator x = if hasTrailingPathSeparator x then x else x <> singleton pathSeparator


-- | Remove any trailing path separators
--
-- > dropTrailingPathSeparator "file/test/" == "file/test"
-- >           dropTrailingPathSeparator "/" == "/"
-- > Windows:  dropTrailingPathSeparator "\\" == "\\"
-- > Posix:    not (hasTrailingPathSeparator (dropTrailingPathSeparator x)) || isDrive x
dropTrailingPathSeparator :: FILEPATH -> FILEPATH
dropTrailingPathSeparator x =
    if hasTrailingPathSeparator x && not (isDrive x)
    then let x' = dropWhileEnd isPathSeparator x
         in if null x' then singleton (last x) else x'
    else x


-- | Get the directory name, move up one level.
--
-- >           takeDirectory "/directory/other.ext" == "/directory"
-- >           isPrefixOf (takeDirectory x) x || takeDirectory x == "."
-- >           takeDirectory "foo" == "."
-- >           takeDirectory "/" == "/"
-- >           takeDirectory "/foo" == "/"
-- >           takeDirectory "/foo/bar/baz" == "/foo/bar"
-- >           takeDirectory "/foo/bar/baz/" == "/foo/bar/baz"
-- >           takeDirectory "foo/bar/baz" == "foo/bar"
-- > Windows:  takeDirectory "foo\\bar" == "foo"
-- > Windows:  takeDirectory "foo\\bar\\\\" == "foo\\bar"
-- > Windows:  takeDirectory "C:\\" == "C:\\"
takeDirectory :: FILEPATH -> FILEPATH
takeDirectory = dropTrailingPathSeparator . dropFileName

-- | Set the directory, keeping the filename the same.
--
-- > replaceDirectory "root/file.ext" "/directory/" == "/directory/file.ext"
-- > Valid x => replaceDirectory x (takeDirectory x) `equalFilePath` x
replaceDirectory :: FILEPATH -> STRING -> FILEPATH
replaceDirectory x dir = combineAlways dir (takeFileName x)


-- | An alias for '</>'.
combine :: FILEPATH -> FILEPATH -> FILEPATH
combine a b | hasLeadingPathSeparator b || hasDrive b = b
            | otherwise = combineAlways a b

-- | Combine two paths, assuming rhs is NOT absolute.
combineAlways :: FILEPATH -> FILEPATH -> FILEPATH
combineAlways a b | null a = b
                  | null b = a
                  | hasTrailingPathSeparator a = a <> b
                  | otherwise = case unpack a of
                      [a1, a2] | isWindows
                               , isLetter a1
                               , a2 == _colon -> a <> b
                      _ -> a <> (pathSeparator `cons` b)


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
(</>) :: FILEPATH -> FILEPATH -> FILEPATH
(</>) = combine


-- | Split a path by the directory separator.
--
-- > splitPath "/directory/file.ext" == ["/","directory/","file.ext"]
-- > concat (splitPath x) == x
-- > splitPath "test//item/" == ["test//","item/"]
-- > splitPath "test/item/file" == ["test/","item/","file"]
-- > splitPath "" == []
-- > Windows: splitPath "c:\\test\\path" == ["c:\\","test\\","path"]
-- > Posix:   splitPath "/file/test" == ["/","file/","test"]
splitPath :: FILEPATH -> [FILEPATH]
splitPath x = [drive | not (null drive)] ++ f path
    where
        (drive, path) = splitDrive x

        f y
          | null y = []
          | otherwise = (a <> c) : f d
            where
                (a,b) = break isPathSeparator y
                (c,d) = span  isPathSeparator b

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
splitDirectories :: FILEPATH -> [FILEPATH]
splitDirectories = L.map dropTrailingPathSeparator . splitPath


-- | Join path elements back together.
--
-- > joinPath z == foldr (</>) "" z
-- > joinPath ["/","directory/","file.ext"] == "/directory/file.ext"
-- > Valid x => joinPath (splitPath x) == x
-- > joinPath [] == ""
-- > Posix: joinPath ["test","file","path"] == "test/file/path"
joinPath :: [FILEPATH] -> FILEPATH
-- Note that this definition on c:\\c:\\, join then split will give c:\\.
joinPath = P.foldr combine mempty






---------------------------------------------------------------------
-- File name manipulators

-- | Equality of two 'FILEPATH's.
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
equalFilePath :: FILEPATH -> FILEPATH -> Bool
equalFilePath a b = f a == f b
    where
        f x | isWindows = dropTrailingPathSeparator $ map toLower $ normalise x
            | otherwise = dropTrailingPathSeparator $ normalise x


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
makeRelative :: FILEPATH -> FILEPATH -> FILEPATH
makeRelative root path
  | equalFilePath root path = singleton _period
  | takeAbs root /= takeAbs path = path
  | otherwise = f (dropAbs root) (dropAbs path)
  where
    f x y
      | null x = dropWhile isPathSeparator y
      | otherwise = let (x1,x2) = g x
                        (y1,y2) = g y
                    in if equalFilePath x1 y1 then f x2 y2 else path
    g x = (dropWhile isPathSeparator a, dropWhile isPathSeparator b)
      where (a, b) = break isPathSeparator $ dropWhile isPathSeparator x

    -- on windows, need to drop '/' which is kind of absolute, but not a drive
    dropAbs x
      | Just (hd, tl) <- uncons x
      , isPathSeparator hd
      , not (hasDrive x)
      = tl
      | otherwise
      = dropDrive x

    takeAbs x
      | Just (hd, _) <- uncons x
      , isPathSeparator hd
      , not (hasDrive x)
      = singleton pathSeparator
      | otherwise
      = map (\y -> if isPathSeparator y then pathSeparator else toLower y) $ takeDrive x

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
-- > Windows: normalise "c:\\\\\\\\" == "C:\\"
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
normalise :: FILEPATH -> FILEPATH
normalise filepath =
  result <>
  (if addPathSeparator
       then singleton pathSeparator
       else mempty)
  where
    (drv,pth) = splitDrive filepath

    result = joinDrive' (normaliseDrive drv) (f pth)

    joinDrive' d p
      = if null d && null p
           then singleton _period
           else joinDrive d p

    addPathSeparator = isDirPath pth
      && not (hasTrailingPathSeparator result)
      && not (isRelativeDrive drv)

    isDirPath xs = hasTrailingPathSeparator xs
        || not (null xs) && last xs == _period
           && hasTrailingPathSeparator (init xs)

    f = joinPath . dropDots . propSep . splitDirectories

    propSep (x:xs)
      | all isPathSeparator x = singleton pathSeparator : xs
      | otherwise                   = x : xs
    propSep [] = []

    dropDots = L.filter (singleton _period /=)

normaliseDrive :: FILEPATH -> FILEPATH
normaliseDrive bs
  | null bs = mempty
  | isPosix = pack [pathSeparator]
  | Just (drv, _) <- readDriveLetter x2
                  = case unpack drv of
                      (x:_:[]) -> pack [toUpper x, _colon]
                      (x:_) -> pack [toUpper x, _colon, pathSeparator]
                      _ -> P.error "impossible"
  | otherwise = x2
    where
        x2 = map repSlash bs
        repSlash x = if isPathSeparator x then pathSeparator else x

-- Information for validity functions on Windows. See [1].
isBadCHARacter :: CHAR -> Bool
isBadCHARacter x = x >= _nul && x <= _US
  || x `L.elem`
      [ _less
      , _greater
      , _colon
      , _quotedbl
      , _bar
      , _question
      , _asterisk
      ]

badElements :: [FILEPATH]
badElements = fmap fromString
    ["CON","PRN","AUX","NUL","CLOCK$"
    ,"COM1","COM2","COM3","COM4","COM5","COM6","COM7","COM8","COM9"
    ,"LPT1","LPT2","LPT3","LPT4","LPT5","LPT6","LPT7","LPT8","LPT9"]


-- | Is a FILEPATH valid, i.e. could you create a file like it? This function checks for invalid names,
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
isValid :: FILEPATH -> Bool
isValid path
  | null path = False
  | _nul `elem` path = False
  | isPosix = True
  | otherwise =
      not (any isBadCHARacter x2) &&
      not (L.any f $ splitDirectories x2) &&
      not (isJust (readDriveShare x1) && all isPathSeparator x1) &&
      not (isJust (readDriveUNC x1) && not (hasTrailingPathSeparator x1))
    where
      (x1,x2) = splitDrive path
      f x = map toUpper (dropWhileEnd (== _space) $ dropExtensions x) `L.elem` badElements


-- | Take a FILEPATH and make it valid; does not change already valid FILEPATHs.
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
makeValid :: FILEPATH -> FILEPATH
makeValid path
  | null path = singleton _underscore
  | isPosix = map (\x -> if x == _nul then _underscore else x) path
  | isJust (readDriveShare drv) && all isPathSeparator drv = take 2 drv <> fromString "drive"
  | isJust (readDriveUNC drv) && not (hasTrailingPathSeparator drv) =
      makeValid (drv <> (pathSeparator `cons` pth))
  | otherwise = joinDrive drv $ validElements $ validCHARs pth

  where
    (drv,pth) = splitDrive path

    validCHARs = map f
    f x = if isBadCHARacter x then _underscore else x

    validElements = joinPath . fmap g . splitPath
    g x = h a <> b
        where (a,b) = break isPathSeparator x
    h x = if map toUpper (dropWhileEnd (== _space) a) `L.elem` badElements then snoc a _underscore  <.> b else x
        where (a,b) = splitExtensions x


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
isRelative :: FILEPATH -> Bool
isRelative x = null drive || isRelativeDrive drive
    where drive = takeDrive x


{- c:foo -}
-- From [1]: "If a file name begins with only a disk designator but not the
-- backslash after the colon, it is interpreted as a relative path to the
-- current directory on the drive with the specified letter."
isRelativeDrive :: STRING -> Bool
isRelativeDrive x =
    maybe False (not . hasTrailingPathSeparator . fst) (readDriveLetter x)


-- | @not . 'isRelative'@
--
-- > isAbsolute x == not (isRelative x)
isAbsolute :: FILEPATH -> Bool
isAbsolute = not . isRelative

#ifndef OS_PATH

-----------------------------------------------------------------------------
-- spanEnd (>2) [1,2,3,4,1,2,3,4] = ([1,2,3,4,1,2], [3,4])
spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p = L.foldr (\x (pref, suff) -> if null pref && p x then (pref, x : suff) else (x : pref, suff)) ([], [])

-- breakEnd (< 2) [1,2,3,4,1,2,3,4] == ([1,2,3,4,1],[2,3,4])
breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd p = spanEnd (not . p)

-- | The stripSuffix function drops the given suffix from a list. It returns
-- Nothing if the list did not end with the suffix given, or Just the list
-- before the suffix, if it does.
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix xs ys = reverse P.<$> stripPrefix (reverse xs) (reverse ys)

cons :: a -> [a] -> [a]
cons = (:)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = L.foldr (\x -> Just . maybe ([], x) (first (x :))) Nothing

uncons2 :: [a] -> Maybe (a, a, [a])
uncons2 [] = Nothing
uncons2 [_] = Nothing
uncons2 (x : y : zs) = Just (x, y, zs)

_period, _quotedbl, _backslash, _slash, _question, _U, _N, _C, _colon, _semicolon, _US, _less, _greater, _bar, _asterisk, _nul, _space, _underscore :: Char
_period = '.'
_quotedbl = '"'
_slash = '/'
_backslash = '\\'
_question = '?'
_colon = ':'
_semicolon = ';'
_U = 'U'
_N = 'N'
_C = 'C'
_US = '\US'
_less = '<'
_greater = '>'
_bar = '|'
_asterisk = '*'
_nul = '\NUL'
_space = ' '
_underscore = '_'

singleton :: Char -> String
singleton c = [c]

pack :: String -> String
pack = id


unpack :: String -> String
unpack = id


snoc :: String -> Char -> String
{- HLINT ignore "Redundant lambda" -}
snoc str = \c -> str <> [c]

#else
#ifdef WINDOWS
fromString :: P.String -> STRING
fromString str = P.either (P.error . P.show) P.id $ unsafePerformIO $ do
  r <- try @SomeException $ GHC.withCStringLen (mkUTF16le ErrorOnCodingFailure) str $ \cstr -> packCStringLen cstr
  evaluate $ force $ first displayException r
#else
fromString :: P.String -> STRING
fromString str = P.either (P.error . P.show) P.id $ unsafePerformIO $ do
  r <- try @SomeException $ GHC.withCStringLen (mkUTF8 ErrorOnCodingFailure) str $ \cstr -> packCStringLen cstr
  evaluate $ force $ first displayException r
#endif

_a, _z, _A, _Z, _period, _quotedbl, _backslash, _slash, _question, _U, _N, _C, _colon, _semicolon, _US, _less, _greater, _bar, _asterisk, _nul, _space, _underscore :: CHAR
_a = 0x61
_z = 0x7a
_A = 0x41
_Z = 0x5a
_period = 0x2e
_quotedbl = 0x22
_slash = 0x2f
_backslash = 0x5c
_question = 0x3f
_colon = 0x3a
_semicolon = 0x3b
_U = 0x55
_N = 0x4e
_C = 0x43
_US = 0x1f
_less = 0x3c
_greater = 0x3e
_bar = 0x7c
_asterisk = 0x2a
_nul = 0x00
_space = 0x20
_underscore = 0x5f

isAsciiUpper :: CHAR -> Bool
isAsciiUpper w = _A <= w && w <= _Z

isAsciiLower :: CHAR -> Bool
isAsciiLower w = _a <= w && w <= _z

----------------------------------------------------------------

toUpper :: CHAR -> CHAR
-- charToWord16 should be safe here, since C.toUpper doesn't go beyond Word16 maxbound
toUpper = charToWord . C.toUpper . wordToChar

toLower :: CHAR -> CHAR
-- charToWord16 should be safe here, since C.toLower doesn't go beyond Word16 maxbound
toLower = charToWord . C.toLower . wordToChar


-- | Total conversion to char.
wordToChar :: CHAR -> Char
wordToChar = C.chr . fromIntegral

-- | This is unsafe and clamps at Word16 maxbound.
charToWord :: Char -> CHAR
charToWord = fromIntegral . C.ord

#endif
