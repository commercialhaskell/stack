


{-# LANGUAGE PatternGuards #-}

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
module Legacy.System.FilePath.Windows
    (
    -- * Separator predicates
    FilePath,
    pathSeparator, pathSeparators, isPathSeparator,
    searchPathSeparator, isSearchPathSeparator,
    extSeparator, isExtSeparator,

    -- * @$PATH@ methods
    splitSearchPath, getSearchPath,

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

import Data.Char(toLower, toUpper, isAsciiLower, isAsciiUpper)
import Data.Maybe(isJust)
import Data.List(stripPrefix, isSuffixOf)

import System.Environment(getEnv)


infixr 7  <.>, -<.>
infixr 5  </>





---------------------------------------------------------------------
-- Platform Abstraction Methods (private)

-- | Is the operating system Unix or Linux like
isPosix :: Bool
isPosix = not isWindows

-- | Is the operating system Windows like
isWindows :: Bool
isWindows = True


---------------------------------------------------------------------
-- The basic functions

-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- > Windows: pathSeparator == '\\'
-- > Posix:   pathSeparator ==  '/'
-- > isPathSeparator pathSeparator
pathSeparator :: Char
pathSeparator = if isWindows then '\\' else '/'

-- | The list of all possible separators.
--
-- > Windows: pathSeparators == ['\\', '/']
-- > Posix:   pathSeparators == ['/']
-- > pathSeparator `elem` pathSeparators
pathSeparators :: [Char]
pathSeparators = if isWindows then "\\/" else "/"

-- | Rather than using @(== 'pathSeparator')@, use this. Test if something
--   is a path separator.
--
-- > isPathSeparator a == (a `elem` pathSeparators)
isPathSeparator :: Char -> Bool
isPathSeparator '/' = True
isPathSeparator '\\' = isWindows
isPathSeparator _ = False


-- | The character that is used to separate the entries in the $PATH environment variable.
--
-- > Windows: searchPathSeparator == ';'
-- > Posix:   searchPathSeparator == ':'
searchPathSeparator :: Char
searchPathSeparator = if isWindows then ';' else ':'

-- | Is the character a file separator?
--
-- > isSearchPathSeparator a == (a == searchPathSeparator)
isSearchPathSeparator :: Char -> Bool
isSearchPathSeparator = (== searchPathSeparator)


-- | File extension character
--
-- > extSeparator == '.'
extSeparator :: Char
extSeparator = '.'

-- | Is the character an extension character?
--
-- > isExtSeparator a == (a == extSeparator)
isExtSeparator :: Char -> Bool
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
splitSearchPath :: String -> [FilePath]
splitSearchPath = f
    where
    f xs = case break isSearchPathSeparator xs of
           (pre, []    ) -> g pre
           (pre, _:post) -> g pre ++ f post

    g "" = ["." | isPosix]
    g ('\"':x@(_:_)) | isWindows && last x == '\"' = [init x]
    g x = [x]


-- | Get a list of 'FilePath's in the $PATH variable.
getSearchPath :: IO [FilePath]
getSearchPath = fmap splitSearchPath (getEnv "PATH")


---------------------------------------------------------------------
-- Extension methods

-- | Split on the extension. 'addExtension' is the inverse.
--
-- > splitExtension "/directory/path.ext" == ("/directory/path",".ext")
-- > uncurry (++) (splitExtension x) == x
-- > Valid x => uncurry addExtension (splitExtension x) == x
-- > splitExtension "file.txt" == ("file",".txt")
-- > splitExtension "file" == ("file","")
-- > splitExtension "file/file.txt" == ("file/file",".txt")
-- > splitExtension "file.txt/boris" == ("file.txt/boris","")
-- > splitExtension "file.txt/boris.ext" == ("file.txt/boris",".ext")
-- > splitExtension "file/path.txt.bob.fred" == ("file/path.txt.bob",".fred")
-- > splitExtension "file/path.txt/" == ("file/path.txt/","")
splitExtension :: FilePath -> (String, String)
splitExtension x = case nameDot of
                       "" -> (x,"")
                       _ -> (dir ++ init nameDot, extSeparator : ext)
    where
        (dir,file) = splitFileName_ x
        (nameDot,ext) = breakEnd isExtSeparator file

-- | Get the extension of a file, returns @\"\"@ for no extension, @.ext@ otherwise.
--
-- > takeExtension "/directory/path.ext" == ".ext"
-- > takeExtension x == snd (splitExtension x)
-- > Valid x => takeExtension (addExtension x "ext") == ".ext"
-- > Valid x => takeExtension (replaceExtension x "ext") == ".ext"
takeExtension :: FilePath -> String
takeExtension = snd . splitExtension

-- | Remove the current extension and add another, equivalent to 'replaceExtension'.
--
-- > "/directory/path.txt" -<.> "ext" == "/directory/path.ext"
-- > "/directory/path.txt" -<.> ".ext" == "/directory/path.ext"
-- > "foo.o" -<.> "c" == "foo.c"
(-<.>) :: FilePath -> String -> FilePath
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
replaceExtension :: FilePath -> String -> FilePath
replaceExtension x y = dropExtension x <.> y

-- | Add an extension, even if there is already one there, equivalent to 'addExtension'.
--
-- > "/directory/path" <.> "ext" == "/directory/path.ext"
-- > "/directory/path" <.> ".ext" == "/directory/path.ext"
(<.>) :: FilePath -> String -> FilePath
(<.>) = addExtension

-- | Remove last extension, and the \".\" preceding it.
--
-- > dropExtension "/directory/path.ext" == "/directory/path"
-- > dropExtension x == fst (splitExtension x)
dropExtension :: FilePath -> FilePath
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
addExtension :: FilePath -> String -> FilePath
addExtension file "" = file
addExtension file xs@(x:_) = joinDrive a res
    where
        res = if isExtSeparator x then b ++ xs
              else b ++ [extSeparator] ++ xs

        (a,b) = splitDrive file

-- | Does the given filename have an extension?
--
-- > hasExtension "/directory/path.ext" == True
-- > hasExtension "/directory/path" == False
-- > null (takeExtension x) == not (hasExtension x)
hasExtension :: FilePath -> Bool
hasExtension = any isExtSeparator . takeFileName


-- | Does the given filename have the specified extension?
--
-- > "png" `isExtensionOf` "/directory/file.png" == True
-- > ".png" `isExtensionOf` "/directory/file.png" == True
-- > ".tar.gz" `isExtensionOf` "bar/foo.tar.gz" == True
-- > "ar.gz" `isExtensionOf` "bar/foo.tar.gz" == False
-- > "png" `isExtensionOf` "/directory/file.png.jpg" == False
-- > "csv/table.csv" `isExtensionOf` "/data/csv/table.csv" == False
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions

-- | Drop the given extension from a FilePath, and the @\".\"@ preceding it.
--   Returns 'Nothing' if the FilePath does not have the given extension, or
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
stripExtension :: String -> FilePath -> Maybe FilePath
stripExtension []        path = Just path
stripExtension ext@(x:_) path = stripSuffix dotExt path
    where dotExt = if isExtSeparator x then ext else '.':ext


-- | Split on all extensions.
--
-- > splitExtensions "/directory/path.ext" == ("/directory/path",".ext")
-- > splitExtensions "file.tar.gz" == ("file",".tar.gz")
-- > uncurry (++) (splitExtensions x) == x
-- > Valid x => uncurry addExtension (splitExtensions x) == x
-- > splitExtensions "file.tar.gz" == ("file",".tar.gz")
splitExtensions :: FilePath -> (FilePath, String)
splitExtensions x = (a ++ c, d)
    where
        (a,b) = splitFileName_ x
        (c,d) = break isExtSeparator b

-- | Drop all extensions.
--
-- > dropExtensions "/directory/path.ext" == "/directory/path"
-- > dropExtensions "file.tar.gz" == "file"
-- > not $ hasExtension $ dropExtensions x
-- > not $ any isExtSeparator $ takeFileName $ dropExtensions x
dropExtensions :: FilePath -> FilePath
dropExtensions = fst . splitExtensions

-- | Get all extensions.
--
-- > takeExtensions "/directory/path.ext" == ".ext"
-- > takeExtensions "file.tar.gz" == ".tar.gz"
takeExtensions :: FilePath -> String
takeExtensions = snd . splitExtensions


-- | Replace all extensions of a file with a new extension. Note
--   that 'replaceExtension' and 'addExtension' both work for adding
--   multiple extensions, so only required when you need to drop
--   all extensions first.
--
-- > replaceExtensions "file.fred.bob" "txt" == "file.txt"
-- > replaceExtensions "file.fred.bob" "tar.gz" == "file.tar.gz"
replaceExtensions :: FilePath -> String -> FilePath
replaceExtensions x y = dropExtensions x <.> y



---------------------------------------------------------------------
-- Drive methods

-- | Is the given character a valid drive letter?
-- only a-z and A-Z are letters, not isAlpha which is more unicodey
isLetter :: Char -> Bool
isLetter x = isAsciiLower x || isAsciiUpper x


-- | Split a path into a drive and a path.
--   On Posix, \/ is a Drive.
--
-- > uncurry (++) (splitDrive x) == x
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
splitDrive :: FilePath -> (FilePath, FilePath)
splitDrive x | isPosix = span (== '/') x
splitDrive x | Just y <- readDriveLetter x = y
splitDrive x | Just y <- readDriveUNC x = y
splitDrive x | Just y <- readDriveShare x = y
splitDrive x = ("",x)

addSlash :: FilePath -> FilePath -> (FilePath, FilePath)
addSlash a xs = (a++c,d)
    where (c,d) = span isPathSeparator xs

-- See [1].
-- "\\?\D:\<path>" or "\\?\UNC\<server>\<share>"
readDriveUNC :: FilePath -> Maybe (FilePath, FilePath)
readDriveUNC (s1:s2:'?':s3:xs) | all isPathSeparator [s1,s2,s3] =
    case map toUpper xs of
        ('U':'N':'C':s4:_) | isPathSeparator s4 ->
            let (a,b) = readDriveShareName (drop 4 xs)
            in Just (s1:s2:'?':s3:take 4 xs ++ a, b)
        _ -> case readDriveLetter xs of
                 -- Extended-length path.
                 Just (a,b) -> Just (s1:s2:'?':s3:a,b)
                 Nothing -> Nothing
readDriveUNC _ = Nothing

{- c:\ -}
readDriveLetter :: String -> Maybe (FilePath, FilePath)
readDriveLetter (x:':':y:xs) | isLetter x && isPathSeparator y = Just $ addSlash [x,':'] (y:xs)
readDriveLetter (x:':':xs) | isLetter x = Just ([x,':'], xs)
readDriveLetter _ = Nothing

{- \\sharename\ -}
readDriveShare :: String -> Maybe (FilePath, FilePath)
readDriveShare (s1:s2:xs) | isPathSeparator s1 && isPathSeparator s2 =
        Just (s1:s2:a,b)
    where (a,b) = readDriveShareName xs
readDriveShare _ = Nothing

{- assume you have already seen \\ -}
{- share\bob -> "share\", "bob" -}
readDriveShareName :: String -> (FilePath, FilePath)
readDriveShareName name = addSlash a b
    where (a,b) = break isPathSeparator name



-- | Join a drive and the rest of the path.
--
-- > Valid x => uncurry joinDrive (splitDrive x) == x
-- > Windows: joinDrive "C:" "foo" == "C:foo"
-- > Windows: joinDrive "C:\\" "bar" == "C:\\bar"
-- > Windows: joinDrive "\\\\share" "foo" == "\\\\share\\foo"
-- > Windows: joinDrive "/:" "foo" == "/:\\foo"
joinDrive :: FilePath -> FilePath -> FilePath
joinDrive = combineAlways

-- | Get the drive from a filepath.
--
-- > takeDrive x == fst (splitDrive x)
takeDrive :: FilePath -> FilePath
takeDrive = fst . splitDrive

-- | Delete the drive, if it exists.
--
-- > dropDrive x == snd (splitDrive x)
dropDrive :: FilePath -> FilePath
dropDrive = snd . splitDrive

-- | Does a path have a drive.
--
-- > not (hasDrive x) == null (takeDrive x)
-- > Posix:   hasDrive "/foo" == True
-- > Windows: hasDrive "C:\\foo" == True
-- > Windows: hasDrive "C:foo" == True
-- >          hasDrive "foo" == False
-- >          hasDrive "" == False
hasDrive :: FilePath -> Bool
hasDrive = not . null . takeDrive


-- | Is an element a drive
--
-- > Posix:   isDrive "/" == True
-- > Posix:   isDrive "/foo" == False
-- > Windows: isDrive "C:\\" == True
-- > Windows: isDrive "C:\\foo" == False
-- >          isDrive "" == False
isDrive :: FilePath -> Bool
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
splitFileName :: FilePath -> (String, String)
splitFileName x = (if null dir then "./" else dir, name)
    where
        (dir, name) = splitFileName_ x

-- version of splitFileName where, if the FilePath has no directory
-- component, the returned directory is "" rather than "./".  This
-- is used in cases where we are going to combine the returned
-- directory to make a valid FilePath, and having a "./" appear would
-- look strange and upset simple equality properties.  See
-- e.g. replaceFileName.
splitFileName_ :: FilePath -> (String, String)
splitFileName_ x = (drv ++ dir, file)
    where
        (drv,pth) = splitDrive x
        (dir,file) = breakEnd isPathSeparator pth

-- | Set the filename.
--
-- > replaceFileName "/directory/other.txt" "file.ext" == "/directory/file.ext"
-- > Valid x => replaceFileName x (takeFileName x) == x
replaceFileName :: FilePath -> String -> FilePath
replaceFileName x y = a </> y where (a,_) = splitFileName_ x

-- | Drop the filename. Unlike 'takeDirectory', this function will leave
--   a trailing path separator on the directory.
--
-- > dropFileName "/directory/file.ext" == "/directory/"
-- > dropFileName x == fst (splitFileName x)
dropFileName :: FilePath -> FilePath
dropFileName = fst . splitFileName


-- | Get the file name.
--
-- > takeFileName "/directory/file.ext" == "file.ext"
-- > takeFileName "test/" == ""
-- > takeFileName x `isSuffixOf` x
-- > takeFileName x == snd (splitFileName x)
-- > Valid x => takeFileName (replaceFileName x "fred") == "fred"
-- > Valid x => takeFileName (x </> "fred") == "fred"
-- > Valid x => isRelative (takeFileName x)
takeFileName :: FilePath -> FilePath
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
takeBaseName :: FilePath -> String
takeBaseName = dropExtension . takeFileName

-- | Set the base name.
--
-- > replaceBaseName "/directory/other.ext" "file" == "/directory/file.ext"
-- > replaceBaseName "file/test.txt" "bob" == "file/bob.txt"
-- > replaceBaseName "fred" "bill" == "bill"
-- > replaceBaseName "/dave/fred/bob.gz.tar" "new" == "/dave/fred/new.tar"
-- > Valid x => replaceBaseName x (takeBaseName x) == x
replaceBaseName :: FilePath -> String -> FilePath
replaceBaseName pth nam = combineAlways a (nam <.> ext)
    where
        (a,b) = splitFileName_ pth
        ext = takeExtension b

-- | Is an item either a directory or the last character a path separator?
--
-- > hasTrailingPathSeparator "test" == False
-- > hasTrailingPathSeparator "test/" == True
hasTrailingPathSeparator :: FilePath -> Bool
hasTrailingPathSeparator "" = False
hasTrailingPathSeparator x = isPathSeparator (last x)


hasLeadingPathSeparator :: FilePath -> Bool
hasLeadingPathSeparator "" = False
hasLeadingPathSeparator x = isPathSeparator (head x)


-- | Add a trailing file path separator if one is not already present.
--
-- > hasTrailingPathSeparator (addTrailingPathSeparator x)
-- > hasTrailingPathSeparator x ==> addTrailingPathSeparator x == x
-- > Posix:    addTrailingPathSeparator "test/rest" == "test/rest/"
addTrailingPathSeparator :: FilePath -> FilePath
addTrailingPathSeparator x = if hasTrailingPathSeparator x then x else x ++ [pathSeparator]


-- | Remove any trailing path separators
--
-- > dropTrailingPathSeparator "file/test/" == "file/test"
-- >           dropTrailingPathSeparator "/" == "/"
-- > Windows:  dropTrailingPathSeparator "\\" == "\\"
-- > Posix:    not (hasTrailingPathSeparator (dropTrailingPathSeparator x)) || isDrive x
dropTrailingPathSeparator :: FilePath -> FilePath
dropTrailingPathSeparator x =
    if hasTrailingPathSeparator x && not (isDrive x)
    then let x' = dropWhileEnd isPathSeparator x
         in if null x' then [last x] else x'
    else x


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
takeDirectory :: FilePath -> FilePath
takeDirectory = dropTrailingPathSeparator . dropFileName

-- | Set the directory, keeping the filename the same.
--
-- > replaceDirectory "root/file.ext" "/directory/" == "/directory/file.ext"
-- > Valid x => replaceDirectory x (takeDirectory x) `equalFilePath` x
replaceDirectory :: FilePath -> String -> FilePath
replaceDirectory x dir = combineAlways dir (takeFileName x)


-- | An alias for '</>'.
combine :: FilePath -> FilePath -> FilePath
combine a b | hasLeadingPathSeparator b || hasDrive b = b
            | otherwise = combineAlways a b

-- | Combine two paths, assuming rhs is NOT absolute.
combineAlways :: FilePath -> FilePath -> FilePath
combineAlways a b | null a = b
                  | null b = a
                  | hasTrailingPathSeparator a = a ++ b
                  | otherwise = case a of
                      [a1,':'] | isWindows && isLetter a1 -> a ++ b
                      _ -> a ++ [pathSeparator] ++ b


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
(</>) :: FilePath -> FilePath -> FilePath
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
splitPath :: FilePath -> [FilePath]
splitPath x = [drive | drive /= ""] ++ f path
    where
        (drive,path) = splitDrive x

        f "" = []
        f y = (a++c) : f d
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
splitDirectories :: FilePath -> [FilePath]
splitDirectories = map dropTrailingPathSeparator . splitPath


-- | Join path elements back together.
--
-- > joinPath a == foldr (</>) "" a
-- > joinPath ["/","directory/","file.ext"] == "/directory/file.ext"
-- > Valid x => joinPath (splitPath x) == x
-- > joinPath [] == ""
-- > Posix: joinPath ["test","file","path"] == "test/file/path"
joinPath :: [FilePath] -> FilePath
-- Note that this definition on c:\\c:\\, join then split will give c:\\.
joinPath = foldr combine ""






---------------------------------------------------------------------
-- File name manipulators

-- | Equality of two 'FilePath's.
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
equalFilePath :: FilePath -> FilePath -> Bool
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
makeRelative :: FilePath -> FilePath -> FilePath
makeRelative root path
 | equalFilePath root path = "."
 | takeAbs root /= takeAbs path = path
 | otherwise = f (dropAbs root) (dropAbs path)
    where
        f "" y = dropWhile isPathSeparator y
        f x y = let (x1,x2) = g x
                    (y1,y2) = g y
                in if equalFilePath x1 y1 then f x2 y2 else path

        g x = (dropWhile isPathSeparator a, dropWhile isPathSeparator b)
            where (a,b) = break isPathSeparator $ dropWhile isPathSeparator x

        -- on windows, need to drop '/' which is kind of absolute, but not a drive
        dropAbs x | hasLeadingPathSeparator x && not (hasDrive x) = tail x
        dropAbs x = dropDrive x

        takeAbs x | hasLeadingPathSeparator x && not (hasDrive x) = [pathSeparator]
        takeAbs x = map (\y -> if isPathSeparator y then pathSeparator else toLower y) $ takeDrive x

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
normalise :: FilePath -> FilePath
normalise path = result ++ [pathSeparator | addPathSeparator]
    where
        (drv,pth) = splitDrive path
        result = joinDrive' (normaliseDrive drv) (f pth)

        joinDrive' "" "" = "."
        joinDrive' d p = joinDrive d p

        addPathSeparator = isDirPath pth
            && not (hasTrailingPathSeparator result)
            && not (isRelativeDrive drv)

        isDirPath xs = hasTrailingPathSeparator xs
            || not (null xs) && last xs == '.' && hasTrailingPathSeparator (init xs)

        f = joinPath . dropDots . propSep . splitDirectories

        propSep (x:xs) | all isPathSeparator x = [pathSeparator] : xs
                       | otherwise = x : xs
        propSep [] = []

        dropDots = filter ("." /=)

normaliseDrive :: FilePath -> FilePath
normaliseDrive "" = ""
normaliseDrive _ | isPosix = [pathSeparator]
normaliseDrive drive = if isJust $ readDriveLetter x2
                       then map toUpper x2
                       else x2
    where
        x2 = map repSlash drive

        repSlash x = if isPathSeparator x then pathSeparator else x

-- Information for validity functions on Windows. See [1].
isBadCharacter :: Char -> Bool
isBadCharacter x = x >= '\0' && x <= '\31' || x `elem` ":*?><|\""

badElements :: [FilePath]
badElements =
    ["CON","PRN","AUX","NUL","CLOCK$"
    ,"COM1","COM2","COM3","COM4","COM5","COM6","COM7","COM8","COM9"
    ,"LPT1","LPT2","LPT3","LPT4","LPT5","LPT6","LPT7","LPT8","LPT9"]


-- | Is a FilePath valid, i.e. could you create a file like it? This function checks for invalid names,
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
isValid :: FilePath -> Bool
isValid "" = False
isValid x | '\0' `elem` x = False
isValid _ | isPosix = True
isValid path =
        not (any isBadCharacter x2) &&
        not (any f $ splitDirectories x2) &&
        not (isJust (readDriveShare x1) && all isPathSeparator x1) &&
        not (isJust (readDriveUNC x1) && not (hasTrailingPathSeparator x1))
    where
        (x1,x2) = splitDrive path
        f x = map toUpper (dropWhileEnd (== ' ') $ dropExtensions x) `elem` badElements


-- | Take a FilePath and make it valid; does not change already valid FilePaths.
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
makeValid :: FilePath -> FilePath
makeValid "" = "_"
makeValid path
        | isPosix = map (\x -> if x == '\0' then '_' else x) path
        | isJust (readDriveShare drv) && all isPathSeparator drv = take 2 drv ++ "drive"
        | isJust (readDriveUNC drv) && not (hasTrailingPathSeparator drv) =
            makeValid (drv ++ [pathSeparator] ++ pth)
        | otherwise = joinDrive drv $ validElements $ validChars pth
    where
        (drv,pth) = splitDrive path

        validChars = map f
        f x = if isBadCharacter x then '_' else x

        validElements x = joinPath $ map g $ splitPath x
        g x = h a ++ b
            where (a,b) = break isPathSeparator x
        h x = if map toUpper (dropWhileEnd (== ' ') a) `elem` badElements then a ++ "_" <.> b else x
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
isRelative :: FilePath -> Bool
isRelative x = null drive || isRelativeDrive drive
    where drive = takeDrive x


{- c:foo -}
-- From [1]: "If a file name begins with only a disk designator but not the
-- backslash after the colon, it is interpreted as a relative path to the
-- current directory on the drive with the specified letter."
isRelativeDrive :: String -> Bool
isRelativeDrive x =
    maybe False (not . hasTrailingPathSeparator . fst) (readDriveLetter x)


-- | @not . 'isRelative'@
--
-- > isAbsolute x == not (isRelative x)
isAbsolute :: FilePath -> Bool
isAbsolute = not . isRelative


-----------------------------------------------------------------------------
-- dropWhileEnd (>2) [1,2,3,4,1,2,3,4] == [1,2,3,4,1,2])
-- Note that Data.List.dropWhileEnd is only available in base >= 4.5.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

-- takeWhileEnd (>2) [1,2,3,4,1,2,3,4] == [3,4])
takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

-- spanEnd (>2) [1,2,3,4,1,2,3,4] = ([1,2,3,4,1,2], [3,4])
spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p xs = (dropWhileEnd p xs, takeWhileEnd p xs)

-- breakEnd (< 2) [1,2,3,4,1,2,3,4] == ([1,2,3,4,1],[2,3,4])
breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd p = spanEnd (not . p)

-- | The stripSuffix function drops the given suffix from a list. It returns
-- Nothing if the list did not end with the suffix given, or Just the list
-- before the suffix, if it does.
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix xs ys = fmap reverse $ stripPrefix (reverse xs) (reverse ys)
