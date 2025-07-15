{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#endif
{- |
Module      :  System.FilePath
Copyright   :  (c) Neil Mitchell 2005-2014
License     :  BSD3

Maintainer  :  ndmitchell@gmail.com
Stability   :  stable
Portability :  portable

A library for 'FilePath' manipulations, using Posix or Windows filepaths
depending on the platform.

Both "System.FilePath.Posix" and "System.FilePath.Windows" provide the
same interface.

Given the example 'FilePath': @\/directory\/file.ext@

We can use the following functions to extract pieces.

* 'takeFileName' gives @\"file.ext\"@

* 'takeDirectory' gives @\"\/directory\"@

* 'takeExtension' gives @\".ext\"@

* 'dropExtension' gives @\"\/directory\/file\"@

* 'takeBaseName' gives @\"file\"@

And we could have built an equivalent path with the following expressions:

* @\"\/directory\" '</>' \"file.ext\"@.

* @\"\/directory\/file" '<.>' \"ext\"@.

* @\"\/directory\/file.txt" '-<.>' \"ext\"@.

Each function in this module is documented with several examples,
which are also used as tests.

Here are a few examples of using the @filepath@ functions together:

/Example 1:/ Find the possible locations of a Haskell module @Test@ imported from module @Main@:

@['replaceFileName' path_to_main \"Test\" '<.>' ext | ext <- [\"hs\",\"lhs\"] ]@

/Example 2:/ Download a file from @url@ and save it to disk:

@do let file = 'makeValid' url
  System.Directory.createDirectoryIfMissing True ('takeDirectory' file)@

/Example 3:/ Compile a Haskell file, putting the @.hi@ file under @interface@:

@'takeDirectory' file '</>' \"interface\" '</>' ('takeFileName' file '-<.>' \"hi\")@

References:
[1] <http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247.aspx Naming Files, Paths and Namespaces> (Microsoft MSDN)
-}


#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
module Legacy.System.FilePath(
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
) where
import Legacy.System.FilePath.Windows
#else
module Legacy.System.FilePath(
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
) where
import Legacy.System.FilePath.Posix
#endif
