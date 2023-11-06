# FilePath [![Hackage version](https://img.shields.io/hackage/v/filepath.svg?label=Hackage)](https://hackage.haskell.org/package/filepath)

The `filepath` package provides functionality for manipulating `FilePath` values, and is shipped with [GHC](https://www.haskell.org/ghc/).
It provides three modules:

* [`System.FilePath.Posix`](http://hackage.haskell.org/package/filepath/docs/System-FilePath-Posix.html)
  manipulates POSIX/Linux style `FilePath` values (with `/` as the path separator).
* [`System.FilePath.Windows`](http://hackage.haskell.org/package/filepath/docs/System-FilePath-Windows.html)
  manipulates Windows style `FilePath` values (with either `\` or `/` as the path separator, and deals with drives).
* [`System.FilePath`](http://hackage.haskell.org/package/filepath/docs/System-FilePath.html)
  is an alias for the module appropriate to your platform.

All three modules provide the same API, and the same documentation (calling out differences in the different variants).

### What is a `FilePath`?

In Haskell, the definition is `type FilePath = String` as of now. A Haskell `String` is a list of Unicode code points.

On unix, filenames don't have a predefined encoding as per the
[POSIX specification](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_170)
and are passed as `char[]` to syscalls.

On windows (at least the API used by `Win32`) filepaths are UTF-16 strings.

This means that Haskell filepaths have to be converted to C-strings on unix
(utilizing the current filesystem encoding) and to UTF-16 strings
on windows.

Further, this is a low-level library and it makes no attempt at providing a more
type safe variant for filepaths (e.g. by distinguishing between absolute and relative
paths) and ensures no invariants (such as filepath validity).

For such libraries, check out the following:

* [hpath](https://hackage.haskell.org/package/hpath)
* [path](https://hackage.haskell.org/package/path)
* [paths](https://hackage.haskell.org/package/paths)
* [strong-path](https://hackage.haskell.org/package/strong-path)
