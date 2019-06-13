# FilePath [![Hackage version](https://img.shields.io/hackage/v/filepath.svg?label=Hackage)](https://hackage.haskell.org/package/filepath) [![Linux Build Status](https://img.shields.io/travis/haskell/filepath.svg?label=Linux%20build)](https://travis-ci.org/haskell/filepath) [![Windows Build Status](https://img.shields.io/appveyor/ci/ndmitchell/filepath.svg?label=Windows%20build)](https://ci.appveyor.com/project/ndmitchell/filepath)

The `filepath` package provides functionality for manipulating `FilePath` values, and is shipped with both [GHC](https://www.haskell.org/ghc/) and the [Haskell Platform](https://www.haskell.org/platform/). It provides three modules:

* [`System.FilePath.Posix`](http://hackage.haskell.org/package/filepath/docs/System-FilePath-Posix.html) manipulates POSIX/Linux style `FilePath` values (with `/` as the path separator).
* [`System.FilePath.Windows`](http://hackage.haskell.org/package/filepath/docs/System-FilePath-Windows.html) manipulates Windows style `FilePath` values (with either `\` or `/` as the path separator, and deals with drives).
* [`System.FilePath`](http://hackage.haskell.org/package/filepath/docs/System-FilePath.html) is an alias for the module appropriate to your platform.

All three modules provide the same API, and the same documentation (calling out differences in the different variants).

### Should `FilePath` be an abstract data type?

The answer for this library is "no". While an abstract `FilePath` has some advantages (mostly type safety), it also has some disadvantages:

* In Haskell the definition is `type FilePath = String`, and all file-oriented functions operate on this type alias, e.g. `readFile`/`writeFile`. Any abstract type would require wrappers for these functions or lots of casts between `String` and the abstraction.
* It is not immediately obvious what a `FilePath` is, and what is just a pure `String`. For example, `/path/file.ext` is a `FilePath`. Is `/`? `/path`? `path`? `file.ext`? `.ext`? `file`?
* Often it is useful to represent invalid files, e.g. `/foo/*.txt` probably isn't an actual file, but a glob pattern. Other programs use `foo//bar` for globs, which is definitely not a file, but might want to be stored as a `FilePath`.
* Some programs use syntactic non-semantic details of the `FilePath` to change their behaviour. For example, `foo`, `foo/` and `foo/.` are all similar, and refer to the same location on disk, but may behave differently when passed to command-line tools.
* A useful step to introducing an abstract `FilePath` is to reduce the amount of manipulating `FilePath` values like lists. This library hopes to help in that effort.
