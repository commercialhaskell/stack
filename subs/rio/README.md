# The rio library

*A standard library for Haskell*

![Rio](https://camo.githubusercontent.com/fc162fb0024699c85f00eae769085a5fe528153e/68747470733a2f2f7777772e61687374617469632e636f6d2f70686f746f732f636974792f76692d76363837315f30305f31343030783434322e6a7067)

__NOTE__ This code is currently in prerelease status, and has been
released as a tech preview. A number of us are actively working on
improving the project and getting it to a useful first release. For
more information, see the
[description of goals](https://github.com/snoyberg/codename-karka#readme)
and the
[issue tracker for discussions](https://github.com/snoyberg/codename-karka/issues). If
you're reading this file anywhere but Github, you should probably
[read the Github version instead](https://github.com/commercialhaskell/stack/tree/rio/subs/rio#readme),
which will be more up to date.

The goal of the `rio` library is to help you jump start your Haskell
coding. It is intended as a cross between:

* Collection of well designed, trusted libraries
* Useful `Prelude` replacement
* A set of best practices for writing production quality Haskell code

You're free to use any subset of functionality desired in your
project. This README will guide you through using `rio` to its fullest
extent.

## Standard library

While GHC ships with a `base` library, as well as another of other
common packages like `directory` and `transformers`, there are large
gaps in functionality provided by these libraries. This choice for a
more minimalistic `base` is by design, but it leads to some
unfortunate consequences:

* For a given task, it's often unclear which is the right library to
  use
* When writing libraries, there is often concern about adding
  dependencies to any libraries outside of `base`, due to creating a
  heavier dependency footprint
* By avoiding adding dependencies, many libraries end up
  reimplementing the same functionality, often with incompatible types
  and type classes, leading to difficulty using libraries together

This library attempts to define a standard library for Haskell. One
immediate response may be [XKCD #927](https://xkcd.com/927/):

![XKCD Standards](https://imgs.xkcd.com/comics/standards.png)

To counter that effect, this library takes a specific approach: __it
reuses existing, commonly used libraries__. Instead of defining an
incompatible `Map` type, for instance, we standardize on the commonly
used one from the `containers` library and reexport it from this
library.

This library attempts to define a set of libraries as "standard,"
meaning they are recommended for use, and should be encouraged as
dependencies for other libraries. It does this by depending on these
libraries itself, and reexporting their types and functions for easy
use.

Beyond the ecosystem effects we hope to achieve, this will hopefully
make the user story much easier. For a new user or team trying to get
started, there is an easy library to depend upon for a large
percentage of common functionality.

See the dependencies of this package to see the list of packages
considered standard. The primary interfaces of each of these packages
is exposed from this library via a `RIO.`-prefixed module reexporting
its interface.

## Prelude replacement

The `RIO` module works as a prelude replacement, providing more
functionality and types out of the box than the standard prelude (such
as common data types like `ByteString` and `Text`), as well as
removing common "gotchas", like partial functions and lazy I/O. The
guiding principle here is:

* If something is safe to use in general and has no expected naming
  conflicts, expose it from `RIO`
* If something should not always be used, or has naming conflicts,
  expose it from another module in the `RIO.` hierarchy.

## Best practices

__NOTE__ There is no need to follow any best practices listed here
when using `rio`. However, in some cases, `rio` will make design
decisions towards optimizing for these use cases. And for Haskellers
looking for a set of best practices to follow: you've come to the
right place!

For now, this is just a collection of links to existing best practices
documents. We'll expand in the future.

* https://www.fpcomplete.com/blog/2017/07/the-rio-monad
* https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell
