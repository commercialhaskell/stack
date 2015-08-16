## Terminology

Since you'll see a bunch of terms floating around, let's try to define them:

* GHC- the Glasgow Haskell Compiler, the de-facto standard Haskell compiler. (Yes, other Haskell compilers do exist, but GHC is by far the most popular one.)
* Cabal- a build system for Haskell. This is unfortunately a point of confusion, since Cabal is used to mean different things:
    * The design of the overall system itself
    * A library, which is used by stack (and others) to perform builds
    * A file format. Each package in Haskell has a .cabal file, e.g. [stack.cabal](https://github.com/commercialhaskell/stack/blob/master/stack.cabal)
    * The cabal executable, more technically known as cabal-install. stack is a replacement for this executable, and it is not needed when using stack[1]
* [Hackage](http://hackage.haskell.org/packages/) is the primary repository of open source Haskell packages, containing thousands of packages, each with multiple versions available.
* [LTS Haskell](https://github.com/fpco/lts-haskell#readme) and [Stackage Nightly](https://github.com/fpco/stackage#readme) provide sets of packages and versions from Hackage which are tested to compile and run together

## General usage

stack's primary purpose is as a project build tool. It does provide functionality for non-project cases, such as installing executables (like pandoc via `stack install pandoc`), compiling a single file (e.g., `stack ghc foo.hs`), or running a REPL (e.g., `stack ghci`). But it's best to start off in the project use case to understand how stack works. From there, using `stack --help` to explore should serve you well.

Make sure you [have stack installed](https://github.com/commercialhaskell/stack/wiki/Downloads). You can confirm that by running:

    stack --version

For reference, the most recent release is [![Release](https://img.shields.io/github/release/commercialhaskell/stack.svg)](https://github.com/commercialhaskell/stack/releases).

To get a project started, you can run:

    stack new first-project new-template

This will create a new project called first-project- in the first-project directory- using the `new-template` template (run `stack templates` for a list of available templates). If you look in the first-project directory, you'll see a number of files. The two most important for getting started are:

* first-project.cabal
* stack.yaml

We'll describe their significance and the distinction between the two next. But for now, we just want to play around. From inside the first-project directory, run `stack build`. It may tell you that you need to run `stack setup`. Running `stack setup` is a safe operation, which will download and install GHC into a local directory which only stack will use. (For more information on exact paths used by stack, run `stack path`.)

You can also use `stack test` to build and run your test suite. But now, let's make things a bit more interesting, and perform an HTTP request from inside our application. Replace the `src/Lib.hs` file's contents with the following code (if you're brand new to Haskell, don't worry about the exact meaning of this yet):

```haskell
module Lib
    ( someFunc
    ) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L

someFunc :: IO ()
someFunc = do
    lbs <- simpleHttp "https://raw.githubusercontent.com/commercialhaskell/stack/master/README.md"
    L.putStrLn lbs
```

Now, if you run `stack build`, you'll get an error message about not being able to find the newly imported modules, which may look something like:

    Could not find module ‘Network.HTTP.Conduit’
    Could not find module ‘Data.ByteString.Lazy.Char8’

In order to fix this, you need to tell stack that your package requires two additional libraries: `http-conduit` and `bytestring`. This is done in the first-project.cabal, in the build-depends of the library stanza. This looks something like:

```
library
  hs-source-dirs:      src
  exposed-modules:     Lib
  build-depends:       base >= 4.7 && < 5
                     , bytestring
                     , http-conduit
  default-language:    Haskell2010
```

Once you make that change, you can now run `stack build`, which will:

* Download the necessary libraries from a Hackage mirror, build them, and register them where GHC can use them
* Build your project's library and executables

You can then run `stack exec first-project-exe` to run it. You should see the stack README contents on your console. Congratulations!

## Difference between stack.yaml and first-project.cabal

FIXME

## Learning more

This guide does not cover all aspects of project management in Haskell, or the stack tool in general, but should hopefully orient you enough to get started. Here are some pointers for more information:

FIXME

[1] stack can use cabal-install's dependency solver if requested, but this is not a commonly needed feature
