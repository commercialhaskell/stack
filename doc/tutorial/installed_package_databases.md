  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 6. Installed package databases

Time to take a short break from hands-on examples and discuss a little
architecture. Stack has the concept of multiple *databases*.

A database consists of a GHC package database (which contains the compiled
version of a library), executables, and a few other things as well. To give you
an idea, the contents of the parent directory of the `stack path --local-pkg-db`
directory are the directories:

~~~text
bin
doc
lib
pkgdb
~~~

Databases in Stack are *layered*. For example, the database listing we just gave
is called a *local* database (also known as a *mutable* database). That is
layered on top of a *snapshot* database (also known as a *write-only* database).
The snapshot database contains the libraries and executables that are considered
to be *immutable*. Finally, GHC itself ships with a number of libraries and
executables, also considered to be immutable, which forms the *global* database.

To get a quick idea of this, we can look at the output of the
`stack exec -- ghc-pkg list` command in our `helloworld` project:

~~~text
<stack path --global-pkg-db directory>
    Cabal-3.6.3.0
    Win32-2.12.0.1
    array-0.5.4.0
    base-4.16.2.0
    binary-0.8.9.0
    bytestring-0.11.3.1
    containers-0.6.5.1
    deepseq-1.4.6.1
    directory-1.3.6.2
    exceptions-0.10.4
    filepath-1.4.2.2
    (ghc-9.2.3)
    ghc-bignum-1.2
    ghc-boot-9.2.3
    ghc-boot-th-9.2.3
    ghc-compact-0.1.0.0
    ghc-heap-9.2.3
    ghc-prim-0.8.0
    ghci-9.2.3
    haskeline-0.8.2
    hpc-0.6.1.0
    integer-gmp-1.1
    libiserv-9.2.3
    mtl-2.2.2
    parsec-3.1.15.0
    pretty-1.1.3.6
    process-1.6.13.2
    rts-1.0.2
    stm-2.5.0.2
    template-haskell-2.18.0.0
    text-1.2.5.0
    time-1.11.1.1
    transformers-0.5.6.2
    xhtml-3000.2.2.1

<stack path --snapshot-pkg-db directory>
    acme-missiles-0.3

<stack path --local-pkg-db directory>
    helloworld-0.1.0.0
~~~

where `<stack path --global-pkg-db directory>` refers to the directory output by
the command `stack path --global-pkg-db`, and so on.

Notice that `acme-missiles` ends up in the *snapshot* database. Any package
which comes from Hackage, an archive, or a repository is considered to be an
*immutable* package.

Anything which is considered *mutable*, or depends on something mutable, ends up
in the *local* database. This includes your own code and any other packages
located on a local file path.

The reason we have this structure is that:

* it lets multiple projects reuse the same binary builds of immutable packages,
* but does not allow different projects to "contaminate" each other by putting
  non-standard content into the shared snapshot database.

As you probably guessed, there can be multiple snapshot databases available. See
the contents of the `snapshots` directory in the
[Stack root](../topics/stack_root.md).

* On Unix-like operating systems, each snapshot is in the last of a sequence of
  three subdirectories named after the platform, a 256-bit hash of the source
  map (how the package should be built -- including the compiler, options, and
  immutable dependencies), and the GHC version.

* On Windows, each snapshot is in a subdirectory that is a shorter hash (eight
  characters) of the sequence of three directories used on Unix-like operating
  systems. This is done to avoid problems created by default limits on file
  path lengths on Windows systems.

These snapshot databases do not get layered on top of each other; they are each
used separately.

In reality, you will rarely — if ever — interact directly with these databases,
but it is good to have a basic understanding of how they work so you can
understand why rebuilding may occur at different points.
