  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 5. Building existing projects

Alright, enough playing around with simple projects. Let's take an open source
package and try to build it. We'll be ambitious and use
[yackage](https://hackage.haskell.org/package/yackage), a local package server
using [Yesod](http://www.yesodweb.com/). To get the code, we'll use the
`stack unpack` command from the root directory for all our Haskell projects:

~~~text
stack unpack yackage
Unpacked yackage-0.8.1 to <root_directory>/yackage-0.8.1/
~~~

You can also unpack to the directory of your liking instead of the current one
by issuing the command:

~~~text
stack unpack yackage --to <desired_directory>
~~~

This will create a `yackage-0.8.1` directory inside `<desired_directory>`.

We will change to that directory, with the command:

~~~text
cd yackage-0.8.1
~~~

## The `stack init` command

This new directory does not have a `stack.yaml` file, so we need to make one
first. We could do it by hand, but let's be lazy instead with the `stack init`
command:

~~~text
stack init
# init output ...
~~~

`stack init` does quite a few things for you behind the scenes:

* Finds all of the Cabal files in your current directory and subdirectories
  (unless you use `--ignore-subdirs`) and determines the packages and versions
  they require
* Finds the best combination of snapshot and package flags that allows
  everything to compile with minimum external dependencies
* It tries to look for the best matching snapshot from latest LTS, latest
  nightly, other LTS versions in that order

Assuming it finds a match, it will write your `stack.yaml` file, and everything
will work.

!!! note

    The `yackage` package does not currently support Hpack, but you can also use
    `hpack-convert` should you need to generate a `package.yaml` file.

### Excluded Packages

Sometimes multiple packages in your project may have conflicting requirements.
In that case `stack init` will fail, so what do you do?

You could manually create `stack.yaml` by omitting some packages to resolve the
conflict. Alternatively you can ask `stack init` to do that for you by
specifying `--omit-packages` flag on the command line. Let's see how that
works.

To simulate a conflict we will use `acme-missiles-0.3` in `yackage` and we will
also copy `yackage.cabal` to another directory and change the name of the file
and package to `yackage-test`. In this new package we will use
`acme-missiles-0.2` instead. Let's see what happens when we command `stack init`
again:

~~~text
stack init --force --omit-packages
# init failure output ...
~~~

Looking at `stack.yaml`, you will see that the excluded packages have been
commented out under the `packages` field. In case wrong packages are excluded
you can uncomment the right one and comment the other one.

Packages may get excluded due to conflicting requirements among user packages or
due to conflicting requirements between a user package and the snapshot
compiler. If all of the packages have a conflict with the compiler then all of
them may get commented out.

When packages are commented out you will see a warning every time you run a
command which needs the configuration file. The warning can be disabled by
editing the configuration file and removing it.

### Using a specific snapshot

Sometimes you may want to use a specific snapshot for your project instead of
`stack init` picking one for you. You can do that by using
`stack init --snapshot <snapshot>`.

You can also init with a compiler snapshot if you do not want to use a
Stackage snapshot. That will result in all of your project's dependencies being
put under the `extra-deps` section.

### Installing the compiler

Stack will automatically install the compiler when you run `stack build` but you
can manually specify the compiler by running `stack setup <GHC-VERSION>`.

### Miscellaneous and diagnostics

_Add selected packages_: If you want to use only selected packages from your
project directory you can do so by explicitly specifying the package directories
on the command line.

_Duplicate package names_: If multiple packages under the directory tree have
same name, `stack init` will report those and automatically ignore one of them.

_Ignore subdirectories_: By default `stack init` searches all the subdirectories
for Cabal files. If you do not want that then you can use `--ignore-subdirs`
command line switch.

_Cabal warnings_: `stack init` will show warnings if there were issues in
reading a Cabal file. You may want to pay attention to the warnings as sometimes
they may result in incomprehensible errors later on during dependency solving.

_Package naming_: If the `Name` field defined in a Cabal file does not match
with the Cabal file name then `stack init` will refuse to continue.

_User warnings_: When packages are excluded or external dependencies added Stack
will show warnings every time the configuration file is loaded. You can suppress
the warnings by editing the configuration file and removing the warnings from
it. If you command:

~~~text
stack build
~~~

you may see something like this:

~~~text
Warning: Warnings (added by new or init): Some packages were found to be
         incompatible with the snapshot and have been left commented out in the
         packages section.

         Warning (added by new or init): Specified snapshot could not satisfy
         all dependencies. Some external packages have been added as
         dependencies.

        You can omit this message by removing it from the project-level
        configuration file.
~~~
