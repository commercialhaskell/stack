  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 7. `stack build` synonyms

Let's look at a subset of the `stack --help` output:

~~~text
build    Build the package(s) in this directory/configuration
install  Shortcut for 'build --copy-bins'
test     Shortcut for 'build --test'
bench    Shortcut for 'build --bench'
haddock  Shortcut for 'build --haddock'
~~~

Four of these commands are just synonyms for the `build` command. They are
provided for convenience for common cases (e.g., `stack test` instead of
`stack build --test`) and so that commonly expected commands just work.

What's so special about these commands being synonyms? It allows us to make
much more composable command lines. For example, we can have a command that
builds executables, generates Haddock documentation (Haskell API-level docs),
and builds and runs your test suites, with:

~~~text
stack build --haddock --test
~~~

You can even get more inventive as you learn about other flags. For example,
take the following command:

~~~text
stack build --pedantic --haddock --test --exec "echo Yay, it succeeded" --file-watch
~~~

This command will:

* turn on all warnings and errors (the `--pedantic` flag)
* build your library and executables
* generate Haddocks (the `--haddock` flag)
* build and run your test suite (the `--test` flag)
* run the command `echo Yay, it succeeded` when that completes (the `--exec`
  option)
* after building, watch for changes in the files used to build the project, and
  kick off a new build when done (the `--file-watch` flag)

## The `stack install` command and `copy-bins` option

It's worth calling out the behavior of the `install` command and `--copy-bins`
option, since this has confused a number of users (especially when compared to
behavior of other tools like Cabal (the tool)). The `install` command does
precisely one thing in addition to the build command: it copies any generated
executables to the local binary directory. You may recognize the default value
for that path:

On Unix-like operating systems, command:

~~~text
stack path --local-bin
/home/<user_name>/.local/bin
~~~

On Windows, command:

~~~text
stack path --local-bin
C:\Users\<user_name>\AppData\Roaming\local\bin
~~~

That's why the download page recommends adding that directory to your PATH. This
feature is convenient, because now you can simply run `executable-name` in your
shell instead of having to run `stack exec executable-name` from inside your
project directory.

Since it's such a point of confusion, let me list a number of things Stack does
*not* do specially for the `install` command:

* Stack will always build any necessary dependencies for your code. The install
  command is not necessary to trigger this behavior. If you just want to build a
  project, run `stack build`.
* Stack will *not* track which files it's copied to your local binary directory
  nor provide a way to automatically delete them. There are many great tools out
  there for managing installation of binaries, and Stack does not attempt to
  replace those.
* Stack will not necessarily be creating a relocatable executable. If your
  executables hard-codes paths, copying the executable will not change those
  hard-coded paths.

  * At the time of writing, there's no way to change those kinds of paths with
    Stack, but see
    [issue #848 about --prefix](https://github.com/commercialhaskell/stack/issues/848)
    for future plans.

That's really all there is to the `install` command: for the simplicity of what
it does, it occupies a much larger mental space than is warranted.
