<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Stack's script interpreter

Stack offers a very useful feature for running files: a script interpreter. For
too long have Haskellers felt shackled to bash or Python because it's just too
hard to create reusable source-only Haskell scripts. Stack attempts to solve
that.

You can use `stack <file_name>` to execute a Haskell source file. Usually, the
Stack command to be applied is specified using a special Haskell comment (the
Stack interpreter options comment) at the start of the source file. That command
is most often [`stack script`](../commands/script_command.md) but it can be, for
example, [`stack runghc`](../commands/runghc_command.md). If there is no Stack
interpreter options comment, Stack will warn that one was expected.

An example will be easiest to understand. Consider the Haskell source file
`turtle-example.hs` with contents:

~~~haskell
#!/usr/bin/env stack
-- stack script --snapshot lts-23.17 --package turtle
{-# LANGUAGE OverloadedStrings #-}
import Turtle (echo)
main = echo "Hello World!"
~~~

=== "Unix-like"

    The first line beginning with the 'shebang' (`#!`) tells Unix to use Stack
    as a script interpreter, if the file's permissions mark it as executable. A
    shebang line is limited to a single argument, here `stack`.

    The file's permissions can be set with command `chmod` and then it can be
    run:

    ~~~text
    chmod +x turtle-example.hs
    ./turtle-example.hs
    ~~~

    !!! note

        On macOS:

        - Avoid `{-# LANGUAGE CPP #-}` in Stack scripts; it breaks the shebang
          line ([GHC #6132](https://gitlab.haskell.org/ghc/ghc/issues/6132))

        - Use a compiled executable, not another script, in the shebang line.
          Eg `#!/usr/bin/env runhaskell` will work but
          `#!/usr/local/bin/runhaskell` would not.

    Alternatively, the script can be run with command:

    ~~~text
    stack turtle-example.hs
    ~~~

=== "Windows"

    The first line beginning with the 'shebang' (`#!`) has a meaning on
    Unix-like operating systems but will be ignored by PowerShell. It can be
    omitted on Windows. The script can be run with command:

    ~~~text
    stack turtle-example.hs
    ~~~

In both cases, the command yields:

~~~text
Hello World!
~~~

the first time after a little delay (as GHC is downloaded, if necessary, and
dependencies are built) and subsequent times more promptly (as the runs are
able to reuse everything already built).

The second line of the source code is the Stack interpreter options comment. In
this example, it specifies the [`stack script`](../commands/script_command.md)
command with the options of a LTS Haskell 23.17 snapshot
(`--snapshot lts-23.17`) and ensuring the
[`turtle`](https://hackage.haskell.org/package/turtle) package is available
(`--package turtle`). The version of the package will be that in the specified
snapshot (`lts-23.17` provides `turtle-1.6.2`).

## Arguments and interpreter options and arguments

Arguments for the script can be specified on the command line after the file
name: `stack <file_name> <arg1> <arg2> ...`.

The Stack interpreter options comment must specify what would be a single valid
Stack command at the command line if the file name were included as an argument,
starting with `stack`. It can include `--` followed by arguments. In particular,
the Stack command `stack <arg1> MyScript.hs <arg4>` with
Stack interpreter options comment:

~~~haskell
-- stack <arg2> <command> <arg3> -- <arg5>
~~~

is equivalent to the following command at the command line:

~~~text
stack <arg1> <arg2> <command> <arg3> -- MyScript.hs <arg4> <arg5>
~~~

The Stack interpreter options comment must be the first line of the file, unless
a shebang line is the first line, when the comment must be the second line. The
comment must start in the first column of the line.

When many options are needed, a block style comment that splits the command over
more than one line may be more convenient and easier to read.

For example, the command `stack MyScript.hs arg1 arg2` with `MyScript.hs`:

~~~haskell
#!/usr/bin/env stack
{- stack script
   --snapshot lts-23.17
   --
   +RTS -s -RTS
-}
import Data.List (intercalate)
import System.Environment (getArgs)
import Turtle (echo, fromString)

main = do
  args <- getArgs
  echo $ fromString $ intercalate ", " args
~~~

is equivalent to the following command at the command line:

~~~text
stack script --snapshot lts-23.17 -- MyScript.hs arg1 arg2 +RTS -s -RTS
~~~

where `+RTS -s -RTS` are some of GHC's
[runtime system (RTS) options](https://downloads.haskell.org/~ghc/latest/docs/users_guide/runtime_control.html).

Arguments that include spaces can be quoted using double quotation marks.

## Just-in-time compilation

As with using [`stack script`](../commands/script_command.md) at the command
line, you can pass the `--compile` flag to make Stack compile the script, and
then run the compiled executable. Compilation is done quickly, without
optimization. To compile with optimization, pass the `--optimize` flag instead.
Compilation is done only if needed; if the executable already exists, and is
newer than the script, Stack just runs the executable directly.

This feature can be good for speed (your script runs faster) and also for
durability (the executable remains runnable even if the script is disturbed, eg
due to changes in your installed GHC/snapshots, changes to source files during
git bisect, etc.)

## Using multiple packages

As with using [`stack script`](../commands/script_command.md) at the command
line, you can also specify multiple packages, either with multiple `--package`
options, or by providing a comma or space separated list. For example:

~~~haskell
#!/usr/bin/env stack
{- stack script
   --snapshot lts-23.17
   --package turtle
   --package "stm async"
   --package http-client,http-conduit
-}
~~~

## Using extra-deps

As with using [`stack script`](../commands/script_command.md) at the command
line, you can also specify one or more extra-deps from the package index using a
valid YAML string for each. For example:

~~~haskell
#!/usr/bin/env stack
{- stack script
   --snapshot lts-23.17
   --extra-dep acme-missile-0.3@rev:0
-}
~~~

## Stack configuration for scripts

When using the [`stack script`](../commands/script_command.md) command, as when
using it at the command line, any project-level configuration file
(`stack.yaml`, by default) (including in the `global-project` directory in the
Stack root), including any specified by the options to the `stack script`
command itself, is ignored.

!!! info

    Non-project level configuration options in global configuration files
    (`config.yaml`), are not ignored by the
    [`stack script`](../commands/script_command.md) command. Such options may be
    useful if [`allow-newer`](../configure/yaml/non-project.md#allow-newer)
    and/or
    [`allow-newer-deps`](../configure/yaml/non-project.md#allow-newer-deps)
    are required.

When using the  [`stack runghc`](../commands/runghc_command.md) command, as when
using it at the command line, if the current working directory is inside a
project, then that project's project-level configuration file is effective when
running the script. Otherwise the script uses the project-level configuration
file in the `global-project` directory in the Stack root.

## Testing scripts

You can use the flag `--script-no-run-compile` on the command line to enable (it
is disabled by default) the use of the `--no-run` option with
[`stack script`](../commands/script_command.md) (and forcing the `--compile`
option). The flag may help test that scripts compile in CI (continuous
integration).

For example, consider the following simple script, in a file named `Script.hs`,
which makes use of the joke package
[`acme-missiles`](https://hackage.haskell.org/package/acme-missiles):

~~~haskell
{- stack script
   --snapshot lts-23.17
   --package acme-missiles
-}
import Acme.Missiles (launchMissiles)

main :: IO ()
main = launchMissiles
~~~

The command `stack --script-no-run-compile Script.hs` then behaves as if the
command
`stack script --snapshot lts-23.17 --package acme-missiles --no-run --compile -- Script.hs`
had been given. `Script.hs` is compiled (without optimisation) and the resulting
executable is not run: no missiles are launched in the process!

## Writing independent and reliable scripts

The [`stack script`](../commands/script_command.md) command will automatically:

* Install GHC and libraries, if missing.
  [`stack script`](../commands/script_command.md) behaves as if the
  `--install-ghc` flag had been passed at the command line.
* Require that all packages used be explicitly stated on the command line.

This ensures that your scripts are _independent_ of any prior deployment
specific configuration, and are _reliable_ by using exactly the same version of
all packages every time it runs so that the script does not break by
accidentally using incompatible package versions.

In earlier versions of Stack, the
[`stack runghc`](../commands/runghc_command.md) command was used for scripts and
can still be used in that way. In order to achieve the same effect with the
[`stack runghc`](../commands/runghc_command.md) command, you can do the
following:

1. Use the `--install-ghc` option to install the compiler automatically
2. Explicitly specify all packages required by the script using the `--package`
   option. Use `-hide-all-packages` GHC option to force explicit specification
   of all packages.
3. Use the `--snapshot` Stack option to ensure a specific GHC version and
   package set is used.

It is possible for a project-level configuration file to affect
[`stack runghc`](../commands/runghc_command.md). For that reason,
[`stack script`](../commands/script_command.md) is strongly recommended. For
those curious, here is an example with
[`stack runghc`](../commands/runghc_command.md):

~~~haskell
#!/usr/bin/env stack
{- stack
  runghc
  --install-ghc
  --snapshot lts-23.17
  --package base
  --package turtle
  --
  -hide-all-packages
  -}
~~~

The [`stack runghc`](../commands/runghc_command.md) command is still useful,
especially when you're working on a project and want to access the package
databases and configurations used by that project. See the next section for more
information on configuration files.

## Loading scripts in GHCi

Sometimes you want to load your script in GHCi to play around with your program.
In those cases, you can use `exec ghci` option in the script to achieve
it. Here is an example:

~~~haskell
#!/usr/bin/env stack
{- stack
   exec ghci
   --install-ghc
   --snapshot lts-23.17
   --package turtle
-}
~~~
