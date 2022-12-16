<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Debugging

To profile a component of the current project, simply pass the `--profile`
flag to `stack`. The `--profile` flag turns on the `--enable-library-profiling`
and `--enable-executable-profiling` Cabal options _and_ passes the `+RTS -p`
runtime options to any testsuites and benchmarks.

For example the following command will build the `my-tests` testsuite with
profiling options and create a `my-tests.prof` file in the current directory
as a result of the test run.

~~~text
stack test --profile my-tests
~~~

The `my-tests.prof` file now contains time and allocation info for the test run.

To create a profiling report for an executable, e.g. `my-exe`, you can command:

~~~text
stack exec --profile -- my-exe +RTS -p
~~~

For more fine-grained control of compilation options there are the
`--library-profiling` and `--executable-profiling` flags which will turn on the
`--enable-library-profiling` and `--enable-executable-profiling` Cabal
options respectively. Custom GHC options can be passed in with
`--ghc-options "more options here"`.

To enable compilation with profiling options by default you can add the
following snippet to your `stack.yaml` or `~/.stack/config.yaml`:

~~~yaml
build:
  library-profiling: true
  executable-profiling: true
~~~

## Further reading

For more commands and uses, see the
[official GHC chapter on profiling](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html),
the [Haskell wiki](https://wiki.haskell.org/How_to_profile_a_Haskell_program),
and the
[chapter on profiling in Real World Haskell](http://book.realworldhaskell.org/read/profiling-and-optimization.html).

## Tracing

To generate a backtrace in case of exceptions during a test or benchmarks run,
use the `--trace` flag. Like `--profile` this compiles with profiling options,
but adds the `+RTS -xc` runtime option.

## Debugging symbols

Building with debugging symbols in the
[DWARF information](https://ghc.haskell.org/trac/ghc/wiki/DWARF) is supported by
Stack. This can be done by passing the flag `--ghc-options="-g"` and also to
override the default behaviour of stripping executables of debugging symbols by
passing either one of the following flags: `--no-strip`,
`--no-library-stripping` or `--no-executable-stripping`.

In Windows, GDB can be installed to debug an executable with
`stack exec -- pacman -S gdb`. Windows' Visual Studio compiler's debugging
format PDB is not supported at the moment. This might be possible by
[separating](https://stackoverflow.com/questions/866721/how-to-generate-gcc-debug-symbol-outside-the-build-target)
debugging symbols and
[converting](https://github.com/rainers/cv2pdb) their format. Or as an option
when
[using the LLVM backend](http://blog.llvm.org/2017/08/llvm-on-windows-now-supports-pdb-debug.html).
