<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack bench` command

~~~text
stack haddock [TARGET] [--dry-run] [--pedantic] [--fast] [--ghc-options OPTIONS]
              [--flag PACKAGE:[-]FLAG] [--dependencies-only | --only-snapshot |
                --only-dependencies | --only-locals] [--file-watch |
                --file-watch-poll] [--watch-all] [--exec COMMAND [ARGUMENT(S)]]
              [--only-configure] [--trace] [--profile] [--no-strip]
              [--[no-]library-profiling] [--[no-]executable-profiling]
              [--[no-]library-stripping] [--[no-]executable-stripping]
              [--[no-]haddock] [--haddock-arguments HADDOCK_ARGS]
              [--[no-]open] [--[no-]haddock-deps] [--[no-]haddock-internal]
              [--[no-]haddock-hyperlink-source] [--[no-]haddock-for-hackage]
              [--[no-]copy-bins] [--[no-]copy-compiler-tool] [--[no-]prefetch]
              [--[no-]keep-going] [--[no-]keep-tmp-files] [--[no-]force-dirty]
              [--[no-]test] [--[no-]rerun-tests]
              [--ta|--test-arguments TEST_ARGS] [--coverage] [--no-run-tests]
              [--test-suite-timeout ARG] [--[no-]tests-allow-stdin]
              [--[no-]bench] [--ba|--benchmark-arguments BENCH_ARGS]
              [--no-run-benchmarks] [--[no-]reconfigure]
              [--cabal-verbosity VERBOSITY | --[no-]cabal-verbose]
              [--[no-]split-objs] [--skip ARG] [--[no-]interleaved-output]
              [--ddump-dir ARG]
~~~

`stack haddock` is a synonym for `stack build --haddock`. For further
information, see the documentation for the [`stack build`](build_command.md)
command.
