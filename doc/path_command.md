<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# The `stack path` command

~~~text
stack path [--stack-root] [--global-config] [--project-root] [--config-location]
           [--bin-path] [--programs] [--compiler-exe] [--compiler-bin]
           [--compiler-tools-bin] [--local-bin] [--extra-include-dirs]
           [--extra-library-dirs] [--snapshot-pkg-db] [--local-pkg-db]
           [--global-pkg-db] [--ghc-package-path] [--snapshot-install-root]
           [--local-install-root] [--snapshot-doc-root] [--local-doc-root]
           [--local-hoogle-root] [--dist-dir] [--local-hpc-root]
           [--local-bin-path] [--ghc-paths] [--global-stack-root]
~~~

`stack path` provides information about files and locations used by Stack.

Pass the following flags for information about specific files or locations:

|Flag                   |File or location                                      |
|-----------------------|------------------------------------------------------|
|--bin-path             |The PATH in the Stack environment.                    |
|--compiler-bin         |The directory containing the GHC executable.          |
|--compiler-exe         |The GHC executable.                                   |
|--compiler-tools-bin   |The directory containing binaries specific to a particular compiler.|
|--config-location      |Stack's project-level YAML configuration file (`stack.yaml`).|
|--dist-dir             |The dist working directory, relative to the package directory.|
|--extra-include-dirs   |Extra include directories.                            |
|--extra-library-dirs   |Extra library directories.                            |
|--ghc-package-path     |The `GHC_PACKAGE_PATH` environment variable.          |
|--ghc-paths            |Deprecated.                                           |
|--global-config        |Stack's user-specific global YAML configuration file (`config.yaml`).|
|--global-pkg-db        |The global package database.                          |
|--global-stack-root    |Deprecated.                                           |
|--local-bin            |The directory in which Stack installs executables.    |
|--local-bin-path       |Deprecated.                                           |
|--local-doc-root       |The root directory for local project documentation.   |
|--local-hoogle-root    |The root directory for local project documentation.   |
|--local-hpc-root       |The root directory for .tix files and HPC reports.    |
|--local-install-root   |The root directory for local project installation.    |
|--local-pkg-db         |The local package database.                           |
|--programs             |The root directory for GHC and other Stack-supplied tools.|
|--project-root         |The project root directory.|
|--snapshot-doc-root    |The root directory for snapshot documentation.        |
|--snapshot-install-root|The root directory for snapshot installation.         |
|--snapshot-pkg-db      |The snapshot package database.                        |
|--stack-root           |The Stack root.                                       |
