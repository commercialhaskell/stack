---
title: Topics
---
<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Topics (advanced)

This part of the guide provides pages dedicated to specific topics.

[Stack root](stack_root.md)

:   The Stack root is a directory where Stack stores important information

[Stack work directories](stack_work.md)

:   Stack work directories are directories within a local project or package
    directory in which Stack stores files created during the build process.

[Snapshot location](snapshot_location.md)

:   How to specify the location of snapshots.

[Package location](package_location.md)

:   How to specify the location of packages.

[Snapshot specification](custom_snapshot.md)

:   How to specify the contents of a snapshot.

[`stack.yaml` vs a Cabal file](stack_yaml_vs_cabal_package_file.md)

:   The difference between Stack's project-level configuration file and a
    Cabal file describing a Haskell package.

[Script interpreter](scripts.md)

:   How to use Stack's script interpreter.

[Docker integration](docker_integration.md)

:   Stack has support for automatically performing builds inside a Docker
    container.

[Nix integration](nix_integration.md)

:   Stack can be configured to integrate with Nix, a purely functional package
    manager.

[Non-standard project initialization](nonstandard_project_init.md)

:   You may need to configure Stack to work with an existing project that has
    one or more Cabal files but no Stack project-level configuration file.

[Debugging](debugging.md)

:   Advice on debugging using Stack.

[Editor integration](editor_integration.md)

:   Advice on intergrating Stack with code editors.

[Stack and Visual Studio Code](Stack_and_VS_Code.md)

:   Advice on using Stack with Visual Studio Code and its Haskell extension.

[Developing on Windows](developing_on_windows.md)

:   Advice on using Stack on Windows.

[Shell auto-completion](shell_autocompletion.md)

:   Adding support for the tab completion of standard Stack arguments to the
    shell programs Bash, Zsh (the Z shell) and fish.

[CI](CI.md)

:   Advice on using Stack with CI.

[Travis CI](travis_ci.md)

:   Advice on using Stack on Travis CI.

[Azure CI](azure_ci.md)

:   Advice on using Stack on Azure CI.

[Lock files](lock_files.md)

:   The contents of Stack's lock files, how they are used, and how they are
    created and updated.

[Haskell and C code](haskell_and_c_code.md)

:   Advice on using Stack with Haskell packages that include C source code,
   including those with a C `main` function.
