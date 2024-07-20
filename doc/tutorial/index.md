  <div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Getting started

Stack is a modern, cross-platform tool for building Haskell code.

This introductory guide takes a new Stack user through the typical workflows.
This guide will not teach Haskell or involve much code, and it requires no prior
experience with the Haskell packaging system or other tools used for, or during,
building Haskell code. Terms used in the guide are defined in the
[glossary](glossary.md).

Some of Stack's features will not be needed regularly or by all users. See
Stack's [commands](commands/index.md) and
[configuration](configure/index.md) for information about those
features.

## Stack's functions

Stack handles the management of your toolchain (including GHC — the Glasgow
Haskell Compiler — and, for Windows users, MSYS2), building and registering
libraries, building dependencies on tools used during building, and more. While
it can use existing tools on your system, Stack has the capacity to be your
one-stop shop for all Haskell tooling you need. This guide will follow that
Stack-centric approach.

## What makes Stack special?

The primary Stack design point is __reproducible builds__. If you run
`stack build` today, you should get the same result running `stack build`
tomorrow. There are some cases that can break that rule (changes in your
operating system configuration, for example), but, overall, Stack follows this
design philosophy closely. To make this a simple process, Stack uses curated
package sets called __snapshots__.

Stack has also been designed from the ground up to be user friendly, with an
intuitive, discoverable command line interface. For many users, simply
downloading Stack and reading `stack --help` will be enough to get up and
running. This guide provides a more gradual tour for users who prefer that
learning style.

To build your project, Stack uses a project-level configuration file, named
`stack.yaml`, in the root directory of your project as a sort of blueprint. That
file contains a reference to the snapshot (also known as a __resolver__) which
your package will be built against.

Finally, Stack is __isolated__: it will not make changes outside of specific
Stack directories. Stack-built files generally go in either the Stack root
directory or `./.stack-work` directories local to each project. The
[Stack root](stack_root.md) directory holds packages belonging to snapshots and
any Stack-installed versions of GHC. Stack will not tamper with any system
version of GHC or interfere with packages installed by other tools used for
building Haskell code, such as Cabal (the tool).

## Downloading and Installation

The [documentation dedicated to downloading Stack](install_and_upgrade.md) has
the most up-to-date information for a variety of operating systems. Instead of
repeating that content here, please go check out that page and come back here
when you can successfully run `stack --version`.

We also assume that the directory reported by `stack path --local-bin` has been
added to the PATH.
