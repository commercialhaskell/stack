<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# 3. Project configuration

Let us continue to look at the `helloworld` example in more detail to understand
better how Stack works.

As discussed in the previous part of this guide to getting started, some of the
contents of the project directory are set out below. The item of interest here
is the `stack.yaml` file.

~~~text
.
├── .stack-work
│   └── ...
...
│
├── package.yaml
├── helloworld.cabal
├── Setup.hs
│
└── stack.yaml
~~~

## `stack.yaml`

Stack requires a Stack project-level configuration file for every project.
`stack.yaml` is that file. The contents of the file set project-specific and
non-project-specific options that apply to the project. (Non-project
specific options that affect the project may also be set in a
[global Stack configuration file](../configure/yaml/index.md#project-level-and-global-configuration-files).)

The contents of the `stack.yaml` file include comments beginning `#`. Ignoring
those comments, the contents will look something like this:

~~~yaml
snapshot:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/23/8.yaml
packages:
- .
~~~

The key [`snapshot`](../configure/yaml/project.md#snapshot) is a
project-specific configuration option. Its value tells Stack *how* to build your
package: which GHC version to use, which versions of package dependencies to
use, and so on. Our value here says to use
[LTS Haskell 23.8](https://www.stackage.org/lts-23.8), which implies GHC 9.8.4
(which is why `stack build` installs that version of GHC if it is not already
available to Stack). There are a number of values you can use for `snapshot`,
which we'll cover later.

The key [`packages`](../configure/yaml/project.md#packages) is another
project-specific configuration option. Its value tells Stack which project
packages, located locally, to build. In our simple example, we have only a
single project package, located in the same directory, so '`.`' suffices.
However, Stack has powerful support for multi-package projects, which we'll
describe as this guide progresses.
