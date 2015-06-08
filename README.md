## The Haskell Tool Stack

`stack` is a cross-platform program for developing Haskell
projects. It is aimed at Haskellers both new and experienced.

<img src="http://i.imgur.com/WW69oTj.gif" width="50%" align="right">

It features:

* Installing GHC automatically.
* Installing packages needed for your project.
* Building your project.
* Testing your project.
* Benchmarking your project.

#### How to install

Downloads are available by operating system:

* [Linux](https://github.com/fpco/stack/wiki/Downloads)
* ~~Windows~~ — Support will be available in next release.
* ~~OS X~~ — Support will be available in next release.

#### How to use

Go into a Haskell project directory and run `stack build`. This will
do the following:

* Automatically create a stack configuration file in the current
  directory.
* Figure out what Stackage release (LTS or nightly) is appropriate
  for the dependencies.
* Download and install GHC.
* Download the package index.
* Download and install all necessary dependencies for the project.
* Build and install the project.

Run `stack` for a complete list of commands.

#### Architecture

A full description of the architecture
[is available here](https://github.com/fpco/stack/blob/master/ARCHITECTURE.md#shared-databases-not-sandboxes).

#### Frequently Asked Questions

For frequently asked questions about detailed or specific use-cases,
please see [the FAQ](https://github.com/fpco/stack/wiki/FAQ) or
[open an issue](https://github.com/fpco/stack/issues/new) with label
`question`.

#### Why stack?

stack is a project of the [Commercial Haskell](http://commercialhaskell.com/)
group, spearheaded by [FP Complete](https://www.fpcomplete.com/). It is
designed to answer the needs of commercial Haskell users, hobbyist Haskellers,
and individuals and companies thinking about starting to use Haskell. It is
intended to be easy to use for newcomers, while providing the customizability
and power experienced developers need.

While stack itself has been around since June of 2015, it is based on codebases
used by FP Complete for its corporate customers and internally for years prior.
stack is a refresh of that codebase combined with other open source efforts
like [stackage-cli](https://github.com/fpco/stackage-cli) to meet the needs of
users everywhere.

A large impetus for the work on stack was a [large survey of people interested
in
Haskell](https://www.fpcomplete.com/blog/2015/05/thousand-user-haskell-survey),
which rated build issues as a major concern. The stack team hopes that stack
can address these concerns.
