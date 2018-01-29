<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://rawgit.com/commercialhaskell/stack/master/doc/img/hidden-warning.svg"></a></div>

# stack.yaml vs cabal package file

Due to their apparent overlap, the purpose of the following three files can be
unclear:

* `stack.yaml`
* A cabal package file, e.g. `my-package.cabal`
* `package.yaml`

The last two are easy to explain: `package.yaml` is a file format supported by
[hpack](https://github.com/sol/hpack#readme). It adds some niceties on top of
cabal. For example, hpack has YAML syntax support and will automatically
generate of `exposed-modules` lists. However, it's just a frontend to cabal
package files. So for this document, we're instead going to focus on the first
two and try to answer:

_What's the difference between a `stack.yaml` file and a cabal package file?_

## Package versus project

Cabal is a build system, which is used by Stack. Cabal defines the concept of a
_package_. A package has:

* A name and version
* 0 or 1 libraries
* 0 or more executables
* A cabal file (or, as mentioned above, an hpack `package.yaml` that
  generates a cabal file)
* And a bunch more

The second to last bullet bears repeating: there's a 1-to-1 correspondence between
packages and cabal files. 

Stack is a build tool that works on top of the Cabal build system, and defines
a new concept called a _project_. A project has:

* A _resolver_, which tells it about a snapshot (more on this later)
* Extra dependencies on top of the snapshot
* 0 or more local Cabal packages
* Flag and GHC options configurations
* And a bunch more Stack configuration

A source of confusion is that, often, you'll have a project that defines
exactly one package you're working on, and in that situation it's unclear why,
for example, you need to specify an extra depedency in both your `stack.yaml`
_and_ cabal file. To explain, let's take a quick detour to talk about snapshots
and how Stack resolves dependencies.

## Resolvers and snapshots

Stack follows a rule that says, for any projects, there is precisely 1 version
of each package available. Obviously there are _many_ versions of many
different packages available in the world. But when resolving a `stack.yaml`
file, Stack requires that you have chosen a specific version for each package
available.

The most common means by which this set of packages is defined is via a
Stackage Snapshot. For example, if you go to the page
<https://www.stackage.org/lts-10.2>, you will see a list of 2,666 packages at
specific version numbers. When you then specify `resolver: lts-10.2`, you're
telling Stack to use those package versions in resolving dependencies down to
concrete version numbers.

Sometimes a snapshot doesn't have all of the packages you want. Or you want a
different version. Or you want to work on a local modification of a package. In
all of those cases, you can add more configuration data to your `stack.yaml` to
override the values it received from your `resolver` setting. At the end of the
day, each of your projects will end up with some way of resolving a package
name into a concrete version number.

## Why specify deps twice?

When you add something like this to your `stack.yaml` file:

```yaml
extra-deps:
- acme-missiles-0.3
```

What you're saying to Stack is: if at any point you find that you need to build
the `acme-missiles` package, please use version `0.3`. You are _not_ saying
"please build `acme-missiles` now." You are also not saying "my package depends
on `acme-missiles`." You are simply making it available should the need arise.

When you add `build-depends: acme-missiles` to your cabal file or
`dependencies: [acme-missiles]` to your `package.yaml` file, you're saying
"this package requires that `acme-missiles` be available." Since
`acme-missiles` doesn't appear in your snapshot, without also modifying your
`stack.yaml` to mention it via `extra-deps`, Stack will complain about the
dependency being unavailable.

You may challenge: but why go through all of that annoyance? Stack knows what
package I want, why not just go grab it? The answer is that, if Stack just
grabbed `acme-missiles` for you without it being specified in the `stack.yaml`
somehow, you'd lose reproducibility. How would Stack know which version to use?
It may elect to use the newest version, but if a new version is available in
the future, will it automatically switch to that?

Stack's baseline philosophy is that build plans are always reproducible\*. The
purpose of the `stack.yaml` file is to define an immutable set of packages. No
matter when in time you use it, and no matter how many new release happen in
the interim, the build plan generated should be the same.

\* There's at least one hole in this theory today, which is Hackage revisions.
When you specify `extra-deps: [acme-missiles-0.3]`, it doesnt' specify which
revision of the cabal file to use, and Stack will just choose the latest. Stack
version 1.6 added the ability to specify exact revisions of cabal files, but
this isn't enforced as a requirement as it's so different from the way most
people work with packages.

And now, how about the other side: why doesn't Stack automatically add
`acme-missiles` to `build-depends` in your cabal file if you add it as an
extra-dep? There are a surprising number reasons actually:

* The cabal spec doesn't support anything like that
* There can be multiple packages in a project, and how do we know which package
  actually needs the dependency?
* There can be multiple components (libraries, executable, etc) in a package,
  and how do we know which of those actually needs the dependency?
* The dependency may only be conditionally needed, based on flags, OS, or
  architecture. As an extreme example, we wouldn't want a Linux-only package to
  be force-built on Windows.

While for simple use cases it seems like automatically adding dependencies from
the cabal file to the `stack.yaml` file or vice-versa would be a good thing, it
breaks down immediately for any semi-difficult case. Therefore, Stack requires
you to add it to both places.

And a final note, in case it wasn't clear. The example I gave above used
`acme-missiles`, which is not in Stackage snapshots. If, however, you want to
depend on a package already present in the snapshot you've selected, there's no
need to add it explicitly to your `stack.yaml` file: it's already there
implicitly via the `resolver` setting. This is what you do the majority of the
time, such as when you add `vector` or `mtl` as a `build-depends` value.
