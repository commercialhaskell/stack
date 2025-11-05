<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# stack.yaml versus package.yaml versus a Cabal file

What is the difference between a `stack.yaml` file, a `package.yaml` file and a
Cabal file (named `<package_name>.cabal`)? This page aims to make that clear.

In short:

* `stack.yaml` contains project-level configuration for Stack, and may contain
  project-specific options and non-project-specific options.

* `package.yaml` contains a description of a package in the
  [Hpack](https://github.com/sol/hpack) format. Hpack, including Stack's
  built-in version, uses the file to create a Cabal file.

* a Cabal file also contains a description of a package, but in the format used
  by Cabal.

## package.yaml versus a Cabal file

Why two different formats to describe packages? Hpack is considered to have some
advantages over the underlying Cabal format, which are explained its project
repository. They include that the Hpack format supports YAML syntax and the
automatic generation of the lists of `exposed-modules` used in the Cabal format.

The remainder of this page will focus on the difference between a `stack.yaml`
file and a package description file.

## Package versus project

Stack is a tool for building Haskell code and it uses Cabal, a build system.
Cabal defines the concept of a _package_. A package has:

* A name and version
* optionally, one library
* optionally, one or more executables
* A Cabal file (or, as mentioned above, an [Hpack](https://github.com/sol/hpack)
  `package.yaml` file that generates a Cabal file)
* And a bunch more

There is a one-to-one correspondence between a package and a Cabal file.

Stack defines a new concept called a _project_. A project has:

* A snapshot _resolver_ (more on this later)
* Extra dependencies on top of the snapshot
* Optionally, one or more local Cabal packages
* Flag and GHC options configurations
* And a bunch more Stack configuration

Often you will have a project that defines only one local Cabal package that you
are working on. If you need to specify a dependency, a source of confusion can
be why you need to specify it both in the `stack.yaml` file _and_
in the Cabal file. To explain, let us take a quick detour to talk about
snapshots and how Stack resolves dependencies.

## Snapshots and resolvers

Stack follows a rule that says, for any project, there is precisely one version
of each package available. Obviously, for many packages there are _many_
versions available in the world. But when resolving a `stack.yaml` file, Stack
requires that you have chosen a specific version for each package available.

The most common means by which this set of packages is defined is via a
snapshot provided by Stackage. For example, if you go to the page
<https://www.stackage.org/lts-24.18>, you will see a list of 3,407 packages at
specific version numbers. When you then specify `snapshot: lts-24.18` or,
alternatively, `resolver: lts-24.18`, you are telling Stack to use those package
versions in resolving dependencies down to specific versions of packages.

Sometimes a snapshot does not have all of the packages that you want. Or you
want a different version of a package. Or you want to work on a local
modification of a package. In all of those cases, you can add more configuration
data to your `stack.yaml` file to override the values it received from your
[`snapshot`](../configure/yaml/project.md#snapshot) or
[`resolver`](../configure/yaml/project.md#resolver) setting. At the end of the
day, each of your projects will end up with some way of resolving a package name
into a specific version of that package.

## Why specify dependencies twice?

The package `acme-missiles` is not included in any Stackage snapshots. When you
add something like this to your `stack.yaml` file:

~~~yaml
extra-deps:
- acme-missiles-0.3
~~~

what you are saying to Stack is: "if at any point you find that you need to
build the `acme-missiles` package, please use version `0.3`". You are _not_
saying "please build `acme-missiles` now." You are also not saying "my package
depends on `acme-missiles`." You are simply making it available should the need
arise.

When you add to your `package.yaml` file:

~~~yaml
dependencies:
- acme-missiles
~~~

or, alternatively, you add directly to your Cabal file:

~~~yaml
build-depends: acme-missiles
~~~

you are saying "this package requires that `acme-missiles` be available." Since
`acme-missiles` does not appear in your snapshot, without also modifying your
`stack.yaml` to mention it via `extra-deps`, Stack will complain about the
dependency being unavailable.

You may challenge: but why go through all of that annoyance? Stack knows what
package I want, why not just go grab it? The answer is that, if Stack just
grabbed `acme-missiles` for you without it being specified in the `stack.yaml`
somehow, you'd lose reproducibility. How would Stack know which version to use?
It may elect to use the newest version, but if a new version is available in
the future, will it automatically switch to that?

Stack's core philosophy is that build plans are always reproducible. The
purpose of the `stack.yaml` file is to define an immutable set of packages. No
matter when in time you use it, and no matter how many new release happen in
the interim, the build plan generated should be the same.

(There is, however, at least one hole in this theory today, which is Hackage
revisions. When you specify `extra-deps: [acme-missiles-0.3]`, it does not
specify which revision of the Cabal file to use, and Stack will just choose the
latest. Stack has the ability to specify exact revisions of Cabal files, but
this is not enforced as a requirement, because it is so different from the way
most people work with packages.)

And now, how about the other side: why does Stack not automatically add
`acme-missiles` to `build-depends` in your Cabal file if you add it as an
extra-dep? There are a surprising number reasons for this:

* The Cabal specification does not support anything like that
* There can be multiple packages in a project, and how do we know which package
  actually needs the dependency?
* There can be multiple components (libraries, executable, etc) in a package,
  and how do we know which of those actually needs the dependency?
* The dependency may only be conditionally needed, based on flags, operating
  system, or architecture. As an extreme example, we would not want a Linux-only
  package to be built by force on Windows.

While for simple use cases it seems like automatically adding dependencies from
the Cabal file to the `stack.yaml` file or vice-versa would be a good thing, it
breaks down immediately for any semi-difficult case. Therefore, Stack requires
you to add it to both places.

And a final note, in case it was not clear. The example above used
`acme-missiles`, which is not in Stackage snapshots. If, however, you want to
depend on a package already present in the snapshot you have selected, there is
no need to add it explicitly to your `stack.yaml` file: it is already there
implicitly via the `snapshot` setting. This is what you do the majority of the
time, such as when you add `vector` or `mtl` as a `build-depends` value.

## Should I check-in automatically generated Cabal files?

Yes, you should. This recommendation was changed in
[issue #5210](https://github.com/commercialhaskell/stack/issues/5210). Please
see the discussion there.
