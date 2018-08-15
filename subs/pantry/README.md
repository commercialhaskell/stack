# pantry

TODO: Add Travis and AppVeyor badges

Content addressable Haskell package management, providing for secure,
reproducible acquisition of Haskell package contents and metadata.

## What is Pantry

* A Haskell library, command line executable, storage specification, and
  network protocol
* Intended for content-addressable storage of Haskell packages
* Allows non-centralized package storage
* Primarily for use by Stackage and Stack, hopefully other tools as well

## Goals

* Efficient, distributed package storage for Haskell
* Superset of existing storage mechanisms
* Security via content addressable storage
* Allow more Stackage-style snapshots to exist
* Allow authors to bypass Hackage for uploads
* Allow Stackage to create forks of packages on Hackage

__TODO__

Content below needs to be updated.

## Package definition

Pantry defines the following concepts:

* __Blob__: a raw byte sequence, identified by its key (SHA256 of the
  contents)
* __Tree entry__: contents of a single file (identified by blob key)
  and whether or not it is executable.
    * NOTE: existing package formats like tarballs support more
      sophisticated options. We explicitly do not support those. If
      such functionality is needed, fallback to those mechanism is
      required.
* __Tree__: mapping from relative path to a tree entry. Some basic
  sanity rules apply to the paths: no `.` or `..` directory
  components, no newlines in filepaths, does not begin with `/`, no
  `\\` (we normalize to POSIX-style paths). A tree is identified by a
  tree key (SHA256 of the tree's serialized format).
* __Package__: a tree key for the package contents, package name,
  version number, and cabal file blob key. Requirements: there must be
  a single file with a `.cabal` file extension at the root of the
  tree, and it must match the cabal file blob key. The cabal file must
  be located at `pkgname.cabal`. Each tree can be in at most one
  package, and therefore tree keys work as package keys too.

Note that with the above, a tree key is all the information necessary
to uniquely identify a package. However, including additional
information (package name, version, cabal key) in config files may be
useful for optimizations or user friendliness. If such extra
information is ever included, it must be validated to concur with the
package contents itself.

### Package location

Packages will optionally be sourced from some location:

* __Hackage__ requires the package name, version number, and revision
  number. Each revision of a package will end up with a different tree
  key.
* __Archive__ takes a URL pointing to a tarball (gzipped or not) or a
  ZIP file. An implicit assumption is that archives remain immutable
  over time. Use tree keys to verify this assumption. (Same applies to
  Hackage for that matter.)
* __Repository__ takes a repo type (Git or Mercurial), URL, and
  commit. Assuming the veracity of the cryptographic hashes on the
  repos, this should guarantee a unique set of files.

In order to deal with _megarepos_ (repos and archives containing more
than one package), there is also a subdirectory for the archive and
repository cases. An empty subdir `""` would be the case for a
standard repo/archive.

In order to meet the rules of a package listed above, the following
logic is applied to all three types above:

* Find all of the files in the raw location, and represent as `Map
  FilePath TreeEntry` (or equivalent).
* Remove a wrapper directory. If _all_ filepaths in that `Map` are
  contained within the same directory, strip it from all of the
  paths. For example, if the paths are `foo/bar` and `foo/baz`, the
  paths will be reduced to `bar` and `baz`.
* After this wrapper is removed, then subdirectory logic is applied,
  essentially applying `stripPrefix` to the filepaths. If the subdir
  is `yesod-bin` and files exist called `yesod-core/yesod-core.cabal`
  and `yesod-bin/yesod-bin.cabal`, the only file remaining after
  subdir stripping would be `yesod-bin.cabal`. Note that trailing
  slashes must be handled appropriately, and that an empty subdir
  string results in this step being a noop.

The result of all of this is that, given one of the three package
locations above, we can receive a tree key which will provide an
installable package. That tree key will remain immutable.

### How tooling refers to packages

We'll get to the caching mechanism for Pantry below. However, the
recommended approach for tooling is to support some kind of composite
of the Pantry keys, parsed info, and raw package location. This allows
for more efficient lookups when available, with a fallback when
mirrors don't have the needed information.

An example:

```yaml
extra-deps:
- name: foobar
  version: 1.2.3.4
  pantry: deadbeef # tree key
  cabal-file: 12345678 # blob key
  archive: https://example.com/foobar-1.2.3.4.tar.gz
```

It is also recommended that tooling provide an easy way to generate
such complete information from, e.g., just the URL of the tarball, and
that upon reading information, hashes, package names, and version
numbers are all checked for correctness.

## Pantry caching

One simplistic option for Pantry would be that, every time a piece of
data is needed, Pantry downloads the necessary tarball/Git
repo/etc. However, this would in practice be highly wasteful, since
downloading Git repos and archives just to get a single cabal file
(for plan construction purposes) is overkill. Instead, here's the
basic idea for how caching works:

* All data for Pantry can be stored in a SQL database. Local tools
  like Stack will use an SQLite database. Servers will use PostgreSQL.
* We'll define a network protocol (initially just HTTP, maybe
  extending to something more efficient if desired) for querying blobs
  and trees.
* When a blob or tree is needed, it is first checked for in the local
  SQLite cache. If it's not available there, a request to the Pantry
  mirrors (configurable) will be made for the data. Since everything
  is content addressable, it is safe to use untrusted mirrors.
* If the data is not available in a mirror, and a location is
  provided, the location will be downloaded and cached locally.

We may also allow these Pantry mirrors to provide some kind of query
interface to find out, e.g., the latest version of a package on
Hackage. That's still TBD.

## Example: resolving a package location

To work through a full example, the following three stanzas are intended to
have equivalent behavior:

```yaml
- archive: https://example.com/foobar-1.2.3.4.tar.gz

- name: foobar
  version: 1.2.3.4
  pantry: deadbeef # tree key
  cabal-file: 12345678 # blob key
  archive: https://example.com/foobar-1.2.3.4.tar.gz

- pantry: deadbeef

```

The question is: how does the first one (presumably what a user would want to
enter) be resolved into the second and third? Pantry would follow this set of
steps:

* Download the tarball from the given URL
* Place each file in the tarball into its store as a blob, getting a blob key
  for each. The tarball is now represented as `Map FilePath BlobKey`
* Perform the root directory stripping step, removing a shared path
* Since there's no subdirectory: no subdirectory stripping would be performed
* Serialize the `Map FilePath BlobKey` to a binary format and take its hash to
  get a tree key
* Store the tree in the store referenced by its tree key. In our example: the
  tree key is `deadbeef`.
* Ensure that the tree is a valid package by checking for a single cabal file
  at the root. In our example, that's found in `foobar.cabal` with blob key
  `12345678`.
* Parse the cabal file and ensure that it is a valid cabal file, and that its
  package name is `foobar`. Grab the version number (1.2.3.4).
* We now know that tree key `deadbeef` is a valid package, and can refer to it
  by tree key exclusively. However, including the other information allows us
  to verify our assumptions, provide user-friendly readable data, and provide a
  fallback if the package isn't in the Pantry cache.

## More advanced content discovery

There are three more advanced cases to consider:

* Providing fall-back locations for content, such as out of concern for a
  single URL being removed in the future
* Closed corporate setups, where access to the general internet may either be
  impossible or undesirable
* Automatic discovery of missing content by hash

The following extensions are possible to address these cases:

* Instead of a single package location, provide a list of package locations
  with fallback semantics.
* Corporate environments will be encouraged to run a local Pantry mirror, and
  configure clients like Stack to speak to these mirrors instead of the default
  ones (or in addition to).
* Provide some kind of federation protocol for Pantry where servers can
  registry with each other and requests for content can be pinged to each
  other.

Providing override at the client level for Pantry mirror locations is a
__MUST__. Making it easy to run in a corporate environment is a __SHOULD__.
Providing the fallback package locations seems easy enough that we should
include it initially, but falls under a __SHOULD__. The federated protocol
should be added on-demand.
