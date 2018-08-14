# curator

Snapshot curator tool for, e.g., creating Stackage snapshots.

This is the "curator 2.0", replacing
https://github.com/fpco/stackage-curator. It relies on pantry for
finding appropriate packages, and Stack for performing the builds. It
is intended to be much simpler to maintain than the old
stackage-curator tool.

## Incomplete!

This tool is not yet complete. Here's a (likely incomplete) list of
things that still need to be handled to replace `stackage-curator`:

* Collect the Haddocks in a way that stackage-server can handle them
* Proper CLI, right now the `app/Main.hs` just runs through a bunch of
  steps. We need to have individual commands like the current tool, so
  each command can be called in an appropriately locked-down Docker
  container.
* Logic for uploading generated snapshots and other info to Github,
  S3, etc.
* Ability to roll an LTS minor version bump.
* Ability to specify package locations from Git.
* External, but: stackage-server needs to be updated to support the
  new snapshot format/location
* No support for custom configure arguments from `build-constraints.yaml`. I'd
  like to see if we can get rid of them entirely and instead just customize the
  Docker build image.

## Basic workflow

Here's a rundown of how this tool is intended to be used.

We update the Hackage index to get a list of all of the most recent
package versions. This is pantry's `updateHackageIndex` command.

We start with `build-constraints.yaml`, the configuration file in
commercialhaskell/stackage. This specifies all of the packages we want
to include in a snapshot, along with a bunch of configuration.

We parse `build-constraints.yaml` and convert it into the
`constraints.yaml` file, which contains a more properly structures set
of constraints. We'll continue to let users edit the
`build-constraints.yaml` file, since it's more user-friendly. But
`constraints.yaml` gives us more flexibility.

* For LTS minor bumps, instead of generating `constraints.yaml` from
  `build-constraints.yaml`, we'll take the `constraints.yaml` used for
  the last LTS release in the series. Details still need to be worked
  out on how upper bounds are added and where this file is stored.

Curator team: at this point, you can edit `constraints.yaml` to make
tweaks to the build plan. This replaces the old `CONSTRAINTS`
environment variable.

We combine the `constraints.yaml` file and the information from
Hackage to produce `snapshot-incomplete.yaml`. This has a concrete
list of all of the packages we intend to include in the
snapshot. Again, this file can be manually modified if desired.

* When we support Git repos, we'll also be checking those repos to
  find the latest appropriate release. We'll need to figure out
  exactly how that plays in with LTS upper bounds; I'm thinking we'll
  have logic like "use commit X, or the latest if it meets version
  range Y."

The `snapshot-incomplete.yaml` file does not have all of the
cryptographic hashes necessary for fully reproducible builds. We next
generate `snapshot.yaml` with all of this information. This file
should _never be manually edited_, instead edits should occur at the
`snapshot-incomplete.yaml` and `constraints.yaml` phases.

We unpack all of the package specified by `snapshot.yaml` into a local
directory, and generate a `stack.yaml` that gives instructions to
build all of those packages.

We build the packages, run test suites, and generate Haddocks.

__TODO__ Grab artifacts and upload them to the right place.
