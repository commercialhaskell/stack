<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Releases

## Branches

* The `release` branch is intended to preserve the most recent release
  (including, if applicable, Hackage-only dependency compatibility patch
  releases).

* The `stable` branch is intended to be a copy of the `release` branch together
  with any subsequent commits that (only) revise the documentation for the most
  recent release. That documentation is presented at <https://haskellstack.org>.

* The `master` branch is the current development branch. It is intended that a
  working version of Stack can be built from the branch. The release process
  begins with a copy of the branch.

* A `rc/vX.Y` branch (named after a release in the Stack X.Y.* series) is
  intended to be for release candidates and final releases. It begins as a copy
  of the `master` branch.

## Pre-release checks

1.  Check that Stack is built against the most recent version of `hpack`
    released on Hackage. If not, create an issue and/or raise a pull request
    accordingly.

2.  Check for any important issues that should be dealt with before release.

3.  Check for un-merged pull requests that should be merged before release.

4.  Ensure the `release` and `stable` branches are merged to the `master`
    branch.

5.  Check the copyright dates, and update if needed.

6.  Check the backwards compatibility section of `CONTRIBUTING.md` is up to
    date.

7.  Ensure CI matrices in docs (travis-complex, appveyor, azure) have current
    stackage snapshots and GHC versions (e.g.
    https://github.com/commercialhaskell/stack/pull/4565/files)

8.  Update any `stack-*.yaml` that uses a `nightly` snapshot to the latest
    nightly (go over the extra-deps too) and ensure the project builds and tests
    pass. For example, command:

    ~~~text
    stack build --stack-yaml=… --haddock --test --bench --no-run-benchmarks
    ~~~

9.  The Windows installer is built using an
    [NSIS compiler](https://nsis.sourceforge.io/Main_Page). Check that the NSIS
    compiler that will be used is capable of handling
    [large strings](https://nsis.sourceforge.io/Special_Builds).

10. Ensure the integration tests pass on Linux, macOS and Windows.

11. Some people prefer, or need, to build Stack with Cabal (the tool). Check
    that `cabal.project` is up to date (the specified `with-compiler:`). Check
    that `cabal.config` is up to date and is not missing dependencies relevant
    on Windows and non-Windows operating systems, following the instructions in
    `cabal.project`.

## Release preparation

### A: In the `master` branch

1.  `package.yaml`: bump to the next release candidate version (bump the second
    component to the next odd number, ensure the third component is `0`, and add
    patchlevel `0`; e.g. from `3.4.0` to `3.5.0.0`).

    !!! attention

        Be sure to update also `stack.cabal` (for example by using
        `stack build --dry-run`).

2.  `cabal.config`: Ensure the `stack` constraint is set to the same version as
    in the `package.yaml`.

3.  `ChangeLog.md`: Check for any entries that snuck into the previous version's
    changes due to merges (`git diff origin/stable HEAD ChangeLog.md`)

4.  Commit the changes to the `master` branch.

### B: Create a new release candidate branch

From the `master` branch, checkout a new release candidate (RC) branch named
`rc/vX.Y` (replacing `X.Y` with the first and second components of the release
version).

~~~text
git checkout -b rc/vX.Y
~~~

### C: Return to the `master` branch

1.  `package.yaml`: bump version to the next unstable version (bump the second
    component to the next even number, ensure the third component is `0`; e.g.
    from `3.5.0` to `3.6.0`).

    !!! attention

        Be sure to update also `stack.cabal` (for example by using
        `stack build --dry-run`).

2.  `cabal.config`: Ensure the `stack` constraint is set to the same version as
    in the `package.yaml`.

3.  `Changelog.md`:

    *   Change the title of the existing **Unreleased changes** section to what
        will be the next final (non-RC) release (e.g. `v3.5.1`).

    *   Add new "Unreleased changes" section:

        ~~~markdown
        ## Unreleased changes

        Release notes:

        **Changes since vX.Y.Z:**

        Major changes:

        Behavior changes:

        Other enhancements:

        Bug fixes:
        ~~~

4.  Commit the changes to the `master` branch.

### D: For each release candidate, in the release candidate branch

1.  Review documentation for any changes that need to be made:

    *   Ensure all the documentation pages are listed in the `mkdocs.yaml` file.
        Use `git diff --stat origin/stable..HEAD doc/` to look for new or
        deleted files.

    *   Any new documentation pages should have the "may not be correct for the
        released version of Stack" warning at the top.

    *   Search for old Stack version, unstable Stack version, and the next
        "obvious" possible versions in sequence, and `UNRELEASED` and replace
        with next release version (`X.Y.1`, where Y is odd).

        !!! attention

            Do **NOT** update the repository's issue and pull request templates
            (in the `.github` directory) to point at the new release version
            yet!

    *   Search for old snapshots, set to latest snapshot (e.g. in documentation
        where it references the "currently the latest LTS")

    *   Look for any links to "latest" (`latest/`) documentation, replace with
        version tag

2.  Check for any platform entries that need to be added to (or removed from):

    * [releases.yaml](https://github.com/fpco/stackage-content/blob/master/stack/releases.yaml),

    * [install_and_upgrade.md](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md),

    * [get-stack.sh](https://github.com/commercialhaskell/stack/blob/master/etc/scripts/get-stack.sh),

    * [doc/README.md](https://github.com/commercialhaskell/stack/blob/master/doc/README.md),
      and

    * `get.haskellstack.org` redirects.

3.  Re-do the pre-release checks (see the section above).

4.  Update `package.yaml` and `ChangeLog.md`. This step differs between a first,
    second etc release candidate and a final release.

    === "First RC"

        *   `package.yaml`: bump to first odd patchlevel version (e.g.
            `X.Y.0.1`).

        *   `ChangeLog.md`: Rename the “Unreleased changes” section to the same
            version as `package.yaml`, and mark it clearly as a release
            candidate (e.g. `vX.Y.0.1 (release candidate)`). Remove any empty
            sections.

    === "Second, third etc RC"

        *   `package.yaml`: bump to next odd patchlevel version (e.g.
            `X.Y.0.3`).

        *   `ChangeLog.md`: Rename the “Unreleased changes” section to the same
            version as `package.yaml`, and mark it clearly as a release
            candidate (e.g. `vX.Y.0.3 (release candidate)`). Remove any empty
            sections.

    === "Final Release"

        *   `package.yaml`: bump version to odd last component and no patchlevel
            (e.g. from `X.Y.0.2` to `X.Y.1`).

        *   `ChangeLog.md`: consolidate all the release candidate changes into a
            single section for the final release version.

    !!! attention

        After updating `package.yaml`, be sure to update also `stack.cabal` (for
        example by using `stack build --dry-run`).

5.  Ensure the `stack ==` constraint in `cabal.config` is set to be equal to the
    same version as `package.yaml`.

6.  Commit the changes to the release candidate branch.

7.  Follow the steps in the *Release process* section below that apply.

## Release process

The release process differs between a first, second etc release candidate and a
final release.

=== "First, second etc RC"

    ### A: Integration tests workflow passes

    Ensure that the GitHub
    [Integration Tests workflow](https://github.com/commercialhaskell/stack/actions?query=workflow%3A%22Integration+tests%22)
    passes on the branch that you are releasing.

    This workflow will run automatically for the `rc/*` branch.

    ### B: Push a Git tag

    Push a Git tag. The tag should be `rc/vX.Y.Z.A`, with `X.Y.Z.A` matching the
    version in `package.yaml`.

    For example, command:

    ~~~text
    git tag -m rc/vX.Y.Z.A rc/vX.Y.Z.A
    git push origin rc/vX.Y.Z.A
    ~~~

    ### C: Edit the draft GitHub release, and publish it

    Wait for the GitHub
    [Integration Tests workflow](https://github.com/commercialhaskell/stack/actions?query=workflow%3A%22Integration+tests%22)
    to complete for the branch you just created. This will create a draft GitHub
    release and upload the bindists (plus signatures and hashes) to it.

    Edit the draft
    [GitHub release](https://github.com/commercialhaskell/stack/releases/):

    * Add `(release candidate)` to the name field and ensure that
      *This is a pre-release* is checked.

    * Add the ChangeLog to the description.

    Publish the GitHub release.

    ### D: Consider adding other platforms to the GitHub release

    The
    [Integration Tests workflow](https://github.com/commercialhaskell/stack/actions?query=workflow%3A%22Integration+tests%22)
    is limited to the platforms supported by the GitHub-hosted runners
    (currently, x86_64 and macOS/AArch64) and any self-hosted runners
    (currently, only Linux/AArch64). However, it is possible to edit the GitHub
    release to include binary distributions for other platforms. The
    prerequisites are:

    * a computer with that platform (operating system, machine architecture);

    * a sufficiently-recent existing version of Stack for that platform;

    * a tool to print SHA checksums, such as `shasum` on Linux and macOS; and

    * the GNU Privacy Guard tool (`gpg`), which has had imported the private key
      used to sign Stack executables (see further below).

    The steps are similar to those in the workflow:

    1.  Change to the root directory of the Stack project.

    2.  `stack etc/scripts/release.hs check`, to check before building.

    3.  `stack etc/scripts/release.hs build`, to build. The output 'assets'
        (`stack-<version>-<os>-<architecture> ...`) will be in
        the `_release` directory in the root directory of the Stack project.

    4.  For each of the output assets, create a corresponding SHA 256 file with
        a `.sha256` extension. For example (where `<asset>` is the name of the
        file):

        ~~~text
        shasum -a 256 <asset> > <asset>.sha256
        ~~~

    5.  For each of the output assets, create a corresponding ASCII-armored
        signature file with an `.asc` extension using `gpg`. For example (where
        `<asset>` is the name of the file):

        ~~~text
        gpg --digest-algo=sha512 --detach-sig --armor -u 0x575159689BEFB442 <asset>
        ~~~

    6.  Edit the GitHub release to include the output assets and their
        corresponding `.sha256` and `.asc` files.

    The private key used to sign Stack executables can be exported from a
    version of `gpg` to which it has previously been imported with:

    ~~~text
    gpg --armor --export-secret-key 0x575159689BEFB442
    ~~~

    The private key, so obtained, can be imported into `gpg` by:

    1.  Commanding `gpg --import`.

    2.  Pasting the private key.

    3.  Entering Ctrl+D and Enter.

    ### E: Update versions and `ChangeLog.md` for 'unreleased'

    In the `rc/vX.Y` branch:

    * `package.yaml`: bump the version number. Bump the fourth component to an
       even number (e.g. from `3.5.0.1` to `3.5.0.2`).

        !!! attention

            Be sure to update also `stack.cabal` (for example by using
            `stack build --dry-run`).

    * `cabal.config`: Ensure the `stack` constraint is set to the same version
      as in the `package.yaml`.

    * `ChangeLog.md`: Add an “Unreleased changes” section (update the “changes
      since” version):

        ~~~markdown
        ## Unreleased changes

        Release notes:

        **Changes since vX.Y.Z:**

        Major changes:

        Behavior changes:

        Other enhancements:

        Bug fixes:
        ~~~

    ### F: Request update of GHCup's metadata

    Raise a pull request at the
    [`haskell/ghcup-metadata`](https://github.com/haskell/ghcup-metadata) GitHub
    repository to request an addition to GHCup's latest metadata configuration
    file for prereleases, tagged as the latest prerelease. In the metadata,
    change the tags for any past Stack prereleases to indicate that they are no
    longer the latest prerelease.

    ### G: Announce the release candidate

    Announce the release candidate:

    * on the
      [Haskell Community](https://discourse.haskell.org/c/announcements/10/l/latest)
      forum;

    * in the Haskell Foundation's
      [general](https://matrix.to/#/#haskell-foundation-general:matrix.org) room
      (address `#haskell-foundation-general:matrix.org`) on
      [Matrix](https://matrix.org/);

    * in the Haskell
      [Stack and Stackage](https://matrix.to/#/#haskell-stack:matrix.org) room
      (address `#haskell-stack:matrix.org`) on [Matrix](https://matrix.org/);
      and

    * in Reddit's [Haskell](https://www.reddit.com/r/haskell/) community.

    In each case, use the subject (change 'first' to 'second' etc for subsequent
    release candidates):

    * `[ANN] First release candidate for Stack X.Y.Z`

    In the message, include:

    * a link to the release on GitHub
      (`https://github.com/commercialhaskell/stack/releases/tag/rc/vX.Y.Z.A`) to
      download it

    * the release description from Github.

=== "Final Release"

    ### A: Integration tests workflow passes

    Ensure that the GitHub
    [Integration Tests workflow](https://github.com/commercialhaskell/stack/actions?query=workflow%3A%22Integration+tests%22)
    passes on the branch that you are releasing.

    This workflow will run automatically for `rc/*` branches.

    ### B: Push a Git tag

    Push a Git tag. The tag should be `vX.Y.Z`, where `X.Y.Z` matches the
    version in `package.yaml`.

    For example, command:

    ~~~text
    git tag -m vX.Y.Z vX.Y.Z
    git push origin vX.Y.Z
    ~~~

    ### C: Edit the draft GitHub release, and publish it

    Wait for the GitHub
    [Integration Tests workflow](https://github.com/commercialhaskell/stack/actions?query=workflow%3A%22Integration+tests%22)
    to complete for the tag you just created. This will create a draft GitHub
    release and upload the bindists (plus signatures and hashes) to it.

    Edit the draft
    [GitHub release](https://github.com/commercialhaskell/stack/releases/):

    *   Add the ChangeLog to the description.
    *   Get the list of contributors to the release and add it to the
        description. For example, command:

        === "Unix-like"

            ~~~text
            git shortlog -s origin/release..HEAD|sed 's/^[0-9 \t]*/* /'|LC_ALL=C sort -f
            ~~~

        === "Windows"

            ~~~text
            (git shortlog -s origin/release..HEAD) -Replace '^[0-9 \t]*', '* ' | Sort-Object
            ~~~

            in PowerShell.

    Publish the GitHub release.

    ### D: Consider adding other platforms to the GitHub release

    The
    [Integration Tests workflow](https://github.com/commercialhaskell/stack/actions?query=workflow%3A%22Integration+tests%22)
    is limited to the platforms supported by the GitHub-hosted runners
    (currently, x86_64 and macOS/AArch64) and any self-hosted runners
    (currently, only Linux/AArch64). However, it is possible to edit the GitHub
    release to include binary distributions for other platforms. The
    prerequisites are:

    * a computer with that platform (operating system, machine architecture);

    * a sufficiently-recent existing version of Stack for that platform;

    * a tool to print SHA checksums, such as `shasum` on Linux and macOS; and

    * the GNU Privacy Guard tool (`gpg`), which has had imported the private key
      used to sign Stack executables (see further below).

    The steps are similar to those in the workflow:

    1.  Change to the root directory of the Stack project.

    2.  `stack etc/scripts/release.hs check`, to check before building.

    3.  `stack etc/scripts/release.hs build`, to build. The output 'assets'
        (`stack-<version>-<os>-<architecture> ...`) will be in
        the `_release` directory in the root directory of the Stack project.

    4.  For each of the output assets, create a corresponding SHA 256 file with
        a `.sha256` extension. For example (where `<asset>` is the name of the
        file):

        ~~~text
        shasum -a 256 <asset> > <asset>.sha256
        ~~~

    5.  For each of the output assets, create a corresponding ASCII-armored
        signature file with an `.asc` extension using `gpg`. For example (where
        `<asset>` is the name of the file):

        ~~~text
        gpg --digest-algo=sha512 --detach-sig --armor -u 0x575159689BEFB442 <asset>
        ~~~

    6.  Edit the GitHub release to include the output assets and their
        corresponding `.sha256` and `.asc` files.

    The private key used to sign Stack executables can be exported from a
    version of `gpg` to which it has previously been imported with:

    ~~~text
    gpg --armor --export-secret-key 0x575159689BEFB442
    ~~~

    The private key, so obtained, can be imported into `gpg` by:

    1.  Commanding `gpg --import`.

    2.  Pasting the private key.

    3.  Entering Ctrl+D and Enter.

    ### E: Upload to Hackage and reset branches

    Upload the `stack` package to Hackage with the command:

    ~~~text
    stack upload . --pvp-bounds=lower
    ~~~

    Reset the `release` branch to the released commit. For example, with the
    commands:

    ~~~text
    git checkout release
    git merge --ff-only vX.Y.Z
    git push origin release
    ~~~

    Update the `stable` branch to the released commit. For example, with the
    commands:

    ~~~text
    git checkout stable
    git merge --ff-only vX.Y.Z
    git push origin stable
    ~~~

    Merge any changes made in the RC, `release` or `stable` branches to the
    `master` branch. Be careful about version and `ChangeLog.md`. It is best to
    do this by making a `ci/merge-stable-to-master` branch and waiting for CI to
    pass, then merging. If anything is complicated to merge, consider making it
    a pull request and getting it reviewed rather than merging immediately.

    Delete the RC branch, both locally and on the remote. For example with the
    commands:

    ~~~text
    git branch -d rc/vX.Y
    git push origin :rc/vX.Y
    ~~~

    ### F: Activate the version on Read The Docs

    Activate the version for new release tag, on
    [readthedocs.org](https://readthedocs.org/projects/stack/versions/).

    Ensure that the `stable` documentation has updated.

    ### G: Update get.haskellstack.org redirects

    Update the https://get.haskellstack.org redirects by updating the
    `_redirects` file in the root of the
    `commercialhaskell/get-haskellstack-org` GitHub
    [repository](https://github.com/commercialhaskell/get-haskellstack-org).

    For further information, see the
    [get.haskellstack.org redirects](haskellstack.org.md#gethaskellstackorg-redirects)
    documentation.

    Test with the commands:

    === "Unix-like"

        ~~~text
        curl -vL https://get.haskellstack.org/stable/linux-x86_64.tar.gz >/dev/null
        curl -vL https://get.haskellstack.org/upgrade/linux-x86_64.tar.gz >/dev/null
        ~~~

    === "Windows"

        ~~~text
        curl -vL https://get.haskellstack.org/stable/linux-x86_64.tar.gz >NUL
        curl -vL https://get.haskellstack.org/upgrade/linux-x86_64.tar.gz >NUL
        ~~~

    and make sure it redirects to the new version.

    ### H: Update versions and `ChangeLog.md` for 'unreleased'

    In the `stable` branch:

    * `package.yaml`: bump the version number. Bump the third component to an
      even number (e.g. from `3.5.1` to `3.5.2`).

        !!! attention

            Be sure to update also `stack.cabal` (for example by using
            `stack build --dry-run`).

    * `cabal.config`: Ensure the `stack` constraint is set to the same version
      as in the `package.yaml`.

    ### I: Update the repository's issue and pull request templates

    The repository's issue and pull request templates are the `.github`
    directory. Update them to refer to the new release version (`X.Y.Z`).

    ### J: Request update of GHCup's metadata

    Raise a pull request at the
    [`haskell/ghcup-metadata`](https://github.com/haskell/ghcup-metadata) GitHub
    repository to request an addition to GHCup's latest metadata configuration
    files for releases and 'vanilla' releases, tagged as the latest release.
    (The GHCup project will decide whether, and when, to recommend the release.)
    In the metadata, change the tags for any past Stack releases to indicate
    that they are no longer the latest release.

    ### K: Announce the release

    Announce the release:

    * on the
      [Haskell Community](https://discourse.haskell.org/c/announcements/10/l/latest)
      forum.

    * in the Haskell Foundation's
      [general](https://matrix.to/#/#haskell-foundation-general:matrix.org)
      room (address `#haskell-foundation-general:matrix.org`) on
      [Matrix](https://matrix.org/).

    * in the Haskell
      [Stack and Stackage](https://matrix.to/#/#haskell-stack:matrix.org) room
      (address `#haskell-stack:matrix.org`) on [Matrix](https://matrix.org/).

    * in Reddit's [Haskell](https://www.reddit.com/r/haskell/) community.

    In each case, use the subject:

    * `[ANN] Stack X.Y.Z`

    In the message, include:

    * the release description from Github.
