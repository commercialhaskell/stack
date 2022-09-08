<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Releases

## TO DO: future release-related tasks

* Simplify the branch or version structure -- just release from the `master`
  branch (but will keep the `stable` branch tracking the latest stable release
  plus updates to documentation).

* At some point (a couple of major releases after 2.3), remove the `-static`
  version from
  https://github.com/commercialhaskell/stackage-content/blob/master/stack/releases.yaml.
  People still using that will get an error, and we'll add a release note to
  switch over to https://get.haskellstack.org/stable/linux-x86_64.tar.gz instead
  (and note that www.stackage.org/stack is deprecated).

## Version scheme

* Versions with an _even_ second component are development versions (the
  `master` branch)
* Versions with an _odd_ second component are stable versions (the `stable`
  branch, or in a `rc/vX.Y` release candidate branch for not-yet-released
  versions)
* Versions with an _even_ third component (e.g. 1.6.2 and 1.7.0) are unreleased
  versions
* Versions with an _odd_ third component (e.g. 1.6.1 or 1.7.3) are released
  versions
* Pre-release unstable binaries will be released with the date as the fourth
  component (e.g. 1.6.0.20171129)
* Release candidate binaries will be released with an even third component and
  and odd number as the fourth component (e.g. 1.7.0.1)
* Hackage-only dependency compatibility patch releases add a fourth patchlevel
  component (e.g. v1.7.3.1, in the `release` branch)
* All branches _except_ `release` (which matches exactly the most recent
  release) must have an even third component (development)
* Branches other than `stable`, `release`, and a `rc/vX.Y` release candidate
  will always have a `0` third component (e.g. 1.7.0).

Examples:

* `1.7.0.0`: v1.7.x series pre-release branch (`v1.7` branch)
* `1.7.0.1`: release candidate for first release of v1.7.x series (`v1.7`
  branch)
* `1.7.0.2`: continuing development on pre-release branch
* `1.7.0.3`: second release candidate for first release of v1.7.x series (`v1.7`
  branch)
* `1.7.1`: first release of the 1.7.x series (`release` branch)
* `1.7.2.0`: development for second release of 1.7.x series (`stable` branch)
* `1.7.2.1`: release candidate for second release of 1.7.x series (`stable`
  branch)
* `1.7.3`: second release of 1.7.x series (`release` branch)
* `1.7.3.1`: first hackage-only patch of 1.7.3 (`release` branch)
* `1.7.3.2`: second hackage-only patch of 1.7.3 (`release` branch)
* `1.8.0`: unstable development code (`master` branch)
* `1.8.0.20181004`: pre-release snapshot of unstable version (`master` branch)

## Pre-release checks

1. Check for any P0 and P1 issues that should be dealt with before release
2. Check for un-merged pull requests that should be merged before release
3. Ensure the `release` and `stable` branches are merged to the `master` branch
4. Check copyright dates, and update if needed
5. Ensure CI matrices in docs (travis-complex, appveyor, azure) have current
   stackage snapshots and GHC versions (e.g.
   https://github.com/commercialhaskell/stack/pull/4565/files)
6. Update the `stack-*.yaml` that uses a `nightly` snapshot to the latest
   nightly (go over the extra-deps too) and ensure the project builds and tests
   pass. For example, command:

    ~~~text
    stack build --stack-yaml=… --haddock --test --bench --no-run-benchmarks
    ~~~

7. Ensure integration tests pass on a Windows, macOS, and Linux. Do so by
   checking that the latest nightly build for the `master` branch succeeded in
   Azure DevOps (or kick one off manually if any significant changes were made
   since the last automated build).

## Release preparation

### A: In the `master` branch

* `package.yaml`: bump to the next release candidate version (bump the second
  component to the next odd number, ensure the third component is `0`, and add
  patchlevel `0`; e.g. from `1.8.0` to `1.9.0.0`). Be sure to also update
  `stack.cabal` (e.g. by running `stack build`).
* `ChangeLog.md`: Check for any entries that snuck into the previous version's
  changes due to merges (`git diff origin/stable HEAD ChangeLog.md`)

### B: Create a new release candidate branch

Cut a new release candidate (RC) branch named `rc/vX.Y` from the `master`
branch.

### C: Return to the `master` branch

* `package.yaml`: bump version to the next unstable version (bump the second
  component to the next even number, ensure the third component is `0`; e.g.
  from `1.9.0` to `1.10.0`). Be sure to also update `stack.cabal` (e.g. by
  running `stack build`).
* `Changelog.md`:
    * Change the title of the existing **Unreleased changes** section to what
      will be the next final (non-RC) release (e.g. `v2.1.1`).
    * add new "Unreleased changes" section:

            ## Unreleased changes

            Release notes:

            **Changes since vX.Y.Z:**

            Major changes:

            Behavior changes:

            Other enhancements:

            Bug fixes:

### D: In the release candidate branch

Review documentation for any changes that need to be made:

* Ensure all the documentation pages are listed in the `mkdocs.yaml` file. Use
  `git diff --stat origin/stable..HEAD doc/` to look for new or deleted files.
* Any new documentation pages should have the "may not be correct for
  the released version of Stack" warning at the top.
* Search for old Stack version, unstable Stack version, and the next "obvious"
  possible versions in sequence, and `UNRELEASED` and replace with next release
  version (`X.Y.1`, where Y is odd).
    * Do **NOT** update the Dockerfiles in
      [stackage/automated/dockerfiles](https://github.com/commercialhaskell/stackage/tree/master/automated/dockerfiles/)
      yet; that will come later)
    * Do **NOT** update templates in `.github` to point at the new release
      version yet!
* Search for old resolvers, set to latest resolver (e.g. in `doc/GUIDE.md` where
  it references the "currently the latest LTS")
* Look for any links to "latest" (`latest/`) documentation, replace with
  version tag

Check for any platform entries that need to be added to (or removed from):

* [releases.yaml](https://github.com/fpco/stackage-content/blob/master/stack/releases.yaml),
* [install_and_upgrade.md](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md),
* [get-stack.sh](https://github.com/commercialhaskell/stack/blob/master/etc/scripts/get-stack.sh),
* [doc/README.md](https://github.com/commercialhaskell/stack/blob/master/doc/README.md),
  and
* `get.haskellstack.org` redirects.

### E: For the first release candidate

1. Re-do the pre-release checks (see the section above).
2. `package.yaml`: bump to first odd patchlevel version (e.g. `X.Y.0.1`). Be
   sure to also update `stack.cabal` (e.g. by running `stack build`).
3. `ChangeLog.md`: Rename the “Unreleased changes” section to the same version
   as `package.yaml`, and mark it clearly as a release candidate (e.g.
   `vX.Y.0.1 (release candidate)`). Remove any empty sections.
4. Follow the steps in the *Release process* section below that are tagged with
   `[RC]` to make a release candidate.

### F: For any subsequent release candidates

1. Re-do the pre-release checks (see the section above).
2. `package.yaml`: bump to next odd patchlevel version (e.g. `X.Y.0.3`). Be
   sure to also update `stack.cabal` (e.g. by running `stack build`).
3. `ChangeLog.md`: Rename the "Unreleased changes" section to the new version,
   clearly marked as a release candidate (e.g. `vX.Y.0.3 (release candidate)`).
   Remove any empty sections.
4. Follow the steps in the *Release process* section below that are tagged with
   `[RC]` to make a release candidate.

### G: For the final release

1. Re-do the pre-release checks (see the section above).
2. `package.yaml`: bump version to odd last component and no patchlevel
   (e.g. from `X.Y.0.2` to `X.Y.1`). Be sure to also update `stack.cabal`
   (e.g. by running `stack build`).
4. `ChangeLog.md`: consolidate all the release candidate changes into a single
   section for the final release version.
5. Follow all of the steps in the *Release process* section below.

## Release process

### A: Integration tests workflow passes `[RC]`

Ensure that the GitHub
[Integration Tests workflow](https://github.com/commercialhaskell/stack/actions?query=workflow%3A%22Integration+tests%22)
passes on the branch that you are releasing.

This workflow will run automatically for `master`, `stable`, and `rc/*`
branches. For another branch, you can run it manually.

### B: Push a signed Git tag `[RC]`

Push a signed Git tag. For further information, see the
[signing key][SIGNING_KEY.md] documentation.

For final releases the tag should be `vX.Y.Z` (where X.Y.Z matches the version
in `package.yaml` from the previous step).

For release candidates the tag should be `rc/vX.Y.Z.A`.

For example, command:

~~~text
git tag -u <YOUR-GPG-KEY> -m vX.Y.Z vX.Y.Z
git push origin vX.Y.Z`
~~~

### C: Edit the draft GitHub release, and publish it `[RC]`

Wait for the GitHub
[Integration Tests workflow](https://github.com/commercialhaskell/stack/actions?query=workflow%3A%22Integration+tests%22)
to complete for the branch you just created. This will create a draft GitHub
release and upload the bindists (plus signatures and hashes) to it.

Edit the draft
[GitHub release](https://github.com/commercialhaskell/stack/releases/):

* In the case of a release candidate, add `(release candidate)` to the name
  field and ensure that *This is a pre-release* is checked.
* Add the ChangeLog to the description.
* For final releases (**not** release candidates) get the list of contributors
  to the release and add it to the description. For example, command:

    ~~~text
    git shortlog -s origin/release..HEAD|sed $'s/^[0-9 \t]*/* /'|grep -v azure-pipelines|LC_ALL=C sort -f
    ~~~

Publish the GitHub release.

### D: Upload to Hackage and reset branches

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

Update the `stable` branch similarly.

Merge any changes made in the RC, `release` or `stable` branches to the `master`
branch. Be careful about version and `ChangeLog.md`. It is best to do this by
making a `ci/merge-stable-to-master` branch and waiting for CI to pass, then
merging. If anything is complicated to merge, consider making it a pull request
and getting it reviewed rather than merging immediately.

Delete the RC branch, both locally and on the remote. For example with the
commands:

~~~text
git branch -d rc/vX.Y
git push origin :rc/vX.Y`
~~~

### E: Activate the version on Read The Docs

Activate the version for new release tag, on
[readthedocs.org](https://readthedocs.org/projects/stack/versions/).

Ensure that the stable documentation has updated.

### F: Update get.haskellstack.org

Update
[get.haskellstack.org /stable and /upgrade rewrite rules](https://gitlab.com/fpco/operations/kube/fpcomplete-sites-project/-/blob/master/fpcomplete-redirects/get-haskellstack_virtualservice.yaml)
with the new version.

Sync the application in
[ArgoCD](https://v5.fpcomplete.com/argocd/applications/fpcomplete-redirects).

Test with the command:

~~~text
curl -vL https://get.haskellstack.org/stable/linux-x86_64.tar.gz >/dev/null
~~~

and make sure it redirects to the new version.

### G: Update versions and `ChangeLog.md` for 'unreleased' `[RC]`

In either the `stable` branch or, in the case of a release candidate, the
`rc/vX.Y` branch:

* `package.yaml`: bump the version number. Either bump the third component to an
  even number (e.g. from `1.6.1` to `1.6.2`) or, in the case of a release
  candidate, bump the fourth component to an even number (e.g. from 1.7.0.1 to
  1.7.0.2). Be sure to also update `stack.cabal` (e.g. by running
  `stack build`).

* `ChangeLog.md`: Add an “Unreleased changes” section (update the “changes
  since” version):

        ## Unreleased changes

        Release notes:

        **Changes since vX.Y.Z:**

        Major changes:

        Behavior changes:

        Other enhancements:

        Bug fixes:

### H: Update the repository's issue and pull request templates

Update the repository's issue and pull request templates in the `.github`
directory to point at the new release version (`X.Y.1`).

### I: Announce the release `[RC]`

Announce the release to the following mailing lists

* haskell-cafe@haskell.org
* haskell-stack@googlegroups.com
* commercialhaskell@googlegroups.com

Use the subject (as applicable):
* `ANN: stack-X.Y.Z`; or
* `ANN: first release candidate for stack-X.Y.x`

Include the release description from GitHub.

For release candidates, also include a link to the GitHub Release
(`https://github.com/commercialhaskell/stack/releases/tag/vX.Y.Z`) to download
it.

### J: Update Docker images

Update the fpco/stack-build Docker images with new version:

* Under
  [commercialhaskell/stackage/automated/dockerfiles](https://github.com/commercialhaskell/stackage/tree/master/automated/dockerfiles/),
  add `lts-X.Y/Dockerfile` (where `X.Y` is the latest Stackage LTS version),
  containing (where `X.Z` is the previous LTS version, and `X.Y.Z` is the newly
  released Stack version):

    ~~~dockerfile
    FROM $DOCKER_REPO:lts-X.Z
    ARG STACK_VERSION=X.Y.Z
    RUN wget -qO- https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'
    ~~~

* Run `./build.sh lts-X.Y` and then test that the new image has the new version
  of Stack. For example, command:

    ~~~text
    docker run --rm fpco/stack-build:lts stack --version
    ~~~

* Use the following commands to push the new image to the registry:

    ~~~text
    ./build.sh --push lts-X.Y
    ./build.sh --push --small lts-X.Y
    ~~~
