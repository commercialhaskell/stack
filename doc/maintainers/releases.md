<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Releases

## Upcoming release tasks:

* Simplify branch/version structure -- just release from `master` (but will keep
  `stable` tracking latest stable release plus doc updates)

* At some point (a couple of major releases after 2.3), remove the `-static`
  version from
  https://github.com/commercialhaskell/stackage-content/blob/master/stack/releases.yaml.
  People still using that will get an error, and we'll add a release note to
  switch over to https://get.haskellstack.org/stable/linux-x86_64.tar.gz instead
  (and note that www.stackage.org/stack is deprecated)

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

* Check for any P0 and P1 issues that should be dealt with before release
* Check for un-merged pull requests that should be merged before release
* Ensure the `release` and `stable` branches are merged to the `master` branch
* Check copyright dates, and update if needed
* Ensure CI matrices in docs (travis-complex, appveyor, azure) have current
  stackage snapshots and GHC versions (e.g.
  https://github.com/commercialhaskell/stack/pull/4565/files)
* Update the `stack-*.yaml` that uses a `nightly` snapshot to the latest nightly
  (go over the extra-deps too) and ensure the project builds and tests pass
  (e.g. `stack build --stack-yaml=… --haddock --test --bench --no-run-benchmarks`)
* Ensure integration tests pass on a Windows, macOS, and Linux. Do so by
  checking that the latest nightly build for the `master` branch succeeded in
  Azure DevOps (or kick one off manually if any significant changes were made
  since the last automated build).

## Release preparation

* In master branch:
    * `package.yaml`: bump to next release candidate version (bump second
      component to next odd number, ensure third component is `0`, and add
      patchlevel `0`; e.g. from `1.8.0` to `1.9.0.0`). Be sure to also update
      `stack.cabal` (e.g. by running `stack build`).
    * `ChangeLog.md`
        * Check for any entries that snuck into the previous version's changes
          due to merges (`git diff origin/stable HEAD ChangeLog.md`)

* Cut a release candidate branch `rc/vX.Y` from master

* In master branch:
    * `package.yaml`: bump version to next unstable version (next even second
      component with `.0` third component (e.g. from 1.9.0 to 1.10.0). Be sure
      to also update `stack.cabal` (e.g. by running `stack build`).
    * `Changelog.md`:
      * Change the title of the existing **Unreleased changes** section to what
        will be the next final (non-RC) release (e.g. `v2.1.1`).
      * add new "Unreleased changes" section:
        ```
        ## Unreleased changes

        Release notes:

        **Changes since vX.Y.Z:**

        Major changes:

        Behavior changes:

        Other enhancements:

        Bug fixes:

        ```

* In RC branch:
    * Review documentation for any changes that need to be made
        * Ensure all documentation pages listed in `mkdocs.yaml` (use `git diff
          --stat origin/stable..HEAD doc/` to look for new/deleted files)
        * Any new documentation pages should have the "may not be correct for
          the released version of Stack" warning at the top.
        * Search for old Stack version, unstable stack version, and the next
          "obvious" possible versions in sequence, and
          `UNRELEASED` and replace with next release version (`X.Y.1`, where Y
          is odd).
            * Do **NOT** update the Dockerfiles in
              [stackage/automated/dockerfiles](https://github.com/commercialhaskell/stackage/tree/master/automated/dockerfiles/)
              yet; that will come later)
            * Do **NOT** update templates in `.github` to point at the new
              release version yet!
        * Search for old resolvers, set to latest resolver (e.g. in
          `doc/GUIDE.md` where it references the "currently the latest LTS")
        * Look for any links to "latest" (`latest/`) documentation, replace with
          version tag
    * Check that for any platform entries that need to be added to (or removed
      from)
      [releases.yaml](https://github.com/fpco/stackage-content/blob/master/stack/releases.yaml),
      [install_and_upgrade.md](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md),
      [get-stack.sh](https://github.com/commercialhaskell/stack/blob/master/etc/scripts/get-stack.sh),
      and [doc/README.md](https://github.com/commercialhaskell/stack/blob/master/doc/README.md),
      and get.haskellstack.org redirects.

* For first release candidate:
    * Re-do the pre-release checks (above section)
    * `package.yaml`: bump to first odd patchlevel version (e.g. `X.Y.0.1`). Be
      sure to also update `stack.cabal` (e.g. by running `stack build`).
    * `ChangeLog.md`
        - Rename the “Unreleased changes” section to the same version as
          `package.yaml`, and mark it clearly as a release candidate (e.g.
          `vX.Y.0.1 (release candidate)`). Remove any empty sections.
    * Follow steps in *Release process* below tagged with `[RC]` to make a
      release candidate

* For subsequent release candidates:
    * Re-do the pre-release checks (above section)
    * `package.yaml`: bump to next odd patchlevel version (e.g. `X.Y.0.3`). Be
      sure to also update `stack.cabal` (e.g. by running `stack build`).
    * `ChangeLog.md`: Rename the "Unreleased changes" section to the new
      version, clearly marked as a release candidate (e.g.
      `vX.Y.0.3 (release candidate)`). Remove any empty sections.
    * Follow steps in *Release process* below tagged with `[RC]` to make a
      release candidate

* For final release:
    * Re-do the pre-release checks (above section)
    * `package.yaml`: bump version to odd last component and no patchlevel
      (e.g. from `X.Y.0.2` to `X.Y.1`). Be sure to also update `stack.cabal`
      (e.g. by running `stack build`).
    * `ChangeLog.md`: consolidate all the RC changes into a single section for
      the release version
    * Follow all steps in the *Release process* section below.

## Release process

* Ensure that the
  [Integration Tests workflow on Github Actions](https://github.com/commercialhaskell/stack/actions?query=workflow%3A%22Integration+tests%22)
  passes the branch you are releasing. This will run automatically for `master`,
  `stable`, and `rc/*` branches (if another branch, you can run it manually). `[RC]`

* Push a signed Git tag. For further information, see the
  [signing key][SIGNING_KEY.md] documentation. For final releases the tag should
  be `vX.Y.Z` (where X.Y.Z matches the version in `package.yaml` from the
  previous step); for release candidates it should be `rc/vX.Y.Z.A`. e.g.:
  `git tag -u <YOUR-GPG-KEY> -m vX.Y.Z vX.Y.Z && git push origin vX.Y.Z`.  `[RC]`

* Wait for
  [Integration Tests workflow on Github Actions](https://github.com/commercialhaskell/stack/actions?query=workflow%3A%22Integration+tests%22)
  to complete for the branch you just created. This will create a draft GitHub
  release and upload the bindists (plus signatures and hashes) to it.

* Edit the draft
  [GitHub release](https://github.com/commercialhaskell/stack/releases/),
  and `[RC]`
    * In the case of a release candidate, add `(release candidate)` to the name
      field and ensure that *This is a pre-release* is checked.
    * Add the ChangeLog to the description.
    * For final releases (**not** release candidates), use e.g.
      `git shortlog -s origin/release..HEAD|sed $'s/^[0-9 \t]*/* /'|grep -v azure-pipelines|LC_ALL=C sort -f`
      to get the list of contributors and add it to the description.
    * Publish the GitHub release. `[RC]`

* Upload `stack` package to Hackage: `stack upload . --pvp-bounds=lower`.

* Reset the `release` branch to the released commit, e.g.:
  `git checkout release && git merge --ff-only vX.Y.Z && git push origin release`

* Update the `stable` branch similarly

* Activate version for new release tag, on
  [readthedocs.org](https://readthedocs.org/projects/stack/versions/), and
  ensure that stable documentation has updated.

* Update
  [get.haskellstack.org /stable and /upgrade rewrite rules](https://gitlab.com/fpco/operations/kube/fpcomplete-sites-project/-/blob/master/fpcomplete-redirects/get-haskellstack_virtualservice.yaml)
  with the new version and
  [sync the application in ArgoCD](https://v5.fpcomplete.com/argocd/applications/fpcomplete-redirects).

    * Test with
      `curl -vL https://get.haskellstack.org/stable/linux-x86_64.tar.gz >/dev/null`,
      make sure it redirects to the new version

* In the `stable` or, in the case of a release candidate, `rc/vX.Y` branch:
    - `package.yaml`: bump the version number even third component (e.g. from
      1.6.1 to 1.6.2) or, in the case of a release candidate even _fourth_
      component (e.g. from 1.7.0.1 to 1.7.0.2). Be sure to also update
      `stack.cabal` (e.g. by running `stack build`). `[RC]`

    - `ChangeLog.md`: Add an “Unreleased changes” section (update “changes
      since” version):`[RC]`

      ```
      ## Unreleased changes

      Release notes:

      **Changes since vX.Y.Z:**

      Major changes:

      Behavior changes:

      Other enhancements:

      Bug fixes:

      ```

    - Update templates in `.github` to point at the new release version
      (`X.Y.1`).

* Delete the RC branch (locally and on origin). E.g.
  `git branch -d rc/vX.Y; git push origin :rc/vX.Y`.

* Merge any changes made in the RC/release/stable branches to master (be careful
  about version and changelog). It is best to do this by making a
  `ci/merge-stable-to-master` branch and waiting for CI to pass, then merging.
  If anything is complicated to merge, consider making it a PR and getting it
  reviewed rather than merging immediately.

* Announce to haskell-cafe@haskell.org; haskell-stack@googlegroups.com;
  commercialhaskell@googlegroups.com mailing lists, subject `ANN: stack-X.Y.Z`
  (or `ANN: first release candidate for stack-X.Y.x`), containing the release
  description from Github. `[RC]`

    * For release candidates, also include a link to the Github Release
      (`https://github.com/commercialhaskell/stack/releases/tag/vX.Y.Z`) to
      download it. `[RC]`

* Update fpco/stack-build Docker images with new version

  * Under
    [commercialhaskell/stackage/automated/dockerfiles](https://github.com/commercialhaskell/stackage/tree/master/automated/dockerfiles/),
    add `lts-X.Y/Dockerfile` (where `X.Y` is the latest stackage LTS version),
    containing (note where X.Z is the previous LTS version, and X.Y.Z is the
    newly released stack version)

    ```
    FROM $DOCKER_REPO:lts-X.Z
    ARG STACK_VERSION=X.Y.Z
    RUN wget -qO- https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'
    ```

  * Run `./build.sh lts-X.Y` and test that the new image has the new version of
    Stack (e.g. `docker run --rm fpco/stack-build:lts stack --version`).

  * Run `./build.sh --push lts-X.Y && ./build.sh --push --small lts-X.Y` to push
    the new image to the registry.
