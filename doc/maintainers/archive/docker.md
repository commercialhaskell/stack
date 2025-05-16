<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Docker images

Each Stackage LTS release has two corresponding docker images in the
[fpco/stack-build](https://hub.docker.com/r/fpco/stack-build/) and
[fpco/stack-build-small](https://hub.docker.com/r/fpco/stack-build-small/)
repositories. The former contains every system library needed to build any
package in the snapshot, while the latter only contains a minimal set of system
libraries for basic programs.

The Dockerfiles for building these images are in
[stackage/automated/dockerfiles](https://github.com/commercialhaskell/stackage/tree/master/automated/dockerfiles/).
There is also a
[build.sh](https://github.com/commercialhaskell/stackage/tree/master/automated/dockerfiles/build.sh)
script to help with building and pushing the images (see the
[README](https://github.com/commercialhaskell/stackage/tree/master/automated/dockerfiles/README.md)
for usage instructions).

## Build images for new minor LTS snapshot

In most cases, a new minor LTS snapshot just needs the previous LTS image to be
re-tagged and pushed. If the image needs a patch for the new minor LTS snapshot,
see the next section.

Below, replace `<N>.<M>` with the minor LTS snapshot version.

- Check out the `stable` branch of the
  [Stack repository](https://github.com/commercialhaskell/stack/).

- Build and push the images (both standard and `small` variants) using the
  [build.sh](https://github.com/commercialhaskell/stackage/tree/master/automated/dockerfiles/build.sh)
  script:

    ~~~text
    ./build.sh --push lts-<N>.<M>
    ./build.sh --push --small lts-<N>.<M>
    ~~~

## Patch images for new minor LTS snapshot

Below, replace `<N>.<M>` with the minor LTS snapshot version. and `<N>.<M-1>`
with the previous minor LTS snapshot version.

- Check out the `stable` branch of the
  [Stack repository](https://github.com/commercialhaskell/stack/).

- In `stackage/automated/dockerfiles`, create a new `lts-<N>.<M>` directory.

- Create `lts-<N>.<M>/Dockerfile`, starting with:

    ~~~dockerfile
    FROM $DOCKER_REPO:lts-<N>.<M-1>
    ~~~

- Add layers for any changes that need to be made to the image.

- Build the new image using the
  [build.sh](https://github.com/commercialhaskell/stackage/tree/master/automated/dockerfiles/build.sh)
  script:

    ~~~text
    ./build.sh lts-<N>.<M>
    ./build.sh --small lts-<N>.<M>
    ~~~

- Test the new image. For example, command:

    ~~~text
    stack --snapshot=lts-<N>.<M> new image-test
    cd image-test
    stack --docker build
    ~~~

  This should use the image you just built. Make sure you test that the new
  image actually contains the desired changes.

- Follow the process in the previous section to push the images.

## Build images for new major LTS snapshot release

### Test a Dockerfile prior to new major LTS snapshot release

Replace `<N>` with major version of new LTS snapshot, and `<N-1>` with previous
major LTS snapshot version.

- Check out the `stable` branch of the
  [Stack repository](https://github.com/commercialhaskell/stack/).

- In `stackage/automated/dockerfiles`, create a new `lts-<N>.0` directory.

- Copy `lts-<N-1>.0/Dockerfile` to `lts-<N>.0/Dockerfile`.

- Check the `FROM` statement, make sure the Ubuntu version matches the Ubuntu
  version used in the
  [Stackage Dockerfile](https://github.com/commercialhaskell/stackage/blob/master/Dockerfile).

- Update `GHC_VERSION` to match the version used by the
  [latest nightly snapshot](https://www.stackage.org/nightly).

- Set `LTS_SLUG` to the
  [latest nightly snapshot](https://www.stackage.org/nightly) (this will be
  temporary until the major LTS snapshot is actually released, at which point it
  will be updated to `lts-<N>.0`).

- Update `PID1_VERSION` and `STACK_VERSION` to the latest versions of those
  tools.

- Make sure `CUDA_VERSION` and `JVM_PATH` match what
  [debian-bootstrap.sh](https://github.com/commercialhaskell/stackage/blob/master/debian-bootstrap.sh)
  uses.

- Update `LLVM_PATH` to the version required for the GHC version. This will be
  shown on the download page for the GHC version, which you can reach from
  https://www.haskell.org/ghc/. It should match the base directory used in
  `CLANG_PURE_LLVM_INCLUDE_DIR` in
  [debian-bootstrap.sh](https://github.com/commercialhaskell/stackage/blob/master/debian-bootstrap.sh)
  (leaving off the `/include` suffix).

- Update `BOOTSTRAP_COMMIT` to the Git commit ID of the latest
  [debian-bootstrap.sh](https://github.com/commercialhaskell/stackage/blob/master/debian-bootstrap.sh).

- Check for any other `lts-<N>.*/Dockerfile`s and make sure
  `lts-<N>.0/Dockerfile` includes anything that was updated in those, if they're
  still relevant for LTS-15 (note that a newer
  [debian-bootstrap.sh](https://github.com/commercialhaskell/stackage/blob/master/debian-bootstrap.sh)
  may already include those changes, so check there first).

### Perform basic tests

- Build the image: `docker build -t local/stack-build lts-<N>.0/`.

- Ensure that all the directories listed in `PATH`, `CUDA_PATH`, and `CPATH` and
  any other path-like environment variables actually exist in the image.

- Try building a test package with the new image. Command:

    ~~~text
    stack --snapshot=nightly new image-test`
    cd image-test
    stack --docker --docker-image=local/stack-build build
    ~~~

  This should build without needing to install GHC.

- Build the "small" variant. Command:

    ~~~text
    docker build -t local/stack-build-small --build-arg "VARIANT=small" lts-<N>.0/
    ~~~

- Try building a test package with the new small image. Command:

    ~~~text
    stack --snapshot=nightly new small-image-test
    cd small-image-test
    stack --docker --docker-image=local/stack-build-small build
    ~~~

  This should build without needing to install GHC.

### Build real image once major LTS snapshot has been released

- Update `LTS_SLUG` to `lts-<N>.0`

- Update `BOOTSTRAP_COMMIT` to the git commit ID of the latest
  [debian-bootstrap.sh](https://github.com/commercialhaskell/stackage/blob/master/debian-bootstrap.sh).

- Repeat the tests above, except use `lts-<N>.0` instead of `nightly`.

- Build and push the real images (both standard and `small` variants) using the
  [build.sh](https://github.com/commercialhaskell/stackage/tree/master/automated/dockerfiles/build.sh)
  script:

    ~~~text
    ./build.sh --push lts-<N>.0
    ./build.sh --push --small lts-<N>.0
    ~~~

- Commit and push the new Dockerfile to the `stable` branch.
