<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Docker images

Docker Hub includes Docker images under
[`fpco/stack-build'](https://hub.docker.com/r/fpco/stack-build).

To update those images with a new version of Stack:

1.  Under
    [commercialhaskell/stackage/automated/dockerfiles](https://github.com/commercialhaskell/stackage/tree/master/automated/dockerfiles/),
    add `lts-X.Y/Dockerfile` (where `X.Y` is the latest Stackage Haskell LTS
    version), containing (where `X.Z` is the previous Haskell LTS version,
    and `X.Y.Z` is the newly released Stack version):

    ~~~dockerfile
    FROM $DOCKER_REPO:lts-X.Z
    ARG STACK_VERSION=X.Y.Z
    RUN wget -qO- https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'
    ~~~

2.  Run `./build.sh lts-X.Y`. Then test that the new image has the new
    version of Stack. For example, command:

    ~~~text
    docker run --rm fpco/stack-build:lts stack --version
    ~~~

3.  Use the following commands to push the new image to the registry:

    ~~~text
    ./build.sh --push lts-X.Y
    ./build.sh --push --small lts-X.Y
    ~~~
