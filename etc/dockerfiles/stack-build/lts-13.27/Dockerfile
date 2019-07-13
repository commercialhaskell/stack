FROM $DOCKER_REPO:lts-13.26
ARG VARIANT
RUN if [ "$VARIANT" = "small" ]; then \
        apt-get update && \
        apt-get install -y --no-install-recommends sudo && \
        rm -rf /var/lib/apt/lists/*; \
    fi
ARG STACK_VERSION=2.1.3
RUN wget -qO- https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64-static.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'
