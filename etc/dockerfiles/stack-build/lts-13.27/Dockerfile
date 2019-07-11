FROM $DOCKER_REPO:lts-13.26
ARG VARIANT
RUN if [ "$VARIANT" = "small" ]; then \
        apt-get update && \
        apt-get install -y --no-install-recommends sudo && \
        rm -rf /var/lib/apt/lists/*; \
    fi
