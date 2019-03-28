FROM fpco/stack-build:lts-13.0

#
# Add g++ version required for building 'double-conversion'
# (see https://github.com/commercialhaskell/stack/issues/4470)
#

RUN apt-get update && \
    apt-get install -y g++-7 && \
    rm -rf /var/lib/apt/lists/*
