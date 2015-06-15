FROM haskell:7.8
MAINTAINER Greg Weber

# Intended as a base haskell development environment
#
#    docker build -t haskell-stack:7.8 .
#    docker run --rm -i -t -v `pwd`:/home/haskell haskell-stack:7.8 /bin/bash
#
# If you want to re-use your host ~/.stack directory you can mount it into the host.
# Setting the STACK_ROOT env variable may be of help
#
#    docker run --rm -i -t -v `pwd`:/home/haskell/ ~/.stack/:/root/.stack haskell-stack /bin/bash
#
# It is also possible to directly use stack from the image
# However, for that purpose this image is very bloated
#
#    docker run --rm haskell-stack:7.8 stack --help

RUN apt-get update \
    # install profiling libraries
    # stack needs a profiled base to install profiled packages
 && apt-get install -y ghc-7.8.4-prof \
    # ssl certificates for uploading to hackage
 && apt-get install -y libssl-dev ca-certificates \
    # stack needs git
 && apt-get install -y git \
 && apt-get clean

# install stackage binaries to /opt/stackage
RUN mkdir -p /opt/stackage/bin
ENV PATH /opt/stackage/bin:.cabal-sandbox/bin:.cabal/bin:$PATH:./
COPY . /opt/stackage/lib
RUN cd /opt/stackage/lib \
 && cabal update && cabal install cpphs && cabal install \
 && mv /root/.cabal/bin/stack /opt/stackage/bin/ \
 && rm -r /root/.cabal 
