FROM haskell:7.8
MAINTAINER Greg Weber

# Intended as a base haskell development environment
#
#    docker build -t haskell-stack:7.8 .
#    docker run --rm -i -t -v `pwd`:/home/haskell haskell-stack /bin/bash
#
# It is also possible to directly use stack from the image
#
#    docker run --rm haskell-stack stack --help
#
# However, for that purpose this image is very bloated

RUN apt-get update && apt-get install sudo \
    # ssl certificates for uploading to hackage
 && apt-get install -y libssl-dev ca-certificates \
    # stack needs git
 && apt-get install -y git \
 && apt-get clean

# install stackage binaries to /opt/stackage
RUN sudo mkdir -p /opt/stackage/bin
ENV PATH /opt/stackage/bin:.cabal-sandbox/bin:.cabal/bin:$PATH:./
ADD . /opt/stackage/lib
RUN cd /opt/stackage/lib \
 && cabal update && cabal install cpphs && cabal install \
 && mv /root/.cabal/bin/stack /opt/stackage/bin/ \
 && rm -r /root/.cabal 
