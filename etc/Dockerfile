#-*- mode:conf; -*-

FROM ubuntu:<<UBUNTU_VERSION>>

ENV DEBIAN_FRONTEND noninteractive
ENV LANG en_US.UTF-8
RUN locale-gen $LANG

# NOTE: This next block can speed up your repeat assembly times
# significantly. Uncomment to allow. Requires apt-cacher-ng running on
# the docker host.
RUN apt-get update
RUN apt-get install -y net-tools
RUN echo "Acquire::http { Proxy \"http://$(netstat -nr|grep '^0\.0\.0\.0'|awk '{print $2}'):3142\"; };" \
  | tee /etc/apt/apt.conf.d/02proxy

# HASKELL
ENV GHCVER=7.8.4
ENV CABALVER=1.20
RUN apt-get update
RUN apt-get install -y python-software-properties
RUN apt-add-repository -y ppa:hvr/ghc
RUN apt-get update
RUN apt-get install -y ghc-$GHCVER cabal-install-$CABALVER zlib1g-dev wget
ENV PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH

# RUBY & FPM
RUN apt-get install -y ruby-dev rubygems libffi-dev make
RUN gem install fpm

# BUILD
ADD ./ /usr/src/
WORKDIR /usr/src/
RUN rm -rf .cabal-sandbox cabal*config dist
RUN wget http://www.stackage.org/lts/cabal.config
RUN cabal update
RUN cabal sandbox init
RUN cabal install -j --ghc-options='-rtsopts -threaded -with-rtsopts=-N' cpphs .

# DEB PKG
RUN mkdir -p /var/tmp/fpm/stack_<<PKG_VERSION>>-<<GIT_REV_COUNT>>-<<GIT_SHA>>/usr/bin
RUN cp .cabal-sandbox/bin/stack /var/tmp/fpm/stack_<<PKG_VERSION>>-<<GIT_REV_COUNT>>-<<GIT_SHA>>/usr/bin/
RUN fpm \
    -s dir \
    -t deb \
    -n stack \
    -v <<PKG_VERSION>>-<<GIT_REV_COUNT>>-<<GIT_SHA>> \
    -d libc6 \
    -d libgmp10 \
    -C /var/tmp/fpm/stack_<<PKG_VERSION>>-<<GIT_REV_COUNT>>-<<GIT_SHA>> \
    -p /stack_<<PKG_VERSION>>-<<GIT_REV_COUNT>>-<<GIT_SHA>>_amd64.deb \
    usr

# SHIP THE DEB TO THE HOST O.S.
VOLUME /mnt
CMD cp /stack_<<PKG_VERSION>>-<<GIT_REV_COUNT>>-<<GIT_SHA>>_amd64.deb /mnt/
