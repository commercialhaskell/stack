#-*- mode:conf; -*-

FROM ubuntu:<<UBUNTU_VERSION>>

ENV DEBIAN_FRONTEND noninteractive
ENV LANG en_US.UTF-8
RUN locale-gen $LANG

RUN apt-get update
RUN apt-get install -y net-tools wget python-software-properties software-properties-common

# NOTE: This next block can speed up your repeat assembly times
# significantly. Uncomment to allow. Requires apt-cacher-ng running on
# the docker host.
RUN echo "Acquire::http { Proxy \"http://$(netstat -nr|grep '^0\.0\.0\.0'|awk '{print $2}'):3142\"; };" \
  | tee /etc/apt/apt.conf.d/02proxy

# HASKELL
ENV GHCVER=7.8.4
RUN wget -q -O- http://download.fpcomplete.com/ubuntu/fpco.key | apt-key add -
RUN echo 'deb http://download.fpcomplete.com/ubuntu/<<UBUNTU_CODENAME>> stable main'|tee /etc/apt/sources.list.d/fpco.list
RUN apt-add-repository -y ppa:hvr/ghc
RUN apt-get update
RUN apt-get install -y ghc-$GHCVER stack zlib1g-dev
ENV PATH=/opt/ghc/$GHCVER/bin:$PATH

# RUBY & FPM
RUN apt-get install -y ruby-dev libffi-dev make
RUN if [ "<<UBUNTU_VERSION>>" \< "14.04" ]; then apt-get install -y rubygems; fi
RUN gem install fpm

# BUILD
ADD ./ /usr/src/
WORKDIR /usr/src/
RUN find . -name .stack-work -print0 |xargs -0 rm -rf
RUN stack install

# DEB PKG
RUN mkdir -p /var/tmp/fpm/stack_<<PKG_VERSION>>-<<GIT_REV_COUNT>>-<<GIT_SHA>>/usr/bin
RUN cp $HOME/.local/bin/stack /var/tmp/fpm/stack_<<PKG_VERSION>>-<<GIT_REV_COUNT>>-<<GIT_SHA>>/usr/bin/
RUN fpm \
    -s dir \
    -t deb \
    -n stack \
    -v <<PKG_VERSION>>-<<GIT_REV_COUNT>>-<<GIT_SHA>> \
    --deb-recommends git \
    --deb-recommends gnupg \
    -d g++ \
    -d gcc \
    -d libc6-dev \
    -d libffi-dev \
    -d libgmp-dev \
    -d make \
    -d xz-utils \
    -d zlib1g-dev \
    -C /var/tmp/fpm/stack_<<PKG_VERSION>>-<<GIT_REV_COUNT>>-<<GIT_SHA>> \
    -p /stack_<<PKG_VERSION>>-<<GIT_REV_COUNT>>-<<GIT_SHA>>_amd64.deb \
    usr

# SHIP THE DEB TO THE HOST O.S.
VOLUME /mnt
CMD cp /stack_<<PKG_VERSION>>-<<GIT_REV_COUNT>>-<<GIT_SHA>>_amd64.deb /mnt/
