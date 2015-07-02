FROM ubuntu:${ubuntuVersion}

ENV DEBIAN_FRONTEND=noninteractive \\
    LANG=en_US.UTF-8
RUN locale-gen $LANG

RUN apt-get update
RUN apt-get install -y net-tools wget python-software-properties software-properties-common

# HASKELL
ENV GHCVER=7.8.4
RUN wget -q -O- http://download.fpcomplete.com/ubuntu/fpco.key | apt-key add -
RUN echo "deb http://download.fpcomplete.com/ubuntu/$(lsb_release -cs) stable main"|tee /etc/apt/sources.list.d/fpco.list
RUN apt-add-repository -y ppa:hvr/ghc
RUN apt-get update
RUN apt-get install -y ghc-$GHCVER stack zlib1g-dev
ENV PATH=/opt/ghc/$GHCVER/bin:$PATH

# RUBY & FPM
RUN apt-get install -y ruby-dev libffi-dev make
RUN bash -c 'if [[ "$(lsb_release -rs)" < "14.04" ]]; then apt-get install -y rubygems; fi'
RUN gem install fpm

# RUN ENVIRONMENT
COPY run.sh /run.sh
RUN chmod a+x /run.sh
CMD /run.sh
