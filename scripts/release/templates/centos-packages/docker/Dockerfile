FROM centos:<<CENTOS-VERSION>>

ENV LANG=en_US.UTF-8

RUN yum -y install perl make automake gcc gmp-devel zlib-devel rpm-build tar which

# RUBY & FPM
RUN yum -y install ruby-devel
RUN bash -c 'if [[ "<<CENTOS-VERSION>>" < "7" ]]; then yum -y install rubygems; fi'
RUN gem install fpm

# STACK
RUN curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/centos/<<CENTOS-VERSION>>/fpco.repo | tee /etc/yum.repos.d/fpco.repo
RUN yum -y install stack

# GHC
#EKB FIXME: remove this once stack can install GHC itself on CentOS 6
RUN bash -c 'if [[ "<<CENTOS-VERSION>>" < "7" ]]; then curl -sSL https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-unknown-linux-centos65.tar.xz |tar xJf - && cd ghc-7.8.4 && ./configure --prefix=/usr/local && make install; fi'

# RUN ENVIRONMENT
COPY run.sh /run.sh
RUN chmod a+x /run.sh
CMD /run.sh
