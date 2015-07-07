FROM <<DISTRO>>:<<DISTRO-VERSION>>

ENV LANG=en_US.UTF-8

RUN yum -y install perl make automake gcc gmp-devel zlib-devel rpm-build tar which

# RUBY & FPM
RUN yum -y install ruby-devel
RUN bash -c 'if [[ "<<DISTRO>>" == "centos" && "<<DISTRO-VERSION>>" < "7" ]]; then yum -y install rubygems; fi'
RUN gem install fpm

# STACK
RUN curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/<<DISTRO>>/<<DISTRO-VERSION>>/fpco.repo >/etc/yum.repos.d/fpco.repo
RUN yum -y install stack

# RUN ENVIRONMENT
COPY run.sh /run.sh
RUN chmod a+x /run.sh
CMD /run.sh
