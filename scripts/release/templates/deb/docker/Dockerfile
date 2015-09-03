FROM <<DISTRO>>:<<DISTRO-VERSION>>

ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get install -y net-tools wget zlib1g-dev lsb-release locales wget ca-certificates libgmp-dev
ENV LANG C.UTF-8

# STACK
RUN wget -q -O- http://download.fpcomplete.com/ubuntu/fpco.key | apt-key add -
RUN echo "deb http://download.fpcomplete.com/<<DISTRO>>/$(lsb_release -cs) stable main" >/etc/apt/sources.list.d/fpco.list
RUN apt-get update
RUN apt-get install -y stack

# RUBY & FPM
RUN apt-get install -y ruby-dev libffi-dev make build-essential
RUN apt-get install -y rubygems || true
RUN gem install fpm

# RUN ENVIRONMENT
COPY run.sh /run.sh
RUN chmod a+x /run.sh
CMD /run.sh
