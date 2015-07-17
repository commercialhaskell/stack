#-*- mode:conf; -*-

FROM ubuntu:14.04
MAINTAINER Tim Dysinger <tim@fpcomplete.com>

ENV DEBIAN_FRONTEND noninteractive
# RUN apt-get update \
#  && apt-get install -y net-tools
# RUN echo "Acquire::http { Proxy \"http://$(netstat -nr|grep '^0\.0\.0\.0'|awk '{print $2}'):3142\"; };" \
#   | tee /etc/apt/apt.conf.d/02proxy
RUN apt-get update \
 && apt-get install -y netbase ca-certificates libgmp10 \
 && apt-get clean
RUN update-ca-certificates
# RUN rm /etc/apt/apt.conf.d/02proxy
