FROM ubuntu:latest
MAINTAINER Christian Schafmeister <meister@temple.edu>

# install all clasp build deps
RUN apt-get update && apt-get upgrade -y && apt-get install -y \
  gcc g++ llvm clang cmake libgc-dev libgmp-dev binutils-gold binutils-dev \
  zlib1g-dev libncurses-dev libboost-filesystem-dev libboost-regex-dev \
  libboost-date-time-dev libboost-program-options-dev libboost-system-dev \
  libboost-iostreams-dev csh flex gfortran zlib1g-dev libbz2-dev patch \
  git sbcl libexpat-dev

# add app user
RUN groupadd -g 9999 app && useradd -u 9999 -g 9999 -ms /bin/bash app
ENV HOME=/home/app
USER app
WORKDIR $HOME

# checkout and build clasp externals
ARG EXTERNALS_REVISION
ENV EXTERNALS_REVISION=master
RUN git clone -b ${EXTERNALS_REVISION} https://github.com/drmeister/externals-clasp \
 && cd externals-clasp && make
