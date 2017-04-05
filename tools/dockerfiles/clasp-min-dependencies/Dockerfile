FROM ubuntu:latest
MAINTAINER Christian Schafmeister <meister@temple.edu>

# install all clasp build deps
RUN apt-get update && apt-get upgrade -y && apt-get install -y \
  llvm clang cmake libgc-dev libgmp-dev binutils-gold binutils-dev zlib1g-dev
RUN apt-get install -y libncurses5 
RUN apt-get install -y libboost-filesystem1.58.0 libboost-regex1.58.0 \
  libboost-date-time1.58.0 libboost-program-options1.58.0 libboost-system1.58.0 \
  libboost-iostreams1.58.0 csh flex gfortran zlib1g libbz2-1.0 patch \
  libexpat1 vim libzmq3-dev

# add app user
RUN groupadd -g 9999 app && useradd -u 9999 -g 9999 -ms /bin/bash app
ENV HOME=/home/app
USER app
WORKDIR $HOME

# checkout and build clasp externals
ARG EXTERNALS_REVISION
ENV EXTERNALS_REVISION=master

USER root
RUN apt-get install -y software-properties-common python-software-properties
RUN apt-get install -y wget git
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key| apt-key add -
RUN apt-add-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-4.0 main"
RUN apt-get update
RUN apt-get install -y clang-4.0 libclang-common-4.0 libclang-4.0 libclang1-4.0 \
  libllvm4.0 lldb-4.0 llvm-4.0 llvm-4.0 llvm-4.0-runtime 
USER app
