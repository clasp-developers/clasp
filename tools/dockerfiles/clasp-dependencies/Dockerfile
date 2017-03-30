FROM ubuntu:latest
MAINTAINER Christian Schafmeister <meister@temple.edu>

# install all clasp build deps
RUN apt-get update && apt-get upgrade -y && apt-get install -y \
  gcc g++ llvm clang cmake libgc-dev libgmp-dev binutils-gold binutils-dev \
  zlib1g-dev libncurses-dev libboost-filesystem-dev libboost-regex-dev \
  libboost-date-time-dev libboost-program-options-dev libboost-system-dev \
  libboost-iostreams-dev csh flex gfortran zlib1g-dev libbz2-dev patch \
  git sbcl libexpat-dev wget

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
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key| apt-key add -
RUN apt-add-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-4.0 main"
RUN apt-get update
RUN apt-get install -y clang-4.0 libclang-common-4.0-dev libclang-4.0-dev libclang1-4.0 clang1-4.0-dbg libllvm4.0 libllvm4.0-dbg lldb-4.0 llvm-4.0 llvm-4.0-dev llvm-4.0-doc llvm-4.0-runtime clang-format-4.0 python-clang-4.0 lld-4.0
USER app
