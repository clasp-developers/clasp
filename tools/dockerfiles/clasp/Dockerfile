FROM ubuntu:latest
MAINTAINER Christian Schafmeister <meister@temple.edu>

# add LLVM repo
RUN apt-get -y update && apt-get -y install wget && apt-get clean
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key| apt-key add -
RUN echo 'deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-4.0 main'\
  >/etc/apt/sources.list.d/llvm4.list

# Copied from the original dependencies

RUN apt-get update && apt-get upgrade -y && apt-get install -y \
  libgc-dev libgmp-dev binutils-gold binutils-dev \
  zlib1g-dev libbz2-dev libexpat-dev
  
# Maybe need these...
#  libncurses-dev libboost-filesystem-dev libboost-regex-dev \
#  libboost-date-time-dev libboost-program-options-dev libboost-system-dev \
#  libboost-iostreams-dev csh flex gfortran \
#  clang-4.0 libclang-common-4.0-dev libclang-4.0-dev libclang1-4.0 clang1-4.0-dbg \
#  libllvm4.0 libllvm4.0-dbg lldb-4.0 llvm-4.0 llvm-4.0-dev llvm-4.0-doc \
#  llvm-4.0-runtime clang-format-4.0 python-clang-4.0 lld-4.0


# install clasp runtime dependencies
RUN apt-get -y update && apt-get -y install \
  libboost-filesystem1.58.0 libboost-date-time1.58.0 libboost-program-options1.58.0 \
  libboost-iostreams1.58.0 libgc1c2 llvm-4.0-runtime libgmpxx4ldbl \
  clang-4.0 binutils python-clang-4.0 lld-4.0 libzmq3-dev \
  && apt-get clean

# add app user
RUN groupadd -g 9999 app && useradd -u 9999 -g 9999 -ms /bin/bash app
ENV HOME=/home/app

USER app
WORKDIR $HOME
# add clasp build output
ADD ./build/clasp.tgz ${HOME}/clasp
ENV PATH "$PATH:$HOME/clasp/bin"
ENTRYPOINT [ "clasp" ]
