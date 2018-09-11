FROM debian:buster
MAINTAINER Christian Schafmeister <meister@temple.edu>

# install all clasp build deps: mostly clang, boost, LLVM4
RUN apt-get update && apt-get -y upgrade && apt-get install -y wget curl

# Copied from the original dependencies
RUN apt-get update && apt-get upgrade -y && apt-get install -y \
  libunwind-dev liblzma-dev libgmp-dev binutils-gold binutils-dev \
  zlib1g-dev libbz2-dev libexpat-dev libgc-dev

# Maybe need these...
#  libncurses-dev libboost-filesystem-dev libboost-regex-dev \
#  libboost-date-time-dev libboost-program-options-dev libboost-system-dev \
#  libboost-iostreams-dev csh flex gfortran \
#  clang-6.0 libclang-common-6.0-dev libclang-6.0-dev libclang1-6.0 clang1-6.0-dbg \
#  libllvm5.0 libllvm5.0-dbg lldb-6.0 llvm-6.0 llvm-6.0-dev llvm-6.0-doc \
#  llvm-6.0-runtime clang-format-6.0 python-clang-6.0 lld-6.0

# install clasp runtime dependencies
RUN apt-get -y install \
  libboost-filesystem1.62.0 libboost-date-time1.62.0 libboost-program-options1.62.0 \
  libboost-iostreams1.62.0 libgc1c2 llvm-6.0-runtime libgmpxx4ldbl \
  clang-6.0 binutils python-clang-6.0 lld-6.0 libzmq3-dev \
  && apt-get clean

RUN apt-get -y install git
RUN apt-get -y install sbcl

RUN mkdir -p /usr/Development/clasp
WORKDIR /usr/Development/clasp

COPY . .

RUN ./waf add_cando_extension
RUN ./waf distclean
RUN ./waf configure
RUN ./waf build_cboehm

CMD ["printenv"]