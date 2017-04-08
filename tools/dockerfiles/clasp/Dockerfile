FROM ubuntu:latest
MAINTAINER Christian Schafmeister <meister@temple.edu>

RUN apt-get -y update && apt-get -y install wget
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key| apt-key add -
RUN echo 'deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-4.0 main'\
  >/etc/apt/sources.list.d/llvm4.list

RUN apt-get -y update && apt-get -y install \
  libboost-filesystem1.58.0 libboost-date-time1.58.0 libboost-program-options1.58.0 \
  libboost-iostreams1.58.0 libgc1c2 llvm-4.0-runtime libgmpxx4ldbl

# add clasp binaries
ADD ./clasp /usr/local/bin/clasp
RUN chmod 0755 /usr/local/bin/clasp

ENTRYPOINT [ "/usr/local/bin/clasp" ]
