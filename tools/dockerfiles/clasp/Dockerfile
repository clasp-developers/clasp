FROM ubuntu:latest
MAINTAINER Christian Schafmeister <meister@temple.edu>

# add LLVM repo
RUN apt-get -y update && apt-get -y install wget && apt-get clean
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key| apt-key add -
RUN echo 'deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-4.0 main'\
  >/etc/apt/sources.list.d/llvm4.list

# install clasp runtime dependencies
RUN apt-get -y update && apt-get -y install \
  libboost-filesystem1.58.0 libboost-date-time1.58.0 libboost-program-options1.58.0 \
  libboost-iostreams1.58.0 libgc1c2 llvm-4.0-runtime libgmpxx4ldbl \
  && apt-get clean

# add app user
RUN groupadd -g 9999 app && useradd -u 9999 -g 9999 -ms /bin/bash app
ENV HOME=/home/app

# add clasp build output
ADD ./build/clasp.tgz ${HOME}/clasp
RUN chown app:app ${HOME}/clasp

USER app
WORKDIR $HOME
ENV PATH "$PATH:$HOME/clasp/bin"
ENTRYPOINT [ "clasp" ]
