FROM ubuntu:17.10
MAINTAINER Christian Schafmeister <meister@temple.edu>

# add LLVM repo
RUN apt-get update && apt-get -y upgrade && apt-get install -y wget curl
RUN curl http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN echo 'deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-6.0 main'\
  >/etc/apt/sources.list.d/llvm4.list

# Copied from the original dependencies
RUN apt-get update && apt-get upgrade -y && apt-get install -y \
  libunwind-dev liblzma-dev libgmp-dev binutils-gold binutils-dev \
  zlib1g-dev libbz2-dev libexpat-dev \
  libncurses-dev libboost-filesystem-dev libboost-regex-dev \
  libboost-date-time-dev libboost-program-options-dev libboost-system-dev \
  libboost-iostreams-dev csh flex gfortran \
  clang-6.0 libclang-common-6.0-dev libclang-6.0-dev libclang1-6.0 \
  libllvm5.0 libllvm5.0-dbg lldb-6.0 llvm-6.0 llvm-6.0-dev llvm-6.0-doc \
  llvm-6.0-runtime clang-format-6.0 python-clang-6.0 lld-6.0

# install clasp runtime dependencies
RUN apt-get -y install \
  libboost-filesystem1.62.0 libboost-date-time1.62.0 libboost-program-options1.62.0 \
  libboost-iostreams1.62.0 libgc1c2 libgmpxx4ldbl \
  binutils python-clang-6.0 libzmq3-dev \
  git make \
  && apt-get clean

# add app user
RUN groupadd -g 9999 app && useradd -u 9999 -g 9999 -ms /bin/bash app

ENV HOME=/home/app

RUN apt-get -y install python3.6

ENV HOME=/home/app

RUN cd $HOME && git clone https://github.com/nodejs/node.git
RUN cd $HOME && git clone https://github.com/jupyterlab/jupyterlab.git
RUN cd $HOME && git clone https://github.com/quicklisp/quicklisp-client.git

RUN apt-get -y install gcc g++ 
RUN mkdir -p /opt/clasp
RUN cd /home/app/node && git checkout v10.8.0 && ./configure --prefix=/opt/clasp && make install

RUN mkdir -p /opt/clasp/bin
ADD tools/dockerfiles/cando-bundle/pip3 /opt/clasp/bin/pip3
RUN chmod a+x /opt/clasp/bin/pip3
ADD tools/dockerfiles/cando-bundle/pip3 /opt/clasp/bin/pip
RUN chmod a+x /opt/clasp/bin/pip
RUN ln -s /usr/bin/python3.6 /opt/clasp/bin/python3
ENV PATH=/opt/clasp/bin:$PATH
RUN apt-get -y install python3-setuptools python3-pip
RUN cd $HOME/jupyterlab && git checkout 0.32.x && mkdir -p /opt/clasp/jupyter/lab && python3 setup.py build && /opt/clasp/bin/pip3 install --prefix=/opt/clasp .

RUN mkdir -p /opt/clasp/lib
RUN cd /opt/clasp/lib && git clone https://github.com/quicklisp/quicklisp-client quicklisp
RUN mkdir -p /opt/clasp/lib/quicklisp/local-projects
RUN cd /opt/clasp/lib/quicklisp/local-projects && git clone https://github.com/clasp-developers/clasp-local-projects.git
RUN cd /opt/clasp/lib/quicklisp/local-projects/clasp-local-projects && make