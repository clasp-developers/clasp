FROM ubuntu:16.10
MAINTAINER Christian Schafmeister <meister@temple.edu>

# add LLVM repo
RUN apt-get update && apt-get -y upgrade && apt-get install -y wget curl
RUN curl http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN echo 'deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-5.0 main'\
  >/etc/apt/sources.list.d/llvm4.list

# Copied from the original dependencies
RUN apt-get update && apt-get upgrade -y && apt-get install -y \
  libgc-dev libunwind-dev liblzma-dev libgmp-dev binutils-gold binutils-dev \
  zlib1g-dev libbz2-dev libexpat-dev

# Maybe need these...
#  libncurses-dev libboost-filesystem-dev libboost-regex-dev \
#  libboost-date-time-dev libboost-program-options-dev libboost-system-dev \
#  libboost-iostreams-dev csh flex gfortran \
#  clang-5.0 libclang-common-5.0-dev libclang-5.0-dev libclang1-5.0 clang1-5.0-dbg \
#  libllvm5.0 libllvm5.0-dbg lldb-5.0 llvm-5.0 llvm-5.0-dev llvm-5.0-doc \
#  llvm-5.0-runtime clang-format-5.0 python-clang-5.0 lld-5.0

# install clasp runtime dependencies
RUN apt-get -y install \
  libboost-filesystem1.61.0 libboost-date-time1.61.0 libboost-program-options1.61.0 \
  libboost-iostreams1.61.0 libgc1c2 llvm-5.0-runtime libgmpxx4ldbl \
  clang-5.0 binutils python-clang-5.0 lld-5.0 libzmq3-dev \
  && apt-get clean

# add app user
RUN groupadd -g 9999 app && useradd -u 9999 -g 9999 -ms /bin/bash app
ENV HOME=/home/app

RUN apt-get update && apt-get -y install git-core 
# AWS command-line utilities
RUN apt-get -y install python-setuptools python-dev build-essential

USER root
# Install sudo
RUN apt-get install -y sudo

RUN easy_install pip
RUN pip install awscli

RUN whoami
RUN python -m pip install numpy
RUN python -m pip install jupyter
RUN python -m pip install ipywidgets
RUN git clone -b "v1.0.a1" https://github.com/arose/nglview /home/app/nglview
RUN whoami 
RUN cd /home/app/nglview && python setup.py install
# probably need to activate widgetsnbextension
RUN python -m ipykernel install --sys-prefix
RUN python -m jupyter nbextension enable --py --sys-prefix widgetsnbextension
RUN python -m jupyter nbextension install --py --sys-prefix nglview
RUN python -m jupyter nbextension enable --py --sys-prefix nglview


ADD tools/dockerfiles/jupyter/kernel.json /home/app/.local/share/jupyter/kernels/lisp/kernel.json
ADD tools/dockerfiles/jupyter/jupyter_notebook_config.py /home/app/.jupyter/jupyter_notebook_config.py
RUN chown -R app:app /home/app/.jupyter /home/app/.local
# $HOME/clasp/Contents/Resources/source-code/extensions/cando/src/lisp
#ADD tools/dockerfiles/start-demos.sh /home/app/start-demos.sh



USER app
WORKDIR $HOME

RUN pwd
RUN ls /tmp


# RUN tar zxvf /build/clasp.tgz

RUN aws s3 cp s3://clasp-cando/docker-cando/clasp.tgz /tmp/clasp.tgz --region us-east-2
RUN aws s3 cp s3://clasp-cando/docker-cando/cando-cache.tgz /tmp/cando-cache.tgz --region us-east-2
RUN mkdir $HOME/clasp
RUN cd $HOME/clasp/ \
    && tar zxvf /tmp/clasp.tgz

RUN cd / \
    && tar zxvf /tmp/cando-cache.tgz

ENV PATH "$PATH:$HOME/clasp/bin"

RUN clasp -l "/home/app/quicklisp/setup.lisp" \
          -e "(ql:quickload :esrap)" \
          -e "(ql:quickload \"parser.common-rules\")" \
          -e "(ql:quickload \"architecture.builder-protocol\")" \
          -l "source-dir:extensions;cando;src;lisp;build-cando-jupyter.lisp" \
          -e "(asdf:make :build-cando-jupyter :print t)" -e "(core:quit)"

# RUN clasp -e "(load \"~/quicklisp/setup.lisp\")" \
#           -e "(ql:quickload :nglview)" \
#           -e "(asdf:load-asd "/home/app//Development/cando/tools/prebuild/jupyter/cando-jupyter.asd")
#           -e "(asdf:operate 'asdf:monolithic-load-bun
#           -e "(core:quit)"


# #RUN tar cvf /tmp/cando-cache.tar $HOME/quicklisp $HOME/.cache/common-lisp
# #RUN gzip /tmp/cando-cache.tar
# #RUN aws s3 cp /tmp/cando-cache.tar.gz s3://clasp-cando/docker-cando/cando-cache.tgz --region us-east-2
# #RUN aws s3 cp /var/log/cloud-init-output.log s3://clasp-cando/docker-cando/cando-cache.could-init-output.log --region us-east-2
