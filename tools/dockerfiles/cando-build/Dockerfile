FROM ubuntu:17.10
MAINTAINER Christian Schafmeister <meister@temple.edu>

# install all clasp build deps: mostly clang, boost, LLVM4
RUN apt-get update && apt-get -y upgrade && apt-get install -y \
  software-properties-common python-software-properties wget curl
RUN curl http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN apt-add-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-6.0 main"
RUN apt-get update && apt-get install -y \
  gcc g++ llvm clang cmake libunwind-dev liblzma-dev libgmp-dev binutils-gold binutils-dev \
  zlib1g-dev libncurses-dev libboost-filesystem-dev libboost-regex-dev \
  libboost-date-time-dev libboost-program-options-dev libboost-system-dev \
  libboost-iostreams-dev csh flex gfortran zlib1g-dev libbz2-dev patch \
  git sbcl libexpat-dev wget vim libzmq3-dev \
  clang-6.0 libclang-common-6.0-dev libclang-6.0-dev libclang1-6.0 \
  libllvm5.0 libllvm5.0-dbg llvm-6.0 llvm-6.0-dev llvm-6.0-doc \
  llvm-6.0-runtime clang-format-6.0 python-clang-6.0 lld-6.0
ENV CC=/usr/bin/clang-6.0 CXX=/usr/bin/clang++-6.0


# Copy the entire clasp directory less .dockerignore and clean the build dir
RUN mkdir /home/app
RUN mkdir /out
RUN mkdir /out/clasp
WORKDIR /home/app
# add app user, to set correct UID and GID on files
RUN groupadd -g 9999 app && useradd -u 9999 -g 9999 -ms /bin/bash app
ADD . clasp
RUN chown -R app:app /home/app
RUN chown -R app:app /out/clasp

# build boehm
RUN cd /tmp && wget http://www.hboehm.info/gc/gc_source/gc-7.6.4.tar.gz
RUN mkdir /home/util
RUN cd /home/util && tar zxvf /tmp/gc-7.6.4.tar.gz
RUN cd /home/util/gc-7.6.4 \
    && git clone git://github.com/ivmai/libatomic_ops.git \
    && ./configure --enable-handle-fork \
    && make install


USER app
WORKDIR /home/app/clasp
RUN rm -rf build/* && mkdir -p /out/clasp

# checkout submodules, configure, and build
RUN echo "LLVM_CONFIG_BINARY='/usr/bin/llvm-config-6.0'" > wscript.config
RUN echo "PREFIX='/out/clasp'" >> wscript.config
RUN echo "CLASP_VERSION='clasp_docker'" >> wscript.config
RUN echo "USE_PARALLEL_BUILD = True" >> wscript.config
RUN echo "USE_LLD = False" >> wscript.config
RUN echo "CLASP_BUILD_MODE = 'object'" >> wscript.config
RUN git clone --depth=1 https://github.com/quicklisp/quicklisp-client.git $HOME/quicklisp
RUN mkdir $HOME/quicklisp/local-projects
RUN git clone --depth=1 https://github.com/slime/slime $HOME/slime
RUN cd $HOME/quicklisp/local-projects \
 && git clone --depth=1 https://github.com/clasp-developers/usocket.git \
 && git clone --depth=1 https://github.com/drmeister/cl-jupyter.git \
 && git clone --depth=1 https://github.com/clasp-developers/cl-jupyter-widgets.git \
 && git clone --depth=1 https://github.com/clasp-developers/cl-nglview.git \
 && git clone --depth=1 https://github.com/clasp-developers/cl-bqplot.git \
 && git clone --depth=1 https://github.com/clasp-developers/trivial-backtrace.git \
 && git clone --depth=1 https://github.com/clasp-developers/uuid.git \
 && git clone --depth=1 https://github.com/clasp-developers/bordeaux-threads.git \
 && git clone --depth=1 https://github.com/clasp-developers/cffi.git

RUN ./waf configure
RUN ./waf -j $(nproc) build_cboehm
RUN ./waf -j $(nproc) build_dboehm
RUN ./waf install_dboehm

