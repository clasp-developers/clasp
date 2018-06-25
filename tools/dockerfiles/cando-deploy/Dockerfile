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
  zlib1g-dev libbz2-dev libexpat-dev

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

# add app user
RUN groupadd -g 9999 app && useradd -u 9999 -g 9999 -ms /bin/bash app

ENV HOME=/home/app

RUN apt-get -y install python3.6

RUN cd /tmp && wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
RUN cd /tmp && bash Miniconda3-latest-Linux-x86_64.sh -b -p /home/app/miniconda

ENV PATH="/home/app/miniconda/bin:$PATH"
RUN /bin/bash -c "source /home/app/miniconda/bin/activate"

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

RUN apt-get install -y nodejs npm

RUN conda create -n lab python=3.6 -y
RUN /bin/bash -c "source activate lab"
#RUN conda install ipywidgets=7.1.2 -c conda-forge -y
RUN conda install ipywidgets -c conda-forge -y
RUN pip install nglview==1.1.2
RUN nglview enable # might need this to enable nglview-js-widgets extension for notebook
RUN conda install jupyterlab -y -c conda-forge
RUN pip install bqplot
RUN jupyter-labextension install @jupyter-widgets/jupyterlab-manager nglview-js-widgets@1.1.2 bqplot
#RUN jupyter-labextension install nglview-js-widgets@1.1.2

ADD cando-build.tgz /home/app/clasp
ADD tools/dockerfiles/jupyter/kernels /home/app/.local/share/jupyter/kernels
ADD tools/dockerfiles/jupyter/jupyter_notebook_config.py /home/app/.jupyter/jupyter_notebook_config.py
# RUN chown -R app:app /home/app/.jupyter /home/app/.local

#USER app
WORKDIR $HOME

ENV PATH "$PATH:$HOME/clasp/bin"

# checkout quicklisp and its subprojects
RUN git clone --depth=1 https://github.com/quicklisp/quicklisp-client.git $HOME/quicklisp
RUN mkdir $HOME/quicklisp/local-projects

# Checkout quicklisp stuff - but not needed
RUN cd $HOME/quicklisp/local-projects \
  && git clone --depth=1 https://github.com/clasp-developers/usocket.git \
  && git clone --depth=1 https://github.com/drmeister/cl-jupyter.git \
  && git clone --depth=1 https://github.com/clasp-developers/cl-jupyter-widgets.git \
  && git clone --depth=1 https://github.com/clasp-developers/cl-nglview.git \
  && git clone --depth=1 https://github.com/clasp-developers/cl-bqplot.git \
  && git clone --depth=1 https://github.com/clasp-developers/uuid.git \
  && git clone --depth=1 https://github.com/clasp-developers/trivial-backtrace.git \
  && git clone --depth=1 https://github.com/clasp-developers/bordeaux-threads.git \
  && git clone --depth=1 https://github.com/clasp-developers/cffi.git

# checkout slime
RUN git clone --depth=1 https://github.com/slime/slime $HOME/slime

RUN clasp -l "/home/app/quicklisp/setup.lisp" \
           -e '(setq core::*swank-home* "/home/app/slime")'\
           -e '(load (format nil "~a/swank-loader.lisp" core::*swank-home*))'\
           -e "(swank-loader:init :delete nil :reload nil :load-contribs nil)"\
           -e "(core:quit)"

ADD --chown=app:app ambertools18-docker.tar.gz /home/app
ENV AMBERHOME=/home/app/amber18/

RUN cp /home/app/clasp/lib/clasp/extensions/cando/src/data/force-field/ATOMTYPE_SYBYL.DEF /home/app/clasp/lib/clasp/extensions/cando/src/data/force-field/atomtype_sybyl.def

ADD tools/dockerfiles/start-demos.sh /home/app/start-demos.sh
ADD demos.tar /home/app/

RUN apt-get install -y libgfortran4

ENV HOME /home/app/
ENTRYPOINT [ "jupyter", "lab" ]
CMD [ "--allow-root", "--no-browser", "--ip=0.0.0.0", "--port=8888" ]
