FROM ubuntu:17.10
MAINTAINER Christian Schafmeister <meister@temple.edu>

# add LLVM repo
RUN apt-get update && apt-get -y upgrade && apt-get install -y wget curl
RUN curl http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN echo 'deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-5.0 main'\
  >/etc/apt/sources.list.d/llvm4.list

# Copied from the original dependencies
RUN apt-get update && apt-get upgrade -y && apt-get install -y \
  libunwind-dev liblzma-dev libgmp-dev binutils-gold binutils-dev \
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
  libboost-filesystem1.62.0 libboost-date-time1.62.0 libboost-program-options1.62.0 \
  libboost-iostreams1.62.0 libgc1c2 llvm-5.0-runtime libgmpxx4ldbl \
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

ADD cando-build.tgz /home/app/clasp
ADD tools/dockerfiles/jupyter/kernel.json /home/app/.local/share/jupyter/kernels/lisp/kernel.json
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
  && git clone --depth=1 https://github.com/clasp-developers/cl-jupyter.git \
  && git clone --depth=1 https://github.com/clasp-developers/uuid.git \
  && git clone --depth=1 https://github.com/clasp-developers/bordeaux-threads.git \
  && git clone --depth=1 https://github.com/clasp-developers/cffi.git

# checkout slime
RUN git clone --depth=1 https://github.com/slime/slime $HOME/slime

RUN clasp -l "/home/app/quicklisp/setup.lisp" \
           -e '(setq core::*swank-home* "/home/app/slime")'\
           -e '(load (format nil "~a/swank-loader.lisp" core::*swank-home*))'\
           -e "(swank-loader:init :delete nil :reload nil :load-contribs nil)"\
           -e "(core:quit)"

ADD amber16-data.tar /home/app
RUN chown -R app:app /home/app/amber16-data
ENV AMBERHOME=/home/app/amber16-data/

RUN cp /home/app/clasp/lib/clasp/extensions/cando/src/data/force-field/ATOMTYPE_SYBYL.DEF /home/app/clasp/lib/clasp/extensions/cando/src/data/force-field/atomtype_sybyl.def

ADD tools/dockerfiles/start-demos.sh /home/app/start-demos.sh
ADD demos.tar /home/app/
# RUN chown -R root:root /home/app/demos

# USER root
# RUN mkdir /home/app/home
# RUN chown -R app:app /home/app/home
# RUN chown -R app:app /home/app/.local
# RUN chown -R app:app /home/app/.jupyter
# # RUN chown -R app:app /home/app/.cache
# RUN chown -R app:app /home/app/amber16
# RUN chown -R app:app /home/app/clasp
# RUN chown -R app:app /home/app/demos
# RUN chown -R app:app /home/app/home
# RUN chown -R app:app /home/app/nglview
# RUN chown -R app:app /home/app/quicklisp
# RUN chown -R app:app /home/app/slime
# USER app

ENV HOME /home/app/
ENTRYPOINT [ "jupyter", "notebook" ]
CMD [ "--allow-root", "--no-browser", "--ip=0.0.0.0", "--port=8888" ]
