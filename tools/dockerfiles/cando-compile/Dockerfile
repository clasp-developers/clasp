FROM clasp
MAINTAINER Christian Schafmeister <meister@temple.edu>

# install Jupyter and its dependencies; configure jupyter kernel
USER root
RUN apt-get update && apt-get -y install git-core 
RUN apt-get -y install python2.7 python-pip python-dev ipython ipython-notebook
RUN pip install --upgrade pip && pip install jupyter
# set ownership and permissions before changing to app user
USER app

# checkout quicklisp and its subprojects
RUN cd $HOME/quicklisp/local-projects \
 && git clone --depth=1 https://github.com/clasp-developers/usocket.git \
 && git clone --depth=1 https://github.com/clasp-developers/cl-jupyter.git \
 && git clone --depth=1 https://github.com/clasp-developers/ironclad.git \
 && git clone --depth=1 https://github.com/clasp-developers/uuid.git \
 && git clone --depth=1 https://github.com/clasp-developers/bordeaux-threads.git \
 && git clone --depth=1 https://github.com/clasp-developers/cffi.git


RUN mkdir /home/app/logs/

RUN clasp -f "setup-cando"\
          -e '(load "/home/app/quicklisp/setup.lisp")'\
          -e "(ql:quickload :trivial-http)"\
          -e "(require :inet)"\
          -e '(setq core::*swank-home* "/home/app/slime")'\
          -e '(load (format nil "~a/swank-loader.lisp" core::*swank-home*))'\
          -e "(swank-loader:init :delete nil :reload nil :load-contribs nil)"\
          -e "(core:quit)"

RUN clasp -e '(load "/home/app/quicklisp/setup.lisp")'\
          -e "(ql:quickload :cl-jupyter)"\
          -e "(ql:quickload :cl-json)"\
          -e "(ql:quickload :cl-base64)"\
          -e "(ql:quickload :pzmq)"\
          -e "(ql:quickload :cffi)"\
          -e "(ql:quickload :cffi-grovel)"\
          -e "(ql:quickload :nibbles)"\
          -e "(ql:quickload :ironclad)"\
          -e "(ql:quickload :uuid)"\
          -e "(ql:quickload :trivial-utf-8)"\
          -l "cando:lisp;setup-cando-jupyter.lisp"\
          -e "(common-lisp-user::configure-jupyter)" \
          -e "(core:quit)"

#USER root
#ADD tools/dockerfiles/demos /home/app/demos
#USER app


