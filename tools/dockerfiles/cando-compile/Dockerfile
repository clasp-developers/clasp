FROM clasp
MAINTAINER Christian Schafmeister <meister@temple.edu>

# install Jupyter and its dependencies; configure jupyter kernel
USER root
RUN apt-get update && apt-get -y install git-core 
RUN apt-get -y install python2.7 python-pip python-dev ipython ipython-notebook
RUN pip install --upgrade pip && pip install jupyter
# set ownership and permissions before changing to app user
USER app


# Overwrite the clasp-build clasp/Contents/Resources/source-code/extensions/cando/src/lisp directory
# with whatever new changes have been made in the hosts  extensions/cando/src/lisp/**   hierarchy
#    This is to refresh the Common Lisp code for Cando
ADD extensions/cando/src/lisp $HOME/clasp/Contents/Resources/source-code/extensions/cando/src/lisp


# checkout quicklisp and its subprojects
RUN cd $HOME/quicklisp/local-projects \
 && git clone --depth=1 https://github.com/clasp-developers/usocket.git \
 && git clone --depth=1 https://github.com/clasp-developers/cl-jupyter.git \
 && git clone --depth=1 https://github.com/clasp-developers/bordeaux-threads.git \
 && git clone --depth=1 https://github.com/clasp-developers/cffi.git


RUN clasp -f "setup-cando"\
          -f "debug-compile-file"\
          -e '(load "/home/app/quicklisp/setup.lisp")'\
          -e "(ql:quickload :trivial-http)"\
          -e "(require :inet)"\
          -e '(setq core::*swank-home* "/home/app/slime")'\
          -e '(load (format nil "~a/swank-loader.lisp" core::*swank-home*))'\
          -e "(swank-loader:init :delete nil :reload nil :load-contribs nil)"\
          -e "(core:quit)"

RUN mkdir /home/app/logs/

RUN clasp -f "debug-compile-file"\
          -e '(load "/home/app/quicklisp/setup.lisp")'\
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
          -e "(core:quit)"

USER root
ADD tools/dockerfiles/examples /home/app/work/examples
RUN chown -R app:app /home/app/.jupyter
RUN chown -R app:app /home/app/work
USER app


