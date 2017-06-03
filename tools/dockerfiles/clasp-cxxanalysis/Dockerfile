FROM clasp
MAINTAINER Christian Schafmeister <meister@temple.edu>

# Overwrite the clasp-build clasp/Contents/Resources/source-code/extensions/cando/src/lisp directory
# with whatever new changes have been made in the hosts  extensions/cando/src/lisp/**   hierarchy
#    This is to refresh the Common Lisp code for Cando
ADD extensions/cando/src/lisp $HOME/clasp/Contents/Resources/source-code/extensions/cando/src/lisp


# checkout quicklisp and its subprojects
RUN git clone --depth=1 https://github.com/quicklisp/quicklisp-client.git \
  $HOME/quicklisp
RUN mkdir $HOME/quicklisp/local-projects
RUN cd $HOME/quicklisp/local-projects \
 && git clone --depth=1 https://github.com/clasp-developers/usocket.git \
 && git clone --depth=1 https://github.com/clasp-developers/bordeaux-threads.git \
 && git clone --depth=1 https://github.com/clasp-developers/cffi.git

# checkout slime
RUN git clone --depth=1 https://github.com/slime/slime $HOME/slime

RUN clasp -e '(load "/home/app/quicklisp/setup.lisp")'\
          -e "(ql:quickload :trivial-http)"\
          -e "(require :inet)"\
          -e '(setq core::*swank-home* "/home/app/slime")'\
          -e '(load (format nil "~a/swank-loader.lisp" core::*swank-home*))'\
          -e "(swank-loader:init :delete nil :reload nil :load-contribs nil)"\
          -e "(quit)"

RUN clasp -e '(load "/home/app/quicklisp/setup.lisp")'\
          -e "(ql:quickload :cffi)"\
          -e "(ql:quickload :cffi-grovel)"\
          -e "(quit)"

ENTRYPOINT [ "/home/app/clasp/bin/clasp" ]
CMD        [ "--load", "/home/app/slime/start-swank.lisp" ]
