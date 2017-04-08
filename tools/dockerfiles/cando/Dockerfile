FROM benton/clasp
MAINTAINER Christian Schafmeister <meister@temple.edu>

RUN apt-get update && apt-get -y install python2.7 python-pip python-dev \
  ipython ipython-notebook git-core

# get quicklisp and slime
RUN git clone https://github.com/quicklisp/quicklisp-client.git /quicklisp
RUN git clone https://github.com/slime/slime /slime
RUN clasp -e '(load "/slime/swank-loader.lisp")'\
          -e "(swank-loader:init :delete nil :reload nil :load-contribs nil)"\
          -e "(quit)"

# get the setup-cando file from this repo?
RUN cd ~/clasp && git pull origin testing --rebase
RUN cd ~/clasp/extensions/cando && git pull origin dev --rebase
RUN clasp -f "setup-cando"\
          -e '(load "/quicklisp/setup.lisp")'\
          -e "(quit)"

# get subprojects for quicklisp
WORKDIR /quicklisp/local-projects
RUN git clone https://github.com/clasp-developers/usocket.git
RUN git clone https://github.com/clasp-developers/cl-jupyter.git
RUN git clone https://github.com/clasp-developers/bordeaux-threads.git
RUN git clone https://github.com/clasp-developers/cffi.git
RUN clasp -f "setup-cando"\
          -e '(load "/quicklisp/setup.lisp")'\
          -e "(require :inet)"\
          -e '(setq core::*swank-home* "/home/app/slime")'\
          -e '(load (format nil "~a/swank-loader.lisp" core::*swank-home*))'\
          -e "(swank-loader:init :delete nil :reload nil :load-contribs nil)"\
          -e "(quit)"

# install Jupyter
RUN pip install jupyter
ADD tools/dockerfiles/jupyter/kernel.json \
  /root/.local/share/jupyter/kernels/lisp/kernel.json
RUN clasp -e '(load "/quicklisp/setup.lisp")'\
          -e "(ql:quickload :cl-jupyter)"\
          -e "(ql:quickload :cffi)"\
          -e "(ql:quickload :cffi-grovel)"\
          -e "(ql:quickload :nibbles)"\
          -e "(ql:quickload :ironclad)"\
          -e "(ql:quickload :uuid)"\
          -e "(ql:quickload :trivial-utf-8)"\
          -e "(quit)"

CMD [ "jupyter", "notebook", "--no-browser", "--ip=0.0.0.0", "--port=8888" ]
