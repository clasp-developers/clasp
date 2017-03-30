FROM clasp-full-build
MAINTAINER Christian Schafmeister <meister@temple.edu>

RUN git clone https://github.com/slime/slime ~/slime
RUN ${HOME}/clasp/build/clasp -e "(load \"~/slime/swank-loader.lisp\")" -e "(swank-loader:init :delete nil :reload nil :load-contribs nil)" -e "(quit)"

RUN cd ~/clasp && git pull origin testing --rebase
RUN cd ~/clasp/extensions/cando && git pull origin dev --rebase
RUN [ "/home/app/clasp/build/clasp", "-f", "setup-cando", \
      				 "-e", "(load \"~/quicklisp/setup.lisp\")", \
				 "-e", "(quit)"]
RUN cd /home/app/quicklisp/local-projects && git clone https://github.com/clasp-developers/usocket.git
RUN [ "/home/app/clasp/build/clasp", "-f", "setup-cando", \
     				 "-e", "(load \"~/quicklisp/setup.lisp\")", \
      				 "-e", "(ql:quickload :trivial-http)", \
      				 "-e", "(require :inet)", \
       				 "-e", "(setq core::*swank-home* \"/home/app/slime\")", \
				 "-e", "(load (format nil \"~a/swank-loader.lisp\" core::*swank-home*))", \
				 "-e", "(swank-loader:init :delete nil :reload nil :load-contribs nil)", \
				 "-e", "(quit)"]
CMD [ "/home/app/clasp/build/clasp", "-f", "docker", \
      				 "-e", "(setq core::*swank-home* \"/home/app/slime\")", \
      				 "-e", "(load \"/home/app/clasp/src/lisp/modules/docker-swank/start-swank.lisp\")"]
