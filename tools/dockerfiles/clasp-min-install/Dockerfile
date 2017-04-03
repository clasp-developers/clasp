FROM clasp-min-dependencies
MAINTAINER Christian Schafmeister <meister@temple.edu>

ADD ./container/install /home/app/clasp

RUN git clone https://github.com/quicklisp/quicklisp-client.git ~/quicklisp

RUN [ "/home/app/clasp/bin/clasp", "-f", "setup-cando", \
      				 "-e", "(load \"~/quicklisp/setup.lisp\")", \
				 "-e", "(quit)"]

