FROM clasp-externals
MAINTAINER Christian Schafmeister <meister@temple.edu>

ARG CLASP_REVISION
ENV CLASP_REVISION=preview
ARG CANDO_REVISION
ENV CANDO_REVISION=preview

# build clasp as the app user into the /home/app/clasp/build directory
RUN git clone -b ${CLASP_REVISION} https://github.com/drmeister/clasp.git ${HOME}/clasp
WORKDIR ${HOME}/clasp
RUN echo "EXTERNALS_CLASP_DIR='$HOME/externals-clasp'" > wscript.config
RUN git clone -b ${CANDO_REVISION} https://github.com/drmeister/cando.git extensions/cando

# add full source repo into the 'app' user's $HOME/clasp
ADD . ${HOME}/clasp
USER root
RUN chown -R app:app ${HOME}/clasp
USER app

<<<<<<< HEAD
RUN ./waf update_submodules && ./waf configure
=======
# build clasp as the app user into the /home/app/clasp/build directory
RUN git clone -b ${CLASP_REVISION} https://github.com/drmeister/clasp.git ${HOME}
WORKDIR ${HOME}/clasp
RUN echo "EXTERNALS_CLASP_DIR='$HOME/externals-clasp'" > wscript.config
RUN git clone -b ${CANDO_REVISION} https://github.com/drmeister/cando.git extensions/cando
RUN ./waf update_submodules
RUN CC=/usr/bin/clang-4.0 CXX=/usr/bin/clang++-4.0 ./waf configure
>>>>>>> Added clasp-dependencies docker files
RUN ./waf -j $(nproc) build_cboehm | tee build/buildlog.txt
RUN git clone https://github.com/slime/slime ~/slime
RUN git clone https://github.com/quicklisp/quicklisp-client.git ~/quicklisp
RUN ${HOME}/clasp -e "(load \"~/slime/swank-loader.lisp\")" -e "(swank-loader:init :delete nil :reload nil :load-contribs nil)" -e "(quit)"
