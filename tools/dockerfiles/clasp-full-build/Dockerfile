FROM clasp-dependencies
MAINTAINER Christian Schafmeister <meister@temple.edu>

ARG CLASP_REVISION
ENV CLASP_REVISION=preview
ARG CANDO_REVISION
ENV CANDO_REVISION=preview

# build clasp as the app user into the /home/app/clasp/build directory
RUN git clone -b ${CLASP_REVISION} https://github.com/drmeister/clasp.git ${HOME}/clasp
WORKDIR ${HOME}/clasp
RUN echo "LLVM_CONFIG_BIN='/usr/bin/llvm-config-4.0'" > wscript.config
RUN git clone -b ${CANDO_REVISION} https://github.com/drmeister/cando.git extensions/cando

RUN ./waf update_submodules
RUN CC=/usr/bin/clang-4.0 CXX=/usr/bin/clang++-4.0 ./waf configure

RUN ./waf -j $(nproc) build_cboehm | tee build/buildlog.txt
RUN git clone https://github.com/quicklisp/quicklisp-client.git ~/quicklisp
