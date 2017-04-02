FROM clasp-dependencies
MAINTAINER Christian Schafmeister <meister@temple.edu>

ARG CLASP_REVISION
ENV CLASP_REVISION=testing
ARG CANDO_REVISION
ENV CANDO_REVISION=testing

# Copy the entire clasp directory less .dockerignore
ADD . ${HOME}/clasp
USER root
RUN chown -R app:app ${HOME}/clasp
USER app

WORKDIR ${HOME}/clasp
RUN echo "LLVM_CONFIG_BINARY='/usr/bin/llvm-config-4.0'" > wscript.config
RUN echo "INSTALL_PATH_PREFIX='/home/app/install'" >> wscript.config
RUN ./waf update_submodules
RUN CC=/usr/bin/clang-4.0 CXX=/usr/bin/clang++-4.0 ./waf configure

RUN ./waf -j $(nproc) build_cboehm -v | tee build/buildlog.txt
