FROM clasp-externals
MAINTAINER Christian Schafmeister <meister@temple.edu>

ARG CANDO_REVISION
ENV CANDO_REVISION=preview

# add full source repo into the 'app' user's $HOME/clasp
ADD . ${HOME}/clasp
USER root
RUN chown -R app:app ${HOME}/clasp
USER app

# build clasp as the app user into the /home/app/clasp/build directory
#RUN git clone -b ${CANDO_REVISION} https://github.com/drmeister/clasp.git ${HOME}
WORKDIR ${HOME}/clasp
RUN echo "EXTERNALS_CLASP_DIR='$HOME/externals-clasp'" > wscript.config
RUN git clone -b ${CANDO_REVISION} https://github.com/drmeister/cando.git extensions/cando
RUN ./waf update_submodules && ./waf configure
RUN ./waf -j $(nproc) build_cboehm | tee build/buildlog.txt
