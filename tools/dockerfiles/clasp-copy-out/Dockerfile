FROM clasp-full-build
MAINTAINER Christian Schafmeister <meister@temple.edu>
USER root
RUN mkdir /tmp/clasp
RUN chown -R app:app /tmp/clasp
USER app
RUN ./waf install_cboehm
