FROM clasp-dependencies
MAINTAINER Christian Schafmeister <meister@temple.edu>

# add full source repo into the 'app' user's $HOME/clasp
ADD ./install ${HOME}/clasp
USER root
RUN chown -R app:app ${HOME}/clasp
USER app

