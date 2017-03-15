FROM ubuntu:latest
MAINTAINER Christian Schafmeister <meister@temple.edu>

# install all clasp runtime deps
RUN apt-get update && apt-get upgrade -y && apt-get install -y \
  libgc libgmp binutils-gold binutils \
  zlib1g libncurses libboost-filesystem libboost-regex \
  libboost-date-time libboost-program-options libboost-system \
  libboost-iostreams csh flex gfortran zlib1g libbz2 patch \
  git sbcl libexpat

# add clasp binaries
ADD build /clasp/build

# install into $PATH

ENTRYPOINT ['clasp']
