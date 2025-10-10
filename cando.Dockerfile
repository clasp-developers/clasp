# NOTE: To build a container, you need to run koga first to populate
# the dependencies in dependencies/ and within src/

FROM ghcr.io/yitzchak/archlinux-makepkg:latest AS base-deps

USER root

RUN pacman-key --init && \
    pacman -Syu --noconfirm binutils inetutils boost clang19 gmp fmt libunwind llvm19 ninja sbcl jupyterlab emacs openssh netcdf expat gocryptfs nodejs npm strace emacs

# Copy source
COPY src/ src/
COPY include/ include/
COPY dependencies/ dependencies/
COPY licenses/ licenses/
COPY tools-for-build tools-for-build/
COPY cscript.lisp koga repos.sexp version.sexp ./

RUN ./koga \
      --skip-sync \
      --reproducible-build \
      --extensions=cando \
      --build-mode=bytecode-faso \
      --llvm-config="/usr/bin/llvm-config-19" \
      --bin-path="/usr/bin/" \
      --share-path="/usr/share/clasp/" \
      --lib-path="/usr/lib/clasp/" \
      --dylib-path="/usr/lib/" \
      --build-path=build/ && \
    ninja -C build && \
    ninja -C build install

RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh && \
    mkdir -p /home/cando/ && \
    tar -xvf systems.tar -C /home/cando && \
    bash Miniconda3-latest-Linux-x86_64.sh -b -p /home/cando/miniconda3 && \
    rm -rf Miniconda3-latest-Linux-x86_64.sh && \
    export PATH="/home/cando/miniconda3/bin:${PATH}" && \
    conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/main && \
    conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/r && \
    conda install -y -c conda-forge jupyterlab sidecar ambertools && \
    jupyter-lab build
