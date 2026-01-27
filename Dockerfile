# NOTE: To build a container, you need to run koga first to populate
# the dependencies in dependencies/ and within src/

FROM ghcr.io/yitzchak/archlinux-makepkg:latest AS base-deps

RUN sudo pacman-key --init && \
    sudo pacman -Syu --noconfirm boost expat fmt gmp libbsd libedit clang19 libelf libffi llvm19 ncurses zlib

FROM base-deps AS build-stage

USER root

# Build dependencies
RUN pacman -Syu --noconfirm sbcl ninja

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
      --build-mode=bytecode-faso \
      --llvm-config="/usr/bin/llvm-config-19" \
      --bin-path="/usr/bin/" \
      --share-path="/usr/share/clasp/" \
      --lib-path="/usr/lib/clasp/" \
      --dylib-path="/usr/lib/" \
      --build-path=build/ && \
    ninja -C build && \
    ninja -C build install

FROM base-deps AS clasp

COPY --from=build-stage /usr/share/clasp/ /usr/share/clasp/
COPY --from=build-stage /usr/lib/clasp/ /usr/lib/clasp/
COPY --from=build-stage /usr/bin/clasp /usr/bin/iclasp /usr/bin/
COPY --from=build-stage /usr/lib/libclasp.so /usr/lib/

RUN sudo useradd -m clasp

USER clasp
WORKDIR /home/clasp

ENTRYPOINT ["clasp"]

FROM clasp AS clasp-ql

# ideally use --checksum, but that needs Docker 1.6
ADD --chown=clasp:clasp https://beta.quicklisp.org/quicklisp.lisp ./

RUN clasp --non-interactive \
      --load quicklisp.lisp \
      --eval "(quicklisp-quickstart:install)" \
      --eval "(ql-util:without-prompting (ql:add-to-init-file))" && \
    rm quicklisp.lisp
