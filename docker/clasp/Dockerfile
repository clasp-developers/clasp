FROM ghcr.io/yitzchak/archlinux-makepkg:latest

RUN sudo pacman-key --init && \
    sudo pacman -Syu --noconfirm sbcl

RUN git clone https://aur.archlinux.org/clasp-cl.git && \
    cd clasp-cl && \
    makepkg --noconfirm --syncdeps --install --nocheck && \
    cd .. && \
    rm -rf clasp-cl

USER root
WORKDIR /root

ENV XDG_CONFIG_HOME=/root/.config
ENV XDG_DATA_HOME=/root/.local/share
ENV XDG_CACHE_HOME=/root/.cache

RUN curl -kLO https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --non-interactive --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql-util:without-prompting (ql:add-to-init-file))" && \
    clasp --non-interactive --load ~/quicklisp/setup.lisp --eval "(ql-util:without-prompting (ql:add-to-init-file))" && \
    rm quicklisp.lisp

