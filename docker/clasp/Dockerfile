FROM ubuntu:jammy

SHELL ["/bin/bash", "-c"]

ARG D_USER=app
ARG D_UID=1000

ENV DEBIAN_FRONTEND=noninteractive
ENV USER ${D_USER}
ENV HOME /home/${D_USER}
ENV PATH "${HOME}/.local/bin:${PATH}"

RUN apt-get update && \
    apt-get dist-upgrade -y && \
    apt-get install -o Dpkg::Options::="--force-overwrite" -y \
        nano wget sudo ecl sbcl git locales curl && \
    bash -c "$(curl -fsSL https://www.thirdlaw.tech/pkg/clasp.sh)"

RUN echo 'en_US.UTF-8 UTF-8' >/etc/locale.gen
RUN sudo -E locale-gen

RUN useradd --create-home --shell=/bin/false --uid=${D_UID} ${D_USER} && \
    usermod -aG sudo $D_USER && \
    passwd -d $D_USER

WORKDIR ${HOME}
USER ${D_USER}

RUN wget https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --non-interactive --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql-util:without-prompting (ql:add-to-init-file))" && \
    rm quicklisp.lisp && \
    ecl --load ~/quicklisp/setup.lisp --eval "(ql-util:without-prompting (ql:add-to-init-file))" --eval "(quit)" && \
    clasp --non-interactive --load ~/quicklisp/setup.lisp git  --eval "(ql-util:without-prompting (ql:add-to-init-file))"
