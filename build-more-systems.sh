#! /bin/sh

. ./lib.sh

# this should exist, it is checked
# into the git tree
. ./env-default.sh || exit

if [ -f env.sh ] ; then
    echo `pwd`/env.sh exists
    echo This is probably an error.
    echo The correct name for local variable overrides is
    echo env-local.sh
    exit 1
fi 1>&2

if [ "${CLASP_WANT_JUPYTER}" = 1 ] ; then
    export CLASP_WANT_CANDO=1
fi

if [ "$1" = configure ] && ! [ "${CLASP_BUILD_NO_UPDATES-}" = 1 ] ; then
    if [ "${CLASP_WANT_CANDO}" = 1 ] ; then
        do_git_tree extensions/cando https://github.com/cando-developers/cando.git "${CANDO_BRANCH_OR_REVISION-dev}"
        if [ "${CLASP_WANT_JUPYTER}" = 1 ] ; then
            get_quicklisp_jupyter
        else
            get_quicklisp_cando
        fi
    fi
fi

if [ "`uname`" = FreeBSD ] ; then
    export MAKE=gmake
#    export COMPILER_CC="clang-6.0"
#    export COMPILER_CXX="clang++-6.0"
fi

# TODO:
# - write /opt/clasp/bin/setenv-clasp (function in lib.sh)
# - take quicklisp compilation entirely out of waf
#   (that means the part where it is invoked by simply
#   loading cando or fep)

# that 3>&1 business is a debugging trick.  It allows output
# to escape waf.
# Currently that is used during during install_cboehm when
# it compiles quicklisp.
if [ "${1-}" = configure ] && [ "${CLASP_WANT_JUPYTER}" = 1 ] ; then
    ./waf "$@" --enable-jupyter
else
    ./waf "$@"
fi 3>&1
