#! /bin/sh

. ./lib.sh

# setting up environment for build.
#
# There are basically two build modes:
# - distribution build with everything
#   in /opt/clasp
#   That is the default, encoded in
#   ./env-default.sh
#   as committed to the git tree
# - personal build with quicklisp in
#   ~/quicklisp, including all jupyter
#   and cando modules
#   To use this version put
#   CLASP_QUICKLISP_HOMEDIR=1
#   in a newly created file
#   ./env-local.sh
#
# Notes:
# - you can kill variables from
#   env-default.sh that you do not
#   like by setting them to the
#   empty string

# this should exist, it is checked
# into the git tree
. ./env-default.sh || exit

if [ -f env-local.sh ] ; then
    . ./env-local.sh
    if [ "${CLASP_QUICKLISP_HOMEDIR-}" = 1 ] ; then
        unset ASDF_OUTPUT_TRANSLATIONS
        unset CLASP_QUICKLISP_DIRECTORY
    fi
fi

if [ "${CLASP_WANT_JUPYTER}" = 1 ] ; then
    export CLASP_WANT_CANDO=1
fi

if [ "$1" = configure ] ; then
    if [ "${CLASP_WANT_CANDO}" = 1 ] ; then
        do_git_tree extensions/cando https://github.com/cando-developers/cando.git "${CANDO_BRANCH_OR_REVISION-dev}"
        if [ "${CLASP_WANT_JUPYTER}" = 1 ] ; then
            get_quicklisp_jupyter
        else
            get_quicklisp_cando
        fi
    fi
fi

# that 3>&1 business is a debugging trick.  It allows output
# to escape waf.
# Currently that is used during during install_cboehm when
# it compiles quicklisp.
if [ "${1-}" = configure ] && [ "${CLASP_WANT_JUPYTER}" = 1 ] ; then
    ./waf "$@" --enable-jupyter
else
    ./waf "$@"
fi 3>&1
