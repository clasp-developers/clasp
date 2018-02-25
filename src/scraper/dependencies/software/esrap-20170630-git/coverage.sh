#!/bin/sh

#  Helper script for coverage report generation.
#
#  Copyright (C) 2014 Jan Moringen
#
#  Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

SBCL="${HOME}/opt/sbcl/bin/sbcl"
QUICKLISP="${HOME}/.local/share/common-lisp/quicklisp"

"${SBCL}" --noinform --disable-ldb --lose-on-corruption \
          --no-userinit --disable-debugger              \
          --load "${QUICKLISP}/setup.lisp"              \
          --load "coverage.lisp"                        \
          --quit
