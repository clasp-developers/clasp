# This is an example configuration that is typically used by a heavy
# Lisp user that edits the Lisp files in the system a lot.
#
# This config does the following:
# - quicklisp directory is $HOME/quicklisp
# - fasl files go into ~/.cache
# - it also turns on compiling the Lisp files for Jupyter, in addition
#   to turning on Cando

#
# To use this config:
# - copy this file to the name 'env-local.sh'
# - do not edit 'env-default.sh'

export ASDF_OUTPUT_TRANSLATIONS=""
export CLASP_QUICKLISP_DIRECTORY=""

export CLASP_WANT_CANDO=1
# implies CLASP_WANT_CANDO
export CLASP_WANT_JUPYTER=1
