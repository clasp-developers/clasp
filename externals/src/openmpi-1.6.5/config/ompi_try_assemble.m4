dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

dnl OMPI_TRY_ASSEMBLE(asm-code, [action-if-success], [action-if-fail])
dnl
dnl Attempt to assemble asm-code.  If success, run action-if-success.
dnl Otherwise, run action-if-fail.  Neither action-if-success nor
dnl action-if-fail are required.
dnl
dnl No preprocessing is guaranteed to be done on asm-code.  Some
dnl compilers do not run the preprocessor on assembly files.  
dnl
dnl On failure, asm-test.s will be included in config.out
AC_DEFUN([OMPI_TRY_ASSEMBLE],
[cat >conftest.s <<EOF
[$1]
EOF
if test "$CC" = "$CCAS" ; then
    ompi_assemble="$CCAS $CCASFLAGS -c conftest.s >conftest.out 2>&1"
else
    ompi_assemble="$CCAS $CCASFLAGS -o conftest.o conftest.s >conftest.out 2>&1"
fi
if AC_TRY_EVAL(ompi_assemble); then
  # save the warnings
  cat conftest.out >&AC_FD_CC
  ifelse([$2],,:,[$2])
else
  # save compiler output and failed program
  cat conftest.out >&AC_FD_CC
  echo "configure: failed program was:" >&AC_FD_CC
  cat conftest.s >&AC_FD_CC
  ifelse([$3],,:,[$3])
fi
rm -rf conftest*
unset ompi_assemble
])dnl
