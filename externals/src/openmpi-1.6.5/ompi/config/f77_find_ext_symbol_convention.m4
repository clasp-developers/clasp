dnl -*- shell-script -*-
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
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl


AC_DEFUN([OMPI_F77_FIND_EXT_SYMBOL_CONVENTION], [
    AC_REQUIRE([AC_PROG_NM])
    AC_REQUIRE([AC_PROG_GREP])
    OMPI_VAR_SCOPE_PUSH([FFLAGS_NEW LDFLAGS_NEW FLAG])

    # invalidate cache if result came from a run where F77 was disabled
    if test "$ompi_cv_f77_external_symbol" = "skipped" ; then
        unset ompi_cv_f77_external_symbol
    fi

    AC_CACHE_CHECK([$F77 external symbol convention],
        [ompi_cv_f77_external_symbol],
        [if test "$F77" = "none" -o "$OMPI_WANT_F77_BINDINGS" = "0"; then
             ompi_cv_f77_external_symbol="skipped"
         else
             cat >conftest.f <<EOF
       subroutine FOO_bar(a)
       integer a
       a = 1
       return
       end
EOF
             # Try without certain optimization flags, which produce object
             # files without the required external symbols;
             # e.g. option -fast turns on -ipo on Intel Compilers 11.0
             FFLAGS_NEW=""
             LDFLAGS_NEW=""
             case $F77 in
                 ifort)
                     for FLAG in $FFLAGS ; do
                         case $FLAG in
                             -fast) ;;
                             -ipo*) ;;
                             *)     FFLAGS_NEW="$FFLAGS_NEW $FLAG" ;;
                         esac
                     done
                     for FLAG in $LDFLAGS ; do
                         case $FLAG in
                             -fast) ;;
                             -ipo*) ;;
                             *)     LDFLAGS_NEW="$LDFLAGS_NEW $FLAG" ;;
                         esac
                     done
                     OMPI_LOG_MSG([Try with new FFLAGS: $FFLAGS_NEW   and new LDFLAGS:$LDFLAGS_NEW])
                 ;;
                 *)
                     FFLAGS_NEW="$FFLAGS"
                     LDFLAGS_NEW="$LDFLAGS"
                 ;;
             esac

             happy=1
             OMPI_LOG_COMMAND([$F77 $FFLAGS_NEW -c conftest.f $LDFLAGS_NEW $LIBS],
                 [if $NM conftest.o | $GREP foo_bar__ >/dev/null 2>&1 ; then
                      ompi_cv_f77_external_symbol="double underscore"
                  elif $NM conftest.o | $GREP foo_bar_ >/dev/null 2>&1 ; then
                      ompi_cv_f77_external_symbol="single underscore"
                  elif $NM conftest.o | $GREP FOO_bar >/dev/null 2>&1 ; then
                      ompi_cv_f77_external_symbol="mixed case"
                  elif $NM conftest.o | $GREP foo_bar >/dev/null 2>&1 ; then
                      ompi_cv_f77_external_symbol="no underscore"
                  elif $NM conftest.o | $GREP FOO_BAR >/dev/null 2>&1 ; then
                      ompi_cv_f77_external_symbol="upper case"
                  else
                      $NM conftest.o >conftest.out 2>&1
                      OMPI_LOG_MSG([Could not determine Fortran naming convention. Output from $NM:])
                      OMPI_LOG_FILE([conftest.out])
                      happy=0
                  fi],
                 [AC_MSG_ERROR([Fortran compiler did not produce object file])])
         fi])

    ompi_fortran_double_underscore=0
    ompi_fortran_single_underscore=0
    ompi_fortran_caps=0
    ompi_fortran_plain=0

    if test "$ompi_cv_f77_external_symbol" = "double underscore" ; then
        ompi_fortran_double_underscore=1
    elif test "$ompi_cv_f77_external_symbol" = "single underscore" ; then
        ompi_fortran_single_underscore=1
    elif test "$ompi_cv_f77_external_symbol" = "mixed case" ; then
        ompi_fortran_caps=1
    elif test "$ompi_cv_f77_external_symbol" = "no underscore" ; then
        ompi_fortran_plain=1
    elif test "$ompi_cv_f77_external_symbol" = "upper case" ; then
        ompi_fortran_caps=1
    elif test "$ompi_cv_f77_external_symbol" != "skipped" ; then
        AC_MSG_ERROR([unknown naming convention: $ompi_cv_f77_external_symbol])
    fi

    AC_DEFINE_UNQUOTED([OMPI_F77_DOUBLE_UNDERSCORE],
        [$ompi_fortran_double_underscore], 
        [Whether fortran symbols have a trailing double underscore or not])
    AC_DEFINE_UNQUOTED([OMPI_F77_SINGLE_UNDERSCORE], 
        [$ompi_fortran_single_underscore],
        [Whether fortran symbols have a trailing underscore or not])
    AC_DEFINE_UNQUOTED([OMPI_F77_CAPS],
        [$ompi_fortran_caps],
        [Whether fortran symbols are all caps or not])
    AC_DEFINE_UNQUOTED([OMPI_F77_PLAIN], 
        [$ompi_fortran_plain],
        [Whether fortran symbols have no trailing underscore or not])

    OMPI_VAR_SCOPE_POP
    rm -rf conftest.*
])dnl


AC_DEFUN([OMPI_F77_MAKE_C_FUNCTION], [
    if test "$ompi_cv_f77_external_symbol" = "double underscore" ; then
        # so the general rule is that if there is an _ in the function
        # name, then there are two trailing underscores.  Otherwise,
        # there is only one trailing underscore.  Any idea how to do
        # that with m4_translit?
        if echo $2 | $GREP _ >/dev/null 2>&1 ; then
            $1[=]m4_translit([$2], [A-Z], [a-z])[__]
        else
            $1[=]m4_translit([$2], [A-Z], [a-z])[_]
        fi
    elif test "$ompi_cv_f77_external_symbol" = "single underscore" ; then
        $1[=]m4_translit([$2], [A-Z], [a-z])[_]
    elif test "$ompi_cv_f77_external_symbol" = "mixed case" ; then
        $1[=]$2
    elif test "$ompi_cv_f77_external_symbol" = "no underscore" ; then
        $1[=]m4_translit([$2], [A-Z], [a-z])
    elif test "$ompi_cv_f77_external_symbol" = "upper case" ; then
        $1[=]m4_translit([$2], [a-z], [A-Z])
    else
        AC_MSG_ERROR([unknown naming convention: $ompi_cv_f77_external_symbol])
    fi
])
