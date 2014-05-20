dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
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


# OMPI_F77_GET_VALUE_TRUE()
# -------------------------------------------------------
# Determine the value of .TRUE. of this Fortran compiler.
AC_DEFUN([OMPI_F77_GET_VALUE_TRUE],[
    # invalidate cache if result came from a run where F77 was disabled
    if test "$ompi_cv_f77_true_value" = "0" ; then
        unset ompi_cv_f77_true_value
    fi

    AC_CACHE_CHECK([Fortran value for .TRUE. logical type],
        [ompi_cv_f77_true_value],
        [if test "$1" = "none" -o "$OMPI_WANT_F77_BINDINGS" = "0" ; then
             ompi_cv_f77_true_value=0
         else
             OMPI_F77_MAKE_C_FUNCTION([ompi_print_logical_fn], [print])

             #
             # C module
             # We really need the confdefs.h Header file for
             # the ompi_fortran_logical_t definition
             #
             if test \! -f confdefs.h ; then
                 AC_MSG_WARN([*** Problem running configure test!])
                 AC_MSG_WARN([*** Cannot find confdefs.h file for config test])
                 AC_MSG_WARN([*** See config.log for details.])
                 AC_MSG_ERROR([*** Cannot continue.])
             fi

             cat > conftest.c <<EOF
#include <stdio.h>
#include <stdlib.h>
#include "confdefs.h"

#ifdef __cplusplus
extern "C" {
#endif

void $ompi_print_logical_fn(ompi_fortran_logical_t * logical);

void $ompi_print_logical_fn(ompi_fortran_logical_t * logical)
{
    FILE *f=fopen("conftestval", "w");
    if (!f) exit(1);

    if( SIZEOF_INT >= sizeof(ompi_fortran_logical_t) ) {
        fprintf(f, "%d\n", (int)*logical);
    } else if (SIZEOF_LONG >= sizeof(ompi_fortran_logical_t) ) {
	fprintf(f, "%ld\n", (long) *logical);
#ifdef HAVE_LONG_LONG
    } else if (SIZEOF_LONG_LONG >= sizeof(ompi_fortran_logical_t) ) {
        fprintf(f, "%lld\n", (long long) *logical);
#endif
    } else {
        exit(1);
    }
}

#ifdef __cplusplus
}
#endif
EOF

             cat > conftestf.f <<EOF
      program main
      logical value
      value=.TRUE.
      CALL print(value)
      end
EOF

             #
             # Try the compilation and run.
             #
             OMPI_LOG_COMMAND([$CC $CFLAGS -I. -c conftest.c],
                 [OMPI_LOG_COMMAND([$F77 $FFLAGS -o conftest conftest.o conftestf.f $LDFLAGS $LIBS],
                      [happy=1], [happy=0])],
                 [happy=0])

             if test "$happy" = "0" ; then
                 AC_MSG_ERROR([Could not determine value of Fortran .TRUE..  Aborting.])
             fi

             AS_IF([test "$cross_compiling" = "yes"],
                 [AC_MSG_ERROR([Can not determine value of .TRUE. when cross-compiling])],
                 [OMPI_LOG_COMMAND([./conftest],
                     [ompi_cv_f77_true_value=`sed 's/  *//' conftestval`],
                     [AC_MSG_ERROR([Could not determine value of Fotran .TRUE..  Aborting.])])])
         fi])

    AC_DEFINE_UNQUOTED([OMPI_FORTRAN_VALUE_TRUE], 
        [$ompi_cv_f77_true_value],
        [Fortran value for LOGICAL .TRUE. value])

    unset happy ompi_print_logical_fn
    rm -rf conftest*
])dnl
