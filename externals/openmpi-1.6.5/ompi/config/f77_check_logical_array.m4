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

AC_DEFUN([OMPI_F77_CHECK_LOGICAL_ARRAY],[
    AC_CACHE_CHECK([for correct handling of Fortran logical arrays],
        [ompi_cv_f77_logical_array_correct],
        [if test "$1" = "none" -o "$OMPI_WANT_F77_BINDINGS" = "0"; then
             ompi_cv_f77_logical_array_correct=yes
         else
             OMPI_F77_MAKE_C_FUNCTION([ompi_check_logical_fn], [check])

             # Fortran module
             cat > conftestf.f <<EOF
        program check_logical_array
        external check
        logical l(2)
        l(1)=.FALSE.
        l(2)=.TRUE.
        CALL check(l)
        end
EOF

             # C module
             # We really need the confdefs.h Header file for 
             # the ompi_fortran_logical_t definition
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
void $ompi_check_logical_fn(ompi_fortran_logical_t * logical);

void $ompi_check_logical_fn(ompi_fortran_logical_t * logical)
{
    int result = 0;
    FILE *f=fopen("conftestval", "w");
    if (!f) exit(1);

    if (logical[[0]] == 0 &&
        logical[[1]] == $ompi_cv_f77_true_value)
      result = 1;
    fprintf(f, "%d\n", result);
}
#ifdef __cplusplus
}
#endif
EOF

             # Try the compilation and run.  Can't use AC_TRY_RUN
             # because it's two module files.
             OMPI_LOG_COMMAND([$CC $CFLAGS -I. -c conftest.c],
                 [OMPI_LOG_COMMAND([$F77 $FFLAGS conftestf.f conftest.o -o conftest $LDFLAGS $LIBS],
                     [happy=1], [happy=0])],
                 [happy=0])
             if test "$happy" = "0" ; then
                 AC_MSG_ERROR([Error determining if arrays of logical values work properly.])
             fi

             AS_IF([test "$cross_compiling" = "yes"], 
                 [ # assume we're ok
                  ompi_cv_f77_logical_array_correct=yes],
                 [OMPI_LOG_COMMAND([./conftest],
                      [if test "`cat conftestval`" = "1" ; then
                           ompi_cv_f77_logical_array_correct=yes
                       else
                           ompi_cv_f77_logical_array_correct=no
                       fi],             
                      [ompi_cv_f77_logical_array_correct=no])])
    fi])

    if test "$ompi_cv_f77_logical_array_correct" = "no" ; then
        AC_MSG_ERROR([Error determining if arrays of logical values work properly.])
    fi

    unset happy ompi_check_logical_fn
    rm -rf conftest*
])dnl
