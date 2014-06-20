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
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_F77_GET_ALIGNMENT(type, shell variable to set)
# ----------------------------------------------------
AC_DEFUN([OMPI_F77_GET_ALIGNMENT],[
    unset happy
    OMPI_VAR_SCOPE_PUSH([happy ompi_conftest_h])
    # Use of m4_translit suggested by Eric Blake:
    # http://lists.gnu.org/archive/html/bug-autoconf/2010-10/msg00016.html
    AS_VAR_PUSHDEF([type_var],
       m4_translit([[ompi_cv_f77_alignment_$1]], [*], [p]))

    AC_CACHE_CHECK([alignment of Fortran $1], type_var,
       [OMPI_F77_MAKE_C_FUNCTION([ompi_ac_align_fn], [align])
        # Fortran module
        cat > conftestf.f <<EOF
      program falign
      external align
      $1  w,x,y,z
      CHARACTER a,b,c
      common /foo/a,w,b,x,y,c,z
      call align(w,x,y,z)
      end
EOF

        # C module
        if test -f conftest.h; then
            ompi_conftest_h="#include \"conftest.h\""
        else
            ompi_conftest_h=""
        fi
        cat > conftest.c <<EOF
#include <stdio.h>
#include <stdlib.h>
$ompi_conftest_h

#ifdef __cplusplus
extern "C" {
#endif
void $ompi_ac_align_fn(char *w, char *x, char *y, char *z)
{   unsigned long aw, ax, ay, az;
    FILE *f=fopen("conftestval", "w");
    if (!f) exit(1);
    aw = (unsigned long) w;
    ax = (unsigned long) x;
    ay = (unsigned long) y;
    az = (unsigned long) z;
    if (! ((aw%16)||(ax%16)||(ay%16)||(az%16))) fprintf(f, "%d\n", 16);
    else if (! ((aw%12)||(ax%12)||(ay%12)||(az%12))) fprintf(f, "%d\n", 12);
    else if (! ((aw%8)||(ax%8)||(ay%8)||(az%8))) fprintf(f, "%d\n", 8);
    else if (! ((aw%4)||(ax%4)||(ay%4)||(az%4))) fprintf(f, "%d\n", 4);
    else if (! ((aw%2)||(ax%2)||(ay%2)||(az%2))) fprintf(f, "%d\n", 2);
    else fprintf(f, "%d\n", 1); 
    fclose(f);
}
#ifdef __cplusplus
}
#endif
EOF

        OMPI_LOG_COMMAND([$CC $CFLAGS -I. -c conftest.c],
            [OMPI_LOG_COMMAND([$F77 $FFLAGS conftestf.f conftest.o -o conftest $LDFLAGS $LIBS],
                [happy="yes"], [happy="no"])], [happy="no"])

        if test "$happy" = "no" ; then
            AC_MSG_RESULT([Error!])
            AC_MSG_ERROR([Could not determine alignment of $1])
        fi

        AS_IF([test "$cross_compiling" = "yes"],
            [AC_MSG_RESULT([Error!])
             AC_MSG_ERROR([Can not determine alignment of $1 when cross-compiling])],
            [OMPI_LOG_COMMAND([./conftest],
                [AS_VAR_SET(type_var, [`cat conftestval`])],
                [AC_MSG_RESULT([Error!])
                 AC_MSG_ERROR([Could not determine alignment of $1])])])
        rm -rf conftest*])

    AS_VAR_COPY([$2], [type_var])
    AS_VAR_POPDEF([type_var])dnl
    OMPI_VAR_SCOPE_POP
])
