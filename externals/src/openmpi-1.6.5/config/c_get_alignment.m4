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
dnl Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_C_GET_ALIGN(type, config_var)
# ----------------------------------
# Determine datatype alignment. 
# First arg is type, 2nd arg is config var to define.
AC_DEFUN([OMPI_C_GET_ALIGNMENT],[
    AC_CACHE_CHECK([alignment of $1],
                   [AS_TR_SH([ompi_cv_c_align_$1])],
		   [AC_RUN_IFELSE([AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT],
[[
    struct foo { char c; $1 x; };
    struct foo *p = (struct foo *) malloc(sizeof(struct foo));
    int diff;
    FILE *f=fopen("conftestval", "w");
    if (!f) exit(1);
    diff = ((char *)&p->x) - ((char *)&p->c);
    fprintf(f, "%d\n", (diff >= 0) ? diff : -diff);
]])],                         [AS_TR_SH([ompi_cv_c_align_$1])=`cat conftestval`],
                               [AC_MSG_WARN([*** Problem running configure test!])
                                AC_MSG_WARN([*** See config.log for details.])
                                AC_MSG_ERROR([*** Cannot continue.])],
                               [ # cross compile - do a non-executable test.  Trick 
                                 # taken from the Autoconf 2.59c.  Switch to using
                                 # AC_CHECK_ALIGNOF when we can require Autoconf 2.60.
                                 _AC_COMPUTE_INT([(long int) offsetof (ompi__type_alignof_, y)],
                                                 [AS_TR_SH([ompi_cv_c_align_$1])],
                                                 [AC_INCLUDES_DEFAULT
#ifndef offsetof
# define offsetof(type, member) ((char *) &((type *) 0)->member - (char *) 0)
#endif
typedef struct { char x; $1 y; } ompi__type_alignof_;
],
                                                 [AC_MSG_WARN([*** Problem running configure test!])
                                                  AC_MSG_WARN([*** See config.log for details.])
                                                  AC_MSG_ERROR([*** Cannot continue.])])])])

AC_DEFINE_UNQUOTED([$2], [$AS_TR_SH([ompi_cv_c_align_$1])], [Alignment of type $1])
eval "$2=$AS_TR_SH([ompi_cv_c_align_$1])"

rm -rf conftest* ]) dnl
