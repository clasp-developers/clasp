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
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

dnl we only want one :)
m4_define(MCA_timer_CONFIGURE_MODE, STOP_AT_FIRST)

AC_DEFUN([MCA_timer_CONFIG],[
        timer_base_include=

        # first, compile all the components
        MCA_CONFIGURE_FRAMEWORK($1, $2, 1)

        # someone should have set this...
        if test "$timer_base_include" = "" ; then
            timer_base_include="base/timer_base_null.h"
        fi

        AC_DEFINE_UNQUOTED([MCA_timer_IMPLEMENTATION_HEADER],
                           ["opal/mca/timer/$timer_base_include"],
                           [Header to include for timer implementation])
])
