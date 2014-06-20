# -*- shell-script -*-
#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2009 Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
#                         reserved. 
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([ORTE_SETUP_DEBUGGER_FLAGS],[
    #
    # Do a final process of the CFLAGS to make a WITHOUT_OPTFLAGS
    # version.  We need this so that we can guarantee to build the
    # debugger-sensitive files with -g and nothing else.
    #

    OMPI_STRIP_OPTFLAGS($CFLAGS)
    CFLAGS_WITHOUT_OPTFLAGS="$s_result"
    # Tweak the compiler flags passed to orterun for Sun Studio SPARC
    # https://svn.open-mpi.org/trac/ompi/ticket/1448
    if test "x$ompi_cv_c_compiler_vendor" = "xsun" -a -n "`echo $host | $GREP sparc`"; then
        DEBUGGER_CFLAGS="-g -xO0"
    else
        # Tweak the compiler flags passed for intel
        # to stop its aggressive inlining of functions
        if test "x$ompi_cv_c_compiler_vendor" = "xintel"; then
            DEBUGGER_CFLAGS="-g -O0"
        else
            DEBUGGER_CFLAGS="-g"
        fi
    fi
    AC_MSG_CHECKING([which of CFLAGS are ok for debugger modules])
    AC_MSG_RESULT([$CFLAGS_WITHOUT_OPTFLAGS])
    AC_MSG_CHECKING([for debugger extra CFLAGS])
    AC_MSG_RESULT([$DEBUGGER_CFLAGS])
    
    AC_SUBST(CFLAGS_WITHOUT_OPTFLAGS)
    AC_SUBST(DEBUGGER_CFLAGS)
])
