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
dnl Copyright (c) 2006-2010 Oracle and/or its affiliates.  All rights reserved.
dnl Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl


AC_DEFUN([ORTE_SETUP_WRAPPER_FINAL],[
    OMPI_UNIQ([orte_WRAPPER_EXTRA_LDFLAGS])
    OMPI_UNIQ([orte_WRAPPER_EXTRA_LIBS])

    AC_MSG_CHECKING([for ORTE CPPFLAGS])
    ORTE_WRAPPER_EXTRA_CPPFLAGS="$WRAPPER_EXTRA_CPPFLAGS"
    AC_SUBST([ORTE_WRAPPER_EXTRA_CPPFLAGS])
    AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_CPPFLAGS])

    AC_MSG_CHECKING([for ORTE CXXFLAGS])
    ORTE_WRAPPER_EXTRA_CXXFLAGS="$WRAPPER_EXTRA_CXXFLAGS"
    AC_SUBST([ORTE_WRAPPER_EXTRA_CXXFLAGS])
    AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_CXXFLAGS])

    AC_MSG_CHECKING([for ORTE CFLAGS])
    ORTE_WRAPPER_EXTRA_CFLAGS="$WRAPPER_EXTRA_CFLAGS"
    AC_SUBST([ORTE_WRAPPER_EXTRA_CFLAGS])
    AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_CFLAGS])

    AC_MSG_CHECKING([for ORTE LDFLAGS])
    ORTE_WRAPPER_EXTRA_LDFLAGS="$orte_WRAPPER_EXTRA_LDFLAGS $WRAPPER_EXTRA_LDFLAGS"
    AC_SUBST([ORTE_WRAPPER_EXTRA_LDFLAGS])
    AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_LDFLAGS])

    AC_MSG_CHECKING([for ORTE LIBS])
    ORTE_WRAPPER_EXTRA_LIBS="$orte_WRAPPER_EXTRA_LIBS $WRAPPER_EXTRA_LIBS"
    AC_SUBST([ORTE_WRAPPER_EXTRA_LIBS])
    AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_LIBS])

    AC_MSG_CHECKING([for ORTE extra include dirs])
    if test "$WANT_INSTALL_HEADERS" = "1" ; then
        ORTE_WRAPPER_EXTRA_INCLUDES="openmpi"
    else
        ORTE_WRAPPER_EXTRA_INCLUDES=
    fi
    AC_SUBST([ORTE_WRAPPER_EXTRA_INCLUDES])
    AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_INCLUDES])


    # For script-based wrappers that don't do relocatable binaries.
    # Don't use if you don't have to.
    exec_prefix_save="${exec_prefix}"
    test "x$exec_prefix" = xNONE && exec_prefix="${prefix}"
    eval "ORTE_WRAPPER_INCLUDEDIR=\"${includedir}\""
    eval "ORTE_WRAPPER_LIBDIR=\"${libdir}\""
    exec_prefix="${exec_prefix_save}"
    AC_SUBST([ORTE_WRAPPER_INCLUDEDIR])
    AC_SUBST([ORTE_WRAPPER_LIBDIR])

    # if wrapper compilers were requested, set the orte one up
    if test "$WANT_SCRIPT_WRAPPER_COMPILERS" = "1" ; then
        AC_CONFIG_FILES([orte/tools/wrappers/orte_wrapper_script],
                        [chmod +x orte/tools/wrappers/orte_wrapper_script])
    fi

])
