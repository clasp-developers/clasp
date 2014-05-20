dnl -*- shell-script -*-
dnl
dnl Copyright (c)      2008 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

######################################################################
#
# OMPI_INTERIX
#
# Detect if the environment is SUA/SFU (i.e. Interix) and modify
# the compiling environment accordingly.
#
# USAGE:
#   OMPI_INTERIX()
#
######################################################################
AC_DEFUN([OMPI_INTERIX],[

    AC_MSG_CHECKING(for Interix environment)
    AC_TRY_COMPILE([],
                   [#if !defined(__INTERIX)
                    #error Normal Unix environment
                    #endif],
                   is_interix=yes,
                   is_interix=no)
    AC_MSG_RESULT([$is_interix])
    if test "$is_interix" = "yes"; then

        ompi_show_subtitle "Interix detection"

        if ! test -d /usr/include/port; then
            AC_MSG_WARN([Compiling Open MPI under Interix require an up-to-date])
            AC_MSG_WARN([version of libport. Please ask your system administrator])
            AC_MSG_WARN([to install it (pkg_update -L libport).])
            AC_MSG_ERROR([*** Cannot continue])
        fi
        #
        # These are the minimum requirements for Interix ...
        #
        AC_MSG_WARN([    -lport was added to the linking flags])
        LDFLAGS="-lport $LDFLAGS"
        AC_MSG_WARN([    -D_ALL_SOURCE -D_USE_LIBPORT was added to the compilation flags])
        CFLAGS="-D_ALL_SOURCE -D_USE_LIBPORT -I/usr/include/port $CFLAGS"
        CPPFLAGS="-D_ALL_SOURCE -D_USE_LIBPORT -I/usr/include/port $CPPFLAGS"
        CXXFLAGS="-D_ALL_SOURCE -D_USE_LIBPORT -I/usr/include/port $CXXFLAGS"

    fi

])
