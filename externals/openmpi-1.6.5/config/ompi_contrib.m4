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
dnl Copyright (c) 2007-2010 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

######################################################################
#
# OMPI_CONTRIB
#
# configure the contributed software components.  Currently fairly
# hard-wired, but someday should be much more like OMPI_MCA.  See
# https://svn.open-mpi.org/trac/ompi/ticket/1162.
#
# USAGE:
#   OMPI_CONTRIB()
#
######################################################################
AC_DEFUN([OMPI_CONTRIB],[
    dnl for OMPI_CONFIGURE_USER env variable
    AC_REQUIRE([OMPI_CONFIGURE_SETUP])

    # May someday be expanded to have autogen find the packages
    # instead of this hard-coded list
    # (https://svn.open-mpi.org/trac/ompi/ticket/1162).
    m4_define([contrib_software_list], [libompitrace, vt])

    # Option to not build some of the contributed software packages
    AC_ARG_ENABLE([contrib-no-build],
        AC_HELP_STRING([--enable-contrib-no-build=LIST],
                        [Comma-separated list of contributed package NAMEs that will not be built.  Possible values: contrib_software_list. Example: "--enable-contrib-no-build=libompitrace,vt" will disable building both the "libompitrace" and "vt" contributed software packages.]))

    # Parse the list to see what we should not build
    ompi_show_subtitle "Configuring contributed software packages"
    AC_MSG_CHECKING([which contributed software packages should be disabled])
    if test "$enable_contrib_no_build" = "yes"; then
        AC_MSG_RESULT([yes])
        AC_MSG_ERROR([*** The enable-contrib-no-build flag requires an explicit list
*** of packages to not build.  For example, --enable-contrib-no-build=vt])
    else
        ifs_save="$IFS"
        IFS="${IFS}$PATH_SEPARATOR,"
        msg=
        for item in $enable_contrib_no_build; do
            str="`echo DISABLE_contrib_${item}=1 | sed s/-/_/g`"
            eval $str
            msg="$item $msg"
        done
        IFS="$ifs_save"
    fi
    AC_MSG_RESULT([$msg])
    unset msg

    # List of contrib subdirs to traverse into
    OMPI_CONTRIB_SUBDIRS=
    OMPI_CONTRIB_DIST_SUBDIRS=

    # Cycle through each of the software packages and
    # configure them if not disabled.  
    m4_foreach(software, [contrib_software_list],
              [m4_include([ompi/contrib/]software[/configure.m4])
              _OMPI_CONTRIB_CONFIGURE(software)])

    # Setup the top-level glue
    AC_SUBST(OMPI_CONTRIB_SUBDIRS)
    AC_SUBST(OMPI_CONTRIB_DIST_SUBDIRS)
])dnl


######################################################################
#
# _OMPI_CONTRIB_SOFTWARE
#
# Setup a specific contributed software package.  This is a subroutine
# because the work to setup each package is essentially the same.
# Currently assumes that there is a configure.m4 file in the
# contributed software directory.  May someday be expanded to handle
# other things.
#
# USAGE:
#   _OMPI_CONTRIB_SOFTARE([package_name])
#
######################################################################
AC_DEFUN([_OMPI_CONTRIB_CONFIGURE],[

    ompi_show_subsubsubtitle "$1 (m4 configuration macro)"

    # Put in a convenient enable/disable switch (it's a little more
    # user friendly than
    # --enable-contrib-no-build=<comma_delimited_list>, although each
    # works just as well as the other).
    AC_ARG_ENABLE([$1],
            [AS_HELP_STRING([--disable-$1],
                            [disable support for contributed package $1 (default: enabled)])])
    AS_IF([test "x$enable_$1" = xno], [DISABLE_contrib_$1=yes])

    OMPI_CONTRIB_HAPPY=0
    if test "$DISABLE_contrib_$1" = "" -a "$DISABLE_contrib_all" = ""; then
        OMPI_contrib_$1_CONFIG([OMPI_CONTRIB_HAPPY=1], [])
        AC_MSG_CHECKING([if contributed component $1 can compile])
        if test "$OMPI_CONTRIB_HAPPY" = "1"; then
            OMPI_CONTRIB_SUBDIRS="$OMPI_CONTRIB_SUBDIRS contrib/$1"
            OMPI_CONTRIB_DIST_SUBDIRS="$OMPI_CONTRIB_DIST_SUBDIRS contrib/$1"
            AC_MSG_RESULT([yes])
        else
            AC_MSG_RESULT([no])

            # If this component was requested via command line switch, then abort.
            if test "x$enable_$1" = xyes ; then
                AC_MSG_WARN([Contributed component "$1" failed to configure properly])
                AC_MSG_WARN([This component was requested via command line switch])
                AC_MSG_ERROR([Cannot continue])
            fi
        fi
    else
        AC_MSG_NOTICE([disabled via command line switch])
    fi
    AC_DEFINE_UNQUOTED(OMPI_ENABLE_CONTRIB_$1, [$OMPI_CONTRIB_HAPPY],
                       [Enable contributed software package $1])
    unset OMPI_CONTRIB_HAPPY
])dnl
