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
dnl Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OPAL_CONFIG_THREADS],[
#
# Arguments: none
#
# Dependencies: None
#
# Modifies:
#  none - see called tests
#
# configure threads
#

# create templates
AH_TEMPLATE([OPAL_HAVE_SOLARIS_THREADS], 
    [Do we have native Solaris threads])
AH_TEMPLATE([OPAL_HAVE_POSIX_THREADS], 
    [Do we have POSIX threads])

#
# Check for thread types - add your type here...
#
OMPI_CONFIG_POSIX_THREADS(HAVE_POSIX_THREADS=1, HAVE_POSIX_THREADS=0)
AC_MSG_CHECKING([for working POSIX threads package])
if test "$HAVE_POSIX_THREADS" = "1" ; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi
export HAVE_POSIX_THREADS

OMPI_CONFIG_SOLARIS_THREADS(HAVE_SOLARIS_THREADS=1, HAVE_SOLARIS_THREADS=0)
AC_MSG_CHECKING([for working Solaris threads package])
if test "$HAVE_SOLARIS_THREADS" = "1" ; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi
export HAVE_SOLARIS_THREADS

#
# Ask what threading we want (allow solaris / posix right now)
#
AC_MSG_CHECKING([for type of thread support])
AC_ARG_WITH(threads, 
  	AC_HELP_STRING([--with-threads],
		       [Set thread type (solaris / posix)]),
	[THREAD_TYPE=$withval])

if test "$THREAD_TYPE" = "solaris"; then

    if test "$HAVE_SOLARIS_THREADS" = "0"; then
	AC_MSG_WARN(["*** You have chosen Solaris threads, which are not"])
	AC_MSG_WARN(["*** available on your system "])
	AC_MSG_ERROR(["*** Can not continue"])
    fi
elif test "$THREAD_TYPE" = "posix"; then

    if test "$HAVE_POSIX_THREADS" = "0"; then
	AC_MSG_WARN(["*** You have chosen POSIX threads, which are not"])
	AC_MSG_WARN(["*** available on your system "])
	AC_MSG_ERROR(["*** Can not continue"])
    fi
elif test "$THREAD_TYPE" = "no"; then
    THREAD_TYPE="none"
elif test -z "$THREAD_TYPE" -o "$THREAD_TYPE" = "yes"; then

    # Actual logic here - properly set THREAD_TYPE - we go for system
    # optimized where ever possible
    case "$host" in
	*solaris*)
	    if test "$HAVE_SOLARIS_THREADS" = "1"; then
		THREAD_TYPE="solaris"
	    elif test "$HAVE_POSIX_THREADS" = "1"; then
		THREAD_TYPE="posix"
	    else
		THEAD_TYPE="none found"
	    fi
	    ;;
	*)
	    if test "$HAVE_POSIX_THREADS" = "1"; then
		THREAD_TYPE="posix"
	    else
		THREAD_TYPE="none found"
	    fi
	    ;;
    esac
else

    AC_MSG_WARN(["*** You have specified a thread type that I do not"])
    AC_MSG_WARN(["*** understand.  Valid options are posix and solaris"])
    AC_MSG_ERROR(["*** Can not continue."])
fi
AC_MSG_RESULT($THREAD_TYPE)


#
# Ok, now run the configuration for that thread package.
#
# Blah - this should be made better, but I don't know how...
#
AH_TEMPLATE([OPAL_THREADS_HAVE_DIFFERENT_PIDS],
    [Do threads have different pids (pthreads on linux)])

if test "$THREAD_TYPE" = "solaris"; then
    AC_DEFINE(OPAL_HAVE_SOLARIS_THREADS, 1)
    AC_DEFINE(OPAL_HAVE_POSIX_THREADS, 0)
    AC_DEFINE(OPAL_THREADS_HAVE_DIFFERENT_PIDS, 0)

    THREAD_CFLAGS="$STHREAD_CFLAGS"
    THREAD_FFLAGS="$STHREAD_FFLAGS"
    THREAD_CXXFLAGS="$STHREAD_CXXFLAGS"
    THREAD_CPPFLAGS="$STHREAD_CPPFLAGS"
    THREAD_CXXCPPFLAGS="$STHREAD_CXXCPPFLAGS"
    THREAD_LDFLAGS="$STHREAD_LDFLAGS"
    THREAD_LIBS="$STHREAD_LIBS"
elif test "$THREAD_TYPE" = "posix"; then
    AC_DEFINE(OPAL_HAVE_SOLARIS_THREADS, 0)
    AC_DEFINE(OPAL_HAVE_POSIX_THREADS, 1)

    THREAD_CFLAGS="$PTHREAD_CFLAGS"
    THREAD_FFLAGS="$PTHREAD_FFLAGS"
    THREAD_CXXFLAGS="$PTHREAD_CXXFLAGS"
    THREAD_CPPFLAGS="$PTHREAD_CPPFLAGS"
    THREAD_CXXCPPFLAGS="$PTHREAD_CXXCPPFLAGS"
    THREAD_LDFLAGS="$PTHREAD_LDFLAGS"
    THREAD_LIBS="$PTHREAD_LIBS"

    OMPI_CHECK_PTHREAD_PIDS
else
    AC_DEFINE(OPAL_HAVE_SOLARIS_THREADS, 0)
    AC_DEFINE(OPAL_HAVE_POSIX_THREADS, 0)
    AC_DEFINE(OPAL_THREADS_HAVE_DIFFERENT_PIDS, 0)

    TRHEAD_CFLAGS=
    THREAD_FFLAGS=
    THREAD_CXXFLAGS=
    THREAD_CPPFLAGS=
    THREAD_CXXCPPFLAGS=
    THREAD_LDFLAGS=
    THREAD_LIBS=
    if test "$THREAD_TYPE" != "none" ; then
        cat <<EOF

************************************************************************

Open MPI was unable to find threading support on your system.  The
OMPI development team is considering requiring threading support for
proper OMPI execution.  This is in part because we are not aware of
any OpenFabrics users that do not have thread support -- so we need
you to e-mail the Open MPI Users mailing list to tell us if this is a
problem for you.

************************************************************************

EOF
    fi
fi

AM_CONDITIONAL(OPAL_HAVE_POSIX_THREADS, test "$THREAD_TYPE" = "posix")
AM_CONDITIONAL(OPAL_HAVE_SOLARIS_THREADS, test "$THREAD_TYPE" = "solaris")

#
# Now configure the whole OPAL and progress thread gorp
#
AC_MSG_CHECKING([if want OPAL thread support])
AC_ARG_ENABLE([opal-multi-threads],
    AC_HELP_STRING([--enable-opal-multi-threads],
        [Enable thread support inside OPAL (default: disabled)]),
    [enable_opal_multi_threads="$enableval"],
    [enable_opal_multi_threads="undef"])

if test "$enable_opal_multi_threads" = "undef" ; then 
dnl    # no argument given either way.  Default to whether
dnl    # we have threads or not
dnl    if test "$THREAD_TYPE" != "none" ; then
dnl        OPAL_ENABLE_MULTI_THREADS=1
dnl        enable_opal_multi_threads="yes"
dnl    else
dnl        OPAL_ENABLE_MULTI_THREADS=0
dnl        enable_opal_multi_threads="no"
dnl    fi
    # no argument - default to no, but leave
    # enable_opal_multi_threads="undef" so we
    # can later detect that it wasn't specified
    OPAL_ENABLE_MULTI_THREADS=0
elif test "$enable_opal_multi_threads" = "no" ; then
    OPAL_ENABLE_MULTI_THREADS=0
else
    # they want OPAL thread support.  Make sure we have threads
    if test "$THREAD_TYPE" != "none" ; then
        OPAL_ENABLE_MULTI_THREADS=1
        enable_opal_multi_threads="yes"
    else
        AC_MSG_ERROR([User requested MPI threads, but no threading model supported])
    fi
fi
AC_DEFINE_UNQUOTED([OPAL_ENABLE_MULTI_THREADS], [$OPAL_ENABLE_MULTI_THREADS],
                   [Whether we should enable thread support within the OPAL code base])
AC_MSG_RESULT([$enable_opal_multi_threads])


#
# Fault Tolerance Thread
#
# --enable-ft-thread
#  #if OPAL_ENABLE_FT_THREAD == 0 /* Disabled */
#  #if OPAL_ENABLE_FT_THREAD == 1 /* Enabled  */
# 
AC_MSG_CHECKING([if want fault tolerance thread])
AC_ARG_ENABLE([ft_thread],
    [AC_HELP_STRING([--disable-ft-thread],
                    [Disable fault tolerance thread running inside all processes. Requires progress and/or MPI threads (default: enabled)])],
    [enable_ft_thread="$enableval"],
    [enable_ft_thread="undef"])

# if they do not want FT support, then they do not want this thread either
if test "$ompi_want_ft" = "0"; then
    ompi_want_ft_thread=0
    AC_MSG_RESULT([Disabled (fault tolerance disabled --without-ft)])
# if --disable-ft-thread
elif test "$enable_ft_thread" = "no"; then
    ompi_want_ft_thread=0
    AC_MSG_RESULT([Disabled])
# if default, and no progress or MPI threads
elif test "$enable_ft_thread" = "undef" -a "$enable_opal_progress_threads" = "no"  -a "$enable_opal_multi_threads" = "no" ; then
    ompi_want_ft_thread=0
    AC_MSG_RESULT([Disabled (OPAL Thread Support Disabled)])
# if default, and MPI threads enabled
else
    # Default: Enable
    # Make sure we have OPAL Threads enabled 
    if test "$enable_opal_multi_threads" = "no"; then
        AC_MSG_RESULT([Must enable OPALbasic thread support to use this option])
        AC_MSG_ERROR([Cannot continue])
    else
        AC_MSG_RESULT([yes])
        ompi_want_ft_thread=1
        AC_MSG_WARN([**************************************************])
        AC_MSG_WARN([*** Fault Tolerance with a thread in Open MPI    *])
        AC_MSG_WARN([*** is an experimental, research quality option. *])
        AC_MSG_WARN([*** It requires OPAL thread support or progress  *])
        AC_MSG_WARN([*** and care should be used when enabling these  *])
        AC_MSG_WARN([*** options.                                     *])
        AC_MSG_WARN([**************************************************])
    fi
fi
AC_DEFINE_UNQUOTED([OPAL_ENABLE_FT_THREAD], [$ompi_want_ft_thread],
                   [Enable fault tolerance thread in Open PAL])
AM_CONDITIONAL(WANT_FT_THREAD, test "$ompi_want_ft_thread" = "1")

])dnl

