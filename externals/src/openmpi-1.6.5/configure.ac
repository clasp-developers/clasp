# -*- shell-script -*-
#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2010 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2013 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2008 Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2006-2011 Los Alamos National Security, LLC.  All rights
#                         reserved. 
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


############################################################################
# Initialization, version number, and other random setup/init stuff
############################################################################

# Load project list and names
m4_include([config/project_list.m4])
# Load the version code.  Because this is also used directly as a
# shell script, no ac_defun
m4_pattern_allow([m4_ifval])
AC_LANG([C])

# Init autoconf

# We don't have the version number to put in here yet, and we can't
# call OMPI_GET_VERSION (etc.) before AC_INIT.  So use the shell
# version.  project_name_* comes from config/project_list.m4, which
# was set during autogen.sh.

AC_INIT([project_name_long],
        [m4_normalize(esyscmd([config/ompi_get_version.sh VERSION --base]))],
        [http://www.open-mpi.org/community/help/], [project_name_short])
AC_PREREQ(2.60)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)

# Get our platform support file.  This has to be done very, very early
# because it twiddles random bits of autoconf
OMPI_LOAD_PLATFORM

#
#
# Start it up
#

OMPI_CONFIGURE_SETUP
ompi_show_title "Configuring project_name_long"

# Setup some things that must be done before AM-INIT-AUTOMAKE
#

ompi_show_subtitle "Startup tests"
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AC_DEFINE_UNQUOTED(OPAL_ARCH, "$target", [OMPI architecture string])
AS_IF([test "$host" != "$target"],
      [AC_MSG_WARN([Cross-compile detected])
       AC_MSG_WARN([Cross-compiling is only partially supported])
       AC_MSG_WARN([Proceed at your own risk!])])
AC_USE_SYSTEM_EXTENSIONS


#
# Init automake
#
AM_INIT_AUTOMAKE([foreign dist-bzip2 subdir-objects no-define 1.11 tar-ustar])

# If Automake supports silent rules, enable them.
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# Make configure depend on the VERSION file, since it's used in AC_INIT
AC_SUBST([CONFIGURE_DEPENDENCIES], ['$(top_srcdir)/VERSION'])

# Set up project specific AM_CONDITIONALs
AM_CONDITIONAL([PROJECT_OMPI], m4_ifdef([project_ompi], [true], [false]))
AM_CONDITIONAL([PROJECT_ORTE], m4_ifdef([project_orte], [true], [false]))

ompi_show_subtitle "Checking versions"

# Get the version of OMPI that we are installing

m4_ifdef([project_ompi],
         [OMPI_SAVE_VERSION([OMPI], [Open MPI], [$srcdir/VERSION],
                            [ompi/include/ompi/version.h])])

m4_ifdef([project_orte],
         [OMPI_SAVE_VERSION([ORTE], [Open MPI Run-Time Environment], 
                            [$srcdir/VERSION],
                            [orte/include/orte/version.h])])

OMPI_SAVE_VERSION([OPAL], [Open Portable Access Layer], [$srcdir/VERSION],
                  [opal/include/opal/version.h])

# Get shared library version numbers

. $srcdir/VERSION
m4_ifdef([project_ompi],
         [AC_SUBST(libmpi_so_version)
          AC_SUBST(libmpi_cxx_so_version)
          AC_SUBST(libmpi_f77_so_version)
          AC_SUBST(libmpi_f90_so_version)
          # It's icky that we have to hard-code the names of the
          # common components here.  :-( This could probably be done
          # transparently by adding some intelligence in autogen.sh
          # and/or ompi_mca.m4, but I don't have the cycles to do this
          # right now.
          AC_SUBST(libmca_common_sm_so_version)
          AC_SUBST(libmca_common_mx_so_version)
          AC_SUBST(libmca_common_portals_so_version)])
m4_ifdef([project_orte],
         [AC_SUBST(libopen_rte_so_version)])
AC_SUBST(libopen_pal_so_version)

#
# Hardwire OPAL progress threads to be off
#
OPAL_ENABLE_PROGRESS_THREADS=0
enable_progress_threads="no"
AC_DEFINE_UNQUOTED([OPAL_ENABLE_PROGRESS_THREADS], [$OPAL_ENABLE_PROGRESS_THREADS],
                   [Hardcode the OPAL progress thread to be off])


# List header files to generate

AC_CONFIG_HEADERS([opal/include/opal_config.h])
m4_ifdef([project_orte],
         [AC_CONFIG_HEADERS([orte/include/orte_config.h])])
m4_ifdef([project_ompi],
         [AC_CONFIG_HEADERS([ompi/include/ompi_config.h ompi/include/mpi.h ompi/include/mpi_portable_platform.h])])

# override/fixup the version numbers set by AC_INIT, since on
# developer builds, there's no good way to know what the version is
# before running configure :(.  We only use the base version number
# (ie, no svn r numbers) for the version set in AC_INIT.  This will
# always match reality because we add the VERSION file (the only way
# to change the major.minor.release{greek}) into the configure
# dependencies.  PACKAGE_VERSION the AC_DEFINE doesn't change once set
# the first time -- AC_INIT's input (so it doesn't have an r number in
# it).  PACKAGE_VERSION the AC_SUBST can be rewritten along the way,
# and we'd like it to have the r number in it so that it shows up in
# the tarball name, so it is set to the full version here.
PACKAGE_VERSION="$OPAL_VERSION"
PACKAGE_STRING="${PACKAGE_NAME} ${PACKAGE_VERSION}"
VERSION="${PACKAGE_VERSION}"

ompi_show_subtitle "Initialization, setup"

OMPI_TOP_BUILDDIR="`pwd`"
AC_SUBST(OMPI_TOP_BUILDDIR)
cd "$srcdir"
OMPI_TOP_SRCDIR="`pwd`"
AC_SUBST(OMPI_TOP_SRCDIR)
cd "$OMPI_TOP_BUILDDIR"

AC_MSG_NOTICE([builddir: $OMPI_TOP_BUILDDIR])
AC_MSG_NOTICE([srcdir: $OMPI_TOP_SRCDIR])
if test "$OMPI_TOP_BUILDDIR" != "$OMPI_TOP_SRCDIR"; then
    AC_MSG_NOTICE([Detected VPATH build])
fi

# Setup the top of the opal/include/opal_config.h file

AH_TOP([/* -*- c -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Function: - OS, CPU and compiler dependent configuration
 */

#ifndef OPAL_CONFIG_H
#define OPAL_CONFIG_H
])
AH_BOTTOM([
#include "opal_config_bottom.h"
#endif /* OPAL_CONFIG_H */
])

# Other basic setup stuff (shared with components)

OMPI_BASIC_SETUP

top_ompi_srcdir="$OMPI_TOP_SRCDIR"
AC_SUBST(top_ompi_srcdir)
top_ompi_builddir="$OMPI_TOP_BUILDDIR"
AC_SUBST(top_ompi_builddir)

############################################################################
# Configuration options
############################################################################

OPAL_CONFIGURE_OPTIONS
m4_ifdef([project_orte], [ORTE_CONFIGURE_OPTIONS])
m4_ifdef([project_ompi], [OMPI_CONFIGURE_OPTIONS])

if test "$enable_binaries" = "no" -a "$enable_dist" = "yes"; then
    AC_MSG_WARN([--disable-binaries is incompatible with --enable dist])
    AC_MSG_ERROR([Cannot continue])
fi

############################################################################
# Libtool: part one
# (before C compiler setup)
############################################################################

#
# Part one of libtool magic.  Enable static so that we have the --with
# tests done up here and can check for OS.  Save the values of
# $enable_static and $enable_shared before setting the defaults,
# because if the user specified --[en|dis]able-[static|shared] on the
# command line, they'll already be set.  In this way, we can tell if
# the user requested something or if the default was set here.
#

ompi_enable_shared="$enable_shared"
ompi_enable_static="$enable_static"
AM_ENABLE_SHARED
AM_DISABLE_STATIC

OPAL_SETUP_WRAPPER_INIT
# There is no [need for] ORTE_SETUP_WRAPPER_INIT; this is not an
# accidental omission
m4_ifdef([project_ompi], [OMPI_SETUP_WRAPPER_INIT])

##################################
# Check for known incompatibility
##################################

# Do *not* print a message that we're checking the OS because this
# test is *not* meant to be an all-inclusive "if it passes this test,
# then configure must succeed" test.  This test is *only* mean to
# screen out the versions of OS X where we know OMPI will cause kernel
# panics because of bad implementations of pty's.  See
# https://svn.open-mpi.org/trac/ompi/ticket/1637 for details.

#   OS X name         OS X Version    $host_os value
# OS X Tiger             10.4.x           darwin8.x
# OS X Leopard           10.5.x           darwin9.x
# OS X Snow Leopard      10.6.x           darwin10.x
# OS X Lion              10.7.x           darwin11.x

# We do not support OS X before version 10.5 (Leopard)
case $host_os in
# Corresponds to OS X 10.0 - 10.4 (additional [] quoting for m4)
darwin[[45678]]*)
    AC_MSG_WARN([Open MPI does not support OS X prior to version 10.5 (Leopard)])
    AC_MSG_ERROR([Cannot continue])
esac

############################################################################
# Check for compilers and preprocessors
############################################################################
ompi_show_title "Compiler and preprocessor tests"

##################################
# C compiler characteristics
##################################

OMPI_SETUP_CC

# If we build on a windows environment with the windows compiler and linker
# then we need some translation functions from the opal/win32 directory.
AM_CONDITIONAL(OMPI_NEED_WINDOWS_REPLACEMENTS,
               test "$ompi_cv_c_compiler_vendor" = "microsoft" )

# Do all Interix detections if necessary
OMPI_INTERIX

# Does the compiler support "ident"-like constructs?

OMPI_CHECK_IDENT([CC], [CFLAGS], [c], [C])

#
# Check for some types
#

AC_CHECK_TYPES(int8_t)
AC_CHECK_TYPES(uint8_t)
AC_CHECK_TYPES(int16_t)
AC_CHECK_TYPES(uint16_t)
AC_CHECK_TYPES(int32_t)
AC_CHECK_TYPES(uint32_t)
AC_CHECK_TYPES(int64_t)
AC_CHECK_TYPES(uint64_t)
AC_CHECK_TYPES(int128_t)
AC_CHECK_TYPES(uint128_t)
AC_CHECK_TYPES(long long)

AC_CHECK_TYPES(long double)
# We only need these types if we're building the OMPI project, but
# OPAL currently doesn't protect for their lack of presence well.
AC_CHECK_TYPES(float _Complex)
AC_CHECK_TYPES(double _Complex)
AC_CHECK_TYPES(long double _Complex)

AC_CHECK_TYPES(intptr_t)
AC_CHECK_TYPES(uintptr_t)
AC_CHECK_TYPES(mode_t)
AC_CHECK_TYPES(ssize_t)
AC_CHECK_TYPES(ptrdiff_t)

#
# Check for type sizes
#

AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
if test $ac_cv_type_long_long = yes; then
    AC_CHECK_SIZEOF(long long)
fi
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)
if test $ac_cv_type_long_double = yes; then
    AC_CHECK_SIZEOF(long double)
fi

# We only need these types if we're building the OMPI project, but
# OPAL currently doesn't protect for their lack of presence well.
if test "$ac_cv_type_float__Complex" = yes; then
    AC_CHECK_SIZEOF(float _Complex)
fi
if test "$ac_cv_type_double__Complex" = yes; then
    AC_CHECK_SIZEOF(double _Complex)
fi
if test "$ac_cv_type_long_double__Complex" = yes; then
    AC_CHECK_SIZEOF(long double _Complex)
fi

AC_CHECK_SIZEOF(void *)
AC_CHECK_SIZEOF(size_t)
if test $ac_cv_type_ssize_t = yes ; then
    AC_CHECK_SIZEOF(ssize_t)
fi
if test $ac_cv_type_ptrdiff_t = yes; then
    AC_CHECK_SIZEOF(ptrdiff_t)
fi
AC_CHECK_SIZEOF(wchar_t)


#
# Check for type alignments
#

OMPI_C_GET_ALIGNMENT(_Bool, OPAL_ALIGNMENT_BOOL)
OMPI_C_GET_ALIGNMENT(int8_t, OPAL_ALIGNMENT_INT8)
OMPI_C_GET_ALIGNMENT(int16_t, OPAL_ALIGNMENT_INT16)
OMPI_C_GET_ALIGNMENT(int32_t, OPAL_ALIGNMENT_INT32)
OMPI_C_GET_ALIGNMENT(int64_t, OPAL_ALIGNMENT_INT64)
if test "$ac_cv_type_int128_t" = yes ; then
  OMPI_C_GET_ALIGNMENT(int128_t, OPAL_ALIGNMENT_INT128)
fi
OMPI_C_GET_ALIGNMENT(char, OPAL_ALIGNMENT_CHAR)
OMPI_C_GET_ALIGNMENT(short, OPAL_ALIGNMENT_SHORT)
OMPI_C_GET_ALIGNMENT(wchar_t, OPAL_ALIGNMENT_WCHAR)
OMPI_C_GET_ALIGNMENT(int, OPAL_ALIGNMENT_INT)
OMPI_C_GET_ALIGNMENT(long, OPAL_ALIGNMENT_LONG)
if test "$ac_cv_type_long_long" = yes; then
    OMPI_C_GET_ALIGNMENT(long long, OPAL_ALIGNMENT_LONG_LONG)
fi
OMPI_C_GET_ALIGNMENT(float, OPAL_ALIGNMENT_FLOAT)
OMPI_C_GET_ALIGNMENT(double, OPAL_ALIGNMENT_DOUBLE)
if test "$ac_cv_type_long_double" = yes; then
    OMPI_C_GET_ALIGNMENT(long double, OPAL_ALIGNMENT_LONG_DOUBLE)
fi

# We only need these types if we're building the OMPI project, but
# OPAL currently doesn't protect for their lack of presence well.
if test "$ac_cv_type_float__Complex" = yes; then
    OMPI_C_GET_ALIGNMENT(float _Complex, OPAL_ALIGNMENT_FLOAT_COMPLEX)
fi
if test "$ac_cv_type_double__Complex" = yes; then
    OMPI_C_GET_ALIGNMENT(double _Complex, OPAL_ALIGNMENT_DOUBLE_COMPLEX)
fi
if test "$ac_cv_type_long_double__Complex" = yes; then
    OMPI_C_GET_ALIGNMENT(long double _Complex, OPAL_ALIGNMENT_LONG_DOUBLE_COMPLEX)
fi

OMPI_C_GET_ALIGNMENT(void *, OPAL_ALIGNMENT_VOID_P)

#
# Does the C compiler native support "bool"? (i.e., without
# <stdbool.h> or any other help)
#

AC_MSG_CHECKING(for C bool type)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
AC_INCLUDES_DEFAULT],
[[bool bar, foo = true; bar = foo;]])],
[OPAL_NEED_C_BOOL=0 MSG=yes],[OPAL_NEED_C_BOOL=1 MSG=no])
AC_DEFINE_UNQUOTED(OPAL_NEED_C_BOOL, $OPAL_NEED_C_BOOL,
    [Whether the C compiler supports "bool" without any other help (such as <stdbool.h>)])
AC_MSG_RESULT([$MSG])
AC_CHECK_SIZEOF(_Bool)

#
# Check for other compiler characteristics
#

if test "$GCC" = "yes"; then

    # gcc 2.96 will emit oodles of warnings if you use "inline" with
    # -pedantic (which we do in developer builds).  However,
    # "__inline__" is ok.  So we have to force gcc to select the
    # right one.  If you use -pedantic, the AC_C_INLINE test will fail
    # (because it names a function foo() -- without the (void)).  So
    # we turn off all the picky flags, turn on -ansi mode (which is
    # implied by -pedantic), and set warnings to be errors.  Hence,
    # this does the following (for 2.96):
    #
    # - causes the check for "inline" to emit a warning, which then
    # fails
    # - checks for __inline__, which then emits no error, and works
    #
    # This also works nicely for gcc 3.x because "inline" will work on
    # the first check, and all is fine.  :-)

    CFLAGS_save="$CFLAGS"
    CFLAGS="$OMPI_CFLAGS_BEFORE_PICKY -Werror -ansi"
fi
AC_C_INLINE
# Microsoft compilers support 2 versions of restrict. One for functions, and
# one for variables. The problem is that they don't have an equivalent
# syntax, and the autoconf restrict detection is unable to detect them
# correctly. It detect the restrict keyword as __restrict which break the
# rules for function syntax which is declspec(restrict).
if test "x$ompi_cv_c_compiler_vendor" != "xmicrosoft"; then
    AC_C_RESTRICT
fi
OMPI_C_WEAK_SYMBOLS
if test "$GCC" = "yes"; then
    CFLAGS="$CFLAGS_save"
fi

if test "x$CC" = "xicc"; then
    OMPI_CHECK_ICC_VARARGS
fi

# If we want the profiling layer:
# - If the C compiler has weak symbols, use those.
# - If not, then set to compile the code again with #define's in a
#   separate directory.

if test "$WANT_WEAK_SYMBOLS" = "0"; then
    OPAL_C_HAVE_WEAK_SYMBOLS=0
fi
if test "$WANT_MPI_PROFILING" = "1"; then
    if test "$OPAL_C_HAVE_WEAK_SYMBOLS" = "1"; then
        OMPI_PROFILING_COMPILE_SEPARATELY=0
    else
        OMPI_PROFILING_COMPILE_SEPARATELY=1
    fi
else
    OMPI_PROFILING_COMPILE_SEPARATELY=0
fi

# Check if we support the offsetof compiler directive

OPAL_CHECK_OFFSETOF

# Setup OMPI bindings (if we're building the OMPI project).  Note that
# opal_wrapper.c has a hard-coded use of the OMPI_ENABLE_MPI_PROFILING
# macro, so we need to define it (to 0) even if we're not building the
# OMPI project.

m4_ifdef([project_ompi], [OMPI_SETUP_MPI_PROFILING],
                         [AC_DEFINE([OMPI_ENABLE_MPI_PROFILING], [0],
                                    [We are not building OMPI, so no profiling])])


##################################
# C++ compiler characteristics
##################################

OPAL_SETUP_CXX
# The MPI project has a boatload more C++ tests (ORTE does not).
m4_ifdef([project_ompi], [OMPI_SETUP_CXX])

##################################
# Only after setting up both
# C and C++ check compiler attributes.
##################################

ompi_show_subtitle "Compiler characteristics"

OMPI_CHECK_ATTRIBUTES

OPAL_CHECK_COMPILER_VERSION_ID


##################################
# Assembler Configuration
##################################

ompi_show_subtitle "Assembler"

AM_PROG_AS
OMPI_CONFIG_ASM


##################################
# Fortran
##################################

# OPAL uses opal_fortran_logical_t and OMPI_FORTRAN_HANDLE_MAX (long
# story; yes, it's an intentional abstraction break).  So ensure to
# define it to a valid C type if we're not building the OMPI project.
m4_ifdef([project_ompi], [OMPI_SETUP_MPI_FORTRAN],
         [AC_DEFINE([ompi_fortran_logical_t], [int], [Bogus type for OPAL])
          AC_DEFINE([OMPI_FORTRAN_HANDLE_MAX], [128], [Bogus type for OPAL])
])

# checkpoint results
AC_CACHE_SAVE


##################################
# Header files
##################################

ompi_show_title "Header file tests"

AC_CHECK_HEADERS([alloca.h aio.h arpa/inet.h dirent.h \
    dlfcn.h execinfo.h err.h fcntl.h grp.h inttypes.h libgen.h \
    libutil.h memory.h netdb.h netinet/in.h netinet/tcp.h \
    poll.h pthread.h pty.h pwd.h sched.h stdint.h stddef.h \
    stdlib.h string.h strings.h stropts.h sys/fcntl.h sys/ipc.h sys/shm.h \
    sys/ioctl.h sys/mman.h sys/param.h sys/queue.h \
    sys/resource.h sys/select.h sys/socket.h sys/sockio.h \
    stdarg.h sys/stat.h sys/statfs.h sys/statvfs.h sys/time.h sys/tree.h \
    sys/types.h sys/uio.h net/uio.h sys/utsname.h sys/vfs.h sys/wait.h syslog.h \
    time.h termios.h ulimit.h unistd.h util.h utmp.h malloc.h \
    ifaddrs.h crt_externs.h regex.h \
    ioLib.h sockLib.h hostLib.h shlwapi.h sys/synch.h])

AC_CHECK_HEADERS([sys/mount.h], [], [],
[AC_INCLUDES_DEFAULT
#if HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
])

AC_CHECK_HEADERS([sys/sysctl.h], [], [],
[AC_INCLUDES_DEFAULT
#if HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
])

# Needed to work around Darwin requiring sys/socket.h for
# net/if.h
AC_CHECK_HEADERS([net/if.h], [], [],
    [#include <stdio.h>
#if STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# if HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#if HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
])

# Note that sometimes we have <stdbool.h>, but it doesn't work (e.g.,
# have both Portland and GNU installed; using pgcc will find GNU's
# <stdbool.h>, which all it does -- by standard -- is define "bool" to
# "_Bool" [see
# http://www.opengroup.org/onlinepubs/009695399/basedefs/stdbool.h.html],
# and Portland has no idea what to do with _Bool).

# So first figure out if we have <stdbool.h> (i.e., check the value of
# the macro HAVE_STDBOOL_H from the result of AC_CHECK_HEADERS,
# above).  If we do have it, then check to see if it actually works.
# Define OPAL_USE_STDBOOL_H as approrpaite.
AC_CHECK_HEADERS([stdbool.h], [have_stdbool_h=1], [have_stdbool_h=0])
AC_MSG_CHECKING([if <stdbool.h> works])
if test "$have_stdbool_h" = "1"; then
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
AC_INCLUDES_DEFAULT[
#if HAVE_STDBOOL_H
#include <stdbool.h>
#endif]],
[[bool bar, foo = true; bar = foo;]])],
[OPAL_USE_STDBOOL_H=1 MSG=yes],[OPAL_USE_STDBOOL_H=0 MSG=no])
else
    OPAL_USE_STDBOOL_H=0
    MSG="no (don't have <stdbool.h>)"
fi
AC_DEFINE_UNQUOTED(OPAL_USE_STDBOOL_H, $OPAL_USE_STDBOOL_H,
    [Whether to use <stdbool.h> or not])
AC_MSG_RESULT([$MSG])

# checkpoint results
AC_CACHE_SAVE


##################################
# Types
##################################

ompi_show_title "Type tests"

# Size of pid_t
AC_CHECK_SIZEOF(pid_t)

AC_CHECK_TYPES([socklen_t, struct sockaddr_in, struct sockaddr_in6,
                struct sockaddr_storage],
               [], [], [AC_INCLUDES_DEFAULT
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif])

AC_CHECK_DECLS([AF_UNSPEC, PF_UNSPEC, AF_INET6, PF_INET6],
               [], [], [AC_INCLUDES_DEFAULT
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif])

# SA_RESTART in signal.h
AC_MSG_CHECKING([if SA_RESTART defined in signal.h])
AC_EGREP_CPP(yes, [
#include <signal.h>
#ifdef SA_RESTART
    yes
#endif ], [MSG=yes VALUE=1], [MSG=no VALUE=0])
AC_DEFINE_UNQUOTED(OPAL_HAVE_SA_RESTART, $VALUE,
    [Whether we have SA_RESTART in <signal.h> or not])
AC_MSG_RESULT([$MSG])

AC_CHECK_MEMBERS([struct sockaddr.sa_len], [], [], [
#include <sys/types.h>
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif])

AC_CHECK_MEMBERS([struct dirent.d_type], [], [], [
#include <sys/types.h>
#include <dirent.h>])

AC_CHECK_MEMBERS([siginfo_t.si_fd],,,[#include <signal.h>])
AC_CHECK_MEMBERS([siginfo_t.si_band],,,[#include <signal.h>])


# checkpoint results
AC_CACHE_SAVE


##################################
# Libraries
##################################

ompi_show_title "Library and Function tests"

# Darwin doesn't need -lutil, as it's something other than this -lutil.
OMPI_CHECK_FUNC_LIB([openpty], [util])

AC_CHECK_LIB([nsl], [gethostbyname])

AC_CHECK_LIB([socket], [socket])

# Solaris has sched_yield in -lrt, usually in libc
OMPI_CHECK_FUNC_LIB([sched_yield], [rt])

# IRIX has dirname in -lgen, usually in libc
OMPI_CHECK_FUNC_LIB([dirname], [gen])

# Darwin doesn't need -lm, as it's a symlink to libSystem.dylib
OMPI_CHECK_FUNC_LIB([ceil], [m])

AC_CHECK_FUNCS([asprintf snprintf vasprintf vsnprintf openpty isatty getpwuid fork waitpid execve pipe ptsname setsid mmap tcgetpgrp posix_memalign strsignal sysconf syslog regcmp regexec regfree _NSGetEnviron socketpair strncpy_s _strdup usleep mkfifo])

# On some hosts, htonl is a define, so the AC_CHECK_FUNC will get
# confused.  On others, it's in the standard library, but stubbed with
# the magic glibc foo as not implemented.  and on other systems, it's
# just not there.  This covers all cases.
AC_CACHE_CHECK([for htonl define],
  [ompi_cv_htonl_define],
  [AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif],[
#ifndef ntohl
#error "ntohl not defined"
#endif
])], [ompi_cv_htonl_define=yes], [ompi_cv_htonl_define=no])])
AC_CHECK_FUNC([htonl], [ompi_have_htonl=yes], [ompi_have_htonl=no])
AS_IF([test "$ompi_cv_htonl_define" = "yes" -o "$ompi_have_htonl" = "yes"],
    [AC_DEFINE_UNQUOTED([HAVE_UNIX_BYTESWAP], [1],
        [whether unix byteswap routines -- htonl, htons, nothl, ntohs -- are available])])

#
# Make sure we can copy va_lists (need check declared, not linkable)
#

AC_CHECK_DECL(va_copy, OPAL_HAVE_VA_COPY=1, OPAL_HAVE_VA_COPY=0,
    [#include <stdarg.h>])
AC_DEFINE_UNQUOTED(OPAL_HAVE_VA_COPY, $OPAL_HAVE_VA_COPY,
    [Whether we have va_copy or not])

AC_CHECK_DECL(__va_copy, OPAL_HAVE_UNDERSCORE_VA_COPY=1,
    OPAL_HAVE_UNDERSCORE_VA_COPY=0, [#include <stdarg.h>])
AC_DEFINE_UNQUOTED(OPAL_HAVE_UNDERSCORE_VA_COPY, $OPAL_HAVE_UNDERSCORE_VA_COPY,
    [Whether we have __va_copy or not])

AC_CHECK_DECLS(__func__)

# checkpoint results
AC_CACHE_SAVE


##################################
# System-specific tests
##################################

ompi_show_title "System-specific tests"

#
# Test to determine type of MPI_Offset. This is searched in the following order
# int64_t, long long, long, int. If none of these are 8 bytes, then we should
# search for int32_t, long long, long, int.
#
MPI_OFFSET_TYPE="not found"
MPI_OFFSET_DATATYPE="not found"
AC_MSG_CHECKING([checking for type of MPI_Offset])
if test $ac_cv_type_long_long = yes -a $ac_cv_sizeof_long_long = 8; then
    MPI_OFFSET_TYPE="long long"
    MPI_OFFSET_DATATYPE=MPI_LONG_LONG
    MPI_OFFSET_SIZE=8
elif test $ac_cv_type_long = yes -a $ac_cv_sizeof_long = 8; then
    MPI_OFFSET_TYPE="long"
    MPI_OFFSET_DATATYPE=MPI_LONG
    MPI_OFFSET_SIZE=8
elif test $ac_cv_sizeof_int = 8; then
    MPI_OFFSET_TYPE="int"
    MPI_OFFSET_DATATYPE=MPI_INT
    MPI_OFFSET_SIZE=8
elif test $ac_cv_type_long_long = yes -a $ac_cv_sizeof_long_long = 4; then
    MPI_OFFSET_TYPE="long long"
    MPI_OFFSET_DATATYPE=MPI_LONG_LONG
    MPI_OFFSET_SIZE=4
elif test $ac_cv_type_long = yes -a $ac_cv_sizeof_long = 4; then
    MPI_OFFSET_TYPE="long"
    MPI_OFFSET_DATATYPE=MPI_LONG
    MPI_OFFSET_SIZE=4
elif test $ac_cv_sizeof_int = 4; then
    MPI_OFFSET_TYPE="int"
    MPI_OFFSET_DATATYPE=MPI_INT
    MPI_OFFSET_SIZE=4
fi
AC_MSG_RESULT([$MPI_OFFSET_TYPE])
if test "$MPI_OFFSET_TYPE" = "not found"; then
    AC_MSG_WARN([*** Unable to find the right definition for MPI_Offset])
    AC_MSG_ERROR([Cannot continue])
fi
AC_MSG_CHECKING([checking for an MPI datatype for MPI_Offset])
AC_MSG_RESULT([$MPI_OFFSET_DATATYPE])
# this should never happens
if test "$MPI_OFFSET_DATATYPE" = "not found"; then
    AC_MSG_WARN([*** Unable to find an MPI datatype corresponding to MPI_Offset])
    AC_MSG_ERROR([Cannot continue])
fi

AC_DEFINE_UNQUOTED(OMPI_MPI_OFFSET_TYPE, $MPI_OFFSET_TYPE, [Type of MPI_Offset -- has to be defined here and typedef'ed later because mpi.h does not get AC SUBST's])
AC_DEFINE_UNQUOTED(OMPI_MPI_OFFSET_SIZE, $MPI_OFFSET_SIZE, [Size of the MPI_Offset])
AC_DEFINE_UNQUOTED(OMPI_OFFSET_DATATYPE, $MPI_OFFSET_DATATYPE, [MPI datatype corresponding to MPI_Offset])

#
# Check for MPI_Aint type.  Yes, there are platforms where
# sizeof(void*) != sizeof(long) (64 bit Windows, apparently).
#
if test $ac_cv_type_ptrdiff_t = yes ; then
    opal_ptrdiff_t="ptrdiff_t"
elif test $ac_cv_sizeof_void_p -eq $ac_cv_sizeof_long ; then
    opal_ptrdiff_t="long"
elif test $ac_cv_type_long_long = yes -a $ac_cv_sizeof_void_p -eq $ac_cv_sizeof_long_long ; then
    opal_ptrdiff_t="long long"
else
    AC_MSG_ERROR([Could not find datatype to emulate ptrdiff_t.  Cannot continue])
fi
AC_DEFINE_UNQUOTED([OPAL_PTRDIFF_TYPE], [$opal_ptrdiff_t],
    [type to use for ptrdiff_t])

# Do we have _SC_NPROCESSORS_ONLN? (only going to pass if we also have
# <unistd.h> and sysconf(), which is ok) OS X 10.4 has <unistd.h> and
# sysconf(), but does not have _SC_NPROCESSORS_ONLN.  Doh!

AC_CACHE_CHECK([for _SC_NPROCESSORS_ONLN],
  [ompi_cv_have__SC_NPROCESSORS_ONLN],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
AC_INCLUDES_DEFAULT
#include <unistd.h>
],
     [int i = _SC_NPROCESSORS_ONLN;])],
     [ompi_cv_have__SC_NPROCESSORS_ONLN="yes"],
     [ompi_cv_have__SC_NPROCESSORS_ONLN="no"])])
AS_IF([test "$ompi_cv_have__SC_NPROCESSORS_ONLN" = "yes"],
      [result=1], [result=0])
AC_DEFINE_UNQUOTED([OPAL_HAVE__SC_NPROCESSORS_ONLN], [$result],
  [Define to 1 ifyou have the declaration of _SC_NPROCESSORS_ONLN, and to 0 otherwise])

# all: endian

AC_C_BIGENDIAN

OMPI_CHECK_BROKEN_QSORT

AC_CACHE_CHECK([if word-sized integers must be word-size aligned],
    [ompi_cv_c_word_size_align],
    [AC_LANG_PUSH(C)
     AC_RUN_IFELSE([AC_LANG_PROGRAM([dnl
#include <stdlib.h>], [[    long data[2] = {0, 0};
    long *lp;
    int *ip;
    ip = (int*) data;
    ip++;
    lp = (long*) ip;
    return lp[0]; ]])],
        [ompi_cv_c_word_size_align=no],
        [ompi_cv_c_word_size_align=yes],
        [ompi_cv_c_word_size_align=yes])])
AS_IF([test $ompi_cv_c_word_size_align = yes], [results=1], [results=0])
AC_DEFINE_UNQUOTED([OPAL_ALIGN_WORD_SIZE_INTEGERS], [$results],
    [set to 1 if word-size integers must be aligned to word-size padding to prevent bus errors])

# all: SYSV semaphores
# all: SYSV shared memory
# all: size of FD_SET
# all: sizeof struct stat members
# all: type of getsockopt optlen
# all: type of recvfrom optlen

#
# Check out what thread support we have
#
OPAL_CONFIG_THREADS
m4_ifdef([project_ompi], [OMPI_CONFIG_THREADS])

CFLAGS="$CFLAGS $THREAD_CFLAGS"
CPPFLAGS="$CPPFLAGS $THREAD_CPPFLAGS"
m4_ifdef([project_ompi],
         [CXXFLAGS="$CXXFLAGS $THREAD_CXXFLAGS"
          CXXCPPFLAGS="$CXXCPPFLAGS $THREAD_CXXCPPFLAGS"])
LDFLAGS="$LDFLAGS $THREAD_LDFLAGS"
LIBS="$LIBS $THREAD_LIBS"

WRAPPER_EXTRA_CFLAGS="$WRAPPER_EXTRA_CFLAGS $THREAD_CFLAGS"
m4_ifdef([project_ompi],
         [WRAPPER_EXTRA_CXXFLAGS="$WRAPPER_EXTRA_CXXFLAGS $THREAD_CXXFLAGS"
          WRAPPER_EXTRA_FFLAGS="$WRAPPER_EXTRA_FFLAGS $THREAD_FFLAGS"
          WRAPPER_EXTRA_FCFLAGS="$WRAPPER_EXTRA_FCFLAGS $THREAD_FFLAGS"])
WRAPPER_EXTRA_LDFLAGS="$WRAPPER_EXTRA_LDFLAGS $THREAD_LDFLAGS"
# no need to update WRAPPER_EXTRA_LIBS - we'll get it from LT later

#
# What is the local equivalent of "ln -s"
#

AC_PROG_LN_S

AC_PROG_GREP
AC_PROG_EGREP

#
# We need as and lex
#
AM_PROG_AS
AM_PROG_LEX

# If we don't have GNU Flex and we don't have a generated .c file
# (distribution tarballs will have the .c file included, but SVN
# checkouts will not), then error.  Must have GNU Flex -- other
# versions of Lex are not workable (all things being equal, since this
# is *only* required for developers, we decided that it really was not
# worth it to be portable between different versions of lex ;-).

if test -z "$LEX" -o -n "`echo $LEX | $GREP missing`" -o \
    "`basename $LEX`" != "flex"; then
    if test ! -f "$srcdir/opal/util/show_help_lex.c"; then
        AC_MSG_WARN([*** Could not find GNU Flex on your system.])
        AC_MSG_WARN([*** GNU Flex required for developer builds of Open MPI.])
        AC_MSG_WARN([*** Other versions of Lex are not supported.])
        AC_MSG_WARN([*** YOU DO NOT NEED FLEX FOR DISTRIBUTION TARBALLS!])
        AC_MSG_WARN([*** If you absolutely cannot install GNU Flex on this system])
        AC_MSG_WARN([*** consider using a distribution tarball, or generate the])
        AC_MSG_WARN([*** following files on another system (using Flex) and])
        AC_MSG_WARN([*** copy them here:])
        for lfile in `find . -name \*.l -print`; do
            cfile="`echo $lfile | cut -d. -f-2`"
            AC_MSG_WARN([***     $cfile.c])
        done
        AC_MSG_ERROR([Cannot continue])
    fi
fi

#
# Look for ps command and arguments for orte-clean
#
m4_ifdef([project_orte], [OMPI_PS_FLAVOR_CHECK])

#
# File system case sensitivity
#

m4_ifdef([project_opal], [OPAL_CASE_SENSITIVE_FS_SETUP])

# AIX: FIONBIO in sys/ioctl.h
# glibc: memcpy

#
# Do we have RLIMIT_NPROC in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_NPROC], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

#
# Do we have RLIMIT_MEMLOCK in <sys/resources.h>? (e.g., Solaris does not)
#

AC_CHECK_DECLS([RLIMIT_MEMLOCK], [], [], [
AC_INCLUDES_DEFAULT
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif])

# checkpoint results
AC_CACHE_SAVE


##################################
# MCA
##################################

ompi_show_title "Modular Component Architecture (MCA) setup"

AC_MSG_CHECKING([for subdir args])
OMPI_CONFIG_SUBDIR_ARGS([ompi_subdir_args])
AC_MSG_RESULT([$ompi_subdir_args])

OMPI_MCA

# checkpoint results
AC_CACHE_SAVE

##################################
# MPI Extended Interfaces
##################################

m4_ifdef([project_ompi], [OMPI_SETUP_MPI_EXT])

# checkpoint results
AC_CACHE_SAVE

##################################
# Contributed software
##################################

m4_ifdef([project_ompi], [OMPI_SETUP_CONTRIB])

# checkpoint results
AC_CACHE_SAVE

##################################
# Visibility
##################################

# Check the visibility declspec at the end to avoid problem with
# the previous tests that are not necessarily prepared for
# the visibility feature.
ompi_show_title "Symbol visibility feature"

OMPI_CHECK_VISIBILITY



############################################################################
# Final top-level OMPI configuration
############################################################################

ompi_show_title "Final top-level OMPI configuration"

############################################################################
# Libevent setup
############################################################################

ompi_show_subtitle "Libevent 3rd party event handler"
CFLAGS_save="$CFLAGS"
CFLAGS="$OMPI_CFLAGS_BEFORE_PICKY"
OPAL_SETUP_LIBEVENT
CFLAGS="$CFLAGS_save"


############################################################################
# Libtool: part two
# (after C compiler setup = no compiler/linker tests after this)
############################################################################

ompi_show_subtitle "Libtool configuration"

# Use the undocumented solaris_use_stlport4 libtool variable to turn off any
# Cstd/stlport4 linkage. This allows Open MPI to be C++ STL agnostic.
if test "x$ompi_cv_c_compiler_vendor" = "xsun"; then
    solaris_use_stlport4="yes"
fi

# Due to this thread:
# http://www.open-mpi.org/community/lists/users/2013/02/21356.php and
# a helpful tip from Dave Goodell, set the precious variables for
# compilers to "no" that we don't want.  Libtool's m4 configry will
# interpret this as "I won't be using this language; don't bother
# setting it up."
AS_IF([test "$OMPI_WANT_F77_BINDINGS" != "1"],[F77=no FC=no])
AS_IF([test "$WANT_MPI_CXX_SUPPORT" != "1"],[CXX=no])

dnl Not all versions of LT set the PACKAGE_VERSION
m4_ifdef([LT_PACKAGE_VERSION], [], [m4_define([LT_PACKAGE_VERSION], [1.5.22])])

m4_if(m4_version_compare(m4_defn([LT_PACKAGE_VERSION]), 2.0), -1, [
AC_LIBLTDL_CONVENIENCE(opal/libltdl)
AC_LIBTOOL_DLOPEN
AC_PROG_LIBTOOL
], [
LT_CONFIG_LTDL_DIR([opal/libltdl], [subproject])
LTDL_CONVENIENCE
LT_INIT([dlopen win32-dll])
])

# What's the suffix of shared libraries?  Inspired by generated
# Libtool code (even though we don't support several of these
# platforms, there didn't seem to be any harm in leaving in some of
# them, alhtough I did remove some that we have never/will never
# support, like OS/2).
OPAL_DYN_LIB_PREFIX=lib
case $host_os in
cygwin*)
    OPAL_DYN_LIB_PREFIX=cyg
    OPAL_DYN_LIB_SUFFIX=dll
    ;;
mingw* | pw32* | cegcc*)
    OPAL_DYN_LIB_SUFFIX=dll
    ;;
darwin* | rhapsody*)
    OPAL_DYN_LIB_SUFFIX=dylib
    ;;
hpux9* | hpux10* | hpux11*)
    case $host_cpu in
        ia64*)
        OPAL_DYN_LIB_SUFFIX=so
        ;;
    *)
        OPAL_DYN_LIB_SUFFIX=sl
        ;;
    esac
    ;;
*)
   OPAL_DYN_LIB_SUFFIX=so
   ;;
esac
AC_SUBST(OPAL_DYN_LIB_PREFIX)
AC_SUBST(OPAL_DYN_LIB_SUFFIX)

OPAL_SETUP_LIBLTDL


############################################################################
# final compiler config
############################################################################

m4_ifdef([project_ompi], [ompi_show_subtitle "Compiler flags"],
         [m4_ifdef([project_orte], [ompi_show_subtitle "Compiler flags"])])

#
# This is needed for VPATH builds, so that it will -I the appropriate
# include directory.  We delayed doing it until now just so that
# '-I$(top_srcdir)' doesn't show up in any of the configure output --
# purely aesthetic.
#
# Because opal_config.h, orte_config.h, and ompi_config.h are all
# created by AC_CONFIG_HEADERS, we don't need to -I the builddir for
# <project>/include.  If we VPATH building, we do need to include the
# source directories, however.
#
if test "$OMPI_TOP_BUILDDIR" != "$OMPI_TOP_SRCDIR"; then
    # Note the embedded m4 directives here -- we must embed them
    # rather than have successive assignments to these shell
    # variables, lest the $(foo) names try to get evaluated here.
    # Yuck!
    CPPFLAGS='-I$(top_srcdir) -I$(top_builddir) -I$(top_srcdir)/opal/include m4_ifdef([project_orte], [-I$(top_srcdir)/orte/include]) m4_ifdef([project_ompi], [-I$(top_srcdir)/ompi/include])'" $CPPFLAGS"
    # C++ is only relevant if we're building OMPI
    m4_ifdef([project_ompi], [CXXCPPFLAGS='-I$(top_srcdir) -I$(top_builddir) -I$(top_srcdir)/opal/include -I$(top_srcdir)/orte/include -I$(top_srcdir)/ompi/include'" $CXXCPPFLAGS"])
else
    CPPFLAGS='-I$(top_srcdir)'" $CPPFLAGS"
    # C++ is only relevant if we're building OMPI
    m4_ifdef([project_ompi], [CXXCPPFLAGS='-I$(top_srcdir)'" $CXXCPPFLAGS"])
fi

# OMPI/ORTE wants some additional processing of the flags (e.g., get
# versions without optimization for debugger modules).

m4_ifdef([project_orte], [ORTE_SETUP_DEBUGGER_FLAGS],
         [m4_ifdef([project_ompi], [ORTE_SETUP_DEBUGGER_FLAGS])])

#
# Delayed the substitution of CFLAGS and CXXFLAGS until now because
# they may have been modified throughout the course of this script.
#

AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(CXXCPPFLAGS)
m4_ifdef([project_ompi], [AC_SUBST(FFLAGS)
                          AC_SUBST(FCFLAGS)

                          AC_SUBST(OMPI_LIBMPI_EXTRA_LIBS)
                          AC_SUBST(OMPI_LIBMPI_EXTRA_LDFLAGS)])

#
# Aggregate MCA parameters directory
#
AC_SUBST([AMCA_PARAM_SETS_DIR], ['$(pkgdatadir)/amca-param-sets'])

############################################################################
# final wrapper compiler config
############################################################################

ompi_show_subtitle "Wrapper compiler final setup"
# The ORTE and OMPI wrapper scripts (i.e., not the C-compiled
# executables) need perl.
AC_PATH_PROG(PERL, perl, perl)

OPAL_SETUP_WRAPPER_FINAL
m4_ifdef([project_orte], [ORTE_SETUP_WRAPPER_FINAL])
m4_ifdef([project_ompi], [OMPI_SETUP_WRAPPER_FINAL])

# Recreate some defines prefixed with OMPI_ so that there are no bare
# autoconf macro defines in mpi.h.  Since AC sometimes changes whether
# things are defined as null tokens or an integer result, two projects
# with different versions of AC can cause problems.
if test $ac_cv_header_stdc = yes; then
    AC_DEFINE(OPAL_STDC_HEADERS, 1,
              [Do not use outside of mpi.h.  Define to 1 if you have the ANSI C header files.])
fi
if test $ac_cv_header_sys_time_h = yes ; then
    AC_DEFINE(OPAL_HAVE_SYS_TIME_H, 1,
              [Do not use outside of mpi.h.  Define to 1 if you have the <sys/time.h> header file.])
fi
if test $ac_cv_type_long_long = yes ; then
    AC_DEFINE(OPAL_HAVE_LONG_LONG, 1,
              [Do not use outside of mpi.h.  Define to 1 if the system has the type `long long'.]) dnl `
fi
if test $ac_cv_header_sys_synch_h = yes ; then
    AC_DEFINE(OPAL_HAVE_SYS_SYNCH_H, 1,
              [Do not use outside of mpi.h.  Define to 1 if you have the <sys/synch.h> header file.])
fi

# If there is a local hook for each project, call it.  This allows 3rd
# parties to add configuration steps to OPAL, ORTE, and/or OMPI simply
# by placing a file in [opal|orte|ompi]/config/whatever.m4 that
# AC_DEFUN's the appropriate macro name -- no patching is necessary.
# If that macro is defined, we'll run it here.
#
# Unfortunately, aclocal is not smart enough to parse something like
# the following in ompi_mca.m4 (when we're already m4 looping over the
# project list):
#
# m4_foreach(mca_project, [mca_project_list], 
#           [m4_ifdef(mca_project[_CONFIG_LOCAL], mca_project[_CONFIG_LOCAL])])
#
# Meaning that aclocal doesn't see that, for example,
# "ompi_CONFIG_LOCAL" is actually invoked at the bottom and therefore
# go look for an .m4 file that contains it.  Instead, we have to
# manually list the macros here.  *Then* aclocal is smart enough to go
# look for an .m4 file containing each macro, and if found,
# automatically m4_include the corresponding in aclocal.m4.  Bummer.
# :-\

m4_ifdef([opal_CONFIG_LOCAL], [opal_CONFIG_LOCAL])
m4_ifdef([orte_CONFIG_LOCAL], [orte_CONFIG_LOCAL])
m4_ifdef([ompi_CONFIG_LOCAL], [ompi_CONFIG_LOCAL])

############################################################################
# Party on
############################################################################

ompi_show_subtitle "Final output"

AC_CONFIG_FILES([
    Makefile

    config/Makefile

    contrib/Makefile

    test/Makefile
    test/event/Makefile
    test/asm/Makefile
    test/datatype/Makefile
    test/class/Makefile
    test/support/Makefile
    test/threads/Makefile
    test/util/Makefile
])

OPAL_CONFIG_FILES
m4_ifdef([project_orte], [ORTE_CONFIG_FILES])
m4_ifdef([project_ompi], [OMPI_CONFIG_FILES])

AC_OUTPUT
