dnl -*- Autoconf -*-
dnl
dnl Copyright (c) 2009-2010 inria.  All rights reserved.
dnl Copyright (c) 2009-2012 Université Bordeaux 1
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2012 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright © 2006-2011  Cisco Systems, Inc.  All rights reserved.
dnl See COPYING in top-level directory.

# Main hwloc m4 macro, to be invoked by the user
#
# Expects two or three paramters:
# 1. Configuration prefix
# 2. What to do upon success
# 3. What to do upon failure
# 4. If non-empty, print the announcement banner
#
AC_DEFUN([HWLOC_SETUP_CORE],[
    AC_REQUIRE([AC_CANONICAL_TARGET])
    AC_REQUIRE([AC_PROG_CC])

    AS_IF([test "x$4" != "x"],
          [cat <<EOF

###
### Configuring hwloc core
###
EOF])

    # If no prefix was defined, set a good value
    m4_ifval([$1], 
             [m4_define([hwloc_config_prefix],[$1/])],
             [m4_define([hwloc_config_prefix], [])])

    # Unless previously set to "standalone" mode, default to embedded
    # mode
    AS_IF([test "$hwloc_mode" = ""], [hwloc_mode=embedded])
    AC_MSG_CHECKING([hwloc building mode])
    AC_MSG_RESULT([$hwloc_mode])

    # Get hwloc's absolute top builddir (which may not be the same as
    # the real $top_builddir, because we may be building in embedded
    # mode).
    HWLOC_startdir=`pwd`
    if test x"hwloc_config_prefix" != "x" -a ! -d "hwloc_config_prefix"; then
        mkdir -p "hwloc_config_prefix"
    fi
    if test x"hwloc_config_prefix" != "x"; then
        cd "hwloc_config_prefix"
    fi
    HWLOC_top_builddir=`pwd`
    AC_SUBST(HWLOC_top_builddir)

    # Get hwloc's absolute top srcdir (which may not be the same as
    # the real $top_srcdir, because we may be building in embedded
    # mode).  First, go back to the startdir incase the $srcdir is
    # relative.

    cd "$HWLOC_startdir"
    cd "$srcdir"/hwloc_config_prefix
    HWLOC_top_srcdir="`pwd`"
    AC_SUBST(HWLOC_top_srcdir)

    # Go back to where we started
    cd "$HWLOC_startdir"

    AC_MSG_NOTICE([hwloc builddir: $HWLOC_top_builddir])
    AC_MSG_NOTICE([hwloc srcdir: $HWLOC_top_srcdir])
    if test "$HWLOC_top_builddir" != "$HWLOC_top_srcdir"; then
        AC_MSG_NOTICE([Detected VPATH build])
    fi

    # Debug mode?
    AC_MSG_CHECKING([if want hwloc maintainer support])
    hwloc_debug=

    # Unconditionally disable debug mode in embedded mode; if someone
    # asks, we can add a configure-time option for it.  Disable it
    # now, however, because --enable-debug is not even added as an
    # option when configuring in embedded mode, and we wouldn't want
    # to hijack the enclosing application's --enable-debug configure
    # switch.
    AS_IF([test "$hwloc_mode" = "embedded"],
          [hwloc_debug=0
           hwloc_debug_msg="disabled (embedded mode)"])
    AS_IF([test "$hwloc_debug" = "" -a "$enable_debug" = "yes"],
          [hwloc_debug=1
           hwloc_debug_msg="enabled"])
    AS_IF([test "$hwloc_debug" = ""],
          [hwloc_debug=0
           hwloc_debug_msg="disabled"])
    # Grr; we use #ifndef for HWLOC_DEBUG!  :-(
    AH_TEMPLATE(HWLOC_DEBUG, [Whether we are in debugging mode or not])
    AS_IF([test "$hwloc_debug" = "1"], [AC_DEFINE([HWLOC_DEBUG])])
    AC_MSG_RESULT([$hwloc_debug_msg])

    # We need to set a path for header, etc files depending on whether
    # we're standalone or embedded. this is taken care of by HWLOC_EMBEDDED.

    AC_MSG_CHECKING([for hwloc directory prefix])
    AC_MSG_RESULT(m4_ifval([$1], hwloc_config_prefix, [(none)]))

    # Note that private/config.h *MUST* be listed first so that it
    # becomes the "main" config header file.  Any AC-CONFIG-HEADERS
    # after that (hwloc/config.h) will only have selective #defines
    # replaced, not the entire file.
    AC_CONFIG_HEADERS(hwloc_config_prefix[include/private/autogen/config.h])
    AC_CONFIG_HEADERS(hwloc_config_prefix[include/hwloc/autogen/config.h])

    # What prefix are we using?
    AC_MSG_CHECKING([for hwloc symbol prefix])
    AS_IF([test "$hwloc_symbol_prefix_value" = ""],
          [AS_IF([test "$with_hwloc_symbol_prefix" = ""],
                 [hwloc_symbol_prefix_value=hwloc_],
                 [hwloc_symbol_prefix_value=$with_hwloc_symbol_prefix])])
    AC_DEFINE_UNQUOTED(HWLOC_SYM_PREFIX, [$hwloc_symbol_prefix_value],
                       [The hwloc symbol prefix])
    # Ensure to [] escape the whole next line so that we can get the
    # proper tr tokens
    [hwloc_symbol_prefix_value_caps="`echo $hwloc_symbol_prefix_value | tr '[:lower:]' '[:upper:]'`"]
    AC_DEFINE_UNQUOTED(HWLOC_SYM_PREFIX_CAPS, [$hwloc_symbol_prefix_value_caps],
                       [The hwloc symbol prefix in all caps])
    AC_MSG_RESULT([$hwloc_symbol_prefix_value])

    # Give an easy #define to know if we need to transform all the
    # hwloc names
    AH_TEMPLATE([HWLOC_SYM_TRANSFORM], [Whether we need to re-define all the hwloc public symbols or not])
    AS_IF([test "$hwloc_symbol_prefix_value" = "hwloc_"],
          [AC_DEFINE([HWLOC_SYM_TRANSFORM], [0])],
          [AC_DEFINE([HWLOC_SYM_TRANSFORM], [1])])

    # GCC specifics.
    if test "x$GCC" = "xyes"; then
        HWLOC_GCC_CFLAGS="-Wall -Wmissing-prototypes -Wundef"
        HWLOC_GCC_CFLAGS="$HWLOC_GCC_CFLAGS -Wpointer-arith -Wcast-align"
    fi

    # Enample system extensions for O_DIRECTORY, fdopen, fssl, etc.
    AC_USE_SYSTEM_EXTENSIONS
    AH_VERBATIM([USE_HPUX_SYSTEM_EXTENSIONS],
[/* Enable extensions on HP-UX. */
#ifndef _HPUX_SOURCE
# undef _HPUX_SOURCE
#endif
])
    AC_DEFINE([_HPUX_SOURCE], [1], [Are we building for HP-UX?])
    
    AC_LANG_PUSH([C])
    
    # Check to see if we're producing a 32 or 64 bit executable by
    # checking the sizeof void*.  Note that AC CHECK_SIZEOF even works
    # when cross compiling (!), according to the AC 2.64 docs.  This
    # check is needed because on some systems, you can instruct the
    # compiler to specifically build 32 or 64 bit executables -- even
    # though the $target may indicate something different.
    AC_CHECK_SIZEOF([void *])

    #
    # Check OS support
    #
    AC_MSG_CHECKING([which OS support to include])
    case ${target} in
      *-*-linux*)
        AC_DEFINE(HWLOC_LINUX_SYS, 1, [Define to 1 on Linux])
        hwloc_linux=yes
        AC_MSG_RESULT([Linux])
        ;;
      *-*-irix*)
        AC_DEFINE(HWLOC_IRIX_SYS, 1, [Define to 1 on Irix])
        hwloc_irix=yes
        AC_MSG_RESULT([IRIX])
        ;;
      *-*-darwin*)
        AC_DEFINE(HWLOC_DARWIN_SYS, 1, [Define to 1 on Darwin])
        hwloc_darwin=yes
        AC_MSG_RESULT([Darwin])
        ;;
      *-*-solaris*)
        AC_DEFINE(HWLOC_SOLARIS_SYS, 1, [Define to 1 on Solaris])
        hwloc_solaris=yes
        AC_MSG_RESULT([Solaris])
        ;;
      *-*-aix*)
        AC_DEFINE(HWLOC_AIX_SYS, 1, [Define to 1 on AIX])
        hwloc_aix=yes
        AC_MSG_RESULT([AIX])
        ;;
      *-*-osf*)
        AC_DEFINE(HWLOC_OSF_SYS, 1, [Define to 1 on OSF])
        hwloc_osf=yes
        AC_MSG_RESULT([OSF])
        ;;
      *-*-hpux*)
        AC_DEFINE(HWLOC_HPUX_SYS, 1, [Define to 1 on HP-UX])
        hwloc_hpux=yes
        AC_MSG_RESULT([HP-UX])
        ;;
      *-*-mingw*|*-*-cygwin*)
        AC_DEFINE(HWLOC_WIN_SYS, 1, [Define to 1 on WINDOWS])
        hwloc_windows=yes
        AC_MSG_RESULT([Windows])
        ;;
      *-*-*freebsd*)
        AC_DEFINE(HWLOC_FREEBSD_SYS, 1, [Define to 1 on *FREEBSD])
        hwloc_freebsd=yes
        AC_MSG_RESULT([FreeBSD])
        ;;
      *)
        AC_MSG_RESULT([Unsupported! ($target)])
        AC_DEFINE(HWLOC_UNSUPPORTED_SYS, 1, [Define to 1 on unsupported systems])
        AC_MSG_WARN([***********************************************************])
        AC_MSG_WARN([*** hwloc does not support this system.])
        AC_MSG_WARN([*** hwloc will *attempt* to build (but it may not work).])
        AC_MSG_WARN([*** hwloc run-time results may be reduced to showing just one processor.])
        AC_MSG_WARN([*** You have been warned.])
        AC_MSG_WARN([*** Pausing to give you time to read this message...])
        AC_MSG_WARN([***********************************************************])
        sleep 10
        ;;
    esac

    #
    # Check CPU support
    #
    AC_MSG_CHECKING([which CPU support to include])
    case ${target} in
      i*86-*-*|x86_64-*-*)
        case ${ac_cv_sizeof_void_p} in
          4)
            AC_DEFINE(HWLOC_X86_32_ARCH, 1, [Define to 1 on x86_32])
            hwloc_x86_32=yes
	    HWLOC_MS_LIB_ARCH=X86
            AC_MSG_RESULT([x86_32])
            ;;
          8)
            AC_DEFINE(HWLOC_X86_64_ARCH, 1, [Define to 1 on x86_64])
            hwloc_x86_64=yes
	    HWLOC_MS_LIB_ARCH=X64
            AC_MSG_RESULT([x86_64])
            ;;
          *)
            AC_DEFINE(HWLOC_X86_64_ARCH, 1, [Define to 1 on x86_64])
            hwloc_x86_64=yes
	    HWLOC_MS_LIB_ARCH=X64
            AC_MSG_RESULT([unknown -- assuming x86_64])
            ;;
        esac
    esac
    AC_SUBST(HWLOC_MS_LIB_ARCH)
    
    AC_CHECK_SIZEOF([unsigned long])
    AC_DEFINE_UNQUOTED([HWLOC_SIZEOF_UNSIGNED_LONG], $ac_cv_sizeof_unsigned_long, [The size of `unsigned long', as computed by sizeof])
    AC_CHECK_SIZEOF([unsigned int])
    AC_DEFINE_UNQUOTED([HWLOC_SIZEOF_UNSIGNED_INT], $ac_cv_sizeof_unsigned_int, [The size of `unsigned int', as computed by sizeof])

    #
    # Check for compiler attributes and visibility
    #
    _HWLOC_C_COMPILER_VENDOR([hwloc_c_vendor])
    _HWLOC_CHECK_ATTRIBUTES
    _HWLOC_CHECK_VISIBILITY
    HWLOC_CFLAGS="$HWLOC_FLAGS $HWLOC_VISIBILITY_CFLAGS"
    AS_IF([test "$HWLOC_VISIBILITY_CFLAGS" != ""],
          [AC_MSG_WARN(["$HWLOC_VISIBILITY_CFLAGS" has been added to the hwloc CFLAGS])])

    # Make sure the compiler returns an error code when function arg
    # count is wrong, otherwise sched_setaffinity checks may fail.
    HWLOC_STRICT_ARGS_CFLAGS=
    hwloc_args_check=0
    AC_MSG_CHECKING([whether the C compiler rejects function calls with too many arguments])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        extern int one_arg(int x);
        int foo(void) { return one_arg(1, 2); }
      ]])],
      [AC_MSG_RESULT([no])],
      [hwloc_args_check=1
       AC_MSG_RESULT([yes])])
    AC_MSG_CHECKING([whether the C compiler rejects function calls with too few arguments])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        extern int two_arg(int x, int y);
        int foo(void) { return two_arg(3); }
      ]])],
      [AC_MSG_RESULT([no])],
      [hwloc_args_check=`expr $hwloc_args_check + 1`
       AC_MSG_RESULT([yes])])
    AS_IF([test "$hwloc_args_check" != "2"],[
         AC_MSG_WARN([Your C compiler does not consider incorrect argument counts to be a fatal error.])
        case "$hwloc_c_vendor" in
        ibm)
            HWLOC_STRICT_ARGS_CFLAGS="-qhalt=e"
            ;;
        intel)
            HWLOC_STRICT_ARGS_CFLAGS="-we140"
            ;;
        *)
            HWLOC_STRICT_ARGS_CFLAGS=FAIL
            AC_MSG_WARN([Please report this warning and configure using a different C compiler if possible.])
            ;;
        esac
        AS_IF([test "$HWLOC_STRICT_ARGS_CFLAGS" != "FAIL"],[
            AC_MSG_WARN([Configure will append '$HWLOC_STRICT_ARGS_CFLAGS' to the value of CFLAGS when needed.])
             AC_MSG_WARN([Alternatively you may configure with a different compiler.])
        ])
    ])

    #
    # Now detect support
    #
    
    hwloc_strncasecmp=strncmp
    AC_CHECK_FUNCS([strncasecmp], [
      _HWLOC_CHECK_DECL([strncasecmp], [
        hwloc_strncasecmp=strncasecmp
      ])
    ])
    AC_DEFINE_UNQUOTED(hwloc_strncasecmp, $hwloc_strncasecmp, [Define this to either strncasecmp or strncmp])

    AC_CHECK_FUNCS([strftime])
    AC_CHECK_FUNCS([setlocale])
    
    AC_CHECK_HEADER([stdint.h], [
      AC_DEFINE([HWLOC_HAVE_STDINT_H], [1], [Define to 1 if you have the <stdint.h> header file.])
    ])
    AC_CHECK_HEADERS([sys/mman.h])
    
    old_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$CPPFLAGS -D_WIN32_WINNT=0x0601"
    AC_CHECK_TYPES([KAFFINITY,
                    PROCESSOR_CACHE_TYPE,
                    CACHE_DESCRIPTOR,
                    LOGICAL_PROCESSOR_RELATIONSHIP,
                    RelationProcessorPackage,
                    SYSTEM_LOGICAL_PROCESSOR_INFORMATION,
                    GROUP_AFFINITY,
                    PROCESSOR_RELATIONSHIP,
                    NUMA_NODE_RELATIONSHIP,
                    CACHE_RELATIONSHIP,
                    PROCESSOR_GROUP_INFO,
                    GROUP_RELATIONSHIP,
                    SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX,
		    PSAPI_WORKING_SET_EX_BLOCK,
		    PSAPI_WORKING_SET_EX_INFORMATION],
                    [],[],[[#include <windows.h>]])
    CPPFLAGS="$old_CPPFLAGS"
    AC_CHECK_LIB([gdi32], [main],
                 [HWLOC_LIBS="-lgdi32 $HWLOC_LIBS"
                  AC_DEFINE([HAVE_LIBGDI32], 1, [Define to 1 if we have -lgdi32])])
    
    AC_CHECK_HEADER([windows.h], [
      AC_DEFINE([HWLOC_HAVE_WINDOWS_H], [1], [Define to 1 if you have the `windows.h' header.])
    ])
    
    AC_CHECK_HEADERS([sys/lgrp_user.h], [
      AC_CHECK_LIB([lgrp], [lgrp_latency_cookie],
                   [HWLOC_LIBS="-llgrp $HWLOC_LIBS"
                    AC_DEFINE([HAVE_LIBLGRP], 1, [Define to 1 if we have -llgrp])])
    ])
    AC_CHECK_HEADERS([kstat.h], [
      AC_CHECK_LIB([kstat], [main], 
                   [HWLOC_LIBS="-lkstat $HWLOC_LIBS"
                    AC_DEFINE([HAVE_LIBKSTAT], 1, [Define to 1 if we have -lkstat])])
    ])
    AC_CHECK_LIB([m], [fabsf],
                 [HWLOC_LIBS="-lm $HWLOC_LIBS"])

    AC_CHECK_DECLS([_SC_NPROCESSORS_ONLN,
    		_SC_NPROCESSORS_CONF,
    		_SC_NPROC_ONLN,
    		_SC_NPROC_CONF,
    		_SC_LARGE_PAGESIZE],,[:],[[#include <unistd.h>]])
    
    AC_HAVE_HEADERS([mach/mach_host.h])
    AC_HAVE_HEADERS([mach/mach_init.h], [
      AC_CHECK_FUNCS([host_info])
    ])

    AC_CHECK_HEADERS([sys/param.h])
    AC_CHECK_HEADERS([sys/sysctl.h], [
      AC_CHECK_DECLS([CTL_HW, HW_NCPU],,,[[
      #if HAVE_SYS_PARAM_H
      #include <sys/param.h>
      #endif
      #include <sys/sysctl.h>
      ]])
    ],,[
      AC_INCLUDES_DEFAULT
      #if HAVE_SYS_PARAM_H
      #include <sys/param.h>
      #endif
    ])
    AC_CHECK_FUNCS([sysctl sysctlbyname])

    case ${target} in
      *-*-mingw*|*-*-cygwin*)
        hwloc_pid_t=HANDLE
        hwloc_thread_t=HANDLE
        ;;
      *)
        hwloc_pid_t=pid_t
        AC_CHECK_TYPES([pthread_t], [hwloc_thread_t=pthread_t], [:], [[#include <pthread.h>]])
        ;;
    esac
    AC_DEFINE_UNQUOTED(hwloc_pid_t, $hwloc_pid_t, [Define this to the process ID type])
    if test "x$hwloc_thread_t" != "x" ; then
      AC_DEFINE_UNQUOTED(hwloc_thread_t, $hwloc_thread_t, [Define this to the thread ID type])
    fi
    
    _HWLOC_CHECK_DECL([sched_setaffinity], [
      AC_DEFINE([HWLOC_HAVE_SCHED_SETAFFINITY], [1], [Define to 1 if glibc provides a prototype of sched_setaffinity()])
      AS_IF([test "$HWLOC_STRICT_ARGS_CFLAGS" = "FAIL"],[
        AC_MSG_WARN([Support for sched_setaffinity() requires a C compiler which])
        AC_MSG_WARN([considers incorrect argument counts to be a fatal error.])
        AC_MSG_ERROR([Cannot continue.])
      ])
      AC_MSG_CHECKING([for old prototype of sched_setaffinity])
      hwloc_save_CFLAGS=$CFLAGS
      CFLAGS="$CFLAGS $HWLOC_STRICT_ARGS_CFLAGS"
      AC_COMPILE_IFELSE([
          AC_LANG_PROGRAM([[
              #define _GNU_SOURCE
              #include <sched.h>
              static unsigned long mask;
              ]], [[ sched_setaffinity(0, (void*) &mask); ]])],
          [AC_DEFINE([HWLOC_HAVE_OLD_SCHED_SETAFFINITY], [1], [Define to 1 if glibc provides the old prototype (without length) of sched_setaffinity()])
           AC_MSG_RESULT([yes])],
          [AC_MSG_RESULT([no])])
      CFLAGS=$hwloc_save_CFLAGS
    ], , [[
#define _GNU_SOURCE
#include <sched.h>
]])
    
    AC_MSG_CHECKING([for working CPU_SET])
    AC_LINK_IFELSE([
      AC_LANG_PROGRAM([[
        #include <sched.h>
        cpu_set_t set;
        ]], [[ CPU_ZERO(&set); CPU_SET(0, &set);]])],
	[AC_DEFINE([HWLOC_HAVE_CPU_SET], [1], [Define to 1 if the CPU_SET macro works])
         AC_MSG_RESULT([yes])],
        [AC_MSG_RESULT([no])])
    
    AC_MSG_CHECKING([for working CPU_SET_S])
    AC_LINK_IFELSE([
      AC_LANG_PROGRAM([[
          #include <sched.h>
          cpu_set_t *set;
        ]], [[
          set = CPU_ALLOC(1024);
          CPU_ZERO_S(CPU_ALLOC_SIZE(1024), set);
          CPU_SET_S(CPU_ALLOC_SIZE(1024), 0, set);
          CPU_FREE(set);
        ]])],
        [AC_DEFINE([HWLOC_HAVE_CPU_SET_S], [1], [Define to 1 if the CPU_SET_S macro works])
         AC_MSG_RESULT([yes])],
        [AC_MSG_RESULT([no])])

    AC_MSG_CHECKING([for working _syscall3])
    AC_LINK_IFELSE([
      AC_LANG_PROGRAM([[
          #include <linux/unistd.h>
          #include <errno.h>
          #define __NR_hwloc_test 123
          _syscall3(int, hwloc_test, int, param1, int, param2, int, param3);
        ]], [[ hwloc_test(1, 2, 3); ]])],
        [AC_DEFINE([HWLOC_HAVE__SYSCALL3], [1], [Define to 1 if the _syscall3 macro works])
         AC_MSG_RESULT([yes])],
        [AC_MSG_RESULT([no])])

    # Check for kerrighed, but don't abort if not found.  It's illegal
    # to pass in an empty 3rd argument, but we trust the output of
    # pkg-config, so just give it a value that will always work:
    # printf.
    HWLOC_PKG_CHECK_MODULES([KERRIGHED], [kerrighed >= 2.0], [printf], [], [:])

    AC_PATH_PROGS([HWLOC_MS_LIB], [lib])
    AC_ARG_VAR([HWLOC_MS_LIB], [Path to Microsoft's Visual Studio `lib' tool])

    AC_PATH_PROG([BASH], [bash])
    
    AC_CHECK_FUNCS([ffs], [
      _HWLOC_CHECK_DECL([ffs],[
        AC_DEFINE([HWLOC_HAVE_DECL_FFS], [1], [Define to 1 if function `ffs' is declared by system headers])
      ])
      AC_DEFINE([HWLOC_HAVE_FFS], [1], [Define to 1 if you have the `ffs' function.])
      if ( $CC --version | grep gccfss ) >/dev/null 2>&1 ; then
        dnl May be broken due to
        dnl    https://forums.oracle.com/forums/thread.jspa?threadID=1997328
        dnl TODO: a more selective test, since bug may be version dependent.
        dnl We can't use AC_TRY_LINK because the failure does not appear until
        dnl run/load time and there is currently no precedent for AC_TRY_RUN
        dnl use in hwloc.  --PHH
	dnl For now, we're going with "all gccfss compilers are broken". 
	dnl Better to be safe and correct; it's not like this is
	dnl performance-critical code, after all.
        AC_DEFINE([HWLOC_HAVE_BROKEN_FFS], [1], 
                  [Define to 1 if your `ffs' function is known to be broken.])
      fi
    ])
    AC_CHECK_FUNCS([ffsl], [
      _HWLOC_CHECK_DECL([ffsl],[
        AC_DEFINE([HWLOC_HAVE_DECL_FFSL], [1], [Define to 1 if function `ffsl' is declared by system headers])
      ])
      AC_DEFINE([HWLOC_HAVE_FFSL], [1], [Define to 1 if you have the `ffsl' function.])
    ])
    
    AC_CHECK_FUNCS([fls], [
      _HWLOC_CHECK_DECL([fls],[
        AC_DEFINE([HWLOC_HAVE_DECL_FLS], [1], [Define to 1 if function `fls' is declared by system headers])
      ])
      AC_DEFINE([HWLOC_HAVE_FLS], [1], [Define to 1 if you have the `fls' function.])
    ])
    AC_CHECK_FUNCS([flsl], [
      _HWLOC_CHECK_DECL([flsl],[
        AC_DEFINE([HWLOC_HAVE_DECL_FLSL], [1], [Define to 1 if function `flsl' is declared by system headers])
      ])
      AC_DEFINE([HWLOC_HAVE_FLSL], [1], [Define to 1 if you have the `flsl' function.])
    ])
    
    AC_CHECK_FUNCS([clz], [
      _HWLOC_CHECK_DECL([clz],[
        AC_DEFINE([HWLOC_HAVE_DECL_CLZ], [1], [Define to 1 if function `clz' is declared by system headers])
      ])
      AC_DEFINE([HWLOC_HAVE_CLZ], [1], [Define to 1 if you have the `clz' function.])
    ])
    AC_CHECK_FUNCS([clzl], [
      _HWLOC_CHECK_DECL([clzl],[
        AC_DEFINE([HWLOC_HAVE_DECL_CLZL], [1], [Define to 1 if function `clzl' is declared by system headers])
      ])
      AC_DEFINE([HWLOC_HAVE_CLZL], [1], [Define to 1 if you have the `clzl' function.])
    ])
    
    AC_CHECK_FUNCS([openat], [hwloc_have_openat=yes])

    AC_CHECK_HEADERS([malloc.h])
    AC_CHECK_FUNCS([getpagesize memalign posix_memalign])

    AC_CHECK_HEADERS([sys/utsname.h])
    AC_CHECK_FUNCS([uname])

    AC_CHECK_HEADERS([pthread_np.h])
    AC_CHECK_DECLS([pthread_setaffinity_np],,[:],[[
      #include <pthread.h>
      #ifdef HAVE_PTHREAD_NP_H
      #  include <pthread_np.h>
      #endif
    ]])
    AC_CHECK_DECLS([pthread_getaffinity_np],,[:],[[
      #include <pthread.h>
      #ifdef HAVE_PTHREAD_NP_H
      #  include <pthread_np.h>
      #endif
    ]])
    AC_CHECK_FUNC([sched_setaffinity], [hwloc_have_sched_setaffinity=yes])
    AC_CHECK_HEADERS([sys/cpuset.h],,,[[#include <sys/param.h>]])
    AC_SEARCH_LIBS([pthread_getthrds_np], [pthread],
      AC_DEFINE([HWLOC_HAVE_PTHREAD_GETTHRDS_NP], 1, `Define to 1 if you have pthread_getthrds_np')
    )

    # Linux libnuma support
    hwloc_linux_libnuma_happy=no
    if test "x$enable_libnuma" != "xno"; then
        hwloc_linux_libnuma_happy=yes
        AC_CHECK_HEADERS([numaif.h], [
            AC_CHECK_LIB([numa], [numa_available], [HWLOC_LINUX_LIBNUMA_LIBS="-lnuma"], [hwloc_linux_libnuma_happy=no])
        ], [hwloc_linux_libnuma_happy=no])
    fi
    AC_SUBST(HWLOC_LINUX_LIBNUMA_LIBS)
    # If we asked for Linux libnuma support but couldn't deliver, fail
    HWLOC_LIBS="$HWLOC_LIBS $HWLOC_LINUX_LIBNUMA_LIBS"
    AS_IF([test "$enable_libnuma" = "yes" -a "$hwloc_linux_libnuma_happy" = "no"],
          [AC_MSG_WARN([Specified --enable-libnuma switch, but could not])
           AC_MSG_WARN([find appropriate support])
           AC_MSG_ERROR([Cannot continue])])
    if test "x$hwloc_linux_libnuma_happy" = "xyes"; then
      tmp_save_LIBS="$LIBS"
      LIBS="$LIBS $HWLOC_LINUX_LIBNUMA_LIBS"

      AC_CHECK_LIB([numa], [set_mempolicy], [
	enable_set_mempolicy=yes
	AC_DEFINE([HWLOC_HAVE_SET_MEMPOLICY], [1], [Define to 1 if set_mempolicy is available.])
      ])
      AC_CHECK_LIB([numa], [mbind], [
	enable_mbind=yes
	AC_DEFINE([HWLOC_HAVE_MBIND], [1], [Define to 1 if mbind is available.])
      ])
      AC_CHECK_LIB([numa], [migrate_pages], [
	enable_migrate_pages=yes
	AC_DEFINE([HWLOC_HAVE_MIGRATE_PAGES], [1], [Define to 1 if migrate_pages is available.])
      ])

      LIBS="$tmp_save_LIBS"
    fi

    # PCI support
    hwloc_pci_happy=no
    if test "x$enable_pci" != "xno"; then
        hwloc_pci_happy=yes
        HWLOC_PKG_CHECK_MODULES([PCI], [libpci], [pci_cleanup], [:], [
          # manually check pciutils in case a old one without .pc is installed
          AC_CHECK_HEADERS([pci/pci.h], [
	    # try first without -lz, it's not always needed (RHEL5, Debian Etch)
	    AC_CHECK_LIB([pci], [pci_init], [
	      HWLOC_PCI_LIBS="-lpci"
	      ], [
              # try again with -lz because it's needed sometimes (FC7).
              # don't use AC_CHECK_LIB again because the cache would
              # return "no" without actually rechecking
              AC_MSG_CHECKING([for pci_init in -lpci with -lz])
              tmp_save_LIBS=$LIBS
              LIBS="-lpci -lz $LIBS"
              AC_LINK_IFELSE([AC_LANG_CALL([], [pci_init])],
                             [HWLOC_PCI_LIBS="-lpci -lz"
                              HWLOC_PCI_ADDITIONAL_LIBS="-lz"
                              AC_MSG_RESULT(yes)],
                             [hwloc_pci_happy=no
                              AC_MSG_RESULT(no)])
              LIBS=$tmp_save_LIBS])
            # Also check with pci_lookup_name, because that sometimes
            # requires -lresolv (RHEL5.6). don't use AC_CHECK_LIB twice
            # because the cache would return "no" without actually rechecking
	    AC_CHECK_LIB([pci], [pci_lookup_name], [],
                [AC_CHECK_LIB([resolv], [inet_ntoa], 
                    [AC_MSG_CHECKING([for pci_lookup_name in -lpci with -lresolv])
                     tmp_save_LIBS=$LIBS
                     LIBS="-lpci -lresolv $LIBS $HWLOC_PCI_ADDITIONAL_LIBS"
                     AC_LINK_IFELSE([AC_LANG_CALL([], [pci_lookup_name])],
                                    [HWLOC_PCI_LIBS="$HWLOC_PCI_LIBS -lresolv"
                                     HWLOC_PCI_ADDITIONAL_LIBS="$HWLOC_PCI_ADDITIONAL_LIBS -lresolv"
                                     AC_MSG_RESULT(yes)],
                                    [hwloc_pci_happy=no
                                     AC_MSG_RESULT(no)])
                     LIBS=$tmp_save_LIBS],
                    [hwloc_pci_happy=no])])
            ], [hwloc_pci_happy=no])
        ])
    fi
    AC_SUBST(HWLOC_PCI_LIBS)
    HWLOC_LIBS="$HWLOC_LIBS $HWLOC_PCI_LIBS"
    # If we asked for pci support but couldn't deliver, fail
    AS_IF([test "$enable_pci" = "yes" -a "$hwloc_pci_happy" = "no"],
          [AC_MSG_WARN([Specified --enable-pci switch, but could not])
           AC_MSG_WARN([find appropriate support])
           AC_MSG_ERROR([Cannot continue])])
    if test "x$hwloc_pci_happy" = "xyes"; then
      tmp_save_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS $HWLOC_PCI_CFLAGS"
      tmp_save_LIBS="$LIBS"
      LIBS="$LIBS $HWLOC_PCI_LIBS"
      AC_CHECK_DECLS([PCI_LOOKUP_NO_NUMBERS],,[:],[[#include <pci/pci.h>]])
      AC_CHECK_DECLS([PCI_LOOKUP_NO_NUMBERS],,[:],[[#include <pci/pci.h>]])
      AC_CHECK_LIB([pci], [pci_find_cap], [enable_pci_caps=yes], [enable_pci_caps=no], [$HWLOC_PCI_ADDITIONAL_LIBS])
      if test "x$enable_pci_caps" = "xyes"; then
        AC_DEFINE([HWLOC_HAVE_PCI_FIND_CAP], [1], [Define to 1 if `libpci' has the `pci_find_cap' function.])
      fi

      AC_MSG_CHECKING(whether struct pci_dev has a device_class field)
      AC_TRY_COMPILE([#include <pci/pci.h>],
	[int f(struct pci_dev *dev) { return dev->device_class; }],
        [pcidev_device_class=yes], [pcidev_device_class=no])
      AC_MSG_RESULT([$pcidev_device_class])
      if test x$pcidev_device_class = xyes; then
        AC_DEFINE([HWLOC_HAVE_PCIDEV_DEVICE_CLASS], [1], [Define to 1 if struct pci_dev has a `device_class' field.])
      fi

      AC_MSG_CHECKING(whether struct pci_dev has a domain field)
      AC_TRY_COMPILE([#include <pci/pci.h>],
	[int f(struct pci_dev *dev) { return dev->domain; }],
        [pcidev_domain=yes], [pcidev_domain=no])
      AC_MSG_RESULT([$pcidev_domain])
      if test x$pcidev_domain = xyes; then
        AC_DEFINE([HWLOC_HAVE_PCIDEV_DOMAIN], [1], [Define to 1 if struct pci_dev has a `domain' field.])
      fi

      HWLOC_REQUIRES="libpci $HWLOC_REQUIRES"
      AC_DEFINE([HWLOC_HAVE_LIBPCI], [1], [Define to 1 if you have the `libpci' library.])
      AC_SUBST([HWLOC_HAVE_LIBPCI], [1])
      CFLAGS="$tmp_save_CFLAGS"
      LIBS="$tmp_save_LIBS"
    else
      AC_SUBST([HWLOC_HAVE_LIBPCI], [0])
    fi
    HWLOC_CFLAGS="$HWLOC_CFLAGS $HWLOC_PCI_CFLAGS"

    # libxml2 support
    hwloc_libxml2_happy=
    if test "x$enable_libxml2" != "xno"; then
        HWLOC_PKG_CHECK_MODULES([LIBXML2], [libxml-2.0], [xmlNewDoc], 
                                [hwloc_libxml2_happy=yes], 
                                [hwloc_libxml2_happy=no])
    fi
    if test "x$hwloc_libxml2_happy" = "xyes"; then
        HWLOC_REQUIRES="libxml-2.0 $HWLOC_REQUIRES"
        AC_DEFINE([HWLOC_HAVE_LIBXML2], [1], [Define to 1 if you have the `libxml2' library.])
        AC_SUBST([HWLOC_HAVE_LIBXML2], [1])
    else
        AC_SUBST([HWLOC_HAVE_LIBXML2], [0])
	AS_IF([test "$enable_libxml2" = "yes"],
              [AC_MSG_WARN([--enable-libxml2 requested, but libxml2 was not found])
               AC_MSG_ERROR([Cannot continue])])
    fi
    HWLOC_CFLAGS="$HWLOC_CFLAGS $HWLOC_LIBXML2_CFLAGS"    
    HWLOC_LIBS="$HWLOC_LIBS $HWLOC_LIBXML2_LIBS"

    # Setup HWLOC's C, CPP, and LD flags, and LIBS
    AC_SUBST(HWLOC_REQUIRES)
    AC_SUBST(HWLOC_CFLAGS)
    HWLOC_CPPFLAGS='-I$(HWLOC_top_builddir)/include -I$(HWLOC_top_srcdir)/include'    
    AC_SUBST(HWLOC_CPPFLAGS)
    AC_SUBST(HWLOC_LDFLAGS)
    AC_SUBST(HWLOC_LIBS)

    # Set these values explicitly for embedded builds.  Exporting
    # these values through *_EMBEDDED_* values gives us the freedom to
    # do something different someday if we ever need to.  There's no
    # need to fill these values in unless we're in embedded mode.
    # Indeed, if we're building in embedded mode, we want HWLOC_LIBS
    # to be empty so that nothing is linked into libhwloc_embedded.la
    # itself -- only the upper-layer will link in anything required.

    AS_IF([test "$hwloc_mode" = "embedded"],
          [HWLOC_EMBEDDED_CFLAGS=$HWLOC_CFLAGS
           HWLOC_EMBEDDED_CPPFLAGS=$HWLOC_CPPFLAGS
           HWLOC_EMBEDDED_LDADD='$(HWLOC_top_builddir)/src/libhwloc_embedded.la'
           HWLOC_EMBEDDED_LIBS=$HWLOC_LIBS
           HWLOC_LIBS=])
    AC_SUBST(HWLOC_EMBEDDED_CFLAGS)
    AC_SUBST(HWLOC_EMBEDDED_CPPFLAGS)
    AC_SUBST(HWLOC_EMBEDDED_LDADD)
    AC_SUBST(HWLOC_EMBEDDED_LIBS)

    # Try to compile the cpuid inlines
    AC_MSG_CHECKING([for cpuid])
    old_CPPFLAGS="$CPPFLAGS"
    CFLAGS="$CFLAGS -I$HWLOC_top_srcdir/include"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdio.h>
        #define __hwloc_inline
        #include <private/cpuid.h>
      ]], [[
        if (hwloc_have_cpuid()) {
          unsigned eax = 0, ebx, ecx = 0, edx;
          hwloc_cpuid(&eax, &ebx, &ecx, &edx);
          printf("highest cpuid %x\n", eax);
          return 0;
        }
      ]])],
      [AC_MSG_RESULT([yes])
       AC_DEFINE(HWLOC_HAVE_CPUID, 1, [Define to 1 if you have cpuid])
       hwloc_have_cpuid=yes],
      [AC_MSG_RESULT([no])])
    CPPFLAGS="$old_CPPFLAGS"

    # Always generate these files
    AC_CONFIG_FILES(
        hwloc_config_prefix[Makefile]
        hwloc_config_prefix[include/Makefile]
        hwloc_config_prefix[src/Makefile ]
    )

    # Cleanup
    AC_LANG_POP

    # Success
    $2
])dnl

#-----------------------------------------------------------------------

# Specify the symbol prefix
AC_DEFUN([HWLOC_SET_SYMBOL_PREFIX],[
    hwloc_symbol_prefix_value=$1
])dnl

#-----------------------------------------------------------------------

# This must be a standalone routine so that it can be called both by
# HWLOC_INIT and an external caller (if HWLOC_INIT is not invoked).
AC_DEFUN([HWLOC_DO_AM_CONDITIONALS],[
    AS_IF([test "$hwloc_did_am_conditionals" != "yes"],[
        AM_CONDITIONAL([HWLOC_BUILD_STANDALONE], [test "$hwloc_mode" = "standalone"])

        AM_CONDITIONAL([HWLOC_HAVE_GCC], [test "x$GCC" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_MS_LIB], [test "x$HWLOC_MS_LIB" != "x"])
        AM_CONDITIONAL([HWLOC_HAVE_OPENAT], [test "x$hwloc_have_openat" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_LINUX_LIBNUMA],
                       [test "x$hwloc_have_linux_libnuma" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_SCHED_SETAFFINITY],
                       [test "x$hwloc_have_sched_setaffinity" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_LIBIBVERBS], 
                       [test "x$hwloc_have_libibverbs" = "xyes"])
	AM_CONDITIONAL([HWLOC_HAVE_CUDA],
		       [test "x$hwloc_have_cuda" = "xyes"])
	AM_CONDITIONAL([HWLOC_HAVE_MYRIEXPRESS],
		       [test "x$hwloc_have_myriexpress" = "xyes"])
	AM_CONDITIONAL([HWLOC_HAVE_CUDART],
		       [test "x$hwloc_have_cudart" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_CAIRO], [test "x$enable_cairo" != "xno"])
        AM_CONDITIONAL([HWLOC_HAVE_LIBPCI], [test "$hwloc_pci_happy" = "yes"])
        AM_CONDITIONAL([HWLOC_HAVE_SET_MEMPOLICY], [test "x$enable_set_mempolicy" != "xno"])
        AM_CONDITIONAL([HWLOC_HAVE_MBIND], [test "x$enable_mbind" != "xno"])
        AM_CONDITIONAL([HWLOC_HAVE_BUNZIPP], [test "x$BUNZIPP" != "xfalse"])

        AM_CONDITIONAL([HWLOC_BUILD_DOXYGEN],
                       [test "x$hwloc_generate_doxs" = "xyes"])
        AM_CONDITIONAL([HWLOC_BUILD_README], 
                       [test "x$hwloc_generate_readme" = "xyes" -a \( "x$hwloc_install_doxs" = "xyes" -o "x$hwloc_generate_doxs" = "xyes" \) ])
        AM_CONDITIONAL([HWLOC_INSTALL_DOXYGEN], 
                       [test "x$hwloc_install_doxs" = "xyes"])

        AM_CONDITIONAL([HWLOC_HAVE_LINUX], [test "x$hwloc_linux" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_IRIX], [test "x$hwloc_irix" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_DARWIN], [test "x$hwloc_darwin" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_FREEBSD], [test "x$hwloc_freebsd" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_SOLARIS], [test "x$hwloc_solaris" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_AIX], [test "x$hwloc_aix" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_OSF], [test "x$hwloc_osf" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_HPUX], [test "x$hwloc_hpux" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_WINDOWS], [test "x$hwloc_windows" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_MINGW32], [test "x$target_os" = "xmingw32"])

        AM_CONDITIONAL([HWLOC_HAVE_X86_32], [test "x$hwloc_x86_32" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_X86_64], [test "x$hwloc_x86_64" = "xyes"])
        AM_CONDITIONAL([HWLOC_DOXYGEN_BROKEN_SHORT_NAMES], [test "$HWLOC_DOXYGEN_VERSION" = "1.6.2"])
        AM_CONDITIONAL([HWLOC_HAVE_CPUID], [test "x$hwloc_have_cpuid" = "xyes"])
        AM_CONDITIONAL([HWLOC_BUILD_UTILS], [test "$hwloc_build_utils" = "yes"])
        AM_CONDITIONAL([HWLOC_BUILD_TESTS], [test "$hwloc_build_tests" = "yes"])
    ])
    hwloc_did_am_conditionals=yes
])dnl

#-----------------------------------------------------------------------

AC_DEFUN([_HWLOC_CHECK_DIFF_U], [
  AC_MSG_CHECKING([whether diff accepts -u])
  if diff -u /dev/null /dev/null 2> /dev/null
  then
    HWLOC_DIFF_U="-u"
  else
    HWLOC_DIFF_U=""
  fi
  AC_SUBST([HWLOC_DIFF_U])
  AC_MSG_RESULT([$HWLOC_DIFF_U])
])

AC_DEFUN([_HWLOC_CHECK_DIFF_W], [
  AC_MSG_CHECKING([whether diff accepts -w])
  if diff -w /dev/null /dev/null 2> /dev/null
  then
    HWLOC_DIFF_W="-w"
  else
    HWLOC_DIFF_W=""
  fi
  AC_SUBST([HWLOC_DIFF_W])
  AC_MSG_RESULT([$HWLOC_DIFF_W])
])

#-----------------------------------------------------------------------

dnl HWLOC_CHECK_DECL
dnl
dnl Check declaration of given function by trying to call it with an insane
dnl number of arguments (10). Success means the compiler couldn't really check.
AC_DEFUN([_HWLOC_CHECK_DECL], [
  AC_MSG_CHECKING([whether function $1 is declared])
  AC_REQUIRE([AC_PROG_CC])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT([$4])],[$1(1,2,3,4,5,6,7,8,9,10);])],
    [AC_MSG_RESULT([no])
     $3],
    [AC_MSG_RESULT([yes])
     $2]
  )
])

#-----------------------------------------------------------------------

dnl HWLOC_CHECK_DECLS
dnl
dnl Same as HWLOCK_CHECK_DECL, but defines HAVE_DECL_foo to 1 or 0 depending on
dnl the result.
AC_DEFUN([_HWLOC_CHECK_DECLS], [
  HWLOC_CHECK_DECL([$1], [ac_have_decl=1], [ac_have_decl=0], [$4])
  AC_DEFINE_UNQUOTED(AS_TR_CPP([HAVE_DECL_$1]), [$ac_have_decl],
    [Define to 1 if you have the declaration of `$1', and to 0 if you don't])
])

