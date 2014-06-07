#
# Process this file with autoconf to produce a configure script.
#
# Copyright 2003 by Gray Watson
#
# This file is part of the dmalloc package.
#
# Permission to use, copy, modify, and distribute this software for
# any purpose and without fee is hereby granted, provided that the
# above copyright notice and this permission notice appear in all
# copies, and that the name of Gray Watson not be used in
# advertising or publicity pertaining to distribution of the
# document or software without specific, written prior permission.
#
# Gray Watson makes no representations about the suitability of the
# software described herein for any purpose.  It is provided "as is"
# without express or implied warranty.
#
# The author may be contacted via http://dmalloc.com/
#
# $Id: configure.ac,v 1.37 2007/05/14 17:23:37 gray Exp $
#
AC_REVISION($Revision: 1.37 $)
AC_INIT(dmalloc, 5.5.2, [http://dmalloc.com/])
AC_CONFIG_SRCDIR(dmalloc.c)
AC_CONFIG_HEADER(conf.h)

AC_MSG_NOTICE([configurations for the dmalloc library])

##############################################################################
# arguments

AC_ARG_ENABLE(cxx,
    [ AC_HELP_STRING([--disable-cxx],
		[do not build in C++ support (enabled by default)] ) ],
    [ # enable_cxx set by default ],
    [ enable_cxx=yes ]
)
AC_ARG_ENABLE(threads,
    [ AC_HELP_STRING([--enable-threads],
		[build in thread support (disabled by default)] ) ],
    [ # enable_threads set by default ],
    [ enable_threads=no ]
)
AC_ARG_ENABLE(shlib,
    [ AC_HELP_STRING([--enable-shlib],
		[build shared libraries (disabled by default)] ) ],
    [ # enable_shlib set by default ],
    [ enable_shlib=no ]
)

##############################################################################
AC_MSG_NOTICE([build utilities])

AC_PROG_CC
AC_PROG_CXX

# see if we actually have a CXX program
if test "$ac_cv_prog_CXX" = "" -o ! -x "$ac_cv_prog_CXX"; then
    AC_MSG_WARN(could not find C++ compiler $ac_cv_prog_CXX)
    enable_cxx=no
fi

# hopefully we have a stdc c-compiler
if test "$ac_cv_prog_cc_stdc" = "no" ; then
	AC_MSG_WARN()
	AC_MSG_WARN(no ansi compiler.  this build may fail.)
	AC_MSG_WARN()
fi
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_C_CONST

# we need this for various settings
AC_HEADER_STDC
AC_CHECK_HEADER([stdarg.h],
		[AC_DEFINE(HAVE_STDARG_H,1) AC_SUBST([HAVE_STDARG_H],1)],
		[AC_DEFINE(HAVE_STDARG_H,0) AC_SUBST([HAVE_STDARG_H],0)])
AC_CHECK_HEADER([stdlib.h],
		[AC_DEFINE(HAVE_STDLIB_H,1) AC_SUBST([HAVE_STDLIB_H],1)],
		[AC_DEFINE(HAVE_STDLIB_H,0) AC_SUBST([HAVE_STDLIB_H],0)])
AC_CHECK_HEADER([string.h],
		[AC_DEFINE(HAVE_STRING_H,1) AC_SUBST([HAVE_STRING_H],1)],
		[AC_DEFINE(HAVE_STRING_H,0) AC_SUBST([HAVE_STRING_H],0)])
AC_CHECK_HEADER([unistd.h],
		[AC_DEFINE(HAVE_UNISTD_H,1) AC_SUBST([HAVE_UNISTD_H],1)],
		[AC_DEFINE(HAVE_UNISTD_H,0) AC_SUBST([HAVE_UNISTD_H],0)])
AC_CHECK_HEADER([sys/types.h],
		[AC_DEFINE(HAVE_SYS_TYPES_H,1) AC_SUBST([HAVE_SYS_TYPES_H],1)],
		[AC_DEFINE(HAVE_SYS_TYPES_H,0) AC_SUBST([HAVE_SYS_TYPES_H],0)])
AC_CHECK_HEADER([sys/mman.h],
		[AC_DEFINE(HAVE_SYS_MMAN_H,1) AC_SUBST([HAVE_SYS_MMAN_H],1)],
		[AC_DEFINE(HAVE_SYS_MMAN_H,0) AC_SUBST([HAVE_SYS_MMAN_H],0)])

###############################################################################
# shared library handling

#
# check to see where we should install our shared libraries
# DONT CACHE in case the change the --prefix
#
# This is pretty tricky here and should be handled by autoconf
# automatically.  I try to see if there is a shlib directory
# to install by shlibs there.  If not I use the $libdir.  Some
# of the variables have values of '${exec_prefix}' and the like
# so the quoting is very important.
#
# We first try and find the true directory where we are
# installing.
#
if test "x$exec_prefix" = xNONE; then
	if test "x$prefix" = xNONE; then
		shlibprefix="$ac_default_prefix"
	else
		shlibprefix="$prefix"
	fi
else
	shlibprefix="$exec_prefix"
fi
#
# Then we see if there is a special shlib directory to use.
#
if test -d $shlibprefix/shlib; then
	shlibdir='${exec_prefix}/shlib'
else
	shlibdir="$libdir"
fi
AC_SUBST(shlibdir)

# see how the local system builds shared libraries
AC_MSG_CHECKING([shared library link args])
AC_COMPILE_IFELSE([ int foo(int val) { return val + 1; } ],[
	# so now we try to create an archive from the compiled .o file
	(ar cr conftest.a conftest.o) 2>&5
	# see which shared-library ld commands work
	#
	# Darwin/Mac OS X - Terry Teague
	# username terry_teague at domain users.sourceforge.net
	ac_cv_shared_lib_link_objs=no
	if test `uname` = "Darwin"; then
          if (ld -dylib -o conftest.so.t -lc conftest.a) 2>&5; then
            # By convention on some platforms
            # libLLL.so, libLLL.X.so are symlinks to libLLL.X.Y.Z.so
            # where X.Y.Z is version # (major.minor.increment) of the library
	    # (API)
            # For the moment set the Mac OS X dylib compatibility version # to
	    # 1.0 and don't create any symlinks or versioned library names.
            # Perhaps this could be the version of the dmalloc library
	    # (e.g. 5.4.0)
            ac_cv_shared_link_args='$(CXX) -dynamiclib -install_name $@ -compatibility_version 1 -current_version 1 -o $@.t'
            # linking static archives into a dynamic lib doesn't seem to work
            # explicitly link the object files
            ac_cv_shared_lib_link_objs=yes
          else
            # oh well, toss an error
            ac_cv_shared_link_args='# Could not configure shlib linking'
            enable_shlib=no
          fi
       elif (ld -shared --whole-archive -soname conftest.so -o conftest.so.t conftest.a) 2>&5; then
		ac_cv_shared_link_args='ld -shared --whole-archive -soname $@ -o $@.t'
	elif (ld -shared -o conftest.so.t -all -soname conftest.so.t -none -lc -all conftest.a) 2>&5; then
		ac_cv_shared_link_args='ld -shared -o $@.t -all -soname $@ -none -lc -all'
	elif (ld -G -o conftest.so.t conftest.a) 2>&5; then
		ac_cv_shared_link_args='ld -G -o $@.t'
	else
		# oh well, toss an error
		ac_cv_shared_link_args='# Could not configure shlib linking'
		enable_shlib=no
	fi
],[
	# oh well, toss an error
	ac_cv_shared_link_args='# Could not configure shlib linking'
	enable_shlib=no
])
shlinkargs="$ac_cv_shared_link_args"
AC_SUBST(shlinkargs)
if test "$ac_cv_shared_lib_link_objs" = "yes"; then
       AC_MSG_RESULT([$ac_cv_shared_link_args objectfiles...])
else
       AC_MSG_RESULT([$ac_cv_shared_link_args])
fi

###############################################################################

AC_MSG_CHECKING([shared library extension])
if test `uname` = "Darwin"; then
	# Darwin/Mac OS X - Terry Teague
	# username terry_teague at domain users.sourceforge.net
	ac_cv_shared_lib_suffix=dylib
else
	ac_cv_shared_lib_suffix=so
fi
shlibext="$ac_cv_shared_lib_suffix"
AC_MSG_RESULT([$ac_cv_shared_lib_suffix])
AC_SUBST(shlibext)

###############################################################################

AC_MSG_CHECKING([C++ support])
if test "$enable_cxx" = "yes"; then
	AC_SUBST([CXX_ON],[])
	AC_SUBST([CXX_OFF],[#])
	AC_MSG_RESULT([enabled])
else
	AC_SUBST([CXX_ON],[#])
	AC_SUBST([CXX_OFF],[])
	AC_MSG_RESULT([disabled])
fi

AC_MSG_CHECKING([thread support])
if test "$enable_threads" = "yes"; then
	AC_SUBST([TH_ON],[])
	AC_SUBST([TH_OFF],[#])
	AC_MSG_RESULT([enabled])
else
	AC_SUBST([TH_ON],[#])
	AC_SUBST([TH_OFF],[])
	AC_MSG_RESULT([disabled])
fi

# shlib support
AC_MSG_CHECKING(shlib support)
if test "$enable_shlib" = "yes"; then
	AC_SUBST([SL_ON],[])
	AC_SUBST([SL_OFF],[#])
	AC_MSG_RESULT(enabled)
else
	AC_SUBST([SL_ON],[#])
	AC_SUBST([SL_OFF],[])
	AC_MSG_RESULT(disabled)
fi

###############################################################################
#
# check for size_t
#
AC_TYPE_SIZE_T
if test "$ac_cv_type_size_t" = "yes"; then
	AC_DEFINE(DMALLOC_SIZE,size_t)
else
	AC_DEFINE(DMALLOC_SIZE,unsigned long)
fi

#
# check for unsigned-ness
#
AC_MSG_CHECKING([dmalloc size unsigned])
AC_RUN_IFELSE([AC_LANG_SOURCE([[
#if HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#if HAVE_STDLIB_H
#  include <stdlib.h>
#endif
main() { DMALLOC_SIZE x = -1; if (x >= 0) exit(0); else exit(1); }
]])],
[AC_DEFINE(DMALLOC_SIZE_UNSIGNED,1)  AC_MSG_RESULT([yes])],
[AC_DEFINE(DMALLOC_SIZE_UNSIGNED,0)  AC_MSG_RESULT([no])],
[AC_DEFINE(DMALLOC_SIZE_UNSIGNED,0)  AC_MSG_RESULT([no])]
)

#
# check for strdup macro (linux)
#
AC_MSG_CHECKING([strdup macro])
AC_RUN_IFELSE([AC_LANG_SOURCE([[
#if HAVE_STDLIB_H
#  include <string.h>
#endif

#ifdef strdup
main() { exit(0); }
#else
main() { exit(1); }
#endif
]])],
[ac_cv_strdup_macro=yes],
[ac_cv_strdup_macro=no],
[ac_cv_strdup_macro=no]
)
AC_MSG_RESULT([$ac_cv_strdup_macro])

#
# check for existance of mprotect and associated defines
#
AC_CHECK_FUNCS(mprotect)
AC_MSG_CHECKING([mprotect works])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#if HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#if HAVE_SYS_MMAN_H
#  include <sys/mman.h>
#endif
]],[[
  int prot = PROT_NONE | PROT_READ | PROT_WRITE;
  (void)mprotect(0, 0, prot);
]])],
[ AC_DEFINE(PROTECT_ALLOWED,1) AC_MSG_RESULT([yes]) ],
[ AC_DEFINE(PROTECT_ALLOWED,0) AC_MSG_RESULT([no]) ]
)

##############################################################################
AC_MSG_NOTICE([important functionality])

AC_CHECK_FUNCS(mmap)
AC_CHECK_FUNCS(sbrk)

if test x$ac_cv_func_mmap != xyes && x$ac_cv_func_sbrk != xyes; then
	AC_MSG_WARN()
	AC_MSG_WARN(no mmap nor sbrk function.  See INTERNAL_MEMORY_SPACE in settings.dist.)
	AC_MSG_WARN()
fi

AC_MSG_CHECKING([mmap usage])
AC_RUN_IFELSE([AC_LANG_SOURCE([[
#if HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#if HAVE_SYS_MMAN_H
#  include <sys/mman.h>
#endif
main() {
  /* we could open /dev/zero and map to that but I did not want the library
     to open another file-descriptor */
  char *dest, *src, *src_p, *dest_p;
  src = "0123456789";
  dest = (char *)mmap(0L, 10, PROT_READ | PROT_WRITE | PROT_EXEC,
	              MAP_PRIVATE | MAP_ANON, -1 /* no fd */,
		      0 /* no offset */);
  src_p = src; dest_p = dest;
  while (src_p < src + 10) { *dest_p++ = *src_p++; }
  src_p = src; dest_p = dest;
  while (src_p < src + 10) {
    if (*src_p++ != *dest_p++) { exit(1); }
  }
  exit(0);
}
]])],
[ AC_DEFINE(USE_MMAP, 1) ac_cv_use_mmap=yes],
[ AC_DEFINE(USE_MMAP, 0) ac_cv_use_mmap=no],
[ AC_DEFINE(USE_MMAP, 0) ac_cv_use_mmap=no]
)
AC_MSG_RESULT([$ac_cv_use_mmap])

#
# check for basic block size
#
AC_CHECK_FUNCS(getpagesize)
AC_MSG_CHECKING([basic-block size])
ac_cv_page_size=0
if test $ac_cv_page_size = 0; then
   AC_RUN_IFELSE([main() { if (getpagesize()<=2048) exit(0); else exit(1); }],
	[ ac_cv_page_size=11 ] )
fi
if test $ac_cv_page_size = 0; then
   AC_RUN_IFELSE([main() { if (getpagesize()<=4096) exit(0); else exit(1); }],
	[ ac_cv_page_size=12 ] )
fi
if test $ac_cv_page_size = 0; then
   AC_RUN_IFELSE([main() { if (getpagesize()<=8192) exit(0); else exit(1); }],
	[ ac_cv_page_size=13 ] )
fi
if test $ac_cv_page_size = 0; then
   AC_RUN_IFELSE([main() { if (getpagesize()<=16384) exit(0); else exit(1); }],
	[ ac_cv_page_size=14 ] )
fi
if test $ac_cv_page_size = 0; then
    ac_cv_page_size=15
fi
AC_DEFINE_UNQUOTED([BASIC_BLOCK],[$ac_cv_page_size])
AC_MSG_RESULT([$ac_cv_page_size])

#
# data-alignment size...
#
AC_CHECK_SIZEOF(long)
AC_MSG_CHECKING([data-alignment size])
if test $ac_cv_sizeof_long = 4; then
	# we have to make a special case for sun sparc idiocy
	ac_cv_data_align=8
else
	ac_cv_data_align=$ac_cv_sizeof_long
fi
AC_DEFINE_UNQUOTED([ALLOCATION_ALIGNMENT],[$ac_cv_data_align])
AC_MSG_RESULT([$ac_cv_data_align])

#
# check the safety of the abort function
#
AC_CHECK_FUNCS(abort)
AC_MSG_CHECKING([abort safe])
AC_RUN_IFELSE([[
static int main_b = 0;
static char heap_mem[102400], *heap_p = heap_mem;
free () { if (main_b) _exit(0); }
char *malloc (int size) {
  char *pnt;
  if (main_b) _exit(0);
  pnt = heap_p;
  heap_p += size;
  return pnt;
}
char *calloc (int number, int size) {
  char *start, *pnt, *end;
  if (main_b) _exit(0);
  /* it should be already 0s */
  start = malloc (number * size);
  pnt = start;
  end = start + size;
  while (pnt < end) { *pnt++ = '\0'; }
  return start;
}
char *realloc (char *old_pnt, int new_size) {
  char *start, *pnt, *end;
  if (main_b) _exit(0);
  start = malloc (new_size);
  pnt = start;
  end = start + new_size;
  while (pnt < end) { *pnt++ = *old_pnt++; }
  return start;
}
main() { main_b = 1; abort(); _exit(1); }
]],
# NOTE: this is reversed because abort core dumps and doesn't exit with
# 0 which means that it should fail
[ AC_DEFINE(ABORT_OKAY, 0) AC_MSG_RESULT([no]) ],
# if it succeeds then something called free on the way out
[ AC_DEFINE(ABORT_OKAY, 1) AC_MSG_RESULT([yes]) ],
[ AC_DEFINE(ABORT_OKAY, 0) AC_MSG_RESULT([no]) ]
)

AC_TYPE_SIGNAL
AC_MSG_CHECKING([signal works])
AC_CHECK_HEADER([signal.h],
		[AC_DEFINE(HAVE_SIGNAL_H,1) AC_SUBST([HAVE_SIGNAL_H],1)],
		[AC_DEFINE(HAVE_SIGNAL_H,0) AC_SUBST([HAVE_SIGNAL_H],0)])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#if HAVE_SIGNAL_H
  #include <signal.h>
#endif
]], [[
	(void)signal(SIGHUP, SIG_IGN);
	(void)signal(SIGINT, SIG_IGN);
	(void)signal(SIGTERM, SIG_IGN);
]])],
[ AC_DEFINE(SIGNAL_OKAY, 1) AC_MSG_RESULT([yes]) ],
[ AC_DEFINE(SIGNAL_OKAY, 0) AC_MSG_RESULT([no]) ]
)

#
# check the safety of the getenv function
#
AC_CHECK_FUNCS(getenv)
AC_MSG_CHECKING([getenv safe])
AC_RUN_IFELSE([[
static char heap_mem[102400], *heap_p = heap_mem;
free (char *pnt) {
}
char *malloc (int size) {
  char *pnt;
  getenv("PATH");
  pnt = heap_p;
  heap_p += size;
  return pnt;
}
char *calloc (int number, int size) {
  char *start, *pnt, *end;
  getenv("PATH");
  /* it should be already 0s */
  start = malloc (number * size);
  pnt = start;
  end = start + size;
  while (pnt < end) { *pnt++ = '\0'; }
  return start;
}
char *realloc (char *old_pnt, int new_size) {
  char *start, *pnt, *end;
  getenv("PATH");
  start = malloc (new_size);
  pnt = start;
  end = start + new_size;
  while (pnt < end) { *pnt++ = *old_pnt++; }
  return start;
}
main() { malloc(10); _exit(0); }
]],
[ AC_DEFINE(GETENV_SAFE, 1)
  AC_MSG_RESULT([yes])
  getenv_safe=yes ],
# if it succeeds then something called free on the way out
[ AC_DEFINE(GETENV_SAFE, 0)
  AC_MSG_RESULT([no])
  getenv_safe=no ],
[ AC_DEFINE(GETENV_SAFE, 1)
  AC_MSG_RESULT([yes])
  getenv_safe=yes ]
)

# now check to see if we can replace getenv with GetEnvironmentVariableA
AC_CHECK_HEADER([sys/cygwin.h],
		[AC_DEFINE(HAVE_SYS_CYGWIN_H,1) AC_SUBST([HAVE_SYS_CYGWIN_H],1)],
		[AC_DEFINE(HAVE_SYS_CYGWIN_H,0) AC_SUBST([HAVE_SYS_CYGWIN_H],0)])
AC_CHECK_HEADER([w32api/windef.h],
		[AC_DEFINE(HAVE_W32API_WINDEF_H,1) AC_SUBST([HAVE_W32API_WINDEF_H],1)],
		[AC_DEFINE(HAVE_W32API_WINDEF_H,0) AC_SUBST([HAVE_W32API_WINDEF_H],0)])
AC_CHECK_HEADER([w32api/winbase.h],
		[AC_DEFINE(HAVE_W32API_WINBASE_H,1) AC_SUBST([HAVE_W32API_WINBASE_H],1)],
		[AC_DEFINE(HAVE_W32API_WINBASE_H,0) AC_SUBST([HAVE_W32API_WINBASE_H],0)],
		[[
#if HAVE_W32API_WINDEF_H
# include <w32api/windef.h>
#endif]])
AC_MSG_CHECKING([for GetEnvironmentVariableA])
AC_RUN_IFELSE([[
#if HAVE_SYS_CYGWIN_H
# include <sys/cygwin.h>
#endif
#if HAVE_STDARG_H
# include <stdarg.h>
#endif
#if HAVE_W32API_WINDEF_H
# include <w32api/windef.h>
#endif
#if HAVE_W32API_WINBASE_H
# include <w32api/winbase.h>
#endif
main() {
  char env_buf[256];
  GetEnvironmentVariableA("PATH", env_buf, sizeof(env_buf));
}
]],
[ AC_DEFINE(HAVE_GETENVIRONMENTVARIABLEA, 1)
  AC_MSG_RESULT([yes])
  have_getenvironmentvariablea=yes ],
[ AC_DEFINE(HAVE_GETENVIRONMENTVARIABLEA, 0)
  AC_MSG_RESULT([no])
  have_getenvironmentvariablea=no ],
[ AC_DEFINE(HAVE_GETENVIRONMENTVARIABLEA, 1)
  AC_MSG_RESULT([yes])
  have_getenvironmentvariablea=yes ]
)

if test x$getenv_safe != xyes && x$have_getenvironmentvariablea != xyes ; then
  AC_MSG_WARN([Since getenv is not safe, you MUST add the following])
  AC_MSG_WARN([code to the front of main():])
  AC_MSG_WARN([[  #ifdef DMALLOC]])
  AC_MSG_WARN([[    dmalloc_debug_setup(getenv("DMALLOC_OPTIONS"));]])
  AC_MSG_WARN([[  #endif]])
  AC_MSG_WARN([You will need to compile your program with: -DDMALLOC])
fi

############

#
# check for constructor attribute support
#
AC_MSG_CHECKING([constructor attribute])
AC_RUN_IFELSE([[
/* if we call the loc_con constructor then exit with error code 0 */
static void loc_con() __attribute__((constructor));
static void loc_con() { exit(0); }
int main() { exit(1); }
]],
[ AC_DEFINE(CONSTRUCTOR_WORKS, 1) AC_MSG_RESULT([yes]) ],
[ AC_DEFINE(CONSTRUCTOR_WORKS, 0) AC_MSG_RESULT([no]) ],
[ AC_DEFINE(CONSTRUCTOR_WORKS, 1) AC_MSG_RESULT([yes]) ]
)

#
# check for destructor attribute support
#
AC_MSG_CHECKING([destructor attribute])
AC_RUN_IFELSE([[
/* if we call the loc_decon destructor then exit with error code 0 */
static void loc_decon() __attribute__((destructor));
static void loc_decon() { exit(0); }
int main() { return 1; }
]],
[ AC_DEFINE(DESTRUCTOR_WORKS, 1)
  AC_MSG_RESULT([yes])
  destructor_works=yes ],
[ AC_DEFINE(DESTRUCTOR_WORKS, 0)
  AC_MSG_RESULT([no])
  destructor_works=no ],
[ AC_DEFINE(DESTRUCTOR_WORKS, 1)
  AC_MSG_RESULT([yes])
  destructor_works=yes ],
)

##############################################################################

#
# check if the return.h macros work
#
AC_MSG_CHECKING([return.h macros work])
AC_RUN_IFELSE([

#define __CONF_H__
#define USE_RETURN_MACROS 1
#define RETURN_MACROS_WORK 1

#include "return.h"

static void foo (void)
{
  char	*ret_addr;
  GET_RET_ADDR(ret_addr);
}

main()
{
  foo();
  exit(0);
}
],
[ AC_DEFINE(RETURN_MACROS_WORK, 1) AC_MSG_RESULT([yes]) ],
[ AC_DEFINE(RETURN_MACROS_WORK, 0) AC_MSG_RESULT([no]) ],
[ AC_DEFINE(RETURN_MACROS_WORK, 0) AC_MSG_RESULT([no]) ]
)

##############################################################################

#
# check to see if ident workds
#
AC_MSG_CHECKING([if ident works])
AC_COMPILE_IFELSE([
#ident "$Id: configure.ac,v 1.37 2007/05/14 17:23:37 gray Exp $"
static void foo (void) { }
],
[AC_DEFINE(IDENT_WORKS, 1) AC_MSG_RESULT([yes])],
[AC_DEFINE(IDENT_WORKS, 0) AC_MSG_RESULT([no])]
)

##############################################################################
AC_MSG_NOTICE([pthread particulars])

XX_OLD_LIBS=$LIBS

AC_SEARCH_LIBS(pthread_mutex_init, pthread pthreads c_r)
AC_CHECK_HEADER([pthread.h],
		[AC_DEFINE(HAVE_PTHREAD_H,1) AC_SUBST([HAVE_PTHREAD_H],1)],
		[AC_DEFINE(HAVE_PTHREAD_H,0) AC_SUBST([HAVE_PTHREAD_H],0)])
AC_CHECK_HEADER([pthreads.h],
		[AC_DEFINE(HAVE_PTHREADS_H,1) AC_SUBST([HAVE_PTHREADS_H],1)],
		[AC_DEFINE(HAVE_PTHREADS_H,0) AC_SUBST([HAVE_PTHREADS_H],0)])

AC_CHECK_FUNCS(pthread_mutex_init pthread_mutex_lock pthread_mutex_unlock)

AC_MSG_CHECKING([pthread mutex type])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#if HAVE_PTHREAD_H
#  include <pthread.h>
#endif
#if HAVE_PTHREADS_H
#  include <pthreads.h>
#endif
]],[[
   pthread_mutex_t dmalloc_mutex;
]])],
[AC_DEFINE(THREAD_MUTEX_T,pthread_mutex_t) AC_MSG_RESULT([pthread_mutex_t])],
[AC_MSG_RESULT([unknown])]
)

AC_MSG_CHECKING([pthread attribute initialization])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#if HAVE_PTHREAD_H
#  include <pthread.h>
#endif
#if HAVE_PTHREADS_H
#  include <pthreads.h>
#endif
]], [[
	THREAD_MUTEX_T dmalloc_mutex;
	pthread_mutex_init(&dmalloc_mutex, pthread_mutexattr_default);
]])],
[AC_DEFINE(THREAD_LOCK_INIT_VAL,pthread_mutexattr_default)
 AC_MSG_RESULT([pthread_mutexattr_default])],
[AC_DEFINE(THREAD_LOCK_INIT_VAL,0)
 AC_MSG_RESULT([0])]
)

LIBS=$XX_OLD_LIBS

##############################################################################
AC_MSG_NOTICE([functions])

# auto-shutdown functions
AC_CHECK_FUNCS(atexit on_exit)
if test $ac_cv_func_atexit = no \
   && test $ac_cv_func_on_exit = no \
   && test $destructor_works = no; then
	AC_MSG_WARN()
	AC_MSG_WARN(The library cannot automatically shut itself down without)
	AC_MSG_WARN(atexit, on_exit, or destructor support.  You will need to)
	AC_MSG_WARN(call dmalloc_shutdown directly before exit to get get the)
	AC_MSG_WARN(final statistics and unfreed memory information.)
	AC_MSG_WARN()
fi

# other bells and whistles
AC_CHECK_FUNCS(fork gethostname getpid getuid time ctime)
AC_CHECK_FUNCS(vprintf snprintf vsnprintf)
AC_CHECK_FUNCS(recalloc memalign valloc)

# required although we have code for them in compat.c
AC_CHECK_FUNCS(memcmp memcpy memmove memset)
AC_CHECK_FUNCS(strchr strrchr)
AC_CHECK_FUNCS(strlen strcmp strcpy strsep)

##############################################################################
AC_MSG_NOTICE([various functions for argv files])

# NOTE: some duplicated from above
AC_CHECK_FUNCS(atoi atol getenv)
AC_CHECK_FUNCS(strchr strcmp strcpy strlen strncmp strncpy strsep)

##############################################################################
AC_MSG_NOTICE([various functions for argument checking])

# NOTE: some duplicated from above
AC_CHECK_FUNCS(atoi atol)
AC_CHECK_FUNCS(bcmp bcopy bzero)
AC_CHECK_FUNCS([[index]])
AC_CHECK_FUNCS(memccpy memchr memcmp memcpy memmove memset)
AC_CHECK_FUNCS(rindex)
AC_CHECK_FUNCS(strcasecmp strcat strchr strcmp strcpy strcspn strlen)
AC_CHECK_FUNCS(strncasecmp strncat strncmp strncpy)
AC_CHECK_FUNCS(strpbrk strrchr strspn strstr strtok)

AC_CONFIG_FILES(Makefile)

AC_CONFIG_COMMANDS([dmalloc.h.2],
[
newfile=dmalloc.h.2
rm -f $newfile.t
echo '/* this is dmalloc.h.2 */' > $newfile.t
echo '/* produced by configure, inserted into dmalloc.h */' >> $newfile.t
echo '' >> $newfile.t

if test "$ac_cv_c_const" = "yes"; then
	echo '/* const is available */' >> $newfile.t
else
        echo '/* const is not available */' >> $newfile.t
	echo '#ifndef const' >> $newfile.t
	echo '#define const' >> $newfile.t
	echo '#endif' >> $newfile.t
fi

if test "$ac_cv_strdup_macro" = "yes"; then
	echo '/* strdup is a macro */' >> $newfile.t
	echo '#define DMALLOC_STRDUP_MACRO' >> $newfile.t
else
	echo '/* strdup is not a macro */' >> $newfile.t
	echo '#undef DMALLOC_STRDUP_MACRO' >> $newfile.t
fi
echo '' >> $newfile.t

echo '/*' >> $newfile.t
echo ' * the definition of DMALLOC_SIZE' >> $newfile.t
echo ' *' >> $newfile.t
echo ' * NOTE: some architectures have malloc, realloc, etc.' >> $newfile.t
echo ' * using unsigned instead of unsigned long.  You may' >> $newfile.t
echo ' * have to edit this by hand to fix any compilation' >> $newfile.t
echo ' * warnings or errors.' >> $newfile.t
echo ' */' >> $newfile.t
if test "$ac_cv_type_size_t" = "yes"; then
	echo "#include <sys/types.h>" >> $newfile.t
	echo "#define DMALLOC_SIZE size_t" >> $newfile.t
else
	echo "/* no include file needed */" >> $newfile.t
	echo "#define DMALLOC_SIZE unsigned long" >> $newfile.t
fi
echo '' >> $newfile.t
echo '/*' >> $newfile.t
echo ' * We use stdarg.h for the dmalloc_message and' >> $newfile.t
echo ' * dmalloc_vmessage functions.' >> $newfile.t
echo ' */' >> $newfile.t
if test "$ac_cv_header_stdarg_h" = "yes"; then
	echo "#include <stdarg.h>" >> $newfile.t
	echo "#define DMALLOC_STDARG 1" >> $newfile.t
else
	echo "/* no stdarg.h available */" >> $newfile.t
	echo "#define DMALLOC_STDARG 0" >> $newfile.t
fi
echo '' >> $newfile.t
if cmp -s $newfile $newfile.t 2>/dev/null; then
	AC_MSG_NOTICE([$newfile is unchanged])
	rm -f $newfile.t
else
	rm -f $newfile
	mv $newfile.t $newfile
fi
],
[
# initialization commands
ac_cv_c_const=$ac_cv_c_const
ac_cv_strdup_macro=$ac_cv_strdup_macro
ac_cv_type_size_t=$ac_cv_type_size_t
ac_cv_header_stdarg_h=$ac_cv_header_stdarg_h
])

AC_CONFIG_COMMANDS([settings.h],
[
newfile=settings.h
rm -f $newfile.t
echo '/*' > $newfile.t
echo ' * WARNING: this file was produced from settings.dist' >> $newfile.t
echo ' * by the configure program.  The configure script, when' >> $newfile.t
echo ' * run again, will overwrite changed made here.' >> $newfile.t
echo ' */' >> $newfile.t
echo '' >> $newfile.t
cat $srcdir/settings.dist >> $newfile.t

if cmp -s $newfile.t $newfile 2>/dev/null; then
	AC_MSG_NOTICE([$newfile is unchanged])
	rm -f $newfile.t
else
	rm -f $newfile
	mv $newfile.t $newfile
fi
])
AC_OUTPUT

AC_MSG_NOTICE([])
AC_MSG_NOTICE([Please check-out Makefile and conf.h to make sure that])
AC_MSG_NOTICE([sane configuration values were a result.])
AC_MSG_NOTICE([])
AC_MSG_NOTICE([You may want to change values in settings.h before])
AC_MSG_NOTICE([running 'make'.])
AC_MSG_NOTICE([])
AC_MSG_NOTICE([To run the basic library tests, you can execute:])
AC_MSG_NOTICE([  make light])
AC_MSG_NOTICE([or])
AC_MSG_NOTICE([  make heavy])
AC_MSG_NOTICE([])
