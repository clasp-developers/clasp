# Process this file with autoconf to produce a configure script.
AC_INIT([libatomic_ops],[7.2])

AC_CANONICAL_TARGET([])
AC_CONFIG_SRCDIR(src/atomic_ops.c)
AM_INIT_AUTOMAKE
AC_PROG_RANLIB

AC_CONFIG_HEADERS([src/config.h])

# Checks for programs.
AM_PROG_CC_C_O
AM_PROG_AS

# Checks for functions.
AC_FUNC_MMAP

# Checks for header files.
AC_HEADER_STDC
# AC_CHECK_HEADERS([ ])

# Determine PIC flag.
need_asm=false
PICFLAG=
AC_MSG_CHECKING(for PIC compiler flag)
if test "$GCC" = yes; then
  case "$host" in
    *-*-cygwin* | *-*-mingw*)
      # Cygwin and Mingw[-w32/64] do not need -fPIC.
      AC_MSG_RESULT("<none>")
      ;;
    *)
      AC_MSG_RESULT(-fPIC)
      PICFLAG=-fPIC
      AC_MSG_CHECKING(whether gcc -fPIC causes __PIC__ definition)
      # Workaround: at least GCC 3.4.6 (Solaris) does not define this macro.
      old_CFLAGS="$CFLAGS"
      CFLAGS="$PICFLAG $CFLAGS"
      AC_TRY_COMPILE([],[
 #ifndef __PIC__
 # error
 #endif
      ], [ac_cv_pic_macro=yes], [ac_cv_pic_macro=no])
      CFLAGS="$old_CFLAGS"
      AC_MSG_RESULT($ac_cv_pic_macro)
      AS_IF([test "$ac_cv_pic_macro" = yes], [],
            [PICFLAG="-D__PIC__=1 $PICFLAG"])
      ;;
  esac
else
  case "$host" in
    *-*-hpux*)
      AC_MSG_RESULT("+Z")
      PICFLAG="+Z"
      CFLAGS="$CFLAGS +O2 -mt"
      ;;
    *-*-solaris*)
      AC_MSG_RESULT(-Kpic)
      PICFLAG=-Kpic
      CFLAGS="$CFLAGS -O"
      need_asm=true
      ;;
    *-*-linux*)
      AC_MSG_RESULT(-fPIC)
      PICFLAG=-fPIC
      # Any Linux compiler had better be gcc compatible.
      ;;
    *)
      AC_MSG_RESULT("<none>")
      ;;
  esac
fi
CFLAGS="$CFLAGS -DNDEBUG"
AC_SUBST(PICFLAG)
AC_SUBST(DEFS)

AH_TEMPLATE([_PTHREADS], [Indicates the use of pthreads (NetBSD).])

THREADDLLIBS=
have_pthreads=true
## Libraries needed to support threads.
AC_CHECK_LIB(pthread, pthread_self, THREADDLLIBS="-lpthread",,)
AC_DEFINE(_REENTRANT, 1, [Required define if using POSIX threads.])
case "$host" in
    *-*-netbsd*)
      AC_DEFINE(_PTHREADS)
      ;;
    *-*-openbsd* | *-*-kfreebsd*-gnu | *-*-dgux*)
      THREADDLLIBS=-pthread
      ;;
    *-*-cygwin* | *-*-darwin*)
      THREADDLLIBS=
      ;;
    *-*-mingw*)
      THREADDLLIBS=
      have_pthreads=false
      ;;
esac
AC_SUBST(THREADDLLIBS)

AM_CONDITIONAL(HAVE_PTHREAD_H, test x$have_pthreads = xtrue)
AM_CONDITIONAL(NEED_ASM, test x$need_asm = xtrue)

AC_CONFIG_FILES([Makefile pkgconfig/atomic_ops.pc \
    pkgconfig/atomic_ops-uninstalled.pc doc/Makefile src/Makefile \
    src/atomic_ops/Makefile src/atomic_ops/sysdeps/Makefile tests/Makefile])
AC_CONFIG_COMMANDS([default],[[]],[[
PICFLAG="${PICFLAG}"
CC="${CC}"
DEFS="${DEFS}"
]])
AC_OUTPUT
