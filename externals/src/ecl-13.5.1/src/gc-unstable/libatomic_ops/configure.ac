# Process this file with autoconf to produce a configure script.
AC_INIT([libatomic_ops],[7.2alpha7])
AC_CANONICAL_TARGET([])
AC_CONFIG_SRCDIR(src/atomic_ops.c)
AM_INIT_AUTOMAKE
AC_PROG_RANLIB

AM_CONFIG_HEADER(src/config.h)

# Checks for programs.
AM_PROG_CC_C_O
AM_PROG_AS

# Checks for functions.
AC_FUNC_MMAP

# Checks for header files.
AC_HEADER_STDC
# AC_CHECK_HEADERS([ ])

# Determine PIC flag, adjust default CFLAGS
need_asm=false
PICFLAG=
AC_MSG_CHECKING(Determining PIC compiler flag)
if test "$GCC" = yes; then
  AC_MSG_RESULT(-fPIC)
  PICFLAG=-fPIC
  case "$host" in
    *-*-solaris*)
      # Workaround: at least GCC 3.4.6 does not define this macro.
      CFLAGS="$CFLAGS -D__PIC__=1"
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
esac
AC_SUBST(THREADDLLIBS)

AM_CONDITIONAL(NEED_ASM, test x$need_asm = xtrue)

AC_CONFIG_FILES([Makefile src/Makefile src/atomic_ops/Makefile \
    src/atomic_ops/sysdeps/Makefile doc/Makefile tests/Makefile])
AC_CONFIG_COMMANDS([default],[[]],[[
PICFLAG="${PICFLAG}"
CC="${CC}"
DEFS="${DEFS}"
]])
AC_OUTPUT
