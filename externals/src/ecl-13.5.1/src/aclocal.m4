dnl -*- autoconf -*-

dnl --------------------------------------------------------------
dnl check existence of long double and supporting functions
AC_DEFUN([ECL_LONG_DOUBLE],[
if test "$enable_longdouble" != "no" ; then
AC_CHECK_TYPES([long double],[enable_longdouble=yes],[enable_longdouble=no])
if test "$enable_longdouble" != "no" ; then
AC_CHECK_FUNCS([sinl cosl tanl logl expl],[],[enable_longdouble=no; break])
if test "$enable_longdouble" != "no" ; then
AC_DEFINE(ECL_LONG_FLOAT)
fi
fi
fi
])

dnl --------------------------------------------------------------
dnl http://autoconf-archive.cryp.to/ac_c_long_long_.html
dnl Provides a test for the existance of the long long int type and defines HAVE_LONG_LONG if it is found.
AC_DEFUN([ECL_LONG_LONG],
[AC_MSG_CHECKING(size of long long)
if test "x$ECL_LONG_LONG_BITS" = "xno"; then
  AC_MSG_RESULT(not available)
  ac_cv_c_long_long=no
  ECL_LONG_LONG_BITS=""
else
  if test "$GCC" = yes; then
    ac_cv_c_long_long=yes
  else
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,[[long long int i;]])],
    ac_cv_c_long_long=yes,
    ac_cv_c_long_long=no)
  fi
fi
if test $ac_cv_c_long_long = yes; then
  if test "x$ECL_LONG_LONG_BITS" = "x"; then
    AC_RUN_IFELSE([AC_LANG_SOURCE([[#include <stdio.h>
int main() {
  const char *int_type;
  int bits;
  unsigned long long x = 1;
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  for (bits = 0; x; bits++) {
    x <<= 1;
  }
  fprintf(f,"ECL_LONG_LONG_BITS='%d'",bits);
  exit(0);
}]])],[eval "`cat conftestval`"],[],[])
  fi
fi
if test "x$ECL_LONG_LONG_BITS" = "x"; then
  AC_MSG_RESULT(not available)
else
  AC_MSG_RESULT([$ECL_LONG_LONG_BITS])
  AC_DEFINE(ecl_long_long_t, long long, [compiler understands long long])
  AC_DEFINE(ecl_ulong_long_t, unsigned long long, [compiler understands long long])
  AC_DEFINE_UNQUOTED([ECL_LONG_LONG_BITS],[$ECL_LONG_LONG_BITS])
fi
])

dnl --------------------------------------------------------------
dnl Add *feature* for conditional compilation.
AC_DEFUN([ECL_ADD_FEATURE], [
LSP_FEATURES="(cons :$1 ${LSP_FEATURES})"
])

dnl --------------------------------------------------------------
dnl Add lisp module to compile; if second argument is given,
dnl compile module into Lisp library if we don't support shared
dnl libraries.
dnl
AC_DEFUN([ECL_ADD_LISP_MODULE], [
  ECL_ADD_FEATURE([wants-$1])
])

dnl --------------------------------------------------------------
dnl Add lisp module and build it into the compiler.
dnl
AC_DEFUN([ECL_ADD_BUILTIN_MODULE], [
  ECL_ADD_FEATURE([builtin-$1])
])

dnl --------------------------------------------------------------
dnl Set up a configuration file for the case when we are cross-
dnl compiling
dnl
AC_DEFUN(ECL_CROSS_CONFIG,[
if test "x${cross_compiling}" = "xyes"; then
  if test -n "${with_cross_config}" -a -f "${with_cross_config}"; then
    . ${with_cross_config}
  elif test -f ./cross_config; then
    . ./cross_config
  elif test -n "${srcdir}" -a -f ${srcdir}/cross_config; then
    . ${srcdir}/cross_config
  else
    test -z ${with_cross_config} && cross_config=`pwd`/cross_config
    cat > ${with_cross_config} <<EOF
###
### YOU ARE TRYING TO CROSS COMPILE ECL.
### PLEASE FOLLOW THESE INSTRUCTIONS:
###
### 1) Vital information cannot be determined at configuration time
### because we are not able to run test programs. A file called
###		${cross_config}
### has been created, that you will have to fill out. Please do
### it before invoking "configure" again.

### 1.1) Direction of growth of the stack
ECL_STACK_DIR=up

### 1.2) Choose an integer datatype which is large enough to host a pointer
CL_FIXNUM_TYPE=int
CL_FIXNUM_BITS=32
CL_FIXNUM_MAX=536870911L
CL_FIXNUM_MIN=-536870912L
CL_INT_BITS=32
CL_LONG_BITS=32

### 1.3) Order of bytes within a word
ECL_BIGENDIAN=no

### 1.4) What characters signal an end of line. May be LF (Linefeed or \\n)
###      CR (Carriage return or \\r), and CRLF (CR followed by LF).
ECL_NEWLINE=LF

### 1.5) Can we guess how many characters are available for reading from
###      the FILE structure?
###          0 = no
###          1 = (f)->_IO_read_end - (f)->_IO_read_ptr
###          2 = (f)->_r
###          3 = (f)->_cnt
ECL_FILE_CNT=0

###
### 1.6) Other integer types (set to 'no' to disable)
###
ECL_STDINT_HEADER="#include <stdint.h>"
ECL_UINT8_T=uint8_t
ECL_UINT16_T=uint16_t
ECL_UINT32_T=uint32_t
ECL_UINT64_T=no
ECL_INT8_T=int8_t
ECL_INT16_T=int16_t
ECL_INT32_T=int32_t
ECL_INT64_T=no
ECL_LONG_LONG_BITS=no

###
### 1.7) Other features (set to 'no' to disable)
###
ECL_WORKING_SEM_INIT=no
ECL_WORKING_ENVIRON=yes

### 2) To cross-compile ECL so that it runs on the system
###		${host}
### you need to first compile ECL on the system in which you are building
### the cross-compiled files, that is
###		${build}
### By default we assume that ECL can be accessed from some directory in
### the path.
ECL_TO_RUN=`which ecl`
EOF
    cat ${with_cross_config}
    AC_MSG_ERROR(Configuration aborted)
  fi
  if test "${ECL_TO_RUN}" = "failed"; then
    AC_MSG_ERROR(The program ECL is not installed in your system)
  fi
  ECL_MIN_TO_RUN=`${ECL_TO_RUN} -norc -eval '(progn (print (truename "sys:ecl_min")) (si:quit))' \
	| grep '\#\P' | sed 's,#P"\(.*\)",\1,'`
  if test -z "${ECL_MIN_TO_RUN}" -o "${ECL_MIN_TO_RUN}" = "failed"  ; then
    AC_MSG_ERROR(The program ECL-MIN is not installed in your system)
  fi
  DPP_TO_RUN=`${ECL_TO_RUN} -norc -eval '(progn (print (truename "sys:dpp")) (si:quit))' \
	| grep '\#\P' | sed 's,#P"\(.*\)",\1,'`
  if test -z "${DPP_TO_RUN}" -o "${DPP_TO_RUN}" = "failed"  ; then
    AC_MSG_ERROR(The program DPP is not installed in your system)
  fi
  dnl (echo '#!/bin/sh'; echo exec ${ECL_TO_RUN} -eval "'"'(push :cross *features*)'"'" '$''*') > CROSS-COMPILER
  (echo '#!/bin/sh'; echo exec ${ECL_MIN_TO_RUN} '$''*') > CROSS-COMPILER
  (echo '#!/bin/sh'; echo exec ${DPP_TO_RUN} '$''*') > CROSS-DPP
  chmod +x CROSS-COMPILER CROSS-DPP
  ECL_ADD_FEATURE([cross])
fi
])

dnl --------------------------------------------------------------
dnl Make srcdir absolute, if it isn't already.  It's important to
dnl avoid running the path through pwd unnecessarily, since pwd can
dnl give you automounter prefixes, which can go away.
dnl
AC_DEFUN(ECL_MAKE_ABSOLUTE_SRCDIR,[
AC_SUBST(true_srcdir)
AC_SUBST(true_builddir)
PWDCMD="pwd";
case "${srcdir}" in
  /* | ?:/* ) ;;
  *  ) srcdir="`(cd ${srcdir}; ${PWDCMD})`";
esac
if uname -a | grep -i 'mingw32' > /dev/null; then
  true_srcdir=`(cd ${srcdir}; pwd -W)`
  true_builddir=`pwd -W`
else
  true_srcdir=`(cd ${srcdir}; pwd)`
  true_builddir=`pwd`
fi
])

dnl
dnl --------------------------------------------------------------
dnl Define a name for this operating system and set some defaults
dnl
AC_DEFUN(ECL_GUESS_HOST_OS,[
#### Some command variations:
AC_SUBST(CP)
AC_SUBST(RM)
AC_SUBST(MV)
AC_SUBST(EXE_SUFFIX)
RM="rm -f"
CP="cp"
MV="mv"

### Guess the operating system
AC_SUBST(ARCHITECTURE)dnl	Type of processor for which this is compiled
AC_SUBST(SOFTWARE_TYPE)dnl	Type of operating system
AC_SUBST(SOFTWARE_VERSION)dnl	Version number of operating system
AC_SUBST(MACHINE_VERSION)dnl	Version of the machine

AC_SUBST(ECL_LDRPATH)dnl	Sometimes the path for finding DLLs must be hardcoded.
AC_SUBST(LIBPREFIX)dnl	Name components of a statically linked library
AC_SUBST(LIBEXT)
AC_SUBST(SHAREDEXT)dnl	Name components of a dynamically linked library
AC_SUBST(SHAREDPREFIX)
AC_SUBST(OBJEXT)dnl	These are set by autoconf
AC_SUBST(EXEEXT)
AC_SUBST(INSTALL_TARGET)dnl Which type of installation: flat directory or unix like.
AC_SUBST(thehost)
AC_SUBST(ECL_GC_DIR)dnl Which version of the Boehm-Weiser library to use
ECL_GC_DIR=gc-unstable
ECL_LDRPATH=''
SHAREDEXT='so'
SHAREDPREFIX='lib'
LIBPREFIX='lib'
LIBEXT='a'
PICFLAG='-fPIC'
THREAD_CFLAGS=''
THREAD_LIBS=''
THREAD_GC_FLAGS='--enable-threads=posix'
INSTALL_TARGET='install'
THREAD_OBJ="$THREAD_OBJ threads/process threads/queue threads/mutex threads/condition_variable threads/semaphore threads/barrier threads/mailbox"
clibs=''
SONAME=''
SONAME_LDFLAGS=''
case "${host_os}" in
	# libdir may have a dollar expression inside
	linux*)
		thehost='linux'
		THREAD_CFLAGS='-D_THREAD_SAFE'
		THREAD_LIBS='-lpthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		ECL_LDRPATH='-Wl,--rpath,~A'
		clibs="-ldl"
		# Maybe CFLAGS="-D_ISOC99_SOURCE ${CFLAGS}" ???
		CFLAGS="-D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 ${CFLAGS}"
		SONAME="${SHAREDPREFIX}ecl.${SHAREDEXT}.SOVERSION"
		SONAME_LDFLAGS="-Wl,-soname,SONAME"
		;;
	gnu*)
		thehost='gnu'
		THREAD_CFLAGS='-D_THREAD_SAFE'
		THREAD_LIBS='-lpthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		ECL_LDRPATH='-Wl,--rpath,~A'
		clibs="-ldl"
		CFLAGS="-D_GNU_SOURCE ${CFLAGS}"
		SONAME="${SHAREDPREFIX}ecl.${SHAREDEXT}.SOVERSION"
		SONAME_LDFLAGS="-Wl,-soname,SONAME"
		;;
	kfreebsd*-gnu)
		thehost='kfreebsd'
		THREAD_CFLAGS='-D_THREAD_SAFE'
		THREAD_LIBS='-lpthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		ECL_LDRPATH='-Wl,--rpath,~A'
		clibs="-ldl"
		CFLAGS="-D_GNU_SOURCE ${CFLAGS}"
		SONAME="${SHAREDPREFIX}ecl.${SHAREDEXT}.SOVERSION"
		SONAME_LDFLAGS="-Wl,-soname,SONAME"
		;;
	dragonfly*)
		thehost='dragonfly'
		THREAD_LIBS='-lpthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		ECL_LDRPATH="-Wl,--rpath,~A"
		clibs=""
		SONAME="${SHAREDPREFIX}ecl.${SHAREDEXT}.SOVERSION"
		SONAME_LDFLAGS="-Wl,-soname,SONAME"
		;;
	freebsd*)
		thehost='freebsd'
		THREAD_LIBS='-lpthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		ECL_LDRPATH="-Wl,--rpath,~A"
		clibs=""
		SONAME="${SHAREDPREFIX}ecl.${SHAREDEXT}.SOVERSION"
		SONAME_LDFLAGS="-Wl,-soname,SONAME"
		;;
	netbsd*)
		thehost='netbsd'
		THREAD_LIBS='-lpthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		ECL_LDRPATH="-Wl,--rpath,~A"
		clibs=""
		SONAME="${SHAREDPREFIX}ecl.${SHAREDEXT}.SOVERSION"
		SONAME_LDFLAGS="-Wl,-soname,SONAME"
		;;
	openbsd*)
		thehost='openbsd'
		THREAD_CFLAGS=''
		THREAD_LIBS=''
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		ECL_LDRPATH="-Wl,--rpath,~A"
		clibs="-lpthread -lm"
		SONAME="${SHAREDPREFIX}ecl.${SHAREDEXT}.SOVERSION"
		SONAME_LDFLAGS="-Wl,-soname,SONAME"
		;;
	solaris*)
		thehost='sun4sol2'
		THREAD_LIBS='-lrt -lpthread'
		SHARED_LDFLAGS="-dy -G ${LDFLAGS}"
		BUNDLE_LDFLAGS="-dy -G ${LDFLAGS}"
		ECL_LDRPATH='-Wl,-R,~A'
		TCPLIBS='-lsocket -lnsl -lintl'
		clibs='-ldl'
		# We should use C99 and _XOPEN_SOURCE=600, but Solaris 10
		# ships with GCC 3.4.3 which does not support C99
                if test "x$GCC" = "xyes"; then
                  CFLAGS="${CFLAGS} -std=gnu99"
                  SHARED_LDFLAGS="-shared $SHARED_LDFLAGS"
                  BUNDLE_LDFLAGS="-shared $BUNDLE_LDFLAGS"
                fi
		;;
	cygwin*)
		enable_threads='no'
		thehost='cygwin'
		shared='yes'
		THREAD_CFLAGS='-D_THREAD_SAFE'
		THREAD_LIBS='-lpthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		SHAREDPREFIX=''
		SHAREDEXT='dll'
		PICFLAG=''
		;;
	mingw*)
		thehost='mingw32'
		clibs=''
		shared='yes'
                enable_threads='yes'
		THREAD_CFLAGS='-D_THREAD_SAFE'
		THREAD_GC_FLAGS='--enable-threads=win32'
		SHARED_LDFLAGS=''
		BUNDLE_LDFLAGS=''
		SHAREDPREFIX=''
		SHAREDEXT='dll'
		PICFLAG=''
		INSTALL_TARGET='flatinstall'
		TCPLIBS='-lws2_32'
		;;
	darwin*)
		thehost='darwin'
		shared='yes'
		SHAREDEXT='dylib'
		PICFLAG='-fPIC -fno-common'
		SHARED_LDFLAGS="-dynamiclib -flat_namespace -undefined suppress ${LDFLAGS}"
		BUNDLE_LDFLAGS="-bundle ${LDFLAGS}"
		ECL_LDRPATH=''
		THREAD_CFLAGS='-D_THREAD_SAFE'
		THREAD_LIBS='-lpthread'
		# The GMP library has not yet been ported to Intel-OSX
		case "`uname -m`" in
		i386*|x86_64) gmp_build=none-apple-${host_os};;
		*) ABI=32;;
		esac
                if test "x$ABI" = "x64"; then
                  if echo "$CFLAGS" | grep -v '[ ]*-m64' >/dev/null ; then
                     CFLAGS="-m64 $CFLAGS"
                     LDFLAGS="-m64 $LDFLAGS"
                  fi
                  # Needed when building the old version of GMP.
                  # Should be removed when moving to MPIR
                  ABI="long"
                fi
                if test "x$ABI" = "x32"; then
                  ABI="long"
                fi
                # The Boehm-Weiser GC library shipped with Fink does not work
                # well with our signal handler.
		# enable_boehm=included
                if test `uname -r | cut -d '.' -f 1` -ge 11; then
                  ECL_GC_DIR=gc-unstable
                fi
		SONAME="${SHAREDPREFIX}ecl.SOVERSION.${SHAREDEXT}"
		SONAME_LDFLAGS="-Wl,-install_name,@libdir\@/SONAME -Wl,-compatibility_version,${PACKAGE_VERSION}"
		;;
	nsk*)
		# HP Non-Stop platform
		thehost='nonstop'
		shared='yes'
		PICFLAG='-call_shared'
		THREAD_CFLAGS='-spthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		ECL_LDRPATH='-Wld=\"-rld_l ~A\"'
		clibs="-Wld=-lrld"
		;;
	*)
		thehost="$host_os"
		shared="no"
		;;
esac
case "${host_cpu}" in
	alpha*)
		CFLAGS="${CFLAGS} -mieee";;
esac
ECL_CFLAGS="-D${thehost}"
AC_MSG_CHECKING(for ld flags when building shared libraries)
if test "${enable_shared}" = "yes"; then
AC_MSG_RESULT([${SHARED_LDFLAGS}])
CFLAGS="${CFLAGS} ${PICFLAG}"
else
shared="no";
AC_MSG_RESULT(cannot build)
fi
LIBS="${clibs} ${LIBS}"
AC_MSG_CHECKING(for required libraries)
AC_MSG_RESULT([${clibs}])
AC_MSG_CHECKING(for architecture)
ARCHITECTURE=`echo "${host_cpu}" | tr a-z A-Z` # i386 -> I386
AC_MSG_RESULT([${ARCHITECTURE}])
AC_MSG_CHECKING(for software type)
SOFTWARE_TYPE="$thehost"
SOFTWARE_VERSION=""
AC_MSG_RESULT([${SOFTWARE_TYPE} / ${SOFTWARE_VERSION}])
])

dnl
dnl --------------------------------------------------------------
dnl Check whether the FILE structure has a field with the number of
dnl characters left in the buffer.
dnl
AC_DEFUN(ECL_FILE_STRUCTURE,[
AC_SUBST(ECL_FILE_CNT)
if test -z "${ECL_FILE_CNT}"; then
ECL_FILE_CNT=0
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>]], [[
  FILE *f = fopen("conftestval","w");
  if ((f)->_IO_read_end - (f)->_IO_read_ptr)
    return 1;
]])],[ECL_FILE_CNT=1],[])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>]], [[
  FILE *f = fopen("conftestval","w");
  if ((f)->_r)
    return 1;
]])],[ECL_FILE_CNT=2],[])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>]], [[
  FILE *f = fopen("conftestval","w");
  if ((f)->_cnt)
    return 1;
]])],[ECL_FILE_CNT=3],[])
fi
])

dnl ---------------------------------------------------------------------
dnl Check availability of standard sized integer types of a given width.  
dnl On success, define the global variables ECL_INTx_T and ECL_UNITx_T to 
dnl hold the names of the corresponding standard C integer types.
AC_DEFUN(ECL_CHECK_SIZED_INTEGER_TYPE,[
AC_TYPE_INT$1_T
AC_TYPE_UINT$1_T
if test "x$ac_cv_c_int$1_t" = xyes; then
  eval ECL_INT$1_T="int$1_t"
  eval ECL_UINT$1_T="uint$1_t"
  AC_DEFINE_UNQUOTED([ecl_int$1_t],[int$1_t])
  AC_DEFINE_UNQUOTED([ecl_uint$1_t],[uint$1_t])
fi])

dnl
dnl --------------------------------------------------------------
dnl Check the existence of different integer types and that they
dnl have the right size;
dnl
AC_DEFUN(ECL_INTEGER_TYPES,[
AC_SUBST(ECL_STDINT_HEADER)
AC_CHECK_HEADER([stdint.h],[AC_DEFINE(HAVE_STDINT_H)
ECL_STDINT_HEADER="#include <stdint.h>"],[])
if test -z "${ECL_STDINT_HEADER}"; then
AC_CHECK_HEADER([inttypes.h],[AC_DEFINE(HAVE_INTTYPES_H)
ECL_STDINT_HEADER="#include <inttypes.h>"],[])
fi

ECL_CHECK_SIZED_INTEGER_TYPE(8)
ECL_CHECK_SIZED_INTEGER_TYPE(16)
ECL_CHECK_SIZED_INTEGER_TYPE(32)
ECL_CHECK_SIZED_INTEGER_TYPE(64)

if test "${ECL_UINT32_T}${CL_FIXNUM_BITS}" = "32"; then
  ECL_UINT32_T="cl_index"
  ECL_INT32_T="cl_fixnum"
fi
if test "${ECL_UINT64_T}${CL_FIXNUM_BITS}" = "64"; then
  ECL_UINT64_T="cl_index"
  ECL_INT64_T="cl_fixnum"
fi
if test "${ECL_UINT16_T}${CL_FIXNUM_BITS}" = "16"; then
  ECL_UINT16_T=$ECL_UINT32_T
  ECL_INT16_T=$ECL_INT32_T
fi

if test "x${ECL_UINT8_T}" = "x" -o "x${ECL_UINT8_T}" = xno; then
  AC_MSG_ERROR(Can not build ECL without byte types)
fi
])
dnl
dnl --------------------------------------------------------------
dnl Check the direction to which the stack grows (for garbage
dnl collection).
dnl
AC_DEFUN(ECL_STACK_DIRECTION,[
  AC_MSG_CHECKING(whether stack growns downwards)
if test -z "${ECL_STACK_DIR}" ; then
  AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>

char *f2() {
  char c[2];
  return c;
}

char *f1() {
  char c[2];
  return c+1;
}

typedef char *(*f_ptr)();
f_ptr f[2] = { f1, f2 };

int signo() {
  char d[1];
  return f[rand() & 1]() - d;
}

int main() {
  if (signo() > 0)
    return 1;
  else
    return 0;
}
]])],[ECL_STACK_DIR=down],[ECL_STACK_DIR=up],[])
fi
case "${ECL_STACK_DIR}" in
  down|DOWN) AC_MSG_RESULT(yes); AC_DEFINE(ECL_DOWN_STACK, [1], [Stack grows downwards]) ;;
  up|UP) AC_MSG_RESULT(no) ;;
  *) AC_MSG_ERROR(Unable to determine stack growth direction)
esac])
dnl
dnl ------------------------------------------------------------
dnl Find out a setjmp() that does not save signals. It is called
dnl in several architectures.
AC_DEFUN(ECL_FIND_SETJMP,[
AC_SUBST(ECL_SETJMP)
AC_SUBST(ECL_LONGJMP)
AC_CHECK_FUNC(_longjmp,
ECL_SETJMP="_setjmp";ECL_LONGJMP="_longjmp",
ECL_SETJMP="setjmp";ECL_LONGJMP="longjmp")])

dnl
dnl --------------------------------------------------------------
dnl Guess the right type and size for cl_fixnum. It must be large
dnl enough that convertion back and forth to pointer implies no
dnl loss of information.
AC_DEFUN(ECL_FIXNUM_TYPE,[
AC_SUBST(CL_FIXNUM_TYPE)
AC_SUBST(CL_FIXNUM_BITS)
AC_SUBST(CL_FIXNUM_MAX)
AC_SUBST(CL_FIXNUM_MIN)
AC_SUBST(CL_INT_BITS)
AC_SUBST(CL_LONG_BITS)
AC_MSG_CHECKING(appropiate type for fixnums)
if test -z "${CL_FIXNUM_TYPE}" ; then
  AC_RUN_IFELSE([AC_LANG_SOURCE([[#include <stdio.h>
int main() {
  const char *int_type;
  int bits;
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  if (sizeof(int) >= sizeof(void*)) {
    unsigned int t = 1;
    signed int l = 0;
    int_type="int";
    for (bits=1; ((t << 1) >> 1) == t; bits++, t <<= 1);
    l = (~l) << (bits - 3);
#if 1
    fprintf(f,"CL_FIXNUM_MIN='%d';",l);
    fprintf(f,"CL_FIXNUM_MAX='%d';",-(l+1));
#else
    l++;
    fprintf(f,"CL_FIXNUM_MIN='%d';",l);
    fprintf(f,"CL_FIXNUM_MAX='%d';",-l);
#endif
  } else if (sizeof(long) >= sizeof(void*)) {
    unsigned long int t = 1;
    signed long int l = 0;
    int_type="long int";
    for (bits=1; ((t << 1) >> 1) == t; bits++, t <<= 1);
    l = (~l) << (bits - 3);
#if 1
    fprintf(f,"CL_FIXNUM_MIN='%ldL';",l);
    fprintf(f,"CL_FIXNUM_MAX='%ldL';",-(l+1));
#else
    l++;
    fprintf(f,"CL_FIXNUM_MIN='%ldL';",l);
    fprintf(f,"CL_FIXNUM_MAX='%ldL';",-l);
#endif
#ifdef ECL_LONG_LONG_BITS
  } else if (sizeof(long long) >= sizeof(void*)) {
    unsigned long long int t = 1;
    signed long long int l = 0;
    int_type="long long";
    for (bits=1; ((t << 1) >> 1) == t; bits++, t <<= 1);
    l = (~l) << (bits - 3);
# if 1
    fprintf(f,"CL_FIXNUM_MIN='%lldLL';",l);
    fprintf(f,"CL_FIXNUM_MAX='%lldLL';",-(l+1));
# else
    l++;
    fprintf(f,"CL_FIXNUM_MIN='%lldLL';",l);
    fprintf(f,"CL_FIXNUM_MAX='%lldLL';",-l);
# endif
#endif
  } else {
    exit(1);
  }
  fprintf(f,"CL_FIXNUM_TYPE='%s';",int_type);
  fprintf(f,"CL_FIXNUM_BITS='%d';",bits);
  {
    unsigned int x = 1;
    for (bits = 0; x; bits++) {
      x <<= 1;
    }
    fprintf(f,"CL_INT_BITS='%d';",bits);
  }
  {
    unsigned long x = 1;
    for (bits = 0; x; bits++) {
      x <<= 1;
    }
    fprintf(f,"CL_LONG_BITS='%d'",bits);
  }
  exit(0);
}]])],[eval "`cat conftestval`"],[],[])
fi
if test -z "${CL_FIXNUM_TYPE}" ; then
AC_MSG_ERROR(There is no appropiate integer type for the cl_fixnum type)
fi
AC_MSG_RESULT([${CL_FIXNUM_TYPE}])])

dnl
dnl ------------------------------------------------------------
dnl Find out what is written for every '\n' character, when
dnl opening a text file.
dnl
AC_DEFUN(ECL_LINEFEED_MODE,[
AC_MSG_CHECKING(character sequence for end of line)
if test -z "${ECL_NEWLINE}" ; then
AC_RUN_IFELSE([AC_LANG_SOURCE([[#include <stdio.h>
int main() {
  FILE *f = fopen("conftestval","w");
  int c1, c2;
  char *output;
  if (f == NULL) exit(1);
  fprintf(f, "\n");
  fclose(f);
  f = fopen("conftestval","rb");
  if (f == NULL) exit(1);
  c1 = fgetc(f);
  c2 = fgetc(f);
  fclose(f);
  f = fopen("conftestval","w");
  if (f == NULL) exit(1);
  if (c1 == '\r')
    if (c2 == EOF)
      output="CR";
    else
      output="CRLF";
  else
    output="LF";
  fclose(f);
  f = fopen("conftestval","w");
  if (f == NULL) exit(1);
  fprintf(f, output);
  fclose(f);
  exit(0);
}
]])],[ECL_NEWLINE=`cat conftestval`],[],[])
fi
case "${ECL_NEWLINE}" in
  LF) AC_MSG_RESULT(lf) ;;
  CR) AC_MSG_RESULT(cr); AC_DEFINE(ECL_NEWLINE_IS_CR, [1], [Define if your newline is CR]) ;;
  CRLF) AC_MSG_RESULT(cr+lf); AC_DEFINE(ECL_NEWLINE_IS_CRLF, [1], [Define if your newline is CRLF]) ;;
  *) AC_MSG_ERROR(Unable to determine linefeed mode) ;;
esac
])

dnl
dnl ------------------------------------------------------------
dnl Find out which program we can use to install INFO files
dnl
AC_DEFUN(ECL_INSTALL_INFO,[
AC_SUBST(INSTALL_INFO)
AC_PATH_PROG(INSTALL_INFO, install-info, [/sbin/install-info],
[$PATH:/usr/bin:/usr/sbin:/usr/etc:/usr/libexec])
])

dnl
dnl ------------------------------------------------------------
dnl Use the configuration scripts in the GMP library for
dnl configuring ECL in a compatible way.
dnl
AC_DEFUN(ECL_GMP_BASED_CONFIG,[
AC_MSG_CHECKING([Using the GMP library to guess good compiler/linker flags])
if test ! -f gmp/config.status; then
  AC_MSG_ERROR([Cannot find GMP's configuration file. Aborting])
fi
GMP_CFLAGS=`grep '^s,@CFLAGS@' gmp/config.status| sed 's&s,@CFLAGS@,\(.*\),;t t&\1&'`
GMP_LDFLAGS=`grep '^s,@GMP_LDFLAGS@' gmp/config.status| sed 's&s,@GMP_LDFLAGS@,\(.*\),;t t&\1&'`;
# Notice that GMP_LDFLAGS is designed to be passed to libtool, and therefore
# some options could be prefixed by -Wc, which means "flag for the compiler".
LDFLAGS=`echo ${LDFLAGS} ${GMP_LDFLAGS} | sed 's%-Wc,%%g'`
CFLAGS=`echo ${CFLAGS} ${GMP_CFLAGS} | sed 's%-Wc,%%g'`
GMP_CFLAGS=""
GMP_LDFLAGS=""
#host=`grep '^s,@host@' config.status | sed 's&s,@host@,\(.*\),;t t&\1&'`
AC_MSG_CHECKING([C/C++ compiler flags])
AC_MSG_RESULT([${CFLAGS}])
AC_MSG_CHECKING([Linker flags])
AC_MSG_RESULT([${LDFLAGS}])
])

dnl --------------------------------------------------------------
dnl Provides a test for the existance of the __thread declaration and
dnl defines WITH___THREAD if it is found
AC_DEFUN([ECL___THREAD],[
AC_CACHE_CHECK(for __thread local data, ac_cv_ecl___thread,
AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,[[static __thread void *data;]])],
   ac_cv_ecl___thread=yes,
   ac_cv_ecl___thread=no))
dnl We deactivate this test because it seems to slow down ECL A LOT!!!
])

dnl --------------------------------------------------------------
dnl Determine whether GCC supports backtraces
dnl
AC_DEFUN([ECL_GCC_BACKTRACE],[
if test "x${cross_compiling}" != "xyes"; then
AC_RUN_IFELSE(
  [AC_LANG_SOURCE([[
    void *foo() { return __builtin_return_address(1); }
    int main() {
      return (foo() == 0);
    }]])],
  [AC_DEFINE(HAVE___BUILTIN_RETURN_ADDRESS)],
  [])
fi
])

dnl ----------------------------------------------------------------------
dnl Choose the type of code to detect floating point exceptions and
dnl raise them.
dnl
AC_DEFUN([ECL_FPE_MODEL],
[AC_MSG_CHECKING([for code to detect FP exceptions])
case "${host_cpu}" in
   i686 |i586 | pentium* | athlon* )
	ECL_FPE_CODE="arch/fpe_x86.c"
	AC_MSG_RESULT([x86])
	;;
   x86_64* )
	ECL_FPE_CODE="arch/fpe_x86.c"
	AC_MSG_RESULT([x86_64])
	;;
   *)
        ECL_FPE_CODE="arch/fpe_none.c"
	AC_MSG_RESULT([not available])
	;;
esac
AC_SUBST(ECL_FPE_CODE)
])

dnl ----------------------------------------------------------------------
dnl Decide whether ECL should export SSE intrinsics
dnl
AC_DEFUN([ECL_SSE],[
if test "x$with_sse" = xyes; then
 AC_MSG_CHECKING([for SSE intrinsics])
 AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <xmmintrin.h>
#include <emmintrin.h>
]],[[__m128 value;
_mm_getcsr();]])],[sse_included=yes],[sse_included=no])
 if test "$sse_included" = "no"; then
  OLD_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -msse2"
  AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <xmmintrin.h>
#include <emmintrin.h>
]],[[__m128 value;
_mm_getcsr();]])],[sse_included=yes],[sse_included=no])
  if test "$sse_included" = "no"; then
   CFLAGS="$OLD_CFLAGS"
   with_sse=no
  fi
 fi
 if test "x$with_sse" = xyes; then
  AC_DEFINE(ECL_SSE2)
  AC_MSG_RESULT([yes])
 else
  AC_MSG_RESULT([no])
 fi
fi
])

dnl ----------------------------------------------------------------------
dnl Check whether we have POSIX read/write locks are available
AC_DEFUN([ECL_POSIX_RWLOCK],[
AC_CHECK_FUNC( [pthread_rwlock_init], [
  AC_DEFINE(ECL_RWLOCK)
  AC_DEFINE(HAVE_POSIX_RWLOCK)
], [])
THREAD_OBJ="$THREAD_OBJ threads/rwlock"
])


dnl ----------------------------------------------------------------------
dnl Check "char **environ" is available
AC_DEFUN([ECL_POSIX_ENVIRON],[
AC_MSG_CHECKING(working environ)
if test -z "$ECL_WORKING_ENVIRON"; then
  AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
extern char **environ;
int main() {
  if (environ)
    exit(0);
  exit(1);
}]])],[ECL_WORKING_ENVIRON=yes],[ECL_WORKING_ENVIRON=no],[])
fi
AC_MSG_RESULT([$ECL_WORKING_ENVIRON])
if test $ECL_WORKING_ENVIRON = yes ; then
  AC_DEFINE(HAVE_ENVIRON)
fi
])

dnl ----------------------------------------------------------------------
dnl Configure libatomic-ops
dnl
AC_DEFUN([ECL_LIBATOMIC_OPS],[
case "${enable_libatomic}" in
  auto|system|included) ;;
  *) AC_MSG_ERROR( [Invalid value of --enable-libatomic: ${enable_libatomic}] );;
esac
if test "x${enable_threads}" != "xno"; then
  AC_CHECK_HEADER([atomic_ops.h],[system_libatomic=yes],[system_libatomic=no],[])
  AC_MSG_CHECKING( [libatomic-ops version] )
  if test "${enable_libatomic}" = auto; then
    if test "${system_libatomic}" = yes; then
      enable_libatomic=system
    else
      enable_libatomic=included
    fi
  fi
  if test "${enable_libatomic}" = system; then
    if test "${system_libatomic}" = no; then
      AC_MSG_ERROR( [Cannot find libatomic-ops in the system] )
      enable_libatomic=included
    fi
  fi
  AC_MSG_RESULT( [${enable_libatomic}] )
  if test "${enable_libatomic}" = included; then
    test -d atomic || mkdir atomic
    (destdir=`${PWDCMD}`; cd atomic && CC="${CC} ${PICFLAG}" \
     $srcdir/${ECL_GC_DIR}/libatomic*/configure --disable-shared --prefix=${destdir} \
	--infodir=${destdir}/doc --includedir=${destdir}/ecl --with-pic \
        --libdir=${destdir} --build=${build_alias} --host=${host_alias} \
        CFLAGS="$CFLAGS" LDFLAGS="$LDFLAGS" CPPFLAGS="$CPPFLAGS" CC="${CC} \
        ${PICFLAG}")
    SUBDIRS="${SUBDIRS} atomic"
    if test "${enable_shared}" = "no"; then
      LIBRARIES="${LIBRARIES} ${LIBPREFIX}eclatomic.${LIBEXT}"
    fi
    AC_DEFINE(ECL_LIBATOMIC_OPS_H)
    CORE_LIBS="-leclatomic ${CORE_LIBS}"
  else
    CORE_LIBS="-latomic_ops ${CORE_LIBS}"
  fi
fi
])

dnl ----------------------------------------------------------------------
dnl Configure included Boehm GC if needed
AC_DEFUN([ECL_BOEHM_GC],[
AC_SUBST(ECL_BOEHM_GC_HEADER)
case "${enable_boehm}" in
  yes) enable_boehm=auto;;
  no|auto|system|included) ;;
  *) AC_MSG_ERROR( [Invalid value of --enable-boehm: ${enable_boehm}] );;
esac
if test "${enable_boehm}" = auto -o "${enable_boehm}" = system; then
 dnl
 dnl Try first with the prebuilt versions, if installed and accessible
 dnl
 if test "${enable_threads}" = no; then
   AC_CHECK_LIB( [gc], [GC_malloc],
                 [system_boehm="yes"], [system_boehm="no"] )
 else
   AC_CHECK_LIB( [gc], [GC_register_my_thread],
                 [system_boehm="yes"], [system_boehm="no"] )
 fi
 if test "${system_boehm}" = yes; then
   AC_CHECK_HEADER([gc.h],[ECL_BOEHM_GC_HEADER='gc.h'],[],[])
   if test -z "$ECL_BOEHM_GC_HEADER"; then
     AC_CHECK_HEADER([gc/gc.h],[ECL_BOEHM_GC_HEADER='gc/gc.h'],[system_boehm=no],[])
   fi
 fi
 AC_MSG_CHECKING( [whether we can use the existing Boehm-Weiser library] )
 AC_MSG_RESULT( [${system_boehm}] )
 if test "${system_boehm}" = "no"; then
   if test "${enable_boehm}" = "auto"; then
     enable_boehm="included";
   else
     AC_MSG_ERROR([System Boehm GC library requested but not found.])
   fi
 else
   FASL_LIBS="${FASL_LIBS} -lgc"
   EXTRA_OBJS="${EXTRA_OBJS} alloc_2.${OBJEXT}"
   AC_DEFINE(GBC_BOEHM, [1], [Use Boehm's garbage collector])
 fi
fi
if test "${enable_boehm}" = "included"; then
 dnl
 dnl Try here with the version shipped with ECL. Note that we have to use
 dnl the same compiler flags and that we will not export this library: it
 dnl is installed in the build directory.
 dnl
 AC_MSG_NOTICE([Configuring included Boehm GC library:])
 test -d gc && rm -rf gc
 if mkdir gc; then
   if (destdir=`${PWDCMD}`; cd gc; \
       $srcdir/${ECL_GC_DIR}/configure --disable-shared --prefix=${destdir} \
	 --includedir=${destdir}/ecl/ --libdir=${destdir} --build=${build_alias} \
	 --host=${host_alias} --enable-large-config \
         CC="${CC} ${PICFLAG}" CFLAGS="$CFLAGS" \
	 LDFLAGS="$LDFLAGS" CPPFLAGS="$CPPFLAGS" \
         ${boehm_configure_flags}); then
     ECL_BOEHM_GC_HEADER='ecl/gc/gc.h'
     SUBDIRS="${SUBDIRS} gc"
     CORE_LIBS="-leclgc ${CORE_LIBS}"
     EXTRA_OBJS="${EXTRA_OBJS} alloc_2.${OBJEXT}"
     if test "${enable_shared}" = "no"; then
       LIBRARIES="${LIBRARIES} ${LIBPREFIX}eclgc.${LIBEXT}"
     fi
     AC_DEFINE(GBC_BOEHM, [0], [Use Boehm's garbage collector])
   fi
 fi
 if test -z "${ECL_BOEHM_GC_HEADER}"; then
   AC_MSG_ERROR([Unable to configure Boehm-Weiser GC])
 fi
fi
if test "${enable_gengc}" != "no" ; then
  AC_DEFINE(GBC_BOEHM_GENGC)
fi
AC_MSG_CHECKING([if we use Boehm-Demers-Weiser precise garbage collector]);
if test "${enable_precisegc}" != "no" ; then
  AC_DEFINE(GBC_BOEHM_PRECISE)
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi
])

dnl ----------------------------------------------------------------------
dnl Configure included Boehm GC if needed
AC_DEFUN([ECL_LIBFFI],[
AC_SUBST(ECL_LIBFFI_HEADER)
case "${enable_libffi}" in
  yes) enable_libffi=auto;;
  no|auto|system|included) ;;
  *) AC_MSG_ERROR( [Invalid value of --enable-dffi: ${enable_libffi}] );;
esac
if test "${enable_libffi}" = auto -o "${enable_libffi}" = system; then
 dnl
 dnl Try first with the prebuilt versions, if installed and accessible
 dnl
 AC_CHECK_LIB( ffi, ffi_closure_alloc, [system_libffi=yes], [system_libffi=no] )
 if test "${system_libffi}" = yes; then
   AC_CHECK_HEADER([ffi/ffi.h],[ECL_LIBFFI_HEADER='ffi/ffi.h'],[],[])
   if test -z "$ECL_LIBFFI_HEADER"; then
     AC_CHECK_HEADER([ffi.h],[ECL_LIBFFI_HEADER='ffi.h'],[system_libffi=no],[])
   fi
 fi
 AC_MSG_CHECKING( [whether we can use the existing libffi library] )
 AC_MSG_RESULT( [${system_libffi}] )
 if test "${system_libffi}" = "no"; then
   if test "${enable_libffi}" = "auto"; then
     enable_libffi="included";
   else
     AC_MSG_ERROR([System libffi library requested but not found.])
   fi
 else
   FASL_LIBS="${FASL_LIBS} -lffi"
 fi
fi
if test "${enable_libffi}" = "included"; then
 dnl
 dnl Try here with the version shipped with ECL. Note that we have to use
 dnl the same compiler flags and that we will not export this library: it
 dnl is installed in the build directory.
 dnl
 AC_MSG_NOTICE([Configuring included libffi library:])
 test -d libffi && rm -rf libffi
 if mkdir libffi; then
   if (destdir=`${PWDCMD}`; cd libffi; \
       $srcdir/libffi/configure --disable-shared --prefix=${destdir} \
	 --includedir=${destdir}/ecl/ --libdir=${destdir} --build=${build_alias} \
	 --host=${host_alias} \
         CC="${CC} ${PICFLAG}" CFLAGS="$CFLAGS" \
	 LDFLAGS="$LDFLAGS" CPPFLAGS="$CPPFLAGS"); then
     ECL_LIBFFI_HEADER='ecl/ffi.h'
     SUBDIRS="${SUBDIRS} libffi"
     CORE_LIBS="-leclffi ${CORE_LIBS}"
     EXTRA_OBJS="${EXTRA_OBJS} alloc_2.${OBJEXT}"
     if test "${enable_shared}" = "no"; then
       LIBRARIES="${LIBRARIES} ${LIBPREFIX}eclffi.${LIBEXT}"
     fi
   fi
 fi
fi
if test -z "${ECL_LIBFFI_HEADER}"; then
  AC_MSG_WARN([Unable to configure or find libffi library; disabling dynamic FFI])
else
  AC_DEFINE(HAVE_LIBFFI)
fi
])

