#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.57)
AC_INIT([OTF],
	[m4_normalize(esyscmd([config/otf_get_version.sh VERSION --full]))],
	[andreas.knuepfer@tu-dresden.de], [OTF])
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config/m4)
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_HEADERS([config.h])

# If Automake supports silent rules, enable them.
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# Check whether we are inside Open MPI package
inside_openmpi="no"
AC_ARG_WITH(openmpi-inside, [],
[
	AS_IF([test x"$withval" != "xno"],
	[
		inside_openmpi="yes"
		CPPFLAGS="-DINSIDE_OPENMPI $CPPFLAGS"
	])
])

# Correct install paths
AS_IF([test "$inside_openmpi" = "yes" -o "$includedir" = "\${prefix}/include"],
[includedir="$includedir/open-trace-format"])
AS_IF([test "$inside_openmpi" = "yes" -o "$datarootdir" = "\${prefix}/share"],
[datarootdir="$datarootdir/open-trace-format"])
AS_IF([test "$docdir" = "\${datarootdir}/doc/\${PACKAGE_TARNAME}"],
[docdir="\${datarootdir}/doc"])

# Get library version
OTF_VERSION_LIBRARY=m4_normalize(esyscmd([config/otf_get_version.sh VERSION --library]))
AC_SUBST([OTF_VERSION_LIBRARY])

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC

AC_LANG([C])
AC_PROG_LIBTOOL

AC_C_BIGENDIAN

# Own stuff

# Add -D_GNU_SOURCE to CPPFLAGS, if necessary
case "$host_os" in
	*-gnu*)
		CPPFLAGS="$CPPFLAGS -D_GNU_SOURCE"
		;;
esac

# Do we want to build and install binaries?
AC_ARG_ENABLE(binaries,
	AC_HELP_STRING([--enable-binaries],
		[build and install binaries, default: enabled]),
	[enable_binaries="$enableval"], [enable_binaries="yes"])
AM_CONDITIONAL(AMBUILDBINARIES, test x"$enable_binaries" = "xyes")

# Checks for MPI.
CHECK_MPI
if test x"$force_mpi" = "xyes" -a x"$mpi_error" = "xyes"; then exit 1; fi
AM_CONDITIONAL(AMHAVEMPI, test x"$have_mpi" = xyes)

# Checks for OpenMP.
CHECK_OMP
if test x"$force_omp" = "xyes" -a x"$omp_error" = "xyes"; then exit 1; fi
AM_CONDITIONAL(AMHAVEOMP, test x"$have_omp" = xyes)

# Checks for VTF3.
CHECK_VTF3
if test x"$force_vtf3" = "xyes" -a x"$vtf3_error" = "xyes"; then exit 1; fi
AM_CONDITIONAL(AMHAVEVTF3, test x"$have_vtf3" = xyes)

# Checks for zlib compression.
CHECK_ZLIB
if test x"$force_zlib" = "xyes" -a x"$zlib_error" = "xyes"; then exit 1; fi
AM_CONDITIONAL(AMHAVEZLIB, test x"$have_zlib" = xyes)

# Checks for zoidfs support.
CHECK_ZOIDFS
if test x"$force_zoidfs" = "xyes" -a x"$zoidfs_error" = "xyes"; then exit 1; fi
AM_CONDITIONAL(AMHAVEZOIDFS, test x"$have_zoidfs" = xyes)

# Checks for math library.
CHECK_MATH

# Checks for swig and python-dev.
CHECK_SWIG_PYTHON
if test x"$force_swig_python" = "xyes" -a x"$swig_python_error" = "xyes"; then exit 1; fi
AM_CONDITIONAL(AMHAVESWIGPYTHON, test x"$have_swig_python" = xyes)

# Checks for pdflatex and PGFPLOTS needed for otfprofile to convert TeX output to PDF
CHECK_PDFLATEX_PGFPLOTS


WITH_DEBUG
WITH_VERBOSE


# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([ctype.h fcntl.h stddef.h stdlib.h string.h sys/param.h sys/select.h sys/time.h unistd.h])
AC_CHECK_HEADER([stdint.h], [have_stdint_h=1], [have_stdint_h=0])
AC_CHECK_HEADER([inttypes.h], [have_inttypes_h=1], [have_inttypes_h=0])
AC_SUBST([have_stdint_h])
AC_SUBST([have_inttypes_h])

# Checks for library functions.
AC_CHECK_DECL([va_copy],
[AC_DEFINE([HAVE_VA_COPY], [1], [Define to 1 if you have the `va_copy' function.])],
[], [#include <stdarg.h>])
AC_CHECK_DECL([__va_copy],
[AC_DEFINE([HAVE_UNDERSCORE_VA_COPY], [1], [Define to 1 if you have the `__va_copy' function.])],
[], [#include <stdarg.h>])
AC_CHECK_FUNCS([asprintf snprintf vasprintf vsnprintf strdup gettimeofday])
AC_FUNC_FSEEKO
#AC_FUNC_MALLOC
#AC_FUNC_REALLOC
#AC_FUNC_STAT

# Checks for typedefs, structures, and compiler characteristics.
# AC_C_CONST ### do not use this if you want to do C++ programs because it might define 'const'
#	to an empty string which produces a total mess later on!!!

AC_CHECK_DECLS([O_NOATIME], [], [], [[#include <fcntl.h>]])
AC_TYPE_SIZE_T

# Checks for datatype sizes.
AC_CHECK_SIZEOF(long, 8)
sizeof_long=$ac_cv_sizeof_long
AC_SUBST([sizeof_long])

# check for library with clock_gettime
pre_clock_gettime_LIBS="$LIBS"
CLOCK_GETTIME_LIB=
AC_SEARCH_LIBS([clock_gettime], [rt],
    [test "x$ac_cv_search_clock_gettime" = "xnone required" || CLOCK_GETTIME_LIB="$ac_cv_search_clock_gettime"])
AM_CONDITIONAL([AMHAVECLOCKGETTIME], [test "x$ac_cv_search_clock_gettime" != "xno"])
AC_SUBST([CLOCK_GETTIME_LIB])
LIBS="$pre_clock_gettime_LIBS"

AC_CONFIG_FILES([Makefile
	vendor/jenkins_hash/Makefile
	otflib/Makefile
	otflib/OTF_inttypes_unix.h
	otfauxlib/Makefile
	otflib_py/Makefile
	tools/Makefile
	tools/otfaux/Makefile
	tools/otfcompress/Makefile
	tools/otfconfig/Makefile
	tools/otfdump/Makefile
	tools/otfinfo/Makefile
	tools/otfmerge/Makefile
	tools/otfmerge/mpi/Makefile
	tools/otfprofile/Makefile
	tools/otfprofile/mpi/Makefile
	tools/otfshrink/Makefile
	tools/otf2vtf/Makefile
	tools/vtf2otf/Makefile
	tests/Makefile
	tests/hello/Makefile
	tests/generic_streams/Makefile
	tests/generic_streams-mpi/Makefile
	tests/progress/Makefile
	tests/read_from_buffer/Makefile
	docu/Makefile
])

## workaround for stupid warning caused by newer autoconf versions
AC_DEFUN([AC_DATAROOTDIR_CHECKED])

AC_OUTPUT
