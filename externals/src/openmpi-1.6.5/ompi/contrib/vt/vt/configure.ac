#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.60)
AC_INIT([VampirTrace], [m4_normalize(esyscmd([cat VERSION]))], [vampirsupport@zih.tu-dresden.de], [VampirTrace])
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config/m4)
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_HEADERS(config.h)

# If Automake supports silent rules, enable them.
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# Check whether we are inside Open MPI package
inside_openmpi="no"
AC_ARG_WITH(openmpi-inside, [],
[
	AS_IF([test x"$withval" = "xyes" -o x"$withval" = "x1.7"],
	[
		inside_openmpi="$withval"
		CPPFLAGS="-DINSIDE_OPENMPI $CPPFLAGS"

		# Set FC to F77 if Open MPI version < 1.7
		AS_IF([test x"$withval" = "xyes" -a x"$FC" = x -a x"$F77" != x],
		[FC="$F77"])
	])
])

ACVT_CONF_INIT

ACVT_CONF_TITLE([Initialization])

# Check for platform
ACVT_PLATFORM
AM_CONDITIONAL(AMMACOS, test "$PLATFORM" = "macos")

# Get configuration options
ACVT_CONF_OPTIONS

# Setup the top and bottom of the config.h file
AH_TOP([
#ifndef _CONFIG_H
#define _CONFIG_H
])
AH_BOTTOM([
#include "config_bottom.h"
#endif /* _CONFIG_H */
])

# Check for absolute paths of $builddir and $srcdir
top_vt_builddir="`pwd`"
cd "$srcdir"
top_vt_srcdir="`pwd`"
cd "$top_vt_builddir"
AC_MSG_NOTICE([builddir: $top_vt_builddir])
eval "echo $top_vt_builddir | grep \" \"" >/dev/null 2>&1
AS_IF([test "$?" = "0"],
[AC_MSG_ERROR([VampirTrace does not allow whitespace in the build directory path!])])
AC_SUBST(top_vt_builddir)
AC_MSG_NOTICE([srcdir: $top_vt_srcdir])
eval "echo $top_vt_srcdir | grep \" \"" >/dev/null 2>&1
AS_IF([test "$?" = "0"],
[AC_MSG_ERROR([VampirTrace does not allow whitespace in the source directory path!])])
AC_SUBST(top_vt_srcdir)
AS_IF([test "$top_vt_builddir" != "$top_vt_srcdir"],
[AC_MSG_NOTICE([detected VPATH build])])

# Check for file system case sensitivity
ACVT_CSFS
AM_CONDITIONAL(AMHAVECSFS, test x"$have_csfs" = "xyes")

# Correct install paths
AS_IF([test x"$prefix" = x -o x"$prefix" = "xNONE"], [prefix="$ac_default_prefix"])
AS_IF([test x"$exec_prefix" = x -o x"$exec_prefix" = "xNONE"], [exec_prefix="\${prefix}"])
AS_IF([test x"$inside_openmpi" != "xno" -o "$includedir" = "\${prefix}/include"],
[includedir="$includedir/vampirtrace"])
AS_IF([test x"$inside_openmpi" != "xno" -o "$datarootdir" = "\${prefix}/share"],
[datarootdir="$datarootdir/vampirtrace"])
AS_IF([test x"$inside_openmpi" != "xno" -a "$datadir" != "\${datarootdir}"],
[datadir="$datadir/vampirtrace"])
AS_IF([test "$docdir" = "\${datarootdir}/doc/\${PACKAGE_TARNAME}"],
[docdir="\${datarootdir}/doc"])

# Declare additional variables
AC_ARG_VAR(AR, [archiver command])
AC_ARG_VAR(NM, [command to list symbols from object files])
AC_ARG_VAR(JAVA, [Java application launcher command])

ACVT_CONF_TITLE([Compiler, preprocessor, and programs])

# Check for compiler and preprocessor
AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CXX
AC_PROG_CPP
AC_PROG_CXXCPP
AM_PROG_AS

AC_C_BIGENDIAN
AC_C_INLINE

# Do we want to support Fortran
check_fortran="yes"
force_fortran="no"
AC_ARG_ENABLE(fortran,
	AC_HELP_STRING([--enable-fortran],
		[enable Fortran support, default: enable if Fortran compiler found by configure]),
	[
		AS_IF([test x"$enableval" = "xyes"],
		[
			force_fortran="yes"
		],
		[
			check_fortran="no"
			FC=
		])
	])

# Check for Fortran compiler
AS_IF([test x"$check_fortran" = "xyes"],
[
	# Exclude F77-only compiler names when searching Fortran compiler
	AC_PROG_FC([gfortran f95 fort xlf95 ifort ifc efc pgfortran pgf95 lf95 f90 xlf90 pgf90 epcf90])
	AS_IF([test x"$FC" = x],
	[
		AC_MSG_NOTICE([error: no Fortran compiler found])
		AS_IF([test x"$force_fortran" = "xyes"], [exit 1])
	])
])

AC_LANG([C])

AS_IF([test x"$FC" != x],
[AC_DEFINE([HAVE_FC], [1], [Define to 1 if VT is configured with Fortran support.])])
AM_CONDITIONAL(AMHAVEFC, test x"$FC" != x)

# Check cross-build related stuff
ACVT_CROSS
AM_CONDITIONAL(AMHAVECROSSPREFIX, test x"$CROSS_PREFIX" != x)

# Do we want to build and install binaries
AC_ARG_ENABLE(binaries,
	AC_HELP_STRING([--enable-binaries],
		[build and install binaries, default: enabled]),
	[enable_binaries="$enableval"], [enable_binaries="yes"])
AM_CONDITIONAL(AMBUILDBINARIES, test x"$enable_binaries" = "xyes")

# Check for programs
#AC_PROG_AR
AC_CHECK_PROGS(AR, ar, ar)
AC_PROG_LN_S
AC_PROG_AWK
AC_CHECK_PROG(JAVA, java, java)

ACVT_CONF_TITLE([Header files, types, and functions])

# Check for header files
AC_HEADER_STDC
AC_CHECK_HEADERS([stdint.h inttypes.h fnmatch.h sys/param.h])
HAVE_STDINT_H=0
HAVE_INTTYPES_H=0
AS_IF([test x"$ac_cv_header_stdint_h"], [HAVE_STDINT_H=1])
AS_IF([test x"$ac_cv_header_inttypes.h"], [HAVE_INTTYPES_H=1])
AC_SUBST(HAVE_STDINT_H)
AC_SUBST(HAVE_INTTYPES_H)

# Check for datatypes
AC_TYPE_SIZE_T
AC_CHECK_SIZEOF(void*, 8)
AC_CHECK_SIZEOF(int, 4)
AC_CHECK_SIZEOF(long, 8)
AC_CHECK_SIZEOF(mode_t, 4)
AC_CHECK_SIZEOF(size_t, 8)
SIZEOF_LONG=$ac_cv_sizeof_long
AC_SUBST(SIZEOF_LONG)

# Check for library functions
#AC_REPLACE_FNMATCH
#AC_FUNC_MALLOC
#AC_FUNC_REALLOC
AC_CHECK_DECL([va_copy],
[AC_DEFINE([HAVE_VA_COPY], [1], [Define to 1 if you have the `va_copy' function.])],
[], [#include <stdarg.h>])
AC_CHECK_DECL([__va_copy],
[AC_DEFINE([HAVE_UNDERSCORE_VA_COPY], [1], [Define to 1 if you have the `__va_copy' function.])],
[], [#include <stdarg.h>])
AC_CHECK_FUNCS([asprintf snprintf vasprintf vsnprintf strdup memmove])

# Check for libtool
ACVT_CONF_TITLE([Libtool])

# Override the configure options '--enable-static' and '--enable-shared' to
# enforce building only static libraries on CrayXT and BlueGene/L platforms.
AS_IF([test "$PLATFORM" = "crayxt" -o "$PLATFORM" = "bgl"],
[enable_static="yes"; enable_shared="no"])

# Save prior set symbol lister command to distinguish whether it is given
# by configure option or determined by AC_PROG_LIBTOOL
given_nm=$NM

AC_PROG_LIBTOOL

# Extract extension for shared libraries from Libtool variables
SHREXT=$shrext_cmds
AC_SUBST(SHREXT)

# If building of shared libraries is desired on BlueGene/P/Q, add '-Wl,-dy'
# to linker flags to lead libtool to believe that dynamic linking is the
# default behaviour of the linker.
AS_IF([test x"$enable_shared" = "xyes"],
[
	case $PLATFORM in
		bgp | bgq)
			export LDFLAGS="$LDFLAGS $lt_prog_compiler_wl-dy"
			;;
	esac
])

# Check for BSD compatible symbol lister command
#AC_PROG_NM # already been checked by AC_PROG_LIBTOOL
AS_IF([test x"$NM" != x -a x"$given_nm" = x],
[
	# Append demangle and file/line number options
	case `$NM -V 2>&1` in
		GNU\ nm\ *)
			NM="$NM --demangle --line-numbers"
			;;
	esac
])
AC_DEFINE_UNQUOTED([DEFAULT_NM], ["$NM"],
[Command to list symbols from object files.])

# Check for timer
ACVT_CONF_TITLE([Timer])
ACVT_TIMER

# Check for compiler instrumentation
ACVT_CONF_TITLE([Compiler instrumentation])
ACVT_COMPINST
AS_IF([test x"$force_compinst" = "xyes" -a x"$compinst_error" = "xyes"], [exit 1])
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_compinst" = "xno"],
[AC_MSG_NOTICE([disabled via command line switch])])
AM_CONDITIONAL(AMHAVECOMPINST, test x"$have_compinst" = "xyes")
AM_CONDITIONAL(AMBUILDCOMPINST_GNU, test x"$compinst_type" = "xgnu" -o x"$compinst_type" = "xpgi9")
AM_CONDITIONAL(AMBUILDCOMPINST_PGI, test x"$compinst_type" = "xpgi")
AM_CONDITIONAL(AMBUILDCOMPINST_CRAYCCE, test x"$compinst_type" = "xcraycce")
AM_CONDITIONAL(AMBUILDCOMPINST_PHAT, test x"$compinst_type" = "xsun")
AM_CONDITIONAL(AMBUILDCOMPINST_XL, test x"$compinst_type" = "xxl")
AM_CONDITIONAL(AMBUILDCOMPINST_FTRACE, test x"$compinst_type" = "xnecsx")
AM_CONDITIONAL(AMBUILDCOMPINST_OPENUH, test x"$compinst_type" = "xopenuh")

# Check for binary instrumentation by Dyninst
ACVT_CONF_TITLE([Binary instrumentation by Dyninst])
ACVT_DYNINST
AS_IF([test x"$force_dyninst" = "xyes" -a x"$dyninst_error" = "xyes"], [exit 1])
AS_IF([test x"$force_dynattlib" = "xyes" -a x"$dynattlib_error" = "xyes"], [exit 1])
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_dyninst" = "xno"],
[AC_MSG_NOTICE([disabled via command line switch])])
AM_CONDITIONAL(AMHAVEDYNINST, test x"$have_dyninst" = "xyes")
AM_CONDITIONAL(AMBUILDDYNATTLIB, test x"$build_dynattlib" = "xyes")

# Check for source conde instrumentation by TAU
ACVT_CONF_TITLE([Automatic source code instrumentation by TAU])
ACVT_TAUINST
AS_IF([test x"$force_tauinst" = "xyes" -a x"$tauinst_error" = "xyes"], [exit 1])
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_tauinst" = "xno"],
[AC_MSG_NOTICE([disabled via command line switch])])
AM_CONDITIONAL(AMHAVETAUINST, test x"$have_tauinst" = "xyes")

# Check for MPI support
ACVT_CONF_TITLE([Message Passing Interface (MPI)])
ACVT_MPI
AS_IF([test x"$force_mpi" = "xyes" -a x"$mpi_error" = "xyes"], [exit 1])
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_mpi" = "xno"],
[AC_MSG_NOTICE([disabled via command line switch])])
AM_CONDITIONAL(AMHAVEMPI, test x"$have_mpi" = "xyes")
AM_CONDITIONAL(AMHAVEMPI2CONST, test x"$have_mpi2_const" = "xyes")
AM_CONDITIONAL(AMHAVEMPI2IO, test x"$have_mpi2_io" = "xyes")
AM_CONDITIONAL(AMHAVEFMPI, test x"$have_fmpi" = "xyes")
AM_CONDITIONAL(AMBUILDMPI, test x"$have_mpi" = "xyes")
AM_CONDITIONAL(AMBUILDFMPIWRAPLIB, test x"$build_fmpiwraplib" = "xyes")
AS_IF([test x"$have_mpi" = "xyes"],
[
#	Check for UniMCI support
	ACVT_CONF_SUBTITLE([Universal MPI Correctness Interface (UniMCI)])
	ACVT_UNIMCI
	AS_IF([test x"$force_unimci" = "xyes" -a x"$unimci_error" = "xyes"], [exit 1])
	AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_unimci" = "xno"],
	[AC_MSG_NOTICE([disabled via command line switch])])
	AM_CONDITIONAL(AMHAVEMPI2CONST, test x"$have_mpi2_const" = "xyes")
	AM_CONDITIONAL(AMBUILDFMPIWRAPLIB, test x"$build_fmpiwraplib" = "xyes")

#       Check for enhanced time synchronization support
	ACVT_CONF_SUBTITLE([Enhanced time synchronization])
	ACVT_ETIMESYNC
	AS_IF([test x"$force_etimesync" = "xyes" -a x"$etimesync_error" = "xyes"], [exit 1])
	AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_etimesync" = "xno"],
	[AC_MSG_NOTICE([disabled via command line switch])])

#	Check for MPI trace unification library
	ACVT_CONF_SUBTITLE([MPI trace unification library])
	ACVT_MPIUNIFYLIB
	AS_IF([test x"$force_mpiunifylib" = "xyes" -a x"$mpiunifylib_error" = "xyes"], [exit 1])
	AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_mpiunifylib" = "xno"],
	[AC_MSG_NOTICE([disabled via command line switch])])
])
AM_CONDITIONAL(AMBUILDETIMESYNC, test x"$build_etimesync" = "xyes")
AM_CONDITIONAL(AMHAVEMATH, test x"$have_math" = "xyes")
AM_CONDITIONAL(AMHAVEUNIMCI, test x"$have_unimci" = "xyes")
AM_CONDITIONAL(AMBUILDMPIUNIFYLIB, test x"$build_mpiunifylib" = "xyes")

# Check for Threads support
ACVT_CONF_TITLE([Multithreading])
ACVT_THREADS
AS_IF([test x"$force_threads" = "xyes" -a x"$threads_error" = "xyes"], [exit 1])
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_threads" = "xno"],
[AC_MSG_NOTICE([disabled via command line switch])])
AM_CONDITIONAL(AMHAVETHREADS, test x"$have_threads" = "xyes")
AM_CONDITIONAL(AMHAVEPTHREAD, test x"$have_pthread" = "xyes")
AM_CONDITIONAL(AMHAVEOMP, test x"$have_omp" = "xyes")
AM_CONDITIONAL(AMBUILDMT, test x"$have_threads" = "xyes")
AM_CONDITIONAL(AMBUILDOPARI, test x"$build_opari" = "xyes")
AM_CONDITIONAL(AMTHRDPTHREAD, test x"$have_pthread" = "xyes")
AM_CONDITIONAL(AMTHRDOMP, test x"$have_pthread" != "xyes" -a x"$have_omp" = "xyes")

# Check for Hybrid (MPI/Threads) support
AS_IF([test x"$have_mpi" = "xyes" -a x"$have_threads" = "xyes"],
[
	build_hybrid="yes"
        AC_DEFINE([HAVE_HYBRID], [1], [Define to 1 if VT is configured with Hybrid (MPI/Threads) support.])

])
AM_CONDITIONAL(AMBUILDHYBRID, test x"$build_hybrid" = "xyes")

# Check for JAVA support
ACVT_CONF_TITLE([Java])
ACVT_JAVA
AS_IF([test x"$force_java" = "xyes" -a x"$java_error" = "xyes"], [exit 1])
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_java" = "xno"],
[AC_MSG_NOTICE([disabled via command line switch])])
AM_CONDITIONAL(AMBUILDJAVA, test x"$have_java" = "xyes")
AM_CONDITIONAL(AMBUILDVTJAVA, test x"$have_java" = "xyes" -a x"$JAVA" != x)

# Check for hardware performace counter support
ACVT_CONF_TITLE([Hardware performance counter])
ACVT_METRICS
AS_IF([test x"$force_metrics" = "xyes" -a x"$metrics_error" = "xyes"], [exit 1])
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_metrics" = "xno"],
[AC_MSG_NOTICE([disabled via command line switch])])
AM_CONDITIONAL(AMHAVEMETRICS, test x"$have_metrics" = "xyes")
AM_CONDITIONAL(AMHAVEPAPI, test x"$have_papi" = "xyes")
AM_CONDITIONAL(AMHAVECPC, test x"$have_cpc" = "xyes")
AM_CONDITIONAL(AMHAVENECSXCNTR, test x"$have_necsxcntr" = "xyes")

# Check for resource usage tracing support
ACVT_CONF_TITLE([Resource usage tracing])
ACVT_RUSAGE
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_rusage" = "xno"],
[AC_MSG_NOTICE([disabled via command line switch])])
AS_IF([test x"$force_rusage" = "xyes" -a x"$rusage_error" = "xyes"], [exit 1])
AM_CONDITIONAL(AMHAVERUSAGE, test x"$have_rusage" = "xyes")

# Check for plugin counter support
ACVT_CONF_TITLE([Plugin Counter support])
ACVT_PLUGINCNTR
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_plugin_cntr" = "xno"],
[AC_MSG_NOTICE([disabled via command line switch])])
AS_IF([test x"$force_plugin_cntr" = "xyes" -a x"plugin_cntr_error" = "xyes"], [exit 1])
AM_CONDITIONAL(AMHAVEPLUGINCNTR, test x"$have_plugin_cntr" = "xyes")

# Check for CPU ID tracing support
ACVT_CONF_TITLE([GLIBC's CPU ID tracing])
ACVT_GETCPU
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_getcpu" = "xno"],
[AC_MSG_NOTICE([disabled via command line switch])])
AS_IF([test x"$force_getcpu" = "xyes" -a x"$getcpu_error" = "xyes"], [exit 1])
AM_CONDITIONAL(AMHAVEGETCPU, test x"$have_getcpu" = "xyes")

# Check for NVIDIA CUDA support
ACVT_CONF_TITLE([NVIDIA CUDA])

# Check for symbol demangling support by libiberty
ACVT_CONF_SUBTITLE([Symbol demangling])
ACVT_LIBERTY
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_liberty" = "xno"],
[AC_MSG_NOTICE([disabled by default])])
AS_IF([test x"$force_liberty" = "xyes" -a x"$liberty_error" = "xyes"], [exit 1])
AM_CONDITIONAL(AMHAVELIBERTY, test x"$have_liberty" = "xyes")

# Check for CUDA Toolkit
ACVT_CONF_SUBTITLE([CUDA Toolkit])
ACVT_CUDA
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_cuda" = "xno"],
[AC_MSG_NOTICE([disabled via command line switch])])
AS_IF([test x"$force_cuda" = "xyes" -a x"$cuda_error" = "xyes"], [exit 1])
AM_CONDITIONAL(AMBUILDCUDA, test x"$have_cuda" = "xyes")
AM_CONDITIONAL(AMBUILDCUDART, test x"$have_cudart" = "xyes")

# Check for CUDA Tools SDK support
ACVT_CONF_SUBTITLE([CUDA Tools])
# Check for CUPTI
ACVT_CUPTI
AM_CONDITIONAL(AMBUILDCUPTI, test x"$have_cupti" = "xyes")
AM_CONDITIONAL(AMHAVECUPTIEVENTS, test x"$have_cupti_events" = "xyes")
AM_CONDITIONAL(AMHAVECUPTICALLBACKS, test x"$have_cupti_callbacks" = "xyes")
AM_CONDITIONAL(AMHAVECUPTIACTIVITY, test x"$have_cupti_activity" = "xyes")

# Check for library tracing support
ACVT_CONF_TITLE([Library tracing])
ACVT_LIBWRAP
AS_IF([test x"$force_libwrap" = "xyes" -a x"$libwrap_error" = "xyes"], [exit 1])
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_libwrap" = "xno"],
[AC_MSG_NOTICE([disabled via command line switch])])
AM_CONDITIONAL(AMBUILDLIBWRAP, test x"$have_libwrap" = "xyes")
AM_CONDITIONAL(AMBUILDLIBWRAPGEN, test x"$build_libwrapgen" = "xyes")
AM_CONDITIONAL(AMHAVEDL, test x"$have_dl" = "xyes")
AM_CONDITIONAL(AMBUILDEXECWRAP, test x"$have_execwrap" = "xyes")
AM_CONDITIONAL(AMBUILDIOWRAP, test x"$have_iowrap" = "xyes")
AM_CONDITIONAL(AMBUILDMALLOCWRAP, test x"$have_mallocwrap" = "xyes")
AM_CONDITIONAL(AMBUILDCUDARTWRAP, test x"$have_cudartwrap" = "xyes")

cond_cuda=0
if test x"$have_cudartwrap" = "xyes" -o x"$have_cupti_callbacks" = "xyes"; then
cond_cuda=1
fi
AM_CONDITIONAL(AMBUILDVTNVCC, test $cond_cuda -eq 1 -a x"$NVCC" != x)

# Check for GPU support
AM_CONDITIONAL(AMHAVEGPU, test x"$have_cupti_callbacks" = "xyes" -o x"$have_cudartwrap" = "xyes")

# Check path for node-local temporary directory
ACVT_LTMPDIR

# Setup compiler wrappers
ACVT_COMPWRAP

# Check for OTF
ACVT_CONF_TITLE([Open Trace Format (OTF)])
ACVT_OTF
AS_IF([test x"$use_extern_otf" = "xyes" -a x"$otf_error" = "xyes"], [exit 1])
AM_CONDITIONAL(AMEXTERNOTF, test x"$use_extern_otf" = "xyes")
AM_CONDITIONAL(AMHAVEIOFSL, test x"$have_iofsl" = "xyes")
AM_CONDITIONAL(AMBUILDIOFSLSCRIPTS, test x"$have_iofsl" = "xyes" -a x"$IOFSLSCRIPTS" != x)

# Check for capabilities of vtsetup
ACVT_CONF_TITLE([VTSetup])
ACVT_SETUP
AS_IF([test x"$force_vtsetup" = "xyes" -a x"$vtsetup_error" = "xyes"], [exit 1])
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_vtsetup" = "xno"],
[AC_MSG_NOTICE([disabled via command line switch])])
AM_CONDITIONAL(AMBUILDVTSETUP, test x"$build_vtsetup" = "xyes")

# Check for capabilities of vtrun
ACVT_CONF_TITLE([VTRun])
ACVT_RUN
AS_IF([test x"$force_vtrun" = "xyes" -a x"$vtrun_error" = "xyes"], [exit 1])
AS_IF([test x"$enable_config_titles" = "xyes" -a x"$check_vtrun" = "xno"],
[AC_MSG_NOTICE([disabled via command line switch])])
AM_CONDITIONAL(AMBUILDVTRUN, test x"$build_vtrun" = "xyes")

# If it's a VPATH build, add the top-level source directory as an include path
# to find '<srcdir>/config_bottom.h' and '<srcdir>/util/util.h'
AS_IF([test "$top_vt_builddir" != "$top_vt_srcdir"],
[
	CPPFLAGS='-I$(top_srcdir)'" $CPPFLAGS"
	CPPFLAGS_FOR_BUILD='-I$(top_srcdir)'" $CPPFLAGS_FOR_BUILD"
])

# Output files
AC_CONFIG_FILES([Makefile
	extlib/Makefile
	util/Makefile
	util/installdirs_conf.h
	rfg/Makefile
	vtlib/Makefile
	tools/Makefile
	tools/opari/Makefile
	tools/opari/tool/Makefile
	tools/opari/doc/Makefile
	tools/vtcpcavail/Makefile
	tools/vtdyn/Makefile
	tools/vtdyn/dynattlib/Makefile
	tools/vtfilter/Makefile
	tools/vtfilter/mpi/Makefile
	tools/vtiofsl/Makefile
	tools/vtiofsl/vtiofsl-common
	tools/vtiofsl/vtiofsl-start
	tools/vtiofsl/vtiofsl-stop
	tools/vtjava/Makefile
	tools/vtjava/vtjava
	tools/vtlibwrapgen/Makefile
	tools/vtlibwrapgen/vt_libwrapgen_defs.h
	tools/vtrun/Makefile
	tools/vtrun/vtrun
	tools/vtsetup/Makefile
	tools/vtsetup/vtsetup
	tools/vtsetup/vtsetup-config.xml
	tools/vtsetup/vtsetup-data.xml
	tools/vtsetup/doc/Makefile
	tools/vtunify/Makefile
	tools/vtunify/mpi/Makefile
	tools/vtwrapper/Makefile
	tools/vtwrapper/vtcc-wrapper-data.txt
	tools/vtwrapper/vtc++-wrapper-data.txt
	tools/vtwrapper/vtfort-wrapper-data.txt
	tools/vtwrapper/vtnvcc-wrapper-data.txt
	include/Makefile
	include/vt_inttypes.h
	doc/Makefile
])

# Add MPI wrapper generator scripts to output files
AS_IF([test x"$have_mpi" = "xyes"],
[
	AC_CONFIG_FILES([config/mpigen/mk_registry.sh])
	AC_CONFIG_FILES([config/mpigen/mk_c_wrapper.sh])
	AS_IF([test x"$build_fmpiwraplib" = "xyes"],
	[AC_CONFIG_FILES([config/mpigen/mk_fortran_wrapper.sh])])
])

# Create output files
ACVT_CONF_TITLE([Final output])
AC_OUTPUT

# Patch Makefiles which define library targets that depend on other libraries
# to prevent the following Libtool warning:
# "libtool: link: warning: `...//*.la' seems to be moved"
#
# Libtool is getting confused by the "//" in the library paths, so remove the
# trailing '/' from the *LIBDIR variables.
# Actually, this should be done in the *.m4 files where the variables are being
# set, but it's less elaborate at this point. ;-)
makefile_to_patch="vtlib/Makefile tools/vtunify/mpi/Makefile"
for makefile in $makefile_to_patch; do
	sed -e "s/^\(.*LIBDIR =.*\)\/$/\1/g" $makefile >$makefile.tmp
	mv $makefile.tmp $makefile
done

# Patch the compiler wrapper configuration file for vtnvcc in order to pass
# compiler/linker options directly to the compiler/linker using the option
# -Xcompiler/-Xlinker.
wrapper_data_file="tools/vtwrapper/vtnvcc-wrapper-data.txt"
# keywords containing compiler/linker options
# (<compiler options> -- <linker options>)
wrapper_data_keys="compiler_flags compinst_compiler_flags dyninst_compiler_flags -- linker_flags libs vtlib vtmpilib vtmtlib vthyblib vtpomplib vtdynattlib"

value_prefix="-Xcompiler"
for key in $wrapper_data_keys; do
	if test $key = "--"; then
		value_prefix="-Xlinker"
	else
		value=`grep "^$key=" $wrapper_data_file | sed "s/^$key=//;s/^ *//;s/ *$//;s/ \{1,\}/ /g;s/ /,/g"`
		if test x"$value" != x; then
			sed -e "s@^$key=.*@$key=$value_prefix $value@" $wrapper_data_file >$wrapper_data_file.tmp
			mv $wrapper_data_file.tmp $wrapper_data_file
		fi
	fi
done

# Generate MPI wrappers
AS_IF([test x"$have_mpi" = "xyes"],
[
	AC_MSG_NOTICE([executing MPI wrapper generator])

	$SHELL $top_vt_builddir/config/mpigen/mk_registry.sh
	AS_IF([test $? != 0], [exit 1])
	$SHELL $top_vt_builddir/config/mpigen/mk_c_wrapper.sh
	AS_IF([test $? != 0], [exit 1])
	AS_IF([test x"$build_fmpiwraplib" = "xyes"],
	[
		$SHELL $top_vt_builddir/config/mpigen/mk_fortran_wrapper.sh
		AS_IF([test $? != 0], [exit 1])
	])
])
	
# Show configure summary
ACVT_CONF_SUMMARY

