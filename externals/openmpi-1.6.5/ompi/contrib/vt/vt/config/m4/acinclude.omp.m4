AC_DEFUN([ACVT_OMP],
[
	omp_error="no"
	build_opari="no"
	have_omp="no"

	VTPOMPLIB=

	AC_ARG_VAR(OPENMP_CFLAGS, [C compiler flags to enable support for OpenMP])

	AX_OPENMP([], [omp_error="yes"])

	AS_IF([test x"$omp_error" = "xno"],
	[
		sav_CFLAGS=$CFLAGS
		CFLAGS="$CFLAGS $OPENMP_CFLAGS"
		AC_CHECK_HEADER([omp.h], [], [omp_error="yes"])
		CFLAGS=$sav_CFLAGS
	])

	AS_IF([test x"$omp_error" = "xno"],
	[
		VTPOMPLIB="-lvt-pomp"
		build_opari="yes"
		have_omp="yes"
		AC_DEFINE([HAVE_OMP], [1], [Define to 1 if VT is configured with OpenMP support.])
	])

	AC_SUBST(VTPOMPLIB)
])

