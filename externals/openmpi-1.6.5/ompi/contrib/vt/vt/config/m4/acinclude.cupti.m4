AC_DEFUN([ACVT_CUPTI],
[
	cupti_error="no"
	check_cupti="yes"
	force_cupti="no"
	have_cupti="no"
	have_cupti_events="no"
	have_cupti_callbacks="no"
	have_cupti_activity="no"

	CUPTIDIR=
	CUPTIINCDIR=
	CUPTILIBDIR=
	CUPTILIB=

	AC_REQUIRE([ACVT_CUDA])

	AC_ARG_ENABLE(cupti,
		AC_HELP_STRING([--enable-cupti],
		[enable support for tracing CUDA via CUPTI, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_cupti="yes"], [check_cupti="no"])])

	AC_ARG_WITH(cupti-dir,
		AC_HELP_STRING([--with-cupti-dir=CUPTIDIR],
		[give the path for CUPTI, default: /usr]),
	[CUPTIDIR="$withval/"],
	[AS_IF([test x"$CUDATKDIR" != x], [CUPTIDIR="$CUDATKDIR/extras/CUPTI/"])])

	AC_ARG_WITH(cupti-inc-dir,
		AC_HELP_STRING([--with-cupti-inc-dir=CUPTIINCDIR],
		[give the path for CUPTI-include files, default: CUPTIDIR/include]),
	[CUPTIINCDIR="-I$withval/"],
	[AS_IF([test x"$CUPTIDIR" != x], [CUPTIINCDIR="-I$CUPTIDIR"include/])])

	AC_ARG_WITH(cupti-lib-dir,
		AC_HELP_STRING([--with-cupti-lib-dir=CUPTILIBDIR],
		[give the path for CUPTI-libraries, default: CUPTIDIR/lib64]),
	[CUPTILIBDIR="-L$withval/"],
	[AS_IF([test x"$CUPTIDIR" != x], [CUPTILIBDIR="-L$CUPTIDIR"lib64/])])

	AC_ARG_WITH(cupti-lib,
		AC_HELP_STRING([--with-cupti-lib=CUPTILIB], [use given cupti lib, default: -lcupti CUDALIB]),
	[CUPTILIB="$withval"])

	AS_IF([test "$check_cupti" = "yes"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $CUPTIINCDIR $CUDATKINCDIR"
		AC_CHECK_HEADER([cupti.h], [],
		[
			AC_MSG_NOTICE([error: no cupti.h found; check path for CUPTI package first...])
			cupti_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS

		AS_IF([test x"$CUPTILIB" = x -a x"$cupti_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $CUPTILIBDIR -lcupti $CUDATKLIBDIR $CUDALIB $CUDARTLIB"
			AC_MSG_CHECKING([whether linking with -lcupti works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); CUPTILIB="-lcupti $CUDATKLIBDIR $CUDALIB $CUDARTLIB"],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$CUPTILIB" = x -a x"$cupti_error" = "xno"],
		[
			AC_MSG_NOTICE([error: no libcupti found; check path for CUPTI package first...])
			cupti_error="yes"
		])

		AS_IF([test x"$cupti_error" = "xno"], [
			have_cupti="yes"

			sav_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$CPPFLAGS $CUPTIINCDIR $CUDATKINCDIR"

			AC_CHECK_HEADER([cupti_events.h], [
				AC_MSG_CHECKING([whether CUDA runtime version >= 4.0])
				AC_TRY_COMPILE([#include "cuda_runtime_api.h"],
	[
	#ifndef CUDART_VERSION
	#  error "CUDART_VERSION not defined"
	#elif CUDART_VERSION < 4000
	#  error "CUDART_VERSION < 4000"
	#endif
	],
				[
					AC_MSG_RESULT([yes])
					have_cupti_events="yes"
				],[
					AC_MSG_RESULT([no])
					AC_MSG_NOTICE([error: CUDA runtime version could not be determined and/or is incompatible (< 4.0)
	See \`config.log' for more details.])
				])
			],[
				AC_MSG_NOTICE([error: no cupti_events.h found])
			])
			AC_CHECK_HEADER([cupti_callbacks.h], [have_cupti_callbacks="yes"],
			[
				AC_MSG_NOTICE([error: no cupti_callbacks.h found])
			])
			AC_CHECK_HEADER([cupti_activity.h], [have_cupti_activity="yes"],
			[
				AC_MSG_NOTICE([error: no cupti_activity.h found])
			])
			CPPFLAGS=$sav_CPPFLAGS
		])
	])

dnl	if no CUPTI found, remove content of CUPTILIBDIR to prevent adding them
dnl	to the linker flags when using the VT compiler wrappers
	AS_IF([test x"$have_cupti" = "xno"],
	[CUPTILIBDIR=])

	AC_SUBST(CUPTIINCDIR)
	AC_SUBST(CUPTILIBDIR)
	AC_SUBST(CUPTILIB)
])
