AC_DEFUN([ACVT_PAPI],
[
	papi_error="no"
	have_papi="no"

	PAPIDIR=
	PAPIINCDIR=
	PAPILIBDIR=
	PAPILIB=

	AC_ARG_WITH(papi-dir,
		AC_HELP_STRING([--with-papi-dir=PAPIDIR],
		[give the path for PAPI, default: /usr]),
	[PAPIDIR="$withval/"])

	AC_ARG_WITH(papi-inc-dir,
		AC_HELP_STRING([--with-papi-inc-dir=PAPIINCDIR],
		[give the path for PAPI-include files, default: PAPIDIR/include]),
	[PAPIINCDIR="-I$withval/"],
	[AS_IF([test x"$PAPIDIR" != x], [PAPIINCDIR="-I$PAPIDIR"include/])])

	AC_ARG_WITH(papi-lib-dir,
		AC_HELP_STRING([--with-papi-lib-dir=PAPILIBDIR],
		[give the path for PAPI-libraries, default: PAPIDIR/lib]),
	[PAPILIBDIR="-L$withval/"],
	[AS_IF([test x"$PAPIDIR" != x], [PAPILIBDIR="-L$PAPIDIR"lib/])])

	AC_ARG_WITH(papi-lib,
		AC_HELP_STRING([--with-papi-lib=PAPILIB], [use given papi lib, default: -lpapi]),
	[PAPILIB="$withval"])

	sav_CPPFLAGS=$CPPFLAGS
	CPPFLAGS="$CPPFLAGS $PAPIINCDIR"
	AC_CHECK_HEADER([papi.h], [],
	[
		AC_MSG_NOTICE([error: no papi.h found; check path for PAPI package first...])
		papi_error="yes"
	])
	CPPFLAGS=$sav_CPPFLAGS

	AS_IF([test x"$PAPILIB" = x -a x"$papi_error" = "xno"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $PAPILIBDIR -lpapi"
		AC_MSG_CHECKING([whether linking with -lpapi works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); PAPILIB=-lpapi],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$PAPILIB" = x -a x"$papi_error" = "xno"],
	[
		AC_MSG_NOTICE([error: no libpapi found; check path for PAPI package first...])
		papi_error="yes"
	])

	AS_IF([test x"$papi_error" = "xno"],
	[
		AC_MSG_CHECKING([whether PAPI version >= 3])

		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $PAPIINCDIR"
		AC_TRY_COMPILE([#include <papi.h>],
[
#ifndef PAPI_VERSION
#  error "PAPI_VERSION not defined; version < 3"
#elif PAPI_VERSION_MAJOR(PAPI_VERSION) < 3
#  error "PAPI_VERSION_MAJOR < 3"
#endif
],
		[AC_MSG_RESULT([yes])],
		[
			AC_MSG_RESULT([no])
			AC_MSG_NOTICE([error: PAPI version could not be determined and/or is incompatible (< 3)
See \`config.log' for more details.])
			papi_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS
	])

	AS_IF([test x"$papi_error" = "xno"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $PAPIINCDIR"
		AC_CHECK_TYPES([long_long], [], [], [#include <papi.h>])
		CPPFLAGS=$sav_CPPFLAGS
	])

	AS_IF([test x"$PAPILIB" != x -a x"$papi_error" = "xno"],
	[
		AC_DEFINE([TIMER_PAPI_REAL_CYC], [10], [PAPI_get_real_cyc])
		AC_DEFINE([TIMER_PAPI_REAL_USEC], [11], [PAPI_get_real_usec])
		AS_IF([test x"$pform_timer" = "xTIMER_GETTIMEOFDAY"],
		[
			pform_timer=TIMER_PAPI_REAL_CYC
			AC_DEFINE_UNQUOTED([TIMER], [$pform_timer], [Use timer (see below)])
			AC_MSG_NOTICE([reselected timer: $pform_timer])
			AC_DEFINE([TIMER_IS_GLOBAL], [0],
			[Define to 1 if the selected timer is global (doesn't need synchronization)])
			AC_MSG_NOTICE([global timer: no])
		])
		have_papi="yes"
	])

	AC_SUBST(PAPIINCDIR)
	AC_SUBST(PAPILIBDIR)
	AC_SUBST(PAPILIB)
])

