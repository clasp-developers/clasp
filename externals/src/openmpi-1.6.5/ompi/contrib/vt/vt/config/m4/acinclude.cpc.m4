AC_DEFUN([ACVT_CPC],
[
	cpc_error="no"
	have_cpc="no"

	CPCDIR=
	CPCINCDIR=
	CPCLIBDIR=
	CPCLIB=

	AC_ARG_WITH(cpc-dir,
		AC_HELP_STRING([--with-cpc-dir=CPCDIR],
		[give the path for CPC, default: /usr]),
	[CPCDIR="$withval/"])

	AC_ARG_WITH(cpc-inc-dir,
		AC_HELP_STRING([--with-cpc-inc-dir=CPCINCDIR],
		[give the path for CPC-include files, default: CPCDIR/include]),
	[CPCINCDIR="-I$withval/"],
	[AS_IF([test x"$CPCDIR" != x], [CPCINCDIR="-I$CPCDIR"include/])])

	AC_ARG_WITH(cpc-lib-dir,
		AC_HELP_STRING([--with-cpc-lib-dir=CPCLIBDIR],
		[give the path for CPC-libraries, default: CPCDIR/lib]),
	[CPCLIBDIR="-L$withval/"],
	[AS_IF([test x"$CPCDIR" != x], [CPCLIBDIR="-L$CPCDIR"lib/])])

	AC_ARG_WITH(cpc-lib,
		AC_HELP_STRING([--with-cpc-lib=CPCLIB], [use given cpc lib, default: -lcpc]),
	[CPCLIB="$withval"])

	sav_CPPFLAGS=$CPPFLAGS
	CPPFLAGS="$CPPFLAGS $CPCINCDIR"
	AC_CHECK_HEADER([libcpc.h], [],
	[
		AC_MSG_NOTICE([error: no libcpc.h found; check path for CPC package first...])
		cpc_error="yes"
	])
	CPPFLAGS=$sav_CPPFLAGS

	AS_IF([test x"$CPCLIB" = x -a x"$cpc_error" = "xno"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $CPCLIBDIR -lcpc"
		AC_MSG_CHECKING([whether linking with -lcpc works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); CPCLIB=-lcpc],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$CPCLIB" = x -a x"$cpc_error" = "xno"],
	[
		AC_MSG_NOTICE([error: no libcpc found; check path for CPC package first...])
		cpc_error="yes"
	])

	AS_IF([test x"$cpc_error" = "xno"],
	[
		AC_MSG_CHECKING([whether CPC version = 2])

		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $CPCINCDIR"
		AC_TRY_COMPILE([#include <libcpc.h>],
[
#ifndef CPC_VER_CURRENT
#  error "CPC_VER_CURRENT not defined"
#elif CPC_VER_CURRENT != 2
#  error "CPC_VER_CURRENT != 2"
#endif
],
		[AC_MSG_RESULT([yes])],
		[
			AC_MSG_RESULT([no])
			AC_MSG_NOTICE([error: CPC version could not be determined and/or is incompatible (!= 2)
See \`config.log' for more details.])
			cpc_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS
	])

	AS_IF([test x"$CPCLIB" != x -a x"$cpc_error" = "xno"], [have_cpc="yes"])

	AC_SUBST(CPCINCDIR)
	AC_SUBST(CPCLIBDIR)
	AC_SUBST(CPCLIB)
])

