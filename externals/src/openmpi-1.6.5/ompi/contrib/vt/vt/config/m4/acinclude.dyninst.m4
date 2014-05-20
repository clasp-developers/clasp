AC_DEFUN([ACVT_DYNINST],
[
	dyninst_error="no"
	dynattlib_error="no"
	check_dyninst="yes"
	force_dyninst="no"
	check_dynattlib="yes"
	force_dynattlib="no"
	build_dynattlib="no"
	have_dyninst="no"

	DYNIDIR=
	DYNIINCDIR=
	DYNILIBDIR=
	DYNILIB=
	VTDYNATTLIB=

	AC_ARG_ENABLE(dyninst,
		AC_HELP_STRING([--enable-dyninst],
		[enable support for binary instrumentation by using Dyninst, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_dyninst="yes"], [check_dyninst="no"])])

	AC_ARG_ENABLE(dyninst-attlib,
		AC_HELP_STRING([--enable-dyninst-attlib],
		[build shared library which attaches Dyninst to running application, default: enable if Dyninst found by configure and system supports shared libraries]),
	[AS_IF([test x"$enableval" = "xyes"], [force_dyninst="yes"; check_dyninst="yes"; force_dynattlib="yes"; build_dynattlib="yes"], [check_dynattlib="no"])])

	AC_ARG_WITH(dyninst-dir,
		AC_HELP_STRING([--with-dyninst-dir=DYNIDIR], [give the path for Dyninst, default: /usr]),
	[DYNIDIR="$withval/"])

	AC_ARG_WITH(dyninst-inc-dir,
		AC_HELP_STRING([--with-dyninst-inc-dir=DYNIINCDIR],
		[give the path for Dyninst-include files, default: DYNIDIR/include]),
	[DYNIINCDIR="-I$withval/"],
	[AS_IF([test x"$DYNIDIR" != x], [DYNIINCDIR="-I$DYNIDIR"include/])])

	AC_ARG_WITH(dyninst-lib-dir,
		AC_HELP_STRING([--with-dyninst-lib-dir=DYNILIBDIR],
		[give the path for Dyninst-libraries, default: DYNIDIR/lib]),
	[DYNILIBDIR="-L$withval/"],
	[AS_IF([test x"$DYNIDIR" != x], [DYNILIBDIR="-L$DYNIDIR"lib/])])

	AC_ARG_WITH(dyninst-lib,
		AC_HELP_STRING([--with-dyninst-lib=DYNILIB], [use given Dyninst lib, default: -ldyninstAPI]),
	[DYNILIB="$withval"])

	AS_IF([test "$check_dyninst" = "yes"],
	[
		AC_LANG([C++])

		AS_IF([test x"$dyninst_error" = "xno"],
		[
			sav_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$CPPFLAGS $DYNIINCDIR"
			AC_CHECK_HEADER([BPatch.h], [],
			[
				AC_MSG_NOTICE([error: no BPatch.h found; check path for Dyninst package first...])
				dyninst_error="yes"
			])
			CPPFLAGS=$sav_CPPFLAGS
		])

		AS_IF([test x"$DYNILIB" = x -a x"$dyninst_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $DYNILIBDIR -ldyninstAPI"
			AC_MSG_CHECKING([whether linking with -ldyninstAPI works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); DYNILIB="-ldyninstAPI"],
			[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$DYNILIB" = x -a x"$dyninst_error" = "xno"],
		[
			AC_MSG_NOTICE([error: no libdyninstAPI found; check path for Dyninst package first...])
			dyninst_error="yes"
		])

		AS_IF([test x"$dyninst_error" = "xno"],
		[
			AC_MSG_CHECKING([whether Dyninst version >= 6.1])

			sav_CXXFLAGS=$CXXFLAGS
			CXXFLAGS="$CXXFLAGS $DYNIINCDIR"
			AC_TRY_COMPILE([#include <BPatch.h>],
[
#ifndef DYNINST_6_1
#  error "DYNINST_6_1 not defined"
#endif
],
			[AC_MSG_RESULT([yes])],
			[
				AC_MSG_RESULT([no])
				AC_MSG_NOTICE([error: Dyninst version could not be determined and/or is incompatible (< 6.1)
See \`config.log' for more details.])
				dyninst_error="yes"
			])
			CXXFLAGS=$sav_CXXFLAGS
		])

		AC_LANG([C])

		AS_IF([test x"$dyninst_error" = "xno"],
		[
			have_dyninst="yes"
			ACVT_CONF_SUBSUBTITLE([Dyninst attach library])
			AS_IF([test x"$check_dynattlib" = "xyes"],
			[
				AC_MSG_CHECKING([whether we can build shared libraries])
				AS_IF([test x"$enable_shared" = "xyes"],
				[
					AC_MSG_RESULT([yes])
					build_dynattlib="yes"
					VTDYNATTLIB="-lvt-dynatt"
				],
				[
					AC_MSG_RESULT([no])
					dynattlib_error="yes"
				])
			],
			[
				AS_IF([test x"$enable_config_titles" = "xyes"],
				[AC_MSG_NOTICE([disabled via command line switch])])
			])

		])
	])

	AC_SUBST(DYNIINCDIR)
	AC_SUBST(DYNILIBDIR)
	AC_SUBST(DYNILIB)
	AC_SUBST(VTDYNATTLIB)
])

