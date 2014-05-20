AC_DEFUN([ACVT_LIBERTY],
[
	liberty_error="no"
	check_liberty="no"
	force_liberty="no"
	have_liberty="no"

	LIBERTYDIR=
	LIBERTYINCDIR=
	LIBERTYLIBDIR=
	LIBERTYLIB=

	AC_ARG_WITH(liberty,
		AC_HELP_STRING([--with-liberty], [use libiberty for symbol demangling, default: no]),
	[AS_IF([test x"$withval" = "xyes"], [check_liberty="yes"; force_liberty="yes"])])

	AC_ARG_WITH(liberty-dir,
		AC_HELP_STRING([--with-liberty-dir=LIBERTYDIR], [give the path for LIBERTY, default: /usr]),
	[LIBERTYDIR="$withval/"])

	AC_ARG_WITH(liberty-inc-dir,
		AC_HELP_STRING([--with-liberty-inc-dir=LIBERTYINCDIR],
		[give the path for LIBERTY-include files, default: LIBERTYDIR/include]),
	[LIBERTYINCDIR="-I$withval/"],
	[AS_IF([test x"$LIBERTYDIR" != x], [LIBERTYINCDIR="-I$LIBERTYDIR"include/])])

	AC_ARG_WITH(liberty-lib-dir,
		AC_HELP_STRING([--with-liberty-lib-dir=LIBERTYLIBDIR],
		[give the path for LIBERTY-libraries, default: LIBERTYDIR/lib]),
	[LIBERTYLIBDIR="-L$withval/"],
	[AS_IF([test x"$LIBERTYDIR" != x], [LIBERTYLIBDIR="-L$LIBERTYDIR"lib/])])

	AC_ARG_WITH(liberty-lib,
		AC_HELP_STRING([--with-liberty-lib=LIBERTYLIB], [use given liberty lib, default: -liberty]),
	[LIBERTYLIB="$withval"])

	AS_IF([test x"$check_liberty" = "xyes"],
	[
		AS_IF([test x"$LIBERTYLIB" = x -a x"$liberty_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $LIBERTYLIBDIR -liberty_pic"
			AC_MSG_CHECKING([whether linking with -liberty_pic works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); LIBERTYLIB=-liberty_pic],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])
           
		AS_IF([test x"$LIBERTYLIB" = x -a x"$liberty_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $LIBERTYLIBDIR -liberty"
			AC_MSG_CHECKING([whether linking with -liberty works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); LIBERTYLIB=-liberty],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$LIBERTYLIB" = x -a x"$liberty_error" = "xno"],
		[
			AC_MSG_NOTICE([error: no libiberty found; check path for LIBERTY package first...])
			liberty_error="yes"
		])

		AS_IF([test x"$LIBERTYLIB" != x -a x"$liberty_error" = "xno"],
		[have_liberty="yes"])

		AS_IF([test x"$liberty_error" = "xno"],
		[
			sav_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$CPPFLAGS $LIBERTYINCDIR"
			HAVE_DEMANGLE_H=0
			AC_CHECK_HEADERS([demangle.h])
			AS_IF([test x"$ac_cv_header_demangle_h"], [HAVE_DEMANGLE_H=1])
			AC_SUBST(HAVE_DEMANGLE_H)
			CPPFLAGS=$sav_CPPFLAGS
		])
	])

	AC_SUBST(LIBERTYINCDIR)
	AC_SUBST(LIBERTYLIBDIR)
	AC_SUBST(LIBERTYLIB)
])
