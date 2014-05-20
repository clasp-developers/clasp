AC_DEFUN([ACVT_ZLIB],
[
	zlib_error="no"
	check_zlib="yes"
        force_zlib="no"
	have_zlib="no"

	ZLIBDIR=
	ZLIBINCDIR=
	ZLIBLIBDIR=
	ZLIBLIB=

	AC_ARG_ENABLE(zlib,
		AC_HELP_STRING([--enable-zlib],
		[enable ZLIB trace compression support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_zlib="yes"], [check_zlib="no"])])

	AC_ARG_WITH(zlib-dir,
		AC_HELP_STRING([--with-zlib-dir=ZLIBDIR], [give the path for ZLIB, default: /usr]),
	[ZLIBDIR="$withval/"])

	AC_ARG_WITH(zlib-inc-dir,
		AC_HELP_STRING([--with-zlib-inc-dir=ZLIBINCDIR],
		[give the path for ZLIB-include files, default: ZLIBDIR/include]),
	[ZLIBINCDIR="-I$withval/"],
	[AS_IF([test x"$ZLIBDIR" != x], [ZLIBINCDIR="-I$ZLIBDIR"include/])])

	AC_ARG_WITH(zlib-lib-dir,
		AC_HELP_STRING([--with-zlib-lib-dir=ZLIBLIBDIR],
		[give the path for ZLIB-libraries, default: ZLIBDIR/lib]),
	[ZLIBLIBDIR="-L$withval/"],
	[AS_IF([test x"$ZLIBDIR" != x], [ZLIBLIBDIR="-L$ZLIBDIR"lib/])])

	AC_ARG_WITH(zlib-lib,
		AC_HELP_STRING([--with-zlib-lib=ZLIBLIB], [use given zlib lib, default: -lz]),
	[ZLIBLIB="$withval"])

	AS_IF([test x"$check_zlib" = "xyes"],
        [
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $ZLIBINCDIR"
		AC_CHECK_HEADER([zlib.h], [],
		[
			AC_MSG_NOTICE([error: no zlib.h found; check path for ZLIB package first...])
			zlib_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS

		AS_IF([test x"$ZLIBLIB" = x -a x"$zlib_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $ZLIBLIBDIR -lz"
			AC_MSG_CHECKING([whether linking with -lz works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); ZLIBLIB=-lz],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$ZLIBLIB" = x -a x"$zlib_error" = "xno"],
		[
			AC_MSG_NOTICE([error: no libz found; check path for ZLIB package first...])
			zlib_error="yes"
		])

		AS_IF([test x"$ZLIBLIB" != x -a x"$zlib_error" = "xno"],
		[
			have_zlib="yes"
			AC_DEFINE([HAVE_ZLIB], [1], [Define to 1 if you have the ZLIB.])
		])

		AS_IF([test x"$force_zlib" = "xyes" -a x"$zlib_error" = "xyes"],
		[exit 1])
	])

	AC_SUBST(ZLIBINCDIR)
	AC_SUBST(ZLIBLIBDIR)
	AC_SUBST(ZLIBLIB)
])

