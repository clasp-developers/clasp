AC_DEFUN([ACVT_CTOOL],
[
	ctool_error="no"
	have_ctool="no"

	CTOOLDIR=
	CTOOLINCDIR=
	CTOOLLIBDIR=
	CTOOLLIB=

	AC_ARG_WITH(ctool-dir,
		AC_HELP_STRING([--with-ctool-dir=CTOOLDIR],
		[give the path for CTool, default: /usr]),
	[CTOOLDIR="$withval/"])

	AC_ARG_WITH(ctool-inc-dir,
		AC_HELP_STRING([--with-ctool-inc-dir=CTOOLINCDIR],
		[give the path for CTool-include files, default: CTOOLDIR/include]),
	[CTOOLINCDIR="-I$withval/"],
	[AS_IF([test x"$CTOOLDIR" != x], [CTOOLINCDIR="-I$CTOOLDIR"include/])])

	AC_ARG_WITH(ctool-lib-dir,
		AC_HELP_STRING([--with-ctool-lib-dir=CTOOLLIBDIR],
		[give the path for CTool-libraries, default: CTOOLDIR/lib]),
	[CTOOLLIBDIR="-L$withval/"],
	[AS_IF([test x"$CTOOLDIR" != x], [CTOOLLIBDIR="-L$CTOOLDIR"lib/])])

	AC_ARG_WITH(ctool-lib,
		AC_HELP_STRING([--with-ctool-lib=CTOOLLIB], [use given CTool lib, default: -lctool]),
	[CTOOLLIB="$withval"])

	AC_LANG([C++])
	sav_CXX=$CXX; CXX=$CXX_FOR_BUILD
	sav_CXXFLAGS=$CXXFLAGS; CXXFLAGS=$CXXFLAGS_FOR_BUILD
	sav_CXXCPP=$CXXCPP; CXXCPP=$CXXCPP_FOR_BUILD
	sav_CPPFLAGS=$CPPFLAGS; CPPFLAGS=$CPPFLAGS_FOR_BUILD
	sav_LDFLAGS=$LDFLAGS; LDFLAGS=$LDFLAGS_FOR_BUILD
	sav_LIBS=$LIBS; LIBS=$LIBS_FOR_BUILD

	sav2_CPPFLAGS=$CPPFLAGS
	CPPFLAGS="$CPPFLAGS $CTOOLINCDIR"
        AC_CHECK_HEADER([ctool/ctool.h], [],
	[
		AC_MSG_NOTICE([error: no ctool/ctool.h found; check path for CTool package first...])
		ctool_error="yes"
	])
	CPPFLAGS=$sav2_CPPFLAGS

	AS_IF([test x"$CTOOLLIB" = x -a x"$ctool_error" = "xno"],
	[
		sav2_LIBS=$LIBS
		LIBS="$LIBS $CTOOLLIBDIR -lctool"
		AC_MSG_CHECKING([whether linking with -lctool works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); CTOOLLIB=-lctool],[AC_MSG_RESULT([no])])
		LIBS=$sav2_LIBS
	])

	CXX=$sav_CXX
	CXXFLAGS=$sav_CXXFLAGS
	CXXCPP=$sav_CXXCPP
	CPPFLAGS=$sav_CPPFLAGS
	LDFLAGS=$sav_LDFLAGS
	LIBS=$sav_LIBS
	AC_LANG([C])

	AS_IF([test x"$CTOOLLIB" = x -a x"$ctool_error" = "xno"],
	[
		AC_MSG_NOTICE([error: no libctool found; check path for CTool package first...])
		ctool_error="yes"
	])

	AS_IF([test x"$ctool_error" = "xno"], [have_ctool="yes"])

	AC_SUBST(CTOOLINCDIR)
	AC_SUBST(CTOOLLIBDIR)
	AC_SUBST(CTOOLLIB)
])

