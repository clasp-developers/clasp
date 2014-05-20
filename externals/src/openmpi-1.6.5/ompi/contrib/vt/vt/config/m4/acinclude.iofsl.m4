AC_DEFUN([ACVT_IOFSL],
[
	iofsl_error="no"
	check_iofsl="yes"
        force_iofsl="no"
	have_iofsl="no"

	zoidfs_lib_default="-lzoidfsclient -lzoidfs-util -lzoidfshints -liofsl-c-util"
	bmi_lib_default="-lbmi -lpthread"

	ZOIDFSDIR=
	ZOIDFSINCDIR=
	ZOIDFSLIBDIR=
	ZOIDFSLIB=

	BMIDIR=
	BMIINCDIR=
	BMILIBDIR=
	BMILIB=

	IOFSLSCRIPTS=

	AC_ARG_ENABLE(iofsl,
		AC_HELP_STRING([--enable-iofsl],
		[enable IOFSL support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_iofsl="yes"], [check_iofsl="no"])])

	AC_ARG_WITH(zoidfs-dir,
		AC_HELP_STRING([--with-zoidfs-dir=ZOIDFSDIR], [give the path for ZOIDFS, default: /usr]),
	[ZOIDFSDIR="$withval/"])

	AC_ARG_WITH(zoidfs-inc-dir,
		AC_HELP_STRING([--with-zoidfs-inc-dir=ZOIDFSINCDIR],
		[give the path for ZOIDFS-include files, default: ZOIDFSDIR/include]),
	[ZOIDFSINCDIR="-I$withval/"],
	[AS_IF([test x"$ZOIDFSDIR" != x], [ZOIDFSINCDIR="-I$ZOIDFSDIR"include/])])

	AC_ARG_WITH(zoidfs-lib-dir,
		AC_HELP_STRING([--with-zoidfs-lib-dir=ZOIDFSLIBDIR],
		[give the path for ZOIDFS-libraries, default: ZOIDFSDIR/lib]),
	[ZOIDFSLIBDIR="-L$withval/"],
	[AS_IF([test x"$ZOIDFSDIR" != x], [ZOIDFSLIBDIR="-L$ZOIDFSDIR"lib/])])

	AC_ARG_WITH(zoidfs-lib,
		AC_HELP_STRING([--with-zoidfs-lib=ZOIDFSLIB], [use given zoidfs lib, default: $zoidfs_lib_default BMILIBDIR BMILIB]),
	[ZOIDFSLIB="$withval"])

	AC_ARG_WITH(bmi-dir,
		AC_HELP_STRING([--with-bmi-dir=BMIDIR], [give the path for BMI, default: ZOIDFSDIR]),
	[BMIDIR="$withval/"])

	AC_ARG_WITH(bmi-inc-dir,
		AC_HELP_STRING([--with-bmi-inc-dir=BMIINCDIR],
		[give the path for BMI-include files, default: BMIDIR/include]),
	[BMIINCDIR="-I$withval/"],
	[AS_IF([test x"$BMIDIR" != x], [BMIINCDIR="-I$BMIDIR"include/])])

	AC_ARG_WITH(bmi-lib-dir,
		AC_HELP_STRING([--with-bmi-lib-dir=BMILIBDIR],
		[give the path for BMI-libraries, default: BMIDIR/lib]),
	[BMILIBDIR="-L$withval/"],
	[AS_IF([test x"$BMIDIR" != x], [BMILIBDIR="-L$BMIDIR"lib/])])

	AC_ARG_WITH(bmi-lib,
		AC_HELP_STRING([--with-bmi-lib=BMILIB], [use given bmi lib, default: $bmi_lib_default]),
	[BMILIB="$withval"])

	AC_ARG_ENABLE(iofsl-scripts,
                AC_HELP_STRING([--enable-iofsl-scripts=PLATFORM],
		[build IOFSL scripts for given platform, possible values: crayxk6, default: disabled]),
        [
		AS_IF([test x"$enableval" != "xno"],
		[
			AS_IF([test x"$enableval" != "xcrayxk6"],
			[AC_MSG_ERROR([value of '--enable-iofsl-scripts' not properly set])],
			[IOFSLSCRIPTS=$enableval])
		])
	])

	AS_IF([test x"$check_iofsl" = "xyes"],
        [
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $ZOIDFSINCDIR $BMIINCDIR"
		AC_CHECK_HEADER([bmi.h], [],
		[
			AC_MSG_NOTICE([error: no bmi.h found; check path for BMI package first...])
			iofsl_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS

		AS_IF([test x"$BMILIB" = x -a x"$iofsl_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $ZOIDFSLIBDIR $BMILIBDIR $bmi_lib_default"
			AC_MSG_CHECKING([whether linking with $bmi_lib_default works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); BMILIB=$bmi_lib_default],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$BMILIB" = x -a x"$iofsl_error" = "xno"],
		[
			AC_MSG_NOTICE([error: no libbmi found; check path for BMI package first...])
			iofsl_error="yes"
		])

		AS_IF([test x"$iofsl_error" = "xno"],
		[
			sav_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$CPPFLAGS $ZOIDFSINCDIR"
			AC_CHECK_HEADER([zoidfs.h], [],
			[
				AC_MSG_NOTICE([error: no zoidfs.h found; check path for ZOIDFS package first...])
				iofsl_error="yes"
			])
			CPPFLAGS=$sav_CPPFLAGS

			AS_IF([test x"$ZOIDFSLIB" = x -a x"$iofsl_error" = "xno"],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $ZOIDFSLIBDIR $zoidfs_lib_default $BMILIBDIR $BMILIB"
				AC_MSG_CHECKING([whether linking with $zoidfs_lib_default $BMILIBDIR $BMILIB works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); ZOIDFSLIB=$zoidfs_lib_default],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])

			AS_IF([test x"$ZOIDFSLIB" = x -a x"$iofsl_error" = "xno"],
			[
				AC_MSG_NOTICE([error: no zoidfs library found; check path for ZOIDFS package first...])
				iofsl_error="yes"
			])
		])

		AS_IF([test x"$iofsl_error" = "xno"],
		[
			have_iofsl="yes"
			AC_DEFINE([HAVE_IOFSL], [1], [Define to 1 if VT is configured with IOFSL support.])
		],
		[
			AS_IF([test x"$force_iofsl" = "xyes"], [exit 1])
		])
	])

	AC_SUBST(ZOIDFSDIR)
	AC_SUBST(ZOIDFSINCDIR)
	AC_SUBST(ZOIDFSLIBDIR)
	AC_SUBST(ZOIDFSLIB)
	AC_SUBST(BMIDIR)
	AC_SUBST(BMIINCDIR)
	AC_SUBST(BMILIBDIR)
	AC_SUBST(BMILIB)
	AC_SUBST(IOFSLSCRIPTS)
])

