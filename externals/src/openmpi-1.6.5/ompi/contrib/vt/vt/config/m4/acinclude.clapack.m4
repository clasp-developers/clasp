AC_DEFUN([ACVT_CLAPACK],
[
	clapack_error="no"
	have_clapack="no"
	clapack_3thparty=

	CLAPACKDIR=
	CLAPACKINCDIR=
	CLAPACKLIBDIR=
	CLAPACKLIB=

	AC_ARG_WITH(clapack-dir,
		AC_HELP_STRING([--with-clapack-dir=LAPACKDIR], [give the path for CLAPACK, default: /usr]),
	[CLAPACKDIR="$withval/"])

	AC_ARG_WITH(clapack-inc-dir,
		AC_HELP_STRING([--with-clapack-inc-dir=CLAPACKINCDIR],
		[give the path for CLAPACK-include files, default: CLAPACKDIR/include)]),
	[CLAPACKINCDIR="-I$withval/"],
	[AS_IF([test x"$CLAPACKDIR" != x], [CLAPACKINCDIR="-I$CLAPACKDIR"include/])])

	AC_ARG_WITH(clapack-lib-dir,
		AC_HELP_STRING([--with-clapack-lib-dir=CLAPACKLIBDIR],
		[give the path for CLAPACK-libraries, default: CLAPACKDIR/lib]),
	[CLAPACKLIBDIR="-L$withval/"],
	[AS_IF([test x"$CLAPACKDIR" != x], [CLAPACKLIBDIR="-L$CLAPACKDIR"lib/])])

	AC_ARG_WITH(clapack-mkl,
		AC_HELP_STRING([--with-clapack-mkl], [set CLAPACK-lib for MKL]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			CLAPACKLIB="-lmkl -lmkl_lapack"
			clapack_3thparty="mkl"
		])
	])

	AC_ARG_WITH(clapack-acml,
		AC_HELP_STRING([--with-clapack-acml], [set CLAPACK-lib for ACML]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			CLAPACKLIB="-lacml"
			clapack_3thparty="acml"
		])
	])

	AC_ARG_WITH(clapack-essl,
		AC_HELP_STRING([--with-clapack-essl], [set CLAPACK-lib for ESSL]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			CLAPACKLIB="-lessl"
			clapack_3thparty="essl"
		])
	])

	AC_ARG_WITH(clapack-sunperf,
		AC_HELP_STRING([--with-clapack-sunperf], [set CLAPACK-lib for Sun Performace Library]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			CLAPACKLIB="-lsunperf -lfsu -lfui"
			clapack_3thparty="sunperf"
		])
	])

	AC_ARG_WITH(clapack-lib,
		AC_HELP_STRING([--with-clapack-lib=CLAPACKLIB], [use given clapack lib, default: -lclapack -lcblas -lf2c]),
	[CLAPACKLIB="$withval"])

	sav_CPPFLAGS=$CPPFLAGS
	sav_CFLAGS=$CFLAGS
	CPPFLAGS="$CPPFLAGS $CLAPACKINCDIR"
	CFLAGS="$CFLAGS $CLAPACKINCDIR"

	AS_IF([test x"$clapack_3thparty" = "xmkl"],
	[
		AC_CHECK_HEADER([mkl.h],
		[AC_DEFINE([HAVE_MKL], [1], [Define to 1 if you have the MKL.])],
		[
			AC_MSG_NOTICE([error: no mkl.h found; check path for MKL package first...])
			clapack_error="yes"
		])
	])
	AS_IF([test x"$clapack_3thparty" = "xacml"],
	[
		AC_CHECK_HEADER([acml.h],
		[AC_DEFINE([HAVE_ACML], [1], [Define to 1 if you have the ACML.])],
		[
			AC_MSG_NOTICE([error: no acml.h found; check path for ACML package first...])
			clapack_error="yes"
		])
	])
	AS_IF([test x"$clapack_3thparty" = "xessl"],
	[
		AC_CHECK_HEADER([essl.h],
		[AC_DEFINE([HAVE_ESSL], [1], [Define to 1 if you have the ESSL.])],
		[
			AC_MSG_NOTICE([error: no essl.h found; check path for ESSL package first...])
			clapack_error="yes"
		])
	])
	AS_IF([test x"$clapack_3thparty" = "xsunperf"],
	[
		AC_CHECK_HEADER([sunperf.h],
		[AC_DEFINE([HAVE_SUNPERF], [1], [Define to 1 if you have the SUN Performance Library.])],
		[
			AC_MSG_NOTICE([error: no sunperf.h found; check path for SUNPERF package first...])
			clapack_error="yes"
		])
	])
	AS_IF([test x"$clapack_3thparty" = x],
	[
		AC_CHECK_HEADER([f2c.h], [],
		[
			AC_MSG_NOTICE([error: no f2c.h found; check path for CLAPACK package first...])
			clapack_error="yes"
		])

		AS_IF([test x"$clapack_error" = "xno"],
		[
			AC_MSG_CHECKING([for clapack.h])
			AC_TRY_COMPILE(
			[
#include "f2c.h"
#include "clapack.h"
			], [],
			[AC_MSG_RESULT([yes])],
			[
				AC_MSG_RESULT([no])
				AC_MSG_NOTICE([error: no clapack.h found; check path for CLAPACK package first...])
				clapack_error="yes"
			])
		])

		AS_IF([test x"$clapack_error" = "xno"],
		[
			AC_MSG_CHECKING([for blaswrap.h])
			AC_TRY_COMPILE(
			[
#include "f2c.h"
#include "blaswrap.h"
			], [],
			[AC_MSG_RESULT([yes])],
			[
				AC_MSG_RESULT([no])
				AC_MSG_NOTICE([error: no blaswrap.h found; check path for CLAPACK package first...])
				clapack_error="yes"
			])
		])
	])

	CPPFLAGS=$sav_CPPFLAGS
        CFLAGS=$sav_CFLAGS

	AS_IF([test x"$CLAPACKLIB" = x -a x"$clapack_error" = "xno"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $CLAPACKLIBDIR -lclapack -lcblas -lf2c"
		AC_MSG_CHECKING([whether linking with -lclapack -lcblas -lf2c works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); CLAPACKLIB="-lclapack -lcblas -lf2c"], [AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$CLAPACKLIB" = x -a x"$clapack_error" = "xno"],
	[
		AC_MSG_NOTICE([error: either libclapack, libcblas or libf2x not found; check path for CLAPACK package first...])
		clapack_error="yes"
	])

	AS_IF([test x"$clapack_error" = "xno"], [have_clapack="yes"])

	AC_SUBST(CLAPACKINCDIR)
	AC_SUBST(CLAPACKLIBDIR)
	AC_SUBST(CLAPACKLIB)
])

