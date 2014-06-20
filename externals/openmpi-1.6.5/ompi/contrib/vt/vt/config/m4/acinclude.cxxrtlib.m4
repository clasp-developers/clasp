AC_DEFUN([ACVT_CXXRTLIB],
[
	cxxrtlib_error="no"

	cxxrtlib=

	AC_ARG_WITH(cxxrtlib,
		AC_HELP_STRING([--with-cxxrtlib=CXXRTLIB],
		[use given C++ runtime library, default: automatically by configure]),
	[
		AS_IF([test x"$withval" = "xyes" -o x"$withval" = "xno"],
		[AC_MSG_ERROR([value of '--with-cxxrtlib' not properly set])])
		cxxrtlib=$withval
	])

	AC_MSG_CHECKING([for C++ runtime library])

	AS_IF([test x"$cxxrtlib" = x],
	[
		AC_LANG([C++])
		AC_TRY_COMPILE([#include <list>],
[
  std::list<int> l;
  l.push_back(100);
],
		[cp -f conftest.$ac_objext conftest_cxxrtlib.$ac_objext],
		[cxxrtlib_error="yes"])
		AC_LANG([C])

		libs_to_check="-lstdc++ \
                               -lCrun -lCstd \
                               -lstlport \
                               -libmc++;-lstdc++;-lm;-lc \
                               -lxlopt;-lxl;-libmc++;-lstdc++;-lm;-lc \
                               -lC \
                               -lstd;-lC \
                               -lstd;-lC;-lzceh \
                               -lstdz;-lCz;-lzceh \
                               -lstdz;-lCz;-lzceh;-lgcc_eh \
                               -lstdc++;-lm;-lc \
                               -limf;-lm;-lipgo;-lstdc++;-lirc;-lipr;-lirc;-lc;-lirc_s;-ldl;-lc \
                               -lC++ \
                               -lcray-c++-rts;-lcraystdc++"

		AS_IF([test x"$cxxrtlib_error" = "xno"],
		[
			for lib in $libs_to_check
			do
				lib=`echo $lib | sed s/\;/\ /g`

				eval "$CC -o conftest_cxxrtlib$ac_exeext $CFLAGS $LDFLAGS conftest_cxxrtlib.$ac_objext $LIBS $lib >/dev/null 2>&1"
				AS_IF([test x"$?" = "x0"],
				[
					cxxrtlib="$lib"
					break
				])
			done
			rm -f conftest_cxxrtlib*

			AS_IF([test x"$cxxrtlib" = x], [cxxrtlib_error="yes"])
		])
	])

	AS_IF([test x"$cxxrtlib_error" = "xno"],
	[AC_MSG_RESULT([$cxxrtlib])], [AC_MSG_RESULT([unknown])])
])

