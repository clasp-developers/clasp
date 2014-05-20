AC_DEFUN([ACVT_CROSS],
[
	CROSS_PREFIX=

	comp_for_build_given="no"

	AC_ARG_VAR(CC_FOR_BUILD, [C compiler command for build system])
	AC_ARG_VAR(CFLAGS_FOR_BUILD, [C compiler flags for build system])
	AC_ARG_VAR(CPP_FOR_BUILD, [C preprocessor for build system])
	AC_ARG_VAR(CXX_FOR_BUILD, [C++ compiler command for build system])
	AC_ARG_VAR(CXXFLAGS_FOR_BUILD, [C++ compiler flags for build system])
	AC_ARG_VAR(CXXCPP_FOR_BUILD, [C++ preprocessor for build system])
	AC_ARG_VAR(CPPFLAGS_FOR_BUILD, [C/C++/Objective C preprocessor flags for build system])
	AC_ARG_VAR(LDFLAGS_FOR_BUILD, [linker flags for build system])
	AC_ARG_VAR(LIBS_FOR_BUILD, [libraries to pass to the linker for build system])

	AS_IF([test x"$cross_compiling" != "xyes"],
	[
		# reset *_FOR_BUILD variables, if no cross compiling
		CC_FOR_BUILD=$CC; CFLAGS_FOR_BUILD=$CFLAGS; CPP_FOR_BUILD=$CPP
		CXX_FOR_BUILD=$CXX; CXXFLAGS_FOR_BUILD=$CXXFLAGS; CXXCPP_FOR_BUILD=$CXXCPP
		CPPFLAGS_FOR_BUILD=$CPPFLAGS; LDFLAGS_FOR_BUILD=$LDFLAGS; LIBS_FOR_BUILD=$LIBS
	],
	[
		AC_MSG_CHECKING([for C compiler for build system ($build)])
		AS_IF([test x"$CC_FOR_BUILD" = x],
		[AC_MSG_RESULT([$CC])], [AC_MSG_RESULT([$CC_FOR_BUILD])])
		AC_MSG_CHECKING([for C++ compiler for build system ($build)])
		AS_IF([test x"$CXX_FOR_BUILD" = x],
		[AC_MSG_RESULT([$CXX])], [AC_MSG_RESULT([$CXX_FOR_BUILD])])

		AS_IF([test x"$CC_FOR_BUILD" = x -a x"$CXX_FOR_BUILD" != x],
		[
			AC_MSG_ERROR([no C compiler command for build system given
Set \`CC_FOR_BUILD' to the C compiler of build system.])
		])
		AS_IF([test x"$CXX_FOR_BUILD" = x -a x"$CC_FOR_BUILD" != x],
		[
			AC_MSG_ERROR([no C++ compiler command for build system given
Set \`CXX_FOR_BUILD' to the C++ compiler of build system.])
		])
		AS_IF([test x"$CC_FOR_BUILD" != x -a x"$CXX_FOR_BUILD" != x],
		[
			comp_for_build_given="yes"
		])
		AS_IF([test x"$CC_FOR_BUILD" = x -a x"$CXX_FOR_BUILD" = x],
		[
			AC_MSG_NOTICE([no C/C++ compiler command for build system given
In cross compile mode, it's recommended to build the compiler wrappers and OPARI for the build system. Set \`CC_FOR_BUILD' and \`CXX_FOR_BUILD' to the C/C++ compiler of build system.])
			CC_FOR_BUILD=$CC
			CXX_FOR_BUILD=$CXX
		])

		AS_IF([test x"$CFLAGS_FOR_BUILD" = x], [CFLAGS_FOR_BUILD=$CFLAGS])
		AS_IF([test x"$CXXFLAGS_FOR_BUILD" = x], [CXXFLAGS_FOR_BUILD=$CXXFLAGS])
		AS_IF([test x"$CPPFLAGS_FOR_BUILD" = x], [CPPFLAGS_FOR_BUILD=$CPPFLAGS])
		AS_IF([test x"$LDFLAGS_FOR_BUILD" = x], [LDFLAGS_FOR_BUILD=$LDFLAGS])
		AS_IF([test x"$LIBS_FOR_BUILD" = x], [LIBS_FOR_BUILD=$LIBS])

		AS_IF([test x"$comp_for_build_given" = "xyes"],
		[
			sav_CPPFLAGS=$CPPFLAGS; sav_LDFLAGS=$LDFLAGS; sav_LIBS=$LIBS
			CPPFLAGS=$CPPFLAGS_FOR_BUILD; LDFLAGS=$LDFLAGS_FOR_BUILD; LIBS=$LIBS_FOR_BUILD

			sav_CC=$CC; sav_CFLAGS=$CFLAGS; sav_CPP=$CPP
			CC=$CC_FOR_BUILD; CFLAGS=$CFLAGS_FOR_BUILD
			AC_MSG_CHECKING([whether the C compiler for build system works])
			AC_TRY_LINK([], [],
			[AC_MSG_RESULT([yes])],
			[AC_MSG_ERROR([C compiler for build system cannot create executables
See \`config.log' for more details.])])

			AS_IF([test x"$CPP_FOR_BUILD" = x],
			[
				unset CPP
				unset ac_cv_prog_CPP # clear cache variable for CPP
				AC_PROG_CPP
				CPP_FOR_BUILD=$CPP
			])
			CC=$sav_CC; CFLAGS=$sav_CFLAGS; CPP=$sav_CPP

			AC_LANG([C++])
			sav_CXX=$CXX; sav_CXXFLAGS=$CXXFLAGS; sav_CXXCPP=$CXXCPP
			CXX=$CXX_FOR_BUILD; CXXFLAGS=$CXXFLAGS_FOR_BUILD
			AC_MSG_CHECKING([whether the C++ compiler for build system works])
			AC_TRY_LINK([], [],
			[AC_MSG_RESULT([yes])],
			[AC_MSG_ERROR([C++ compiler for build system cannot create executables
See \`config.log' for more details.])])

			AS_IF([test x"$CXXCPP_FOR_BUILD" = x],
			[
				unset CXXCPP
				unset ac_cv_prog_CXXCPP # clear cache variable for CXXCPP
				AC_PROG_CXXCPP
				CXXCPP_FOR_BUILD=$CXXCPP
			])
			CXX=$sav_CXX; CXXFLAGS=$sav_CXXFLAGS; CXXCPP=$sav_CXXCPP
			AC_LANG([C])

			CPPFLAGS=$sav_CPPFLAGS; LDFLAGS=$sav_LDFLAGS; LIBS=$sav_LIBS
		])
	])

	# Check prefix for cross-tool's executables
	AC_ARG_WITH(cross-prefix,
		AC_HELP_STRING([--with-cross-prefix=PREFIX],
		[prefix for cross-tool's executables (e.g. PREFIXvtcc, PREFIXvtfort), default: cross-]),
	[
		AS_IF([test x"$cross_compiling" = "xyes" -a x"$comp_for_build_given" = "xyes"],
		[
			AS_IF([test x"$withval" = "xyes"], [CROSS_PREFIX="cross-"],
			[AS_IF([test x"$withval" != "xno"], [CROSS_PREFIX="$withval"])])
		])
	])

	AC_SUBST(CROSS_PREFIX)
])

