AC_DEFUN([ACVT_UNIMCI],
[
	unimci_error="no"
	check_unimci="yes"
	force_unimci="no"
	have_unimci="no"

	unimci_config_cmd=
	unimci_checker_name=
	unimci_checker_version=
	unimci_cc=
	unimci_cxx=
	unimci_fc=

	UNIMCI_CFLAGS=
	UNIMCI_CLIBS=
	UNIMCI_CXXFLAGS=
	UNIMCI_CXXLIBS=
	UNIMCI_FCFLAGS=
	UNIMCI_FCLIBS=
	UNIMCI_LDFLAGS=

	AC_ARG_ENABLE(mpicheck, 
		AC_HELP_STRING([--enable-mpicheck],
		[enable support for Universal MPI Correctness Interface (UniMCI), default: enable if unimci-config found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_unimci="yes"], [check_unimci="no"])])

	AC_ARG_WITH(unimci-config,
		AC_HELP_STRING([--with-unimci-config=UNIMCICONFIG],
		[give the command for UniMCI config utility, default: unimci-config]),
	[
		AS_IF([test x"$withval" = "xyes" -o x"$withval" = "xno"],
		[AC_MSG_ERROR([value of '--with-unimci-config' not properly set!])])
		unimci_config_cmd=$withval
	])

	AS_IF([test x"$check_unimci" = "xyes"],
	[
		AS_IF([test x"$inside_openmpi" = "xyes"],
		[
			AC_MSG_NOTICE([error: MPI Correctness Checking support cannot be built inside Open MPI])
			unimci_error="yes"
		])

dnl		check for unimci-config

		AS_IF([test x"$unimci_error" = "xno"],
		[
			AC_CHECK_PROG(unimci_config_cmd, unimci-config, unimci-config)
			AS_IF([test x"$unimci_config_cmd" = x],
			[
				AC_MSG_NOTICE([error: no unimci-config found; check path for UniMCI package first...])
				unimci_error="yes"
			])
		])

dnl		check for version

		AS_IF([test x"$unimci_error" = "xno"],
		[
			AC_MSG_CHECKING([for UniMCI's version])
			unimci_version=`eval "$unimci_config_cmd --version"`
			AS_IF([test "$?" = "0"],
			[
				AC_MSG_RESULT([$unimci_version])
				unimci_major_version=`echo $unimci_version | cut -d '.' -f 1`
				AS_IF([test $unimci_major_version -ne 1],
				[
					AC_MSG_NOTICE([error: the version of UniMCI isn't supported])
					unimci_error="yes"
				])
			],
			[
				unimci_error="yes"
			])
		])

dnl		check for UniMCI's checker name

		AS_IF([test x"$unimci_error" = "xno"],
		[
			AC_MSG_CHECKING([for UniMCI's checker name])
			unimci_checker_name=`eval "$unimci_config_cmd --checker-name"`
			AS_IF([test "$?" = "0"],
			[AC_MSG_RESULT([$unimci_checker_name])], [unimci_error="yes"])
		])

dnl		check for UniMCI's checker version

		AS_IF([test x"$unimci_error" = "xno"],
		[
			AC_MSG_CHECKING([for UniMCI's checker version])
			unimci_checker_version=`eval "$unimci_config_cmd --checker-version"`
			AS_IF([test "$?" = "0"],
			[AC_MSG_RESULT([$unimci_checker_version])], [unimci_error="yes"])
		])

dnl		check for C support

		AS_IF([test x"$unimci_error" = "xno"],
		[
			AC_MSG_CHECKING([whether UniMCI's checker supports C])
			unimci_cc=`eval "$unimci_config_cmd --cc"`
			AS_IF([test "$?" = "0"],
			[
				AS_IF([test x"$unimci_cc" != x],
				[
					AC_MSG_RESULT([yes])

dnl					check for C compiler flags and libs

					AC_MSG_CHECKING([for UniMCI's C compiler flags])
					UNIMCI_CFLAGS=`eval "$unimci_config_cmd --cflags"`
					AS_IF([test "$?" = "0"],
					[
						AC_MSG_RESULT([$UNIMCI_CFLAGS])
						AC_MSG_CHECKING([for UniMCI's C libs])
						UNIMCI_CLIBS=`eval "$unimci_config_cmd --clib-dirs --clibs"`
						AS_IF([test "$?" = "0"],
						[AC_MSG_RESULT([$UNIMCI_CLIBS])], [unimci_error="yes"])
					], [unimci_error="yes"])
				], [AC_MSG_RESULT([no])])
			], [unimci_error="yes"])
		])

dnl		check for C++ support

		AS_IF([test x"$unimci_error" = "xno"],
		[
			AC_MSG_CHECKING([whether UniMCI's checker supports C++])
			unimci_cxx=`eval "$unimci_config_cmd --cxx"`
			AS_IF([test "$?" = "0"],
			[
				AS_IF([test x"$unimci_cxx" != x],
				[
					AC_MSG_RESULT([yes])

dnl					check for C++ compiler flags and libs

					AC_MSG_CHECKING([for UniMCI's C++ compiler flags])
					UNIMCI_CXXFLAGS=`eval "$unimci_config_cmd --cxxflags"`
					AS_IF([test "$?" = "0"],
					[
						AC_MSG_RESULT([$UNIMCI_CXXFLAGS])
						AC_MSG_CHECKING([for UniMCI's C++ libs])
						UNIMCI_CXXLIBS=`eval "$unimci_config_cmd --cxxlib-dirs --cxxlibs"`
						AS_IF([test "$?" = "0"],
						[AC_MSG_RESULT([$UNIMCI_CXXLIBS])], [unimci_error="yes"])
					], [unimci_error="yes"])
				], [AC_MSG_RESULT([no])])
			], [unimci_error="yes"])
		])

dnl		check for Fortran support

		AS_IF([test x"$unimci_error" = "xno" -a x"$FC" != x],
		[
			AC_MSG_CHECKING([whether UniMCI's checker supports Fortran])
			unimci_fc=`eval "$unimci_config_cmd --f90"`
			AS_IF([test "$?" = "0"],
			[
				AS_IF([test x"$unimci_fc" != x],
				[
					AC_MSG_RESULT([yes])

dnl					check for Fortran compiler flags and libs

					AC_MSG_CHECKING([for UniMCI's Fortran compiler flags])
					UNIMCI_FCFLAGS=`eval "$unimci_config_cmd --f90flags"`
					AS_IF([test "$?" = "0"],
					[
						AC_MSG_RESULT([$UNIMCI_FCFLAGS])
						AC_MSG_CHECKING([for UniMCI's Fortran libs])
						UNIMCI_FCLIBS=`eval "$unimci_config_cmd --f90lib-dirs --f90libs"`
						AS_IF([test "$?" = "0"],
						[AC_MSG_RESULT([$UNIMCI_FCLIBS])], [unimci_error="yes"])
					], [unimci_error="yes"])
				], [AC_MSG_RESULT([no])])
			], [unimci_error="yes"])
		])

		AS_IF([test x"$unimci_cc" = x -a x"$unimci_cxx" = x -a x"$unimci_fc" = x],
		[unimci_error="yes"])

dnl		check for linker flags

		AS_IF([test x"$unimci_error" = "xno"],
		[
			AC_MSG_CHECKING([for UniMCI's linker flags])
			UNIMCI_LDFLAGS=`eval "$unimci_config_cmd --ldflags"`
			AS_IF([test "$?" = "0"],
			[
				AC_MSG_RESULT([$UNIMCI_LDFLAGS])
				have_unimci="yes"
			],
			[
				unimci_error="yes"
			])
		])

dnl		check for MPI Fortran interoperability

		AS_IF([test x"$unimci_error" = "xno" -a x"$have_fmpi" = "xyes" -a x"$build_fmpiwraplib" = "xno"],
		[
			ACVT_CONF_SUBSUBTITLE([MPI Fortran interoperability])
			ACVT_FMPIWRAPLIB
			AS_IF([test x"$fmpiwraplib_error" = "xno"],
			[
				build_fmpiwraplib="yes"
				FMPILIB="-lvt-fmpi"
			],
			[
				AC_MSG_NOTICE([error: MPI Fortran interoperability checks failed])
				unimci_error = "yes"
			])
		])

		AS_IF([test x"$unimci_error" = "xno"],
		[
			AC_DEFINE_UNQUOTED([UNIMCI_CHECKER_NAME],
			["$unimci_checker_name"], [UniMCI's checker name])
			AC_DEFINE_UNQUOTED([UNIMCI_CHECKER_VERSION],
			["$unimci_checker_version"], [UniMCI's checker version])

			have_unimci="yes"
		])
	])

	AC_SUBST(UNIMCI_CFLAGS)
	AC_SUBST(UNIMCI_CLIBS)
	AC_SUBST(UNIMCI_CXXFLAGS)
	AC_SUBST(UNIMCI_CXXLIBS)
	AC_SUBST(UNIMCI_FCFLAGS)
	AC_SUBST(UNIMCI_FCLIBS)
	AC_SUBST(UNIMCI_LDFLAGS)
])

