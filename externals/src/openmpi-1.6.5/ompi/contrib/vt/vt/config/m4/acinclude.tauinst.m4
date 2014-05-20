AC_DEFUN([ACVT_TAUINST],
[
	tauinst_error="no"
	check_tauinst="yes"
	force_tauinst="no"
	have_tauinst="no"

	tauinst_cmd=
	tauinst_cparse_cmd=
	tauinst_cxxparse_cmd=
	tauinst_fparse_cmd=

	AC_ARG_ENABLE(tauinst,
		AC_HELP_STRING([--enable-tauinst],
		[enable support for automatic source code instrumentation by using TAU, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_tauinst="yes"], [check_tauinst="no"])])

	AC_ARG_WITH(tau-instrumentor,
		AC_HELP_STRING([--with-tau-instrumentor=TAUINSTUMENTOR],
		[give the command for the TAU instrumentor, default: tau_instrumentor]),
	[
		AS_IF([test x"$withval" = "xyes" -o x"$withval" = "xno"],
		[AC_MSG_ERROR([value of '--with-tau-instrumentor' not properly set!])])
		tauinst_cmd=$withval
	])

	AC_ARG_WITH(pdt-cparse,
		AC_HELP_STRING([--with-pdt-cparse=PDTCPARSE],
		[give the command for PDT C source code parser, default: cparse]),
	[
		AS_IF([test x"$withval" = "xyes" -o x"$withval" = "xno"],
		[AC_MSG_ERROR([value of '--with-pdt-cparse' not properly set!])])
		tauinst_cparse_cmd=$withval
	])

	AC_ARG_WITH(pdt-cxxparse,
		AC_HELP_STRING([--with-pdt-cxxparse=PDTCXXPARSE],
		[give the command for PDT C++ source code parser, default: cxxparse]),
	[
		AS_IF([test x"$withval" = "xyes" -o x"$withval" = "xno"],
		[AC_MSG_ERROR([value of '--with-pdt-cxxparse' not properly set!])])
		tauinst_cxxparse_cmd=$withval
	])

	AC_ARG_WITH(pdt-fparse,
		AC_HELP_STRING([--with-pdt-fparse=PDTFPARSE],
		[give the command for PDT Fortran source code parser, default: f95parse, f90parse, or gfparse]),
	[
		AS_IF([test x"$withval" = "xyes" -o x"$withval" = "xno"],
		[AC_MSG_ERROR([value of '--with-pdt-fparse' not properly set!])])
		tauinst_fparse_cmd=$withval
	])

	AS_IF([test "$check_tauinst" = "yes"],
	[
		AC_CHECK_PROG(tauinst_cmd, tau_instrumentor, tau_instrumentor)
		AS_IF([test x"$tauinst_cmd" = x],
		[
			AC_MSG_NOTICE([error: no tau_instrumentor found; check path for PDToolkit first...])
			tauinst_error="yes"
		])

		AS_IF([test x"$tauinst_error" = "xno"],
		[
			AC_CHECK_PROG(tauinst_cparse_cmd, cparse, cparse)
			AS_IF([test x"$tauinst_cparse_cmd" = x],
			[AC_MSG_WARN([no cparse found; C source code cannot be instrumented by TAU])])
			AC_CHECK_PROG(tauinst_cxxparse_cmd, cxxparse, cxxparse)
			AS_IF([test x"$tauinst_cxxparse_cmd" = x],
			[AC_MSG_WARN([no cxxparse found; C++ source code cannot be instrumented by TAU])])
			AS_IF([test x"$FC" != x],
			[
				AC_CHECK_PROGS(tauinst_fparse_cmd, f95parse f90parse gfparse)
				AS_IF([test x"$tauinst_fparse_cmd" = x],
				[AC_MSG_WARN([no f95parse, f90parse, or gfparse found; Fortran source code cannot be instrumented by TAU])])
			],
			[
				tauinst_fparse_cmd=
			])

			AS_IF([test x"$tauinst_cparse_cmd$tauinst_cxxparse_cmd$tauinst_fparse_cmd" = x],
			[
				AC_MSG_NOTICE([error: no PDT source code parser command found; check path for PDToolkit first...])
				tauinst_error="yes"
			])
		])

		AS_IF([test x"$tauinst_error" = "xno"],
		[have_tauinst="yes"])
	])
])

