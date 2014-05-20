AC_DEFUN([ACVT_METRICS],
[
	metrics_error="no"
	check_metrics="papi cpc necsx"
	force_metrics="no"
	have_metrics="no"

	AC_REQUIRE([ACVT_PLATFORM])

	AC_ARG_ENABLE(metrics,
		AC_HELP_STRING([--enable-metrics=TYPE],
			[enable support for hardware performance counter (papi,cpc,necsx), default: automatically by configure]),
	[
		AS_IF([test x"$enableval" = "xno"], [check_metrics="no"])
		AS_IF([test x"$enableval" = "xyes"], [force_metrics="yes"])
		AS_IF([test x"$enableval" != "xyes" -a x"$enableval" != "xno"],
		[
			AS_IF([test x"$enableval" != "xpapi" -a x"$enableval" != "xcpc" -a x"$enableval" != "xnecsx"],
			[AC_MSG_ERROR([value of '--enable-metrics' not properly set])])
			check_metrics="$enableval"
			force_metrics="yes"
		])
	])

	AS_IF([test x"$check_metrics" != "xno"],
	[
		for cm in $check_metrics
		do
			AS_IF([test x"$cm" = "xpapi"],
			[
				ACVT_CONF_SUBTITLE([PAPI])
				ACVT_PAPI
				AS_IF([test x"$have_papi" = "xyes"], [have_metrics="yes"; break])
			])
			AS_IF([test x"$cm" = "xcpc"],
			[
				ACVT_CONF_SUBTITLE([CPC])
				ACVT_CPC
				AS_IF([test x"$have_cpc" = "xyes"], [have_metrics="yes"; break])
			])
			AS_IF([test x"$cm" = "xnecsx"],
			[
				ACVT_CONF_SUBTITLE([NEC SX])
				AC_MSG_CHECKING([whether we are on a NEC SX])
				AS_IF([test x"$PLATFORM" = "xnecsx"],
				[
					AC_MSG_RESULT([yes])
					CCASFLAGS="$CCASFLAGS -m"
					have_necsxcntr="yes"
					have_metrics="yes"
					break
				],
				[
					AC_MSG_RESULT([no])
				])
			])
		done

		AS_IF([test x"$have_metrics" = "xno"], [metrics_error="yes"])
	])
])

