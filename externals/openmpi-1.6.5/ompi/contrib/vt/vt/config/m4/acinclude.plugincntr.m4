AC_DEFUN([ACVT_PLUGINCNTR],
[
	plugin_cntr_error="no"
	check_plugin_cntr="yes"
	force_plugin_cntr="no"
	have_plugin_cntr="no"

	AC_REQUIRE([ACVT_PLATFORM])

	AC_ARG_ENABLE(plugincntr,
		AC_HELP_STRING([--enable-plugincntr], [enable plugin counter support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_plugin_cntr="yes"], [check_plugin_cntr="no"])])
	
	AS_IF([test x"$check_plugin_cntr" = "xyes"],
	[
		AS_IF([test x"$enable_shared" = "xno"],
		[
			case $PLATFORM in
				bgp | bgq)
					AC_MSG_NOTICE([error: the plugin counter support requires building of shared libraries on this platform; re-configure with \`--enable-shared'])
					plugin_cntr_error="yes"
					;;
			esac
		])

		AS_IF([test x"$plugin_cntr_error" = "xno"],
		[
			ACVT_DL
			AS_IF([test x"$have_dl" = "xyes"],
			[have_plugin_cntr="yes"], [plugin_cntr_error="yes"])
		])
	])
])

