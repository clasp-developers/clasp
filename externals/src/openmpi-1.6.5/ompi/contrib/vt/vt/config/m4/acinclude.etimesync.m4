AC_DEFUN([ACVT_ETIMESYNC],
[
	etimesync_error="no"
	check_etimesync="yes"
	force_etimesync="no"
	build_etimesync="no"

	AC_ARG_ENABLE(etimesync, 
		AC_HELP_STRING([--enable-etimesync],
		[enable enhanced time synchronization support, default: enable if LAPACK found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_etimesync="yes"], [check_etimesync="no"])])

	AS_IF([test x"$check_etimesync" = "xyes"],
	[
		AS_IF([test x"$math_error" = x], [ACVT_MATH])
		AS_IF([test x"$have_math" = "xno"], [etimesync_error="yes"])

		AS_IF([test x"$etimesync_error" = "xno"],
		[
			ACVT_CLAPACK
			AS_IF([test x"$have_clapack" = "xyes"],
			[build_etimesync="yes"], [etimesync_error="yes"])
		])
	])
])

