AC_DEFUN([ACVT_JAVA],
[
	java_error="no"
	check_java="yes"
	force_java="no"
	have_java="no"

	VTJAVALIB=

	AC_REQUIRE([ACVT_PLATFORM])

	AC_ARG_ENABLE(java, 
		AC_HELP_STRING([--enable-java],
		[enable Java support, default: enable if JVMTI found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_java="yes"], [check_java="no"])])

	AS_IF([test x"$check_java" = "xyes"],
	[
		AS_IF([test "$PLATFORM" = "bgp" -o "$PLATFORM" = "bgq"],
		[
			AC_MSG_NOTICE([error: Java tracing not supported on this platform])
			java_error="yes"
		])

		AS_IF([test x"$java_error" = "xno"],
		[
			AC_MSG_CHECKING([whether we can build shared libraries])
			AS_IF([test x"$enable_shared" = "xyes"],
			[AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no]); java_error="yes"])
		])

		AS_IF([test x"$java_error" = "xno"],
		[
			ACVT_JVMTI
			AS_IF([test x"$jvmti_error" = "xyes"],
			[java_error="yes"])
		])

		AS_IF([test x"$java_error" = "xno"],
		[
			AS_IF([test "$PLATFORM" = "macos"],
			[
				VTJAVALIB="libvt-java.jnilib"
			],
			[
				VTJAVALIB="libvt-java$SHREXT"
			])

			have_java="yes"
		])
	])

	AC_SUBST(VTJAVALIB)
])

