AC_DEFUN([ACVT_RUN],
[
	vtrun_error="no"
	check_vtrun="yes"
	force_vtrun="no"
	build_vtrun="no"

	VT_RUN_MACOS=0
	VT_RUN_HAVE_THREADS=0
	VT_RUN_HAVE_MPI=0
	VT_RUN_HAVE_FMPI=0
	VT_RUN_HAVE_DYNINST=0
	VT_RUN_NM=
	VT_RUN_VTLIB=
	VT_RUN_VTMPILIB=
	VT_RUN_VTMTLIB=
	VT_RUN_VTHYBLIB=
	VT_RUN_FMPILIB=
	VT_RUN_DYNATTLIB=

	AC_REQUIRE([ACVT_PLATFORM])
	AC_REQUIRE([ACVT_MPI])
	AC_REQUIRE([ACVT_THREADS])
	AC_REQUIRE([ACVT_DYNINST])

	AC_ARG_ENABLE(vtrun,
		AC_HELP_STRING([--enable-vtrun],
		[build vtrun - an application execution wrapper script, default: enable if shared libraries are supported]),
	[AS_IF([test x"$enableval" = "xyes"], [force_vtrun="yes"], [check_vtrun="no"])])

	AS_IF([test x"$check_vtrun" = "xyes"],
	[
		AS_IF([test "$PLATFORM" = "bgl" -o "$PLATFORM" = "bgp" -o "$PLATFORM" = "bgq"],
		[
			AC_MSG_NOTICE([error: application execution wrapper not supported on this platform])
			vtrun_error="yes"
		])

		AS_IF([test x"$vtrun_error" = "xno"],
		[
			AC_MSG_CHECKING([whether we can build shared libraries])
			AS_IF([test x"$enable_shared" = "xyes"],
			[AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no]); vtrun_error="yes"])
		])

		AS_IF([test x"$vtrun_error" = "xno"],
		[
			AC_MSG_NOTICE([setup capabilities of vtrun])

			AS_IF([test $PLATFORM        = "macos"], [VT_RUN_MACOS=1])
			AS_IF([test x"$have_threads" = "xyes"], [VT_RUN_HAVE_THREADS=1])
			AS_IF([test x"$have_mpi"     = "xyes"], [VT_RUN_HAVE_MPI=1])
			AS_IF([test x"$have_fmpi"    = "xyes"], [VT_RUN_HAVE_FMPI=1])
			AS_IF([test x"$have_dyninst" = "xyes" -a x"$build_dynattlib" = "xyes"], [VT_RUN_HAVE_DYNINST=1])
			VT_RUN_NM=`echo $NM | sed s/--line-numbers//g`
			VT_RUN_VTLIB="libvt$SHREXT"
			VT_RUN_VTMPILIB="libvt-mpi$SHREXT"
			VT_RUN_VTMTLIB="libvt-mt$SHREXT"
			VT_RUN_VTHYBLIB="libvt-hyb$SHREXT"
			AS_IF([test x"$build_fmpiwraplib" = "xyes"], [VT_RUN_FMPILIB="libvt-fmpi$SHREXT"])
			VT_RUN_DYNATTLIB="libvt-dynatt$SHREXT"

			build_vtrun=yes
		])
	])

	AC_SUBST(VT_RUN_MACOS)
	AC_SUBST(VT_RUN_HAVE_THREADS)
	AC_SUBST(VT_RUN_HAVE_MPI)
	AC_SUBST(VT_RUN_HAVE_FMPI)
	AC_SUBST(VT_RUN_HAVE_DYNINST)
	AC_SUBST(VT_RUN_NM)
	AC_SUBST(VT_RUN_VTLIB)
	AC_SUBST(VT_RUN_VTMPILIB)
	AC_SUBST(VT_RUN_VTMTLIB)
	AC_SUBST(VT_RUN_VTHYBLIB)
	AC_SUBST(VT_RUN_FMPILIB)
	AC_SUBST(VT_RUN_DYNATTLIB)
])

