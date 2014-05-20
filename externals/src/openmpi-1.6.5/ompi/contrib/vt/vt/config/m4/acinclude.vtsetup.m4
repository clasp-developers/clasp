AC_DEFUN([ACVT_SETUP],
[
	vtsetup_error="no"
	check_vtsetup="yes"
	force_vtsetup="no"
	build_vtsetup="no"

	VT_SETUP_NM=
	VT_SETUP_COMPINST_GNU=0
	VT_SETUP_COMPRESSION=0
	VT_SETUP_CPUIDTRACE=0
	VT_SETUP_DYNINST=0
	VT_SETUP_ETIMESYNC=0
	VT_SETUP_IOTRACE=0
	VT_SETUP_JAVA=0
	VT_SETUP_EXECTRACE=0
	VT_SETUP_MEMTRACE=0
	VT_SETUP_METRICS=0
	VT_SETUP_MPI=0
	VT_SETUP_MPICHECK=0
	VT_SETUP_OMP=0
	VT_SETUP_PLUGIN_CNTR=0
	VT_SETUP_PTHREAD=0
	VT_SETUP_RUSAGE=0
	VT_SETUP_IOFSL=0

	AC_ARG_ENABLE(vtsetup,
		AC_HELP_STRING([--enable-vtsetup],
		[build vtsetup - a GUI to prepare measurement runs with VampirTrace, default: enable if JAVA found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_vtsetup="yes"], [check_vtsetup="no"])])

	AS_IF([test x"$check_vtsetup" = "xyes"],
	[
		AC_CHECK_PROG(JAVA, java, java)
		AS_IF([test x"$JAVA" = x],
		[
			AC_MSG_NOTICE([error: no java found; check path for JAVA package first...])
			vtsetup_error="yes"
		])

		AS_IF([test x"$vtsetup_error" = "xno"],
		[
			AC_MSG_NOTICE([setup capabilities of vtsetup])

			VT_SETUP_NM=`echo $NM | sed s/--line-numbers//g`
			AS_IF([test x"$compinst_type"    = "xgnu"], [VT_SETUP_COMPINST_GNU=1])
			AS_IF([test x"$have_zlib"        = "xyes"], [VT_SETUP_COMPRESSION=1])
			AS_IF([test x"$have_getcpu"      = "xyes"], [VT_SETUP_CPUIDTRACE=1])
			AS_IF([test x"$have_dyninst"     = "xyes" -a x"$build_dynattlib" = "xyes"], [VT_SETUP_DYNINST=1])
			AS_IF([test x"$build_etimesync"  = "xyes"], [VT_SETUP_ETIMESYNC=1])
			AS_IF([test x"$have_iowrap"      = "xyes"], [VT_SETUP_IOTRACE=1])
			AS_IF([test x"$have_execwrap"    = "xyes"], [VT_SETUP_EXECTRACE=1])
			AS_IF([test x"$have_mallocwrap"  = "xyes"], [VT_SETUP_MEMTRACE=1])
			AS_IF([test x"$have_java"        = "xyes"], [VT_SETUP_JAVA=1])
			AS_IF([test x"$have_papi"        = "xyes"], [VT_SETUP_METRICS=1])
			AS_IF([test x"$have_mpi"         = "xyes"], [VT_SETUP_MPI=1])
			AS_IF([test x"$have_unimci"      = "xyes"], [VT_SETUP_MPICHECK=1])
			AS_IF([test x"$have_omp"         = "xyes"], [VT_SETUP_OMP=1])
			AS_IF([test x"$have_plugin_cntr" = "xyes"], [VT_SETUP_PLUGIN_CNTR=1])
			AS_IF([test x"$have_pthread"     = "xyes"], [VT_SETUP_PTHREAD=1])
			AS_IF([test x"$have_rusage"      = "xyes"], [VT_SETUP_RUSAGE=1])
			AS_IF([test x"$have_iofsl"       = "xyes"], [VT_SETUP_IOFSL=1])

			build_vtsetup="yes"
		])
	])

	AC_SUBST(VT_SETUP_NM)
	AC_SUBST(VT_SETUP_COMPINST_GNU)
	AC_SUBST(VT_SETUP_COMPRESSION)
	AC_SUBST(VT_SETUP_CPUIDTRACE)
	AC_SUBST(VT_SETUP_DYNINST)
	AC_SUBST(VT_SETUP_ETIMESYNC)
	AC_SUBST(VT_SETUP_IOTRACE)
	AC_SUBST(VT_SETUP_JAVA)
	AC_SUBST(VT_SETUP_EXECTRACE)
	AC_SUBST(VT_SETUP_MEMTRACE)
	AC_SUBST(VT_SETUP_METRICS)
	AC_SUBST(VT_SETUP_MPI)
	AC_SUBST(VT_SETUP_MPICHECK)
	AC_SUBST(VT_SETUP_OMP)
	AC_SUBST(VT_SETUP_PLUGIN_CNTR)
	AC_SUBST(VT_SETUP_PTHREAD)
	AC_SUBST(VT_SETUP_RUSAGE)
	AC_SUBST(VT_SETUP_IOFSL)
])

