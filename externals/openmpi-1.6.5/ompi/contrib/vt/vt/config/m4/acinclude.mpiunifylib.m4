AC_DEFUN([ACVT_MPIUNIFYLIB],
[
	mpiunifylib_error="no"
	check_mpiunifylib="yes"
	force_mpiunifylib="no"
	build_mpiunifylib="no"

	VTMPIUNIFYLIB=

	AC_REQUIRE([ACVT_PLATFORM])
	AC_REQUIRE([ACVT_ETIMESYNC])
	AC_REQUIRE([ACVT_UNIMCI])

	AC_ARG_ENABLE(mpiunify-lib,
		AC_HELP_STRING([--enable-mpiunify-lib],
		[build MPI trace unification library, default: enable if C++ runtime library found]),
	[AS_IF([test x"$enableval" = "xyes"], [force_mpiunifylib="yes"], [check_mpiunifylib="no"])])

	AS_IF([test x"$check_mpiunifylib" = "xyes"],
	[
		AS_IF([test "$PLATFORM" = "ibm" -o "$PLATFORM" = "necsx"],
		[
			AC_MSG_NOTICE([error: The MPI trace unification library cannot be built on this platform])
			mpiunifylib_error="yes"
		])

		AS_IF([test x"$mpiunifylib_error" = "xno" -a x"$inside_openmpi" = "xno"],
		[
			bad_mpi="no"
			AC_CHECK_DECL([PLATFORM_MPI], [bad_mpi="yes"],
			[AC_CHECK_DECL([HP_MPI], [bad_mpi="yes"], [], [#include "mpi.h"])],
			[], [#include "mpi.h"])

			AS_IF([test "$bad_mpi" = "yes"],
			[
				AC_MSG_NOTICE([error: The MPI trace unification library cannot be built with HP/Platform MPI])
				mpiunifylib_error="yes"
			])
		])

		AS_IF([test x"$mpiunifylib_error" = "xno"],
		[
			AS_IF([test x"$have_unimci" = "xyes" -a x"$unimci_checker_name" = "xMARMOT"],
			[
				VTMPIUNIFYLIB="-lvt-mpi-unify -lotfaux $CLAPACKLIBDIR $CLAPACKLIB"
				build_mpiunifylib="yes"
			],
			[
				ACVT_CXXRTLIB
				AS_IF([test x"$cxxrtlib_error" = "xno"],
				[
					VTMPIUNIFYLIB="-lvt-mpi-unify -lotfaux $CLAPACKLIBDIR $CLAPACKLIB $cxxrtlib"
					build_mpiunifylib="yes"
				],
				[
					mpiunifylib_error="yes"
				])
			])
		])
	])

	AC_SUBST(VTMPIUNIFYLIB)
])

