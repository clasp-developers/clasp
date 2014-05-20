AC_DEFUN([ACVT_MPI],
[
	mpi_error="no"
	check_mpi="yes"
	force_mpi="no"
	have_mpi="no"

	check_fmpi="yes"; force_fmpi="no"; have_fmpi="no"
	check_mpi2_thread="yes"; force_mpi2_thread="no"; have_mpi2_thread="no"
	check_mpi2_1sided="yes"; force_mpi2_1sided="no"; have_mpi2_1sided="no"
	check_mpi2_extcoll="yes"; force_mpi2_extcoll="no"; have_mpi2_extcoll="no"
	check_mpi2_io="yes"; force_mpi2_io="no"; have_mpi2_io="no"

	fmpiwraplib="vt-fmpi"
	build_fmpiwraplib="no"

	MPIDIR=
	MPIINCDIR=
	FMPIINCDIR=
	MPILIBDIR=
	MPILIB=
	PMPILIB=
	FMPILIB=

	VT_MPIGEN_HAVE_MPI2_THREAD=0
	VT_MPIGEN_HAVE_MPI2_1SIDED=0
	VT_MPIGEN_HAVE_MPI2_EXTCOLL=0
	VT_MPIGEN_HAVE_MPI2_IO=0

	AC_ARG_VAR(MPICC, [MPI C compiler command])
	AC_ARG_VAR(MPICXX, [MPI C++ compiler command])
	AC_ARG_VAR(MPIFC, [MPI Fortran compiler command])
	AC_ARG_VAR(MPICFLAGS, [MPI C compiler flags (append to CFLAGS)])
	AC_ARG_VAR(MPICXXFLAGS, [MPI C++ compiler flags (append to CXXFLAGS)])
	AC_ARG_VAR(MPIFCFLAGS, [MPI Fortran compiler flags (append to FCFLAGS)])

	AS_IF([test x"$inside_openmpi" != "xno"],
	[
		AC_MSG_NOTICE([we are configuring inside Open MPI; preset some test results])

		ac_cv_prog_MPICC="$CC"
		ac_cv_prog_MPICXX="$CXX"
		ac_cv_prog_MPIFC="$FC"

		CPPFLAGS="$CPPFLAGS -I$top_vt_srcdir/../../../include -I$top_vt_builddir/../../../include"
		LDFLAGS="$LDFLAGS -L$top_vt_builddir/../../../.libs"

		enable_mpi="yes"
		AS_IF([test "$inside_openmpi" = "1.7"],
		[with_openmpi17="yes"], [with_openmpi="yes"])

		AC_ARG_ENABLE(mpi-io, [], [enable_mpi2_io="$enableval"])
		AC_ARG_ENABLE(mpi-f77, [], [enable_fmpi="$enableval"])     # <  OMPI v1.7
		AC_ARG_ENABLE(mpi-fortran, [], [enable_fmpi="$enableval"]) # >= OMPI v1.7

dnl		further presets below when handling '--with-openmpi[17]'
	])

	AC_ARG_ENABLE(mpi,
		AC_HELP_STRING([--enable-mpi],
		[enable MPI support, default: enable if MPI-installation found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_mpi="yes"], [check_mpi="no"])])

	AC_ARG_WITH(mpi-dir,
		AC_HELP_STRING([--with-mpi-dir=MPIDIR], [give the path for MPI, default: /usr]),
	[MPIDIR="$withval/"])

	AC_ARG_WITH(mpi-inc-dir,
		AC_HELP_STRING([--with-mpi-inc-dir=MPIINCDIR],
		[give the path for MPI-include files, default: MPIDIR/include]),
	[MPIINCDIR="-I$withval/"],
	[AS_IF([test x"$MPIDIR" != x], [MPIINCDIR="-I$MPIDIR"include/])])

	AC_ARG_WITH(fmpi-inc-dir,
		AC_HELP_STRING([--with-fmpi-inc-dir=FMPIINCDIR],
		[give the path for Fortran MPI-include files, default: MPIINCDIR]),
	[FMPIINCDIR="-I$withval/"], [FMPIINCDIR="$MPIINCDIR"])

	AC_ARG_WITH(mpi-lib-dir,
		AC_HELP_STRING([--with-mpi-lib-dir=MPILIBDIR],
		[give the path for MPI-libraries, default: MPIDIR/lib]),
	[MPILIBDIR="-L$withval/"],
	[AS_IF([test x"$MPIDIR" != x], [MPILIBDIR="-L$MPIDIR"lib/])])

	AC_ARG_WITH(hpmpi,
		AC_HELP_STRING([--with-hpmpi], [set MPI-libs for HP MPI]))
	AC_ARG_WITH(pcmpi,
		AC_HELP_STRING([--with-pcmpi], [set MPI-libs for Platform MPI]))
	AS_IF([test x"$with_hpmpi" = "xyes" -o x"$with_pcmpi" = "xyes"],
	[
		MPILIB="-lmpi"
		PMPILIB="$MPILIB"
		FMPILIB="-l$fmpiwraplib"
		check_mpi2_thread="no"; have_mpi2_thread="yes"
		check_mpi2_1sided="no"; have_mpi2_1sided="yes"
		check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
		ac_cv_have_decl_MPI_IN_PLACE="yes"
		ac_cv_have_decl_MPI_ROOT="yes"
	])

	AC_ARG_WITH(intelmpi,
		AC_HELP_STRING([--with-intelmpi], [set MPI-libs for Intel MPI]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpi"
			PMPILIB="$MPILIB"
			FMPILIB="-l$fmpiwraplib"
			check_mpi2_thread="no"; have_mpi2_thread="no"
			check_mpi2_1sided="no"; have_mpi2_1sided="no"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="no"
			check_mpi2_io="no"; have_mpi2_io="no"
			ac_cv_have_decl_MPI_IN_PLACE="no"
			ac_cv_have_decl_MPI_ROOT="no"
		])
	])

	AC_ARG_WITH(intelmpi2,
		AC_HELP_STRING([--with-intelmpi2], [set MPI-libs for Intel MPI2]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpi"
			PMPILIB="$MPILIB"
			FMPILIB="-l$fmpiwraplib"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
			ac_cv_have_decl_MPI_ROOT="yes"
		])
	])

	AC_ARG_WITH(lam,
		AC_HELP_STRING([--with-lam], [set MPI-libs for LAM/MPI]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpi -llam"
			PMPILIB="$MPILIB"
			FMPILIB="-llamf77mpi"
			check_mpi2_io="no"; have_mpi2_io="no"
		])
	])

	AC_ARG_WITH(mpibgl,
		AC_HELP_STRING([--with-mpibgl], [set MPI-libs for IBM BG/L]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpich.rts"
			PMPILIB="-lmpich.rts"
			FMPILIB="-lfmpich.rts"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
		])
	])

	AC_ARG_WITH(mpibgp,
		AC_HELP_STRING([--with-mpibgp], [set MPI-libs for IBM BG/P]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpich.cnk"
			PMPILIB="-lmpich.cnk"
			FMPILIB="-lfmpich.cnk"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
			ac_cv_have_decl_MPI_ROOT="yes"
		])
	])

	AC_ARG_WITH(mpibgq,
		AC_HELP_STRING([--with-mpibgq], [set MPI-libs for IBM BG/Q]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpich"
			PMPILIB="-lmpich"
			FMPILIB="-lfmpich"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
			ac_cv_have_decl_MPI_ROOT="yes"
		])
	])

	AC_ARG_WITH(mpich,
		AC_HELP_STRING([--with-mpich], [set MPI-libs for MPICH]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpich"
			PMPILIB="-lpmpich"
			FMPILIB="-lfmpich"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_thread="no"; have_mpi2_thread="no"
			check_mpi2_1sided="no"; have_mpi2_1sided="no"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="no"
			check_mpi2_io="no"; have_mpi2_io="no"
			ac_cv_have_decl_MPI_IN_PLACE="no"
			ac_cv_have_decl_MPI_ROOT="no"
		])
	])

	AC_ARG_WITH(mpich2,
		AC_HELP_STRING([--with-mpich2], [set MPI-libs for MPICH2]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpich"
			PMPILIB="$MPILIB"
			FMPILIB="-lfmpich"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
			ac_cv_have_decl_MPI_ROOT="yes"
		])
	])

	AC_ARG_WITH(mvapich,
		AC_HELP_STRING([--with-mvapich], [set MPI-libs for MVAPICH]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpich"
			PMPILIB="-lpmpich"
			FMPILIB="-lfmpich"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_thread="no"; have_mpi2_thread="no"
			check_mpi2_1sided="no"; have_mpi2_1sided="no"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="no"
			check_mpi2_io="no"; have_mpi2_io="no"
			ac_cv_have_decl_MPI_IN_PLACE="no"
			ac_cv_have_decl_MPI_ROOT="no"
		])
	])

	AC_ARG_WITH(mvapich2,
		AC_HELP_STRING([--with-mvapich2], [set MPI-libs for MVAPICH2]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpich"
			PMPILIB="$MPILIB"
			FMPILIB="-lfmpich"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
			ac_cv_have_decl_MPI_ROOT="yes"
		])
	])

	AC_ARG_WITH(mpisx,
		AC_HELP_STRING([--with-mpisx], [set MPI-libs for NEC MPI/SX]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpi"
			PMPILIB="-lpmpi"
			FMPILIB="-lfmpi"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			check_mpi2_io="no"; have_mpi2_io="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
			ac_cv_have_decl_MPI_ROOT="yes"
		])
	])

	AC_ARG_WITH(mpisx-ew,
		AC_HELP_STRING([--with-mpisx-ew], [set MPI-libs for NEC MPI/SX with Fortran -ew]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpiw"
			PMPILIB="-lpmpiw"
			FMPILIB="-lfmpiw"
			MPICFLAGS="$MPICFLAGS -D_W8"
			MPICXXFLAGS="$MPICXXFLAGS -D_W8"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			check_mpi2_io="no"; have_mpi2_io="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
			ac_cv_have_decl_MPI_ROOT="yes"
		])
	])

	AC_ARG_WITH(openmpi,
		AC_HELP_STRING([--with-openmpi], [set MPI-libs for Open MPI version < 1.7]))
	AC_ARG_WITH(openmpi17,
		AC_HELP_STRING([--with-openmpi17], [set MPI-libs for Open MPI version >= 1.7]))
	AS_IF([test x"$with_openmpi" = "xyes" -o x"$with_openmpi17" = "xyes"],
	[
		MPILIB="-lmpi"
		PMPILIB="$MPILIB"
		AS_IF([test x"$with_openmpi" = "xyes"],
		[FMPILIB="-lmpi_f77"], [FMPILIB="-lmpi_mpifh"])
		check_mpi2_thread="no"; have_mpi2_thread="yes"
		check_mpi2_1sided="no"; have_mpi2_1sided="yes"
		check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
		ac_cv_func_MPI_Add_error_class="yes"
		ac_cv_func_MPI_Add_error_code="yes"
		ac_cv_func_MPI_Add_error_string="yes"
		ac_cv_func_MPI_Get_address="yes"
		ac_cv_func_MPI_Finalized="yes"
		ac_cv_func_MPI_Type_create_f90_complex="yes"
		ac_cv_func_MPI_Type_create_f90_integer="yes"
		ac_cv_func_MPI_Type_create_f90_real="yes"
		ac_cv_func_MPI_Type_create_struct="yes"
		ac_cv_func_MPI_Type_dup="yes"
		ac_cv_func_MPI_Type_match_size="yes"
		ac_cv_func_PMPI_Win_test="yes"
		ac_cv_func_PMPI_Win_lock="yes"
		ac_cv_func_PMPI_Win_unlock="yes"
		AS_IF([test x"$inside_openmpi" != "xno"],
		[
			AS_IF([test x"$enable_mpi2_io" != "xno"],
			[
				ac_cv_func_MPI_File_open="yes"
				ac_cv_func_MPI_File_close="yes"
				ac_cv_func_PMPI_File_read_ordered="yes"
				ac_cv_func_PMPI_File_read_ordered_begin="yes"
				ac_cv_func_PMPI_File_write_ordered="yes"
				ac_cv_func_PMPI_File_write_ordered_begin="yes"
				ac_cv_func_MPI_Register_datarep="yes"
			])
		])
		ac_cv_have_decl_MPI_IN_PLACE="yes"
		ac_cv_have_decl_MPI_ROOT="yes"
	])

	AC_ARG_WITH(sgimpt,
		AC_HELP_STRING([--with-sgimpt], [set MPI-libs for SGI MPT]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpi"
			PMPILIB="$MPILIB"
			FMPILIB="-l$fmpiwraplib"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="no"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
			ac_cv_have_decl_MPI_ROOT="yes"
		])
	])

	AC_ARG_WITH(sunmpi,
		AC_HELP_STRING([--with-sunmpi], [set MPI-libs for SUN MPI]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpi"
			PMPILIB="$MPILIB"
			FMPILIB="-l$fmpiwraplib"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
			ac_cv_have_decl_MPI_ROOT="yes"
		])
	])

	AC_ARG_WITH(sunmpi-mt,
		AC_HELP_STRING([--with-sunmpi-mt], [set MPI-libs for SUN MPI-MT]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			MPILIB="-lmpi_mt"
			PMPILIB="$MPILIB"
			FMPILIB="-l$fmpiwraplib"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
			ac_cv_have_decl_MPI_ROOT="yes"
		])
	])

	AC_ARG_WITH(mpibull2,
		AC_HELP_STRING([--with-mpibull2], [set MPI-libs for Bull MPICH2]),
	[
		AS_IF([test x"$withval" = "xyes"],
		[
			AS_IF([test x"$MPILIBDIR" = x],
			[pmilibdir="-L/usr/lib/pmi"], [pmilibdir="$MPILIBDIR/pmi"])

			MPILIBDIR="$MPILIBDIR $pmilibdir"
			MPILIB="-lmpi -lpmi"
			PMPILIB="$MPILIB"
			FMPILIB="-lmpibinding_f77"
			MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"
			check_mpi2_thread="no"; have_mpi2_thread="yes"
			check_mpi2_1sided="no"; have_mpi2_1sided="yes"
			check_mpi2_extcoll="no"; have_mpi2_extcoll="yes"
			ac_cv_have_decl_MPI_IN_PLACE="yes"
			ac_cv_have_decl_MPI_ROOT="yes"
		])
	])

	AC_ARG_WITH(mpi-lib,
		AC_HELP_STRING([--with-mpi-lib], [use given mpi lib]),
	[MPILIB="$withval"])

	AC_ARG_WITH(pmpi-lib,
		AC_HELP_STRING([--with-pmpi-lib], [use given pmpi lib]),
	[PMPILIB="$withval"])

	AC_ARG_WITH(fmpi-lib,
		AC_HELP_STRING([--with-fmpi-lib], [use given fmpi lib]),
	[FMPILIB="$withval"])

	AC_ARG_ENABLE(fmpi,
		AC_HELP_STRING([--enable-fmpi],
		[build MPI Fortran support, default: enable if an MPI Fortran library found by configure]),
	[AS_IF([test x"$enableval" = "xyes"],
	[force_fmpi="yes"], [check_fmpi="no"; FMPILIB=])])
	AS_IF([test x"$FC" = x],
	[check_fmpi="no"; force_fmpi="no"; FMPILIB=])

	AC_ARG_ENABLE(fmpi-lib,
		AC_HELP_STRING([--enable-fmpi-lib],
		[build MPI Fortran wrapper library, default: enable if no MPI Fortran library found by configure]), 
	[AS_IF([test x"$enableval" = "xyes" -a x"$check_fmpi" = "xyes"],
	[force_fmpi="yes"; FMPILIB="-l$fmpiwraplib"], [FMPILIB=])])

	AC_ARG_ENABLE(mpi2-thread,
		AC_HELP_STRING([--enable-mpi2-thread],
		[enable MPI-2 Thread support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"],
	[check_mpi2_thread="yes"; force_mpi2_thread="yes"; have_mpi2_thread="no"],
	[check_mpi2_thread="no"]; force_mpi2_thread="no"; have_mpi2_thread="no")])

	AC_ARG_ENABLE(mpi2-1sided,
		AC_HELP_STRING([--enable-mpi2-1sided],
		[enable MPI-2 One-Sided Communication support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"],
	[check_mpi2_1sided="yes"; force_mpi2_1sided="yes"; have_mpi2_1sided="no"],
	[check_mpi2_1sided="no"]; force_mpi2_1sided="no"; have_mpi2_1sided="no")])

	AC_ARG_ENABLE(mpi2-extcoll,
		AC_HELP_STRING([--enable-mpi2-extcoll],
		[enable MPI-2 Extended Collective Operation support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"],
	[check_mpi2_extcoll="yes"; force_mpi2_extcoll="yes"; have_mpi2_extcoll="no"],
	[check_mpi2_extcoll="no"; force_mpi2_extcoll="no"; have_mpi2_extcoll="no"])])

	AC_ARG_ENABLE(mpi2-io,
		AC_HELP_STRING([--enable-mpi2-io],
		[enable MPI-2 I/O support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"],
	[check_mpi2_io="yes"; force_mpi2_io="yes"; have_mpi2_io="no"],
	[check_mpi2_io="no"; force_mpi2_io="no"; have_mpi2_io="no"])])

	AS_IF([test x"$check_mpi" = "xyes"],
	[
dnl		check for MPICC

		AC_CHECK_PROGS(MPICC, mpicc hcc mpcc_r mpcc mpxlc_r mpxlc mpixlc_r mpixlc cmpicc mpiicc, $CC)

dnl		check for mpi.h, using *CC*

		AS_IF([test x"$inside_openmpi" = "xno"],
		[
dnl			guess the MPI include directory based on MPICC's pathname

			mpiincdir_guessed="no"
			AS_IF([test x"$MPICC" != x"$CC" -a x"$MPIINCDIR" = x],
			[
				mpicc_pathname="`which $MPICC 2>/dev/null`"
				AS_IF([test x"$mpicc_pathname" != x],
				[
					MPIINCDIR="-I`dirname $mpicc_pathname`/../include"
					mpiincdir_guessed="yes"
				],
				[
					AC_MSG_NOTICE([error: $MPICC not found; check path for MPI package first...])
					mpi_error="yes"
				])
			])

dnl			check for mpi.h; print a warning message if MPIINCDIR was guessed, otherwise trigger an error

			AS_IF([test x"$mpi_error" = "xno"],
			[
				sav_CPPFLAGS=$CPPFLAGS
				CPPFLAGS="$CPPFLAGS $MPIINCDIR"
				AC_CHECK_HEADER([mpi.h],
				[
					AS_IF([test x"$mpiincdir_guessed" = "xyes" -a x"$FMPIINCDIR" = x],
					[FMPIINCDIR=$MPIINCDIR])
				],
				[
					AS_IF([test x"$mpiincdir_guessed" = "xyes"],
					[
						AC_MSG_WARN([could not determine the MPI include directory based on $MPICC; use the '--with-mpi-inc-dir' option to specify it...])
						MPIINCDIR=
					],
					[
						AC_MSG_NOTICE([error: no mpi.h found; check path for MPI package first...])
						mpi_error="yes"
					])
				])
				CPPFLAGS=$sav_CPPFLAGS
			])
		])

dnl		check for MPICXX

		AS_IF([test x"$mpi_error" = "xno"],
		[
			AC_CHECK_PROGS(MPICXX, mpicxx mpic++ mpiCC hcp mpxlC_r mpxlC mpCC_r mpCC cmpic++, $CXX)
			MPICXXFLAGS="$MPICXXFLAGS -DMPICH_SKIP_MPICXX -DOMPI_SKIP_MPICXX -DMPI_NO_CPPBIND"
		])

dnl		check for MPILIB

		AS_IF([test "$MPICC" = "$CC" -a x"$MPILIB" = x -a x"$mpi_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -lmpi_r"
			AC_MSG_CHECKING([whether linking with -lmpi_r works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); MPILIB=-lmpi_r],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS

			AS_IF([test x"$MPILIB" = x],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -lmpi"
				AC_MSG_CHECKING([whether linking with -lmpi works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); MPILIB=-lmpi],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS

				AS_IF([test x"$MPILIB" != x],
				[
					sav_LIBS=$LIBS
					LIBS="$LIBS $MPILIBDIR -llam"
					AC_MSG_CHECKING([whether linking with -llam works])
					AC_TRY_LINK([],[],
					[AC_MSG_RESULT([yes]); MPILIB="-lmpi -llam"],[AC_MSG_RESULT([no])])
					LIBS=$sav_LIBS
				])
			])

			AS_IF([test x"$MPILIB" = x],
			[
				AS_IF([test x"$MPILIBDIR" = x],
				[pmilibdir="-L/usr/lib/pmi"], [pmilibdir="$MPILIBDIR/pmi"])

				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR $pmilibdir -lmpi -lpmi"
				AC_MSG_CHECKING([whether linking with -lmpi $pmilibdir -lpmi works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); MPILIBDIR="$MPILIBDIR $pmilibdir"; MPILIB="-lmpi -lpmi"],
				[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])
			
			AS_IF([test x"$MPILIB" = x],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -lmpich"
				AC_MSG_CHECKING([whether linking with -lmpich works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); MPILIB=-lmpich],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
				AS_IF([test x"$MPILIB" != x],
				[MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"])
			])

			AS_IF([test x"$MPILIB" = x],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -lmpichg2"
				AC_MSG_CHECKING([whether linking with -lmpichg2 works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); MPILIB=-lmpichg2],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
				AS_IF([test x"$MPILIB" != x],
				[MPICFLAGS="$MPICFLAGS -DMPICH_IGNORE_CXX_SEEK"])
			])

			AS_IF([test x"$MPILIB" = x],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -lhpmpi"
				AC_MSG_CHECKING([whether linking with -lhpmpi works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); MPILIB=-lhpmpi],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
				AS_IF([test x"$MPILIB" != x],
				[
					sav_LIBS=$LIBS
					LIBS="$LIBS $MPILIBDIR -lmtmpi"
					AC_MSG_CHECKING([whether linking with -lmtmpi works])
					AC_TRY_LINK([],[],
					[AC_MSG_RESULT([yes]); MPILIB=-lmtmpi],[AC_MSG_RESULT([no])])
					LIBS=$sav_LIBS
				])
			])

			AS_IF([test x"$MPILIB" = x],
			[
				AC_MSG_NOTICE([error: no libmpi_r, libmpi, liblam, libmpich, or libhpmpi found; check path for MPI package first...])
				mpi_error="yes"
			])
		])

dnl		check for PMPILIB

		AS_IF([test x"$PMPILIB" = x -a x"$mpi_error" = "xno"],
		[
			sav_CC=$CC
			CC=$MPICC

			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -lpmpich $MPILIB"
			AC_MSG_CHECKING([whether linking with -lpmpich $MPILIB works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); PMPILIB=-lpmpich],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS

			AS_IF([test x"$PMPILIB" = x],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -lpmpichg2 $MPILIB"
				AC_MSG_CHECKING([whether linking with -lpmpichg2 $MPILIB works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); PMPILIB=-lpmpichg2],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])

dnl			Do not check for libpmpi. When using the Open MPI compiler wrapper, this check
dnl			succeeds even if this library isn't present.
dnl			AS_IF([test x"$PMPILIB" = x],
dnl			[
dnl				sav_LIBS=$LIBS
dnl				LIBS="$LIBS $MPILIBDIR -lpmpi $MPILIB"
dnl				AC_MSG_CHECKING([whether linking with -lpmpi $MPILIB works])
dnl				AC_TRY_LINK([],[],
dnl				[AC_MSG_RESULT([yes]); PMPILIB=-lpmpi],[AC_MSG_RESULT([no])])
dnl				LIBS=$sav_LIBS
dnl				AS_IF([test x"$PMPILIB" != x],
dnl				[
dnl					sav_LIBS=$LIBS
dnl					LIBS="$LIBS $MPILIBDIR -lmtpmpi"
dnl					AC_MSG_CHECKING([whether linking with -lmtpmpi works])
dnl					AC_TRY_LINK([],[],
dnl					[AC_MSG_RESULT([yes]); PMPILIB=-lmtpmpi],[AC_MSG_RESULT([no])])
dnl					LIBS=$sav_LIBS
dnl				])
dnl			])

			AS_IF([test x"$PMPILIB" = x -a x"$MPILIB" != x],
			[
				PMPILIB="$MPILIB"
				AC_MSG_NOTICE([no libpmpich or libpmpichg2 found; assume $MPILIB])
			])

			CC=$sav_CC
		])

dnl		check for FMPILIB

		AS_IF([test x"$check_fmpi" = "xyes" -a x"$FMPILIB" = x -a x"$mpi_error" = "xno"],
		[
			sav_CC=$CC
			CC=$MPICC

			sav_LIBS=$LIBS
			LIBS="$LIBS $MPILIBDIR -lmpi_f77 $MPILIB"
			AC_MSG_CHECKING([whether linking with -lmpi_f77 $MPILIB works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); FMPILIB=-lmpi_f77],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS

			AS_IF([test x"$FMPILIB" = x],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -lmpi_mpifh $MPILIB"
				AC_MSG_CHECKING([whether linking with -lmpi_mpifh $MPILIB works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); FMPILIB="-lmpi_mpifh"],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])

			AS_IF([test x"$FMPILIB" = x],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -lmpibinding_f77 $MPILIB"
				AC_MSG_CHECKING([whether linking with -lmpibinding_f77 $MPILIB works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); FMPILIB=-lmpibinding_f77],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])

			AS_IF([test x"$FMPILIB" = x],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -lfmpich $MPILIB"
				AC_MSG_CHECKING([whether linking with -lfmpich $MPILIB works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); FMPILIB=-lfmpich],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])

			AS_IF([test x"$FMPILIB" = x],
			[
				sav_LIBS=$LIBS
				LIBS="$LIBS $MPILIBDIR -llamf77mpi $MPILIB"
				AC_MSG_CHECKING([whether linking with -llamf77mpi $MPILIB works])
				AC_TRY_LINK([],[],
				[AC_MSG_RESULT([yes]); FMPILIB=-llamf77mpi],[AC_MSG_RESULT([no])])
				LIBS=$sav_LIBS
			])

			AS_IF([test x"$FMPILIB" = x],
			[
				AC_MSG_NOTICE([no libmpi_f77, libmpi_mpifh, libmpibinding_f77, libfmpich, or liblamf77mpi found; build lib$fmpiwraplib])
				FMPILIB="-l$fmpiwraplib"
			])

			CC=$sav_CC
		])

dnl		check for MPI-2

		AS_IF([test x"$mpi_error" = "xno"],
		[
			sav_CC=$CC
			sav_CPPFLAGS=$CPPFLAGS
			sav_LIBS=$LIBS
			CC=$MPICC
			CPPFLAGS="$CPPFLAGS $MPICFLAGS $MPIINCDIR"
			LIBS="$LIBS $MPILIBDIR $MPILIB $PMPILIB"

dnl			check for MPI-2 constants

			ACVT_CONF_SUBTITLE([MPI-2 constants])

			AC_CHECK_DECLS([MPI_IN_PLACE], [], [], [#include "mpi.h"])
			AC_CHECK_DECLS([MPI_ROOT], [], [], [#include "mpi.h"])

dnl			check for MPI-2 functions

			ACVT_CONF_SUBTITLE([MPI-2 functions])

			AC_CHECK_FUNCS([MPI_Add_error_class \
                                        MPI_Add_error_code \
                                        MPI_Add_error_string \
                                        MPI_Get_address \
                                        MPI_Finalized \
                                        MPI_Type_create_f90_complex \
                                        MPI_Type_create_f90_integer \
                                        MPI_Type_create_f90_real \
                                        MPI_Type_create_struct \
                                        MPI_Type_dup \
                                        MPI_Type_match_size])
			
dnl			check for MPI-2 Thread support

			ACVT_CONF_SUBSUBTITLE([MPI-2 Thread support])
			AS_IF([test x"$check_mpi2_thread" = "xyes"],
			[
				AC_CHECK_FUNC([MPI_Init_thread], [have_mpi2_thread="yes"])
				AS_IF([test x"$force_mpi2_thread" = "xyes" -a x"$have_mpi2_thread" = "xno"], [exit 1])
			],
			[
				AS_IF([test x"$enable_config_titles" = "xyes"],
				[
					AS_IF([test x"$have_mpi2_thread" = "xyes"],
					[AC_MSG_NOTICE([enabled via command line switch])],
					[AC_MSG_NOTICE([disabled via command line switch])])
				])
			])

dnl			check for MPI-2 One-Sided Communications

			ACVT_CONF_SUBSUBTITLE([MPI-2 One-Sided Communications])
			AS_IF([test x"$check_mpi2_1sided" = "xyes"],
			[
				AC_CHECK_FUNC([MPI_Get],
				 [AC_CHECK_FUNC([MPI_Put], [have_mpi2_1sided="yes"])])
				AS_IF([test x"$force_mpi2_1sided" = "xyes" -a x"$have_mpi2_1sided" = "xno"], [exit 1])
			],
			[
				AS_IF([test x"$enable_config_titles" = "xyes"],
				[
					AS_IF([test x"$have_mpi2_1sided" = "xyes"],
					[AC_MSG_NOTICE([enabled via command line switch])],
					[AC_MSG_NOTICE([disabled via command line switch])])
				])
			])
                        AS_IF([test x"$have_mpi2_1sided" = "xyes"],
			[
                        	AC_CHECK_FUNCS([PMPI_Win_test \
                                                PMPI_Win_lock \
                                                PMPI_Win_unlock])
			])

dnl			check for MPI-2 Extended Collective Operations

			ACVT_CONF_SUBSUBTITLE([MPI-2 Extended Collective Operations])
			AS_IF([test x"$check_mpi2_extcoll" = "xyes"],
			[
				AC_CHECK_FUNC([MPI_Alltoallw],
				 [AC_CHECK_FUNC([MPI_Exscan], [have_mpi2_extcoll="yes"])])
				AS_IF([test x"$force_mpi2_extcoll" = "xyes" -a x"$have_mpi2_extcoll" = "xno"], [exit 1])
			],
			[
				AS_IF([test x"$enable_config_titles" = "xyes"],
				[
					AS_IF([test x"$have_mpi2_extcoll" = "xyes"],
					[AC_MSG_NOTICE([enabled via command line switch])],
					[AC_MSG_NOTICE([disabled via command line switch])])
				])
			])

dnl			check for MPI-2 I/O

			ACVT_CONF_SUBSUBTITLE([MPI-2 I/O])
			AS_IF([test x"$check_mpi2_io" = "xyes"],
			[
				AC_CHECK_DECL([LAM_MPI],
				[
					AC_MSG_NOTICE([error: MPI-2 I/O isn't supported for LAM/MPI])
				],
				[
					AC_CHECK_FUNC([MPI_File_open],
					 [AC_CHECK_FUNC([MPI_File_close], [have_mpi2_io="yes"])])
				], [#include "mpi.h"])
				AS_IF([test x"$force_mpi2_io" = "xyes" -a x"$have_mpi2_io" = "xno"], [exit 1])
			],
			[
				AS_IF([test x"$enable_config_titles" = "xyes"],
				[
					AS_IF([test x"$have_mpi2_io" = "xyes"],
					[AC_MSG_NOTICE([enabled via command line switch])],
					[AC_MSG_NOTICE([disabled via command line switch])])
				])
			])
			AS_IF([test x"$have_mpi2_io" = "xyes"],
			[
				AC_CHECK_FUNCS([MPI_Register_datarep \
                                                PMPI_File_read_ordered \
                                                PMPI_File_read_ordered_begin \
                                                PMPI_File_write_ordered \
                                                PMPI_File_write_ordered_begin])
			])
	
			CC=$sav_CC
			CPPFLAGS=$sav_CPPFLAGS
			LIBS=$sav_LIBS
		])

dnl		check for Fortran interoperability

		AS_IF([test x"$FMPILIB" = x"-l$fmpiwraplib" -a x"$mpi_error" = "xno"],
		[
			ACVT_CONF_SUBTITLE([Fortran interoperability])
			ACVT_FMPIWRAPLIB
			AS_IF([test x"$fmpiwraplib_error" = "xno"],
			[
				build_fmpiwraplib="yes"
			],
			[
				AS_IF([test x"$force_fmpi" = "xyes"], [exit 1])
				FMPILIB=
			])
		])

		AS_IF([test x"$mpi_error" = "xno"], [have_mpi="yes"],
		[MPICC="$CC"; MPICXX="$CXX"; MPIFC="$FC"])
		AS_IF([test x"$FMPILIB" != x], [have_fmpi="yes"], [MPIFC=])
		AS_IF([test x"$have_mpi2_thread" = "xyes"], [VT_MPIGEN_HAVE_MPI2_THREAD=1])
		AS_IF([test x"$have_mpi2_1sided" = "xyes"], [VT_MPIGEN_HAVE_MPI2_1SIDED=1])
		AS_IF([test x"$have_mpi2_extcoll" = "xyes"], [VT_MPIGEN_HAVE_MPI2_EXTCOLL=1])
		AS_IF([test x"$have_mpi2_io" = "xyes"], [VT_MPIGEN_HAVE_MPI2_IO=1])
	])

	AC_DEFINE([MPI_TRACE_INSIDE], [0], [Define to 1 to record MPI functions called within MPI functions.])

	AS_IF([test x"$have_mpi" = "xyes"],
	[AC_DEFINE([HAVE_MPI], [1], [Define to 1 if VT is configured with MPI support.])])

	AS_IF([test x"$have_fmpi" = "xyes"],
	[AC_DEFINE([HAVE_FMPI], [1], [Define to 1 if VT is configured with MPI Fortran support.])])

	AS_IF([test x"$have_mpi2_thread" = "xyes"],
	[AC_DEFINE([HAVE_MPI2_THREAD], [1], [Define to 1 if you have functions for MPI-2 Thread support.])])

	AS_IF([test x"$have_mpi2_1sided" = "xyes"],
	[AC_DEFINE([HAVE_MPI2_1SIDED], [1], [Define to 1 if you have functions for MPI-2 One-Sided Communications.])])

	AS_IF([test x"$have_mpi2_extcoll" = "xyes"],
	[AC_DEFINE([HAVE_MPI2_EXTCOLL], [1], [Define to 1 if you have functions for MPI-2 Extended Collective Operations.])])

	AS_IF([test x"$have_mpi2_io" = "xyes"],
	[AC_DEFINE([HAVE_MPI2_IO], [1], [Define to 1 if you have functions for MPI-2 I/O.])])

	AC_SUBST(MPIDIR)
	AC_SUBST(MPIINCDIR)
	AC_SUBST(FMPIINCDIR)
	AC_SUBST(MPILIBDIR)
	AC_SUBST(MPILIB)
	AC_SUBST(PMPILIB)
	AC_SUBST(FMPILIB)

	AC_SUBST(VT_MPIGEN_HAVE_MPI2_THREAD)
	AC_SUBST(VT_MPIGEN_HAVE_MPI2_1SIDED)
	AC_SUBST(VT_MPIGEN_HAVE_MPI2_EXTCOLL)
	AC_SUBST(VT_MPIGEN_HAVE_MPI2_IO)
])

AC_DEFUN([ACVT_FMPIWRAPLIB],
[
	fmpiwraplib_error="no"

	have_mpi2_const="no"
	have_mpi_status_size="no"

	VT_MPIGEN_HAVE_FC_CONV_COMM=0
	VT_MPIGEN_HAVE_FC_CONV_ERRH=0
	VT_MPIGEN_HAVE_FC_CONV_FILE=0
	VT_MPIGEN_HAVE_FC_CONV_GROUP=0
	VT_MPIGEN_HAVE_FC_CONV_INFO=0
	VT_MPIGEN_HAVE_FC_CONV_OP=0
	VT_MPIGEN_HAVE_FC_CONV_REQUEST=0
	VT_MPIGEN_HAVE_FC_CONV_STATUS=0
	VT_MPIGEN_HAVE_FC_CONV_TYPE=0
	VT_MPIGEN_HAVE_FC_CONV_WIN=0
	VT_MPIGEN_HAVE_FC_CONV_MPI2CONST=0

	AC_CHECK_PROGS(MPIFC, mpif77 hf77 mpxlf_r mpxlf mpf77 cmpifc mpifort mpif90 mpxlf95_r mpxlf90_r mpxlf95 mpxlf90 mpf90 cmpif90c, $FC)

	AS_IF([test x"$MPIFC" = x"$FC" -a x"$inside_openmpi" = "xno"],
	[
		AC_MSG_CHECKING([for mpif.h])
		rm -f conftest.f conftest.o
		cat > conftest.f << EOF
      PROGRAM conftest
      INCLUDE 'mpif.h'
      END
EOF
		eval "$FC $FCFLAGS $MPIFCFLAGS $FMPIINCDIR -c conftest.f" >/dev/null 2>&1
		AS_IF([test "$?" = "0" -a -s conftest.o], [AC_MSG_RESULT([yes])],
		[
			AC_MSG_RESULT([no])
			AC_MSG_NOTICE([error: no mpif.h found; check path for MPI package first...])
			fmpiwraplib_error="yes"
		])
		rm -f conftest.f conftest.o
	])

	AS_IF([test x"$fmpiwraplib_error" = "xno"],
	[
		sav_CC=$CC
		sav_CPPFLAGS=$CPPFLAGS
		CC=$MPICC
		CPPFLAGS="$CPPFLAGS $MPICFLAGS $MPIINCDIR"

dnl		check for MPI handle conversion functions

		AC_CHECK_DECL([MPI_Comm_f2c],
		 [AC_CHECK_DECL([MPI_Comm_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_COMM=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

		AC_CHECK_DECL([MPI_Errhandler_f2c],
		 [AC_CHECK_DECL([MPI_Errhandler_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_ERRH=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

		AC_CHECK_DECL([MPI_File_f2c],
		 [AC_CHECK_DECL([MPI_File_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_FILE=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

		AC_CHECK_DECL([MPI_Group_f2c],
		 [AC_CHECK_DECL([MPI_Group_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_GROUP=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

		AC_CHECK_DECL([MPI_Info_f2c],
		 [AC_CHECK_DECL([MPI_Info_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_INFO=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

		AC_CHECK_DECL([MPI_Op_f2c],
		 [AC_CHECK_DECL([MPI_Op_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_OP=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

		AC_CHECK_DECL([MPI_Request_f2c],
		 [AC_CHECK_DECL([MPI_Request_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_REQUEST=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

		AC_CHECK_DECL([MPI_Status_f2c],
		 [AC_CHECK_DECL([MPI_Status_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_STATUS=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

		AC_CHECK_DECL([MPI_Type_f2c],
		 [AC_CHECK_DECL([MPI_Type_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_TYPE=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

		AC_CHECK_DECL([MPI_Win_f2c],
		 [AC_CHECK_DECL([MPI_Win_c2f],
		   [VT_MPIGEN_HAVE_FC_CONV_WIN=1], [], [#include "mpi.h"])],
		 [], [#include "mpi.h"])

dnl		check for MPI-2 constants to convert

		AC_CHECK_DECLS([MPI_IN_PLACE],
		 [VT_MPIGEN_HAVE_FC_CONV_MPI2CONST=1; have_mpi2_const="yes"], [], [#include "mpi.h"])

dnl		check for MPI_STATUS_SIZE
		AC_CHECK_DECLS([MPI_STATUS_SIZE], [have_mpi_status_size="yes"], [], [#include "mpi.h"])

		CC=$sav_CC
		CPPFLAGS=$sav_CPPFLAGS
	])

	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_COMM)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_ERRH)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_FILE)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_GROUP)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_INFO)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_OP)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_REQUEST)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_STATUS)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_TYPE)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_WIN)
	AC_SUBST(VT_MPIGEN_HAVE_FC_CONV_MPI2CONST)
])

