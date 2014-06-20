AC_DEFUN([ACVT_LIBWRAP],
[
	libwrap_error="no"
	check_libwrap="gen exec io malloc cudart"
	force_libwrap="no"
	have_libwrap="no"

	force_libwrapgen="no"
	build_libwrapgen="no"
	force_execwrap="no"
	force_iowrap="no"
	force_mallocwrap="no"
	force_cudartwrap="no"

	shlibc_pathname=

	AC_REQUIRE([ACVT_PLATFORM])

	AC_ARG_ENABLE(libtrace,
		AC_HELP_STRING([--enable-libtrace=LIST],
			[enable library tracing support (gen,exec,io,malloc,cudart), default: automatically by configure]),
	[
		AS_IF([test x"$enableval" = "xno"], [check_libwrap="no"])
		AS_IF([test x"$enableval" = "xyes"], [force_libwrap="yes"])
		AS_IF([test x"$enableval" != "xyes" -a x"$enableval" != "xno"],
		[
			check_libwrap=`echo $enableval | sed 's/,/ /g'`
			for lw in $check_libwrap
			do
				case $lw in
					gen)
						force_libwrapgen="yes"
						;;
					libc | exec)
						check_execwrap="yes"
						force_execwrap="yes"
						;;
					io)
						check_iowrap="yes"
						force_iowrap="yes"
						;;
					malloc)
						check_mallocwrap="yes"
						force_mallocwrap="yes"
						;;
					cuda | cudart)
						check_cudartwrap="yes"
						force_cudartwrap="yes"
						;;
					*)
						AC_MSG_ERROR([value of '--enable-libwrap' not properly set])
						;;
				esac
			done
			force_libwrap="yes"
		])
	])

	AC_ARG_WITH(shlibc,
		AC_HELP_STRING([--with-shlibc=SHLIBC],
		[give the pathname for shared LIBC, default: automatically by configure]),
	[
		AS_IF([test x"$withval" = "xyes" -o x"$withval" = "xno"],
		[AC_MSG_ERROR([value of '--with-shlibc' not properly set])])
		shlibc_pathname=$withval
	])

	AS_IF([test x"$check_libwrap" != "xno"],
	[
		AS_IF([test x"$enable_shared" = "xno"],
		[
			case $PLATFORM in
				bgp | bgq)
					AC_MSG_NOTICE([error: library tracing requires building of shared libraries on this platform; re-configure with \`--enable-shared'])
					libwrap_error="yes"
					;;
			esac
		])

		AS_IF([test x"$libwrap_error" = "xno"],
		[
			ACVT_DL
			AS_IF([test x"$have_dl" = "xno"], [libwrap_error="yes"])
		])

		AS_IF([test x"$libwrap_error" = "xno"],
		[
			AC_MSG_CHECKING([for shared LIBC's pathname])

			AS_IF([test x"$shlibc_pathname" = x],
			[
				rm -f conftest
				AC_TRY_LINK([], [],
				[
					AS_IF([test -r "conftest"],
					[
						shlibc_pathname=`ldd conftest 2>/dev/null | grep "libc\." | \
							         sed -e "s/.*=>//"                          \
							             -e "s/[ [\(].*[\)]]//"                 \
							             -e "s/[[[:space:]]]//g"              | \
							         head -n1`
					])
				])
			])

			AS_IF([test x"$shlibc_pathname" != x],
			[
				AC_MSG_RESULT([$shlibc_pathname])
				AC_DEFINE_UNQUOTED([SHLIBC_PATHNAME],
				["$shlibc_pathname"], [pathname of shared LIBC])

				AC_CHECK_DECLS([__errno_location], [],
				[AC_CHECK_DECLS([_Errno], [], [], [#include <errno.h>])], [#include <errno.h>])
			],
			[
				AC_MSG_RESULT([unknown])
			])

			for lw in $check_libwrap
			do
				case $lw in
				gen)
					ACVT_CONF_SUBTITLE([Library wrapper generator])
					ACVT_CTOOL
					AS_IF([test x"$have_ctool" = "xyes"],
					[have_libwrap="yes"; build_libwrapgen="yes"],
					[
						AS_IF([test x"$force_libwrapgen" = "xyes"],
						[
							libwrap_error="yes"
							break
						])
					])
					;;
				libc | exec)
					ACVT_CONF_SUBTITLE([LIBC-EXEC])
					ACVT_EXECWRAP
					AS_IF([test x"$have_execwrap" = "xyes"], [have_execwrap="yes"],
					[
						AS_IF([test x"$force_execwrap" = "xyes"],
						[
							force_libwrap="yes"
							libwrap_error="yes"
							break
						])
					])
					;;
				io)
					ACVT_CONF_SUBTITLE([LIBC-I/O])
					ACVT_IOWRAP
					AS_IF([test x"$have_iowrap" = "xyes"], [have_libwrap="yes"],
					[
						AS_IF([test x"$force_iowrap" = "xyes"],
						[
							force_libwrap="yes"
							libwrap_error="yes"
							break
						])
					])
					;;
				malloc)
					ACVT_CONF_SUBTITLE([LIBC-MALLOC])
					ACVT_MALLOCWRAP
					AS_IF([test x"$have_mallocwrap" = "xyes"], [have_libwrap="yes"],
					[
						AS_IF([test x"$force_mallocwrap" = "xyes"],
						[
							force_libwrap="yes"
							libwrap_error="yes"
							break
						])
					])
					;;
				cuda | cudart)
					ACVT_CONF_SUBTITLE([CUDA-RT])
					ACVT_CUDARTWRAP
					AS_IF([test x"$have_cudartwrap" = "xyes"], [have_libwrap="yes"],
					[
						AS_IF([test x"$force_cudartwrap" = "xyes"],
						[
							force_libwrap="yes"
							libwrap_error="yes"
							break
						])
					])
					;;
				esac
			done
		])
	])
])

