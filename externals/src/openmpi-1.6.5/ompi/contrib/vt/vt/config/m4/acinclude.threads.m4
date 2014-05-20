AC_DEFUN([ACVT_THREADS],
[
	threads_error="no"
	check_threads="pthread omp"
	force_threads="no"
	force_threads_list=
	have_threads="no"

	AC_ARG_ENABLE(threads,
		AC_HELP_STRING([--enable-threads=LIST],
			[enable support for threads (pthread, omp), default: automatically by configure]),
	[
		AS_IF([test x"$enableval" = "xno"], [check_threads="no"])
		AS_IF([test x"$enableval" = "xyes"], [force_threads="yes"])
		AS_IF([test x"$enableval" != "xyes" -a x"$enableval" != "xno"],
		[
			check_threads="`echo $enableval | sed -e 's/,/ /g'`"
			force_threads_list="$check_threads"
			force_threads="yes"
		])
	])

	AS_IF([test x"$check_threads" != "xno"],
	[
		for ct in $check_threads
		do
			case $ct in
				pthread)
					ACVT_CONF_SUBSUBTITLE([POSIX threads])
					ACVT_PTHREAD
					AS_IF([test x"$force_threads_list" != x -a x"$pthread_error" = "xyes"],
					[threads_error="yes"; break],
					[AS_IF([test x"$have_pthread" = "xyes"], [have_threads="yes"])])
					;;
				omp)
					ACVT_CONF_SUBSUBTITLE([OpenMP])
					ACVT_OMP
					AS_IF([test x"$force_threads_list" != x -a x"$omp_error" = "xyes"],
					[threads_error="yes"; break],
					[AS_IF([test x"$have_omp" = "xyes"], [have_threads="yes"])])
					;;
				*)
					AC_MSG_ERROR([unknown thread type '$ct'])
					;;
			esac
		done

		AS_IF([test x"$have_threads" = "xno"], [threads_error="yes"])
	])

	AS_IF([test x"$have_threads" = "xyes"],
	[AC_DEFINE([HAVE_THREADS], [1], [Define to 1 if VT is configured with Threads support.])])
])

