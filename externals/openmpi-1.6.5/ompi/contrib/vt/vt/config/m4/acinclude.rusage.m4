AC_DEFUN([ACVT_RUSAGE],
[
	rusage_error="no"
	check_rusage="yes"
	force_rusage="no"
	have_rusage="no"

	AC_ARG_ENABLE(rutrace,
		AC_HELP_STRING([--enable-rutrace], [enable resource usage tracing support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_rusage="yes"], [check_rusage="no"])])

	AS_IF([test x"$check_rusage" = "xyes"],
	[
		AC_CHECK_HEADER([sys/resource.h], [],
		[
			AC_MSG_NOTICE([error: no sys/resource.h found])
			rusage_error="yes"
		])

		AS_IF([test x"$rusage_error" = "xno"],
		[
			AC_CHECK_FUNC([getrusage], [], [rusage_error="yes"])
		])

		AS_IF([test x"$rusage_error" = "xno"],
		[
			sav_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$CPPFLAGS -D_GNU_SOURCE"
			AC_CHECK_DECLS([RUSAGE_THREAD], [], [], [#include <sys/resource.h>])
			CPPFLAGS=$sav_CPPFLAGS

			have_rusage="yes"
		])
	])
])

