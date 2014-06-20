AC_DEFUN([ACVT_GETCPU],
[
	getcpu_error="no"
	check_getcpu="yes"
	force_getcpu="no"
	have_getcpu="no"

	AC_ARG_ENABLE(cpuidtrace,
		AC_HELP_STRING([--enable-cpuidtrace], [enable CPU ID tracing support, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_getcpu="yes"], [check_getcpu="no"])])

	AS_IF([test x"$check_getcpu" = "xyes"],
	[
		AC_CHECK_HEADER([sched.h], [],
		[
			AC_MSG_NOTICE([error: no sched.h found])
			getcpu_error="yes"
		])

		AS_IF([test x"$getcpu_error" = "xno"],
		[
			sav_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$CPPFLAGS -D_GNU_SOURCE"
			AC_CHECK_FUNC([sched_getcpu], [], [getcpu_error="yes"])
			CPPFLAGS=$sav_CPPFLAGS
		])

		AS_IF([test x"$getcpu_error" = "xno" -a x"$cross_compiling" = "xno"],
		[
			AC_MSG_CHECKING([whether sched_getcpu works])
			AC_TRY_RUN(
[
#define _GNU_SOURCE
#include <sched.h>
int main() { return (sched_getcpu() != -1) ? 0 : 1; }
],
			[AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no]); getcpu_error="yes"], [])
		])

		AS_IF([test x"$getcpu_error" = "xno"],
		[
			have_getcpu="yes"
		])
	])
])

