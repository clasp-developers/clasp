AC_DEFUN([ACVT_EXECWRAP],
[
	execwrap_error="no"
	check_execwrap="yes"
	have_execwrap="no"

	AC_ARG_ENABLE(exectrace,
		AC_HELP_STRING([--enable-exectrace],
		[enable support for tracing LIBC functions for creating and controling child processes (e.g. execl,fork,system,wait) via library wrapping, default: enable]),
        [AS_IF([test x"$enableval" = "xyes"], [force_execwrap="yes"], [check_execwrap="no"])])

	AS_IF([test "$check_execwrap" = "yes"],
	[
		AS_IF([test x"$shlibc_pathname" = x],
		[
			AC_MSG_NOTICE([error: pathname of shared LIBC required for LIBC-EXEC tracing; please specify it by --with-shlibc])
			execwrap_error="yes"
		])

		AS_IF([test x"$execwrap_error" = "xno"],
		[
			AC_CHECK_TYPES([__WAIT_STATUS], [], [], [#include <sys/wait.h>])

			sav_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$CPPFLAGS -D_GNU_SOURCE -D_SVID_SOURCE -D_BSD_SOURCE"
			AC_CHECK_DECLS([environ], [], [], [#include <unistd.h>])
			AC_CHECK_FUNCS([execvpe fexecve waitid wait3 wait4])
			CPPFLAGS=$sav_CPPFLAGS

			have_execwrap="yes"
		])
	])
])

