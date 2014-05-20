AC_DEFUN([ACVT_MALLOCWRAP],
[
	mallocwrap_error="no"
	check_mallocwrap="yes"
	have_mallocwrap="no"

	AC_ARG_ENABLE(memtrace,
		AC_HELP_STRING([--enable-memtrace],
		[enable support for tracing LIBC functions for memory de/allocation (e.g. malloc,calloc,realloc,free) via library wrapping, default: enable]),
        [AS_IF([test x"$enableval" = "xyes"], [force_mallocwrap="yes"], [check_mallocwrap="no"])])

	AS_IF([test "$check_mallocwrap" = "yes"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $DLINCDIR -D_GNU_SOURCE"
		AC_CHECK_DECLS([RTLD_NEXT], [], [mallocwrap_error="yes"], [#include <dlfcn.h>])
		CPPFLAGS=$sav_CPPFLAGS

		AS_IF([test x"$mallocwrap_error" = "xno"],
		[
			AC_CHECK_FUNC([malloc_usable_size], [], [mallocwrap_error="yes"])
		])

		AS_IF([test x"$mallocwrap_error" = "xno"],
		[
			sav_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$CPPFLAGS $DLINCDIR -D_BSD_SOURCE -D_XOPEN_SOURCE=600"
			AC_CHECK_FUNCS([memalign posix_memalign valloc])
			CPPFLAGS=$sav_CPPFLAGS

			have_mallocwrap="yes"
		])
	])
])

