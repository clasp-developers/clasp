AC_DEFUN([ACVT_IOWRAP],
[
	iowrap_error="no"
	check_iowrap="yes"
	have_iowrap="no"

	AC_ARG_ENABLE(iotrace,
		AC_HELP_STRING([--enable-iotrace],
		[enable support for tracing LIBC I/O functions (e.g. fopen,fclose,fread,fwrite) via library wrapping, default: enable]),
	[AS_IF([test x"$enableval" = "xyes"], [force_iowrap="yes"], [check_iowrap="no"])])


	AS_IF([test "$check_iowrap" = "yes"],
	[
		AS_IF([test x"$shlibc_pathname" = x],
		[
			AC_MSG_NOTICE([error: pathname of shared LIBC required for LIBC-I/O tracing; please specify it by --with-shlibc])
			iowrap_error="yes"
		])

		AS_IF([test x"$iowrap_error" = "xno"],
		[
			sav_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$CPPFLAGS -D_GNU_SOURCE -D_LARGEFILE64_SOURCE"

			AC_CHECK_FUNCS([ \
				creat64 \
				fopen64 \
				fseeko \
				fseeko64 \
				lseek64 \
				fsetpos64 \
				open64 \
				pread64 \
				pwrite64 \
				flockfile \
				ftrylockfile \
				funlockfile \
				sync \
				fflush \
				fsync \
				fdatasync \
				lockf])

			AC_CHECK_FUNCS([__fprintf_chk],
			[
dnl				Check whether <stdio.h> declares __vfprintf_chk. This should be the case if
dnl				_FORTIFY_SOURCE is defined (default when using the GNU compiler).
dnl				Otherwise, we have to declare this function to avoid compiler warnings.
				AC_CHECK_DECLS([__vfprintf_chk], [], [], [#include <stdio.h>])
			])

			CPPFLAGS=$sav_CPPFLAGS

			have_iowrap="yes"
		])
	])
])

