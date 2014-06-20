AC_DEFUN([ACVT_PTHREAD],
[
	pthread_error="no"
	have_pthread="no"

	AC_ARG_VAR(PTHREAD_CFLAGS, [C compiler flags to enable support for POSIX threads])
	AC_ARG_VAR(PTHREAD_LIBS, [POSIX threads libraries])

	ACX_PTHREAD([], [pthread_error="yes"])

	AS_IF([test x"$pthread_error" = "xno"],
	[
		sav_CFLAGS=$CFLAGS
		CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
		AC_CHECK_HEADER([pthread.h], [], [pthread_error="yes"])
		CFLAGS=$sav_CFLAGS
	])

	AS_IF([test x"$pthread_error" = "xno"],
	[
		sav_CFLAGS=$CFLAGS
		sav_LIBS=$LIBS
		CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
		LIBS="$LIBS $PTHREAD_LIBS"
		AC_CHECK_FUNCS([pthread_condattr_getpshared \
                                pthread_condattr_setpshared \
                                pthread_mutexattr_getpshared \
                                pthread_mutexattr_setpshared])
		CFLAGS=$sav_CFLAGS
		LIBS=$sav_LIBS

		have_pthread="yes"
		AC_DEFINE([HAVE_PTHREAD], [1], [Define to 1 if VT is configured with Pthreads support.])
	])
])

