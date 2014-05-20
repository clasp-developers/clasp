AC_DEFUN([ACVT_JVMTI],
[
	jvmti_error="no"
	have_jvmti="no"

	JVMTIDIR=
	JVMTIINCDIR=

	AC_REQUIRE([ACVT_PLATFORM])

	AC_ARG_WITH(jvmti-dir,
		AC_HELP_STRING([--with-jvmti-dir=JVMTIDIR],
		[give the path for JVMTI, default: JAVA_HOME]),
	[JVMTIDIR="$withval/"],
	[AS_IF([test x"$JAVA_HOME" != x], [JVMTIDIR="$JAVA_HOME/"])])

	AC_ARG_WITH(jvmti-inc-dir,
		AC_HELP_STRING([--with-jvmti-inc-dir=JVMTIINCDIR],
		[give the path for JVMTI-include files, default: JVMTI/include]),
	[JVMTIINCDIR="-I$withval/"],
	[AS_IF([test x"$JVMTIDIR" != x], [JVMTIINCDIR="-I$JVMTIDIR"include/])])

	extra_inc_dir=
	case $PLATFORM in
		altix | linux)
			extra_inc_dir=linux
			;;
		sun)
			extra_inc_dir=solaris
			;;
	esac
	AS_IF([test x"$extra_inc_dir" != x],
	[
		AS_IF([test x"$JVMTIINCDIR" != x],
		[JVMTIINCDIR="$JVMTIINCDIR $JVMTIINCDIR$extra_inc_dir/"],
		[JVMTIINCDIR="-I/usr/include/$extra_inc_dir/"])
	])

	sav_CPPFLAGS=$CPPFLAGS
	CPPFLAGS="$CPPFLAGS $JVMTIINCDIR"
	AC_CHECK_HEADER([jvmti.h], [have_jvmti="yes"],
	[
		AC_MSG_NOTICE([error: no jvmti.h found; check path for JVMTI package first...])
		jvmti_error="yes"
	])
	CPPFLAGS=$sav_CPPFLAGS

	AC_SUBST(JVMTIINCDIR)
])

