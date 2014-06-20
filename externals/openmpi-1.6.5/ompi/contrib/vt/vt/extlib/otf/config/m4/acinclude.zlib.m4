dnl 	have the zlib dirs specified
AC_DEFUN([CHECK_ZLIB],
[
    zlib_error="no"
    check_zlib="yes"
    force_zlib="no"
    have_zlib="no"

    AH_TEMPLATE(HAVE_ZLIB, [], [defined if zlib library is to be used])

    AC_ARG_WITH([zlib],
        AC_HELP_STRING([--with-zlib],
            [use zlib, default: yes if found by configure]),
        [if test "$withval" = "yes"; then force_zlib="yes"; else check_zlib="no"; fi])

    AC_ARG_WITH([zlib-dir],
        AC_HELP_STRING([--with-zlib-dir],
            [give the path for zlib, default: /usr]),
        [zlib_dir="$withval/"])

    AC_ARG_WITH([zlib-inc-dir],
        AC_HELP_STRING([--with-zlib-inc-dir],
            [give the path dir zlib-include files, default: ZLIBDIR/include]),
        [zlib_inc_dir="$withval/"],
        [if test x"$zlib_dir" != x; then zlib_inc_dir="$zlib_dir"include/; fi])

    AC_ARG_WITH([zlib-lib-dir],
        AC_HELP_STRING([--with-zlib-lib-dir],
            [give the path for ZLIB-libraries, default: ZLIBDIR/lib]),
        [zlib_lib_dir="$withval/"],
        [if test x"$zlib_dir" != x; then zlib_lib_dir="$zlib_dir"lib/; fi])

    AC_ARG_WITH([zlib-lib],
        AC_HELP_STRING([--with-zlib-lib],
            [use given zlib, default: -lz]),
        [zlib_lib="$withval"])

    if test "$check_zlib" = "yes"; then
	sav_CPPFLAGS=$CPPFLAGS
	if test x"$zlib_inc_dir" != x; then
                CPPFLAGS="$CPPFLAGS -I$zlib_inc_dir"
        fi
        AC_CHECK_HEADER([zlib.h], [],
        [
            AC_MSG_NOTICE([error: no zlib.h found; check path for ZLIB package first...])
            zlib_error="yes"
        ])
	CPPFLAGS=$sav_CPPFLAGS

        if test x"$zlib_lib" = x -a "$zlib_error" = "no"; then
            sav_LIBS=$LIBS
            cl="-lz"
            if test x"$zlib_lib_dir" != x; then
                cl="-L$zlib_lib_dir $cl"
            fi
            LIBS="$LIBS $cl"
            AC_MSG_CHECKING([whether linking with -lz works])
            AC_TRY_LINK([],[],
            [AC_MSG_RESULT([yes]); zlib_lib=-lz],[AC_MSG_RESULT([no])])
            LIBS=$sav_LIBS
        fi

        if test x"$zlib_lib" = x -a "$zlib_error" = "no"; then
            AC_MSG_NOTICE([error: no libz found; check path for ZLIB package first...])
            zlib_error="yes"
        fi

        if test $zlib_error = "no"; then
            AC_DEFINE(HAVE_ZLIB)
            have_zlib="yes"
        fi
   fi

    ZLIB_LIB_DIR=$zlib_lib_dir
    ZLIB_LIB_LINE=$zlib_lib
    if test x"$zlib_lib_dir" != x; then
        ZLIB_LIB_LINE="-L$zlib_lib_dir $ZLIB_LIB_LINE"
    fi

    ZLIB_INCLUDE_DIR=$zlib_inc_dir
    ZLIB_INCLUDE_LINE=
    if test x"$zlib_inc_dir" != x; then
        ZLIB_INCLUDE_LINE="-I$zlib_inc_dir"
    fi

    AC_SUBST(ZLIB_LIB_DIR)
    AC_SUBST(ZLIB_LIB_LINE)
    AC_SUBST(ZLIB_INCLUDE_DIR)
    AC_SUBST(ZLIB_INCLUDE_LINE)
])
