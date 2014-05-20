dnl 	have the vtf3 dirs specified
AC_DEFUN([CHECK_VTF3],
[
    vtf3_error="no"
    check_vtf3="yes"
    force_vtf3="no"
    have_vtf3="no"

    AH_TEMPLATE(HAVE_VTF3, [], [defined if vtf3 library is to be used])

    AC_ARG_WITH([vtf3],
        AC_HELP_STRING([--with-vtf3],
            [use vtf3, default: yes if found by configure]),
        [if test "$withval" = "yes"; then force_vtf3="yes"; else check_vtf3="no"; fi])

    AC_ARG_WITH([vtf3-dir],
        AC_HELP_STRING([--with-vtf3-dir],
            [give the path for vtf3, default: /usr]),
        [vtf3_dir="$withval/"])

    AC_ARG_WITH([vtf3-inc-dir],
        AC_HELP_STRING([--with-vtf3-inc-dir],
            [give the path dir vtf3-include files, default: VTF3DIR/include]),
        [vtf3_inc_dir="$withval/"],
        [if test x"$vtf3_dir" != x; then vtf3_inc_dir="$vtf3_dir"include/; fi])

    AC_ARG_WITH([vtf3-lib-dir],
        AC_HELP_STRING([--with-vtf3-lib-dir],
            [give the path for VTF3-libraries, default: VTF3DIR/lib]),
        [vtf3_lib_dir="$withval/"],
        [if test x"$vtf3_dir" != x; then vtf3_lib_dir="$vtf3_dir"lib/; fi])

    AC_ARG_WITH([vtf3-lib],
        AC_HELP_STRING([--with-vtf3-lib],
            [use given vtf3, default: -lvtf3]),
        [vtf3_lib="$withval"])

    if test "$check_vtf3" = "yes"; then
	sav_CPPFLAGS=$CPPFLAGS
	if test x"$vtf3_inc_dir" != x; then
		CPPFLAGS="$CPPFLAGS -I$vtf3_inc_dir"
	fi
        AC_CHECK_HEADER([vtf3.h], [],
        [
            AC_MSG_NOTICE([error: no vtf3.h found; check path for VTF3 package first...])
            vtf3_error="yes"
        ])
	CPPFLAGS=$sav_CPPFLAGS

        if test x"$vtf3_lib" = x -a "$vtf3_error" = "no"; then
            sav_LIBS=$LIBS
            cl="-lvtf3"
            if test x"$vtf3_lib_dir" != x; then
                cl="-L$vtf3_lib_dir $cl"
            fi
            LIBS="$LIBS $cl"
            AC_MSG_CHECKING([whether linking with -lvtf3 works])
            AC_TRY_LINK([],[],
            [AC_MSG_RESULT([yes]); vtf3_lib=-lvtf3],[AC_MSG_RESULT([no])])
            LIBS=$sav_LIBS
        fi

        if test x"$vtf3_lib" = x -a "$vtf3_error" = "no"; then
            AC_MSG_NOTICE([error: no libvtf3 found; check path for VTF3 package first...])
            vtf3_error="yes"
        fi

        if test $vtf3_error = "no"; then
            AC_DEFINE(HAVE_VTF3)
            have_vtf3="yes"
        fi
   fi

    VTF3_LIB_DIR=$vtf3_lib_dir
    VTF3_LIB_LINE=$vtf3_lib
    if test x"$vtf3_lib_dir" != x; then
        VTF3_LIB_LINE="-L$vtf3_lib_dir $VTF3_LIB_LINE"
    fi

    VTF3_INCLUDE_DIR=$vtf3_inc_dir
    VTF3_INCLUDE_LINE=
    if test x"$vtf3_inc_dir" != x; then
        VTF3_INCLUDE_LINE="-I$vtf3_inc_dir"
    fi

    AC_SUBST(VTF3_LIB_DIR)
    AC_SUBST(VTF3_LIB_LINE)
    AC_SUBST(VTF3_INCLUDE_DIR)
    AC_SUBST(VTF3_INCLUDE_LINE)
])
