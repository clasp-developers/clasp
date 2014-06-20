dnl 	have the ZOIDFS dirs specified
AC_DEFUN([CHECK_ZOIDFS],
[
    zoidfs_error="no"
    check_zoidfs="yes"
    force_zoidfs="no"
    have_zoidfs="no"

    zoidfs_lib_default="-lzoidfsclient -lzoidfs-util -lzoidfshints -liofsl-c-util"
    bmi_lib_default="-lbmi -lpthread"

    AH_TEMPLATE(HAVE_ZOIDFS, [], [defined if ZOIDFS library is to be used])

    AC_ARG_WITH([zoidfs],
        AC_HELP_STRING([--with-zoidfs],
            [use ZOIDFS, default: yes if found by configure]),
        [if test "$withval" = "yes"; then force_zoidfs="yes"; else check_zoidfs="no"; fi])

    AC_ARG_WITH([zoidfs-dir],
        AC_HELP_STRING([--with-zoidfs-dir],
            [give the path for ZOIDFS, default: /usr]),
        [zoidfs_dir="$withval/"])

    AC_ARG_WITH([zoidfs-inc-dir],
        AC_HELP_STRING([--with-zoidfs-inc-dir],
            [give the path dir ZOIDFS-include files, default: ZOIDFSDIR/include]),
        [zoidfs_inc_dir="$withval/"],
        [if test x"$zoidfs_dir" != x; then zoidfs_inc_dir="$zoidfs_dir"include/; fi])

    AC_ARG_WITH([zoidfs-lib-dir],
        AC_HELP_STRING([--with-zoidfs-lib-dir],
            [give the path for ZOIDFS-libraries, default: ZOIDFSDIR/lib]),
        [zoidfs_lib_dir="$withval/"],
        [if test x"$zoidfs_dir" != x; then zoidfs_lib_dir="$zoidfs_dir"lib/; fi])

    AC_ARG_WITH([zoidfs-lib],
        AC_HELP_STRING([--with-zoidfs-lib],
            [use given ZOIDFS-lib, default: $zoidfs_lib_default BMILIBDIR BMILIB]),
        [zoidfs_lib="$withval"])

    AC_ARG_WITH([bmi-dir],
        AC_HELP_STRING([--with-bmi-dir],
            [give the path for BMI, default: ZOIDFSDIR]),
        [bmi_dir="$withval/"])

    AC_ARG_WITH([bmi-inc-dir],
        AC_HELP_STRING([--with-bmi-inc-dir],
            [give the path dir BMI-include files, default: BMIDIR/include]),
        [bmi_inc_dir="$withval/"],
        [if test x"$bmi_dir" != x; then bmi_inc_dir="$bmi_dir"include/; fi])

    AC_ARG_WITH([bmi-lib-dir],
        AC_HELP_STRING([--with-bmi-lib-dir],
            [give the path for BMI-libraries, default: BMIDIR/lib]),
        [bmi_lib_dir="$withval/"],
        [if test x"$bmi_dir" != x; then bmi_lib_dir="$bmi_dir"lib/; fi])

    AC_ARG_WITH([bmi-lib],
        AC_HELP_STRING([--with-bmi-lib],
            [use given BMI-lib, default: $bmi_lib_default]),
        [bmi_lib="$withval"])

    if test "$check_zoidfs" = "yes"; then
        sav_CPPFLAGS=$CPPFLAGS
        if test x"$bmi_inc_dir" != x; then
            CPPFLAGS="$CPPFLAGS -I$bmi_inc_dir"
        elif test x"$zoidfs_inc_dir" != x; then
            CPPFLAGS="$CPPFLAGS -I$zoidfs_inc_dir"
        fi
        AC_CHECK_HEADER([bmi.h], [],
        [
            AC_MSG_NOTICE([error: no bmi.h found; check path for BMI package first...])
            zoidfs_error="yes"
        ])
        CPPFLAGS=$sav_CPPFLAGS

        if test x"$bmi_lib" = x -a "$zoidfs_error" = "no"; then
            sav_LIBS=$LIBS
            cl=$bmi_lib_default
            if test x"$bmi_lib_dir" != x; then
                cl="-L$bmi_lib_dir $cl"
            elif test x"$zoidfs_lib_dir" != x; then
                cl="-L$zoidfs_lib_dir $cl"
            fi
            LIBS="$LIBS $cl"
            AC_MSG_CHECKING([whether linking with $bmi_lib_default works])
            AC_TRY_LINK([],[],
            [AC_MSG_RESULT([yes]); bmi_lib=$bmi_lib_default],[AC_MSG_RESULT([no])])
            LIBS=$sav_LIBS
        fi

        if test x"$bmi_lib" = x -a "$zoidfs_error" = "no"; then
            AC_MSG_NOTICE([error: no libbmi found; check path for BMI package first...])
            zoidfs_error="yes"
        fi

        if test "$zoidfs_error" = "no"; then
            sav_CPPFLAGS=$CPPFLAGS
            if test x"$zoidfs_inc_dir" != x; then
                CPPFLAGS="$CPPFLAGS -I$zoidfs_inc_dir"
            fi
            AC_CHECK_HEADER([zoidfs.h], [],
            [
                AC_MSG_NOTICE([error: no zoidfs.h found; check path for ZOIDFS package first...])
                zoidfs_error="yes"
            ])
            AC_CHECK_HEADER([zoidfs-hints.h], [],
            [
                AC_MSG_NOTICE([error: no zoidfs-hints.h found; check path for ZOIDFS package first...])
                zoidfs_error="yes"
            ])
            CPPFLAGS=$sav_CPPFLAGS
        fi

        if test x"$zoidfs_lib" = x -a "$zoidfs_error" = "no"; then
            sav_LIBS=$LIBS
            cl=$zoidfs_lib_default
            if test x"$zoidfs_lib_dir" != x; then
                cl="-L$zoidfs_lib_dir $cl"
            fi
            if test x"$bmi_lib_dir" != x; then
                cl="$cl -L$bmi_lib_dir"
            fi
            cl="$cl $bmi_lib"
            LIBS="$LIBS $cl"
            AC_MSG_CHECKING([whether linking with $zoidfs_lib_default $bmi_lib works])
            AC_TRY_LINK([],[],
            [
                AC_MSG_RESULT([yes])
                zoidfs_lib=$zoidfs_lib_default
            ],
            [
                AC_MSG_RESULT([no])
            ])
            LIBS=$sav_LIBS
        fi

        if test x"$zoidfs_lib" = x -a "$zoidfs_error" = "no"; then
            AC_MSG_NOTICE([error: no zoidfs library found; check path for ZOIDFS package first...])
            zoidfs_error="yes"
        fi

        if test $zoidfs_error = "no"; then
            AC_DEFINE(HAVE_ZOIDFS)
            have_zoidfs="yes"
        fi
    fi

    ZOIDFS_LIB_DIR=$zoidfs_lib_dir
    ZOIDFS_INCLUDE_DIR=$zoidfs_inc_dir
    BMI_LIB_DIR=$zoidfs_lib_dir
    BMI_INCLUDE_DIR=$zoidfs_inc_dir

    ZOIDFS_LIB_LINE=$zoidfs_lib
    if test x"$zoidfs_lib_dir" != x; then
        ZOIDFS_LIB_LINE="-L$zoidfs_lib_dir $ZOIDFS_LIB_LINE"
    fi
    if test x"$bmi_lib_dir" != x; then
        ZOIDFS_LIB_LINE="$ZOIDFS_LIB_LINE -L$bmi_lib_dir"
    fi
    ZOIDFS_LIB_LINE="$ZOIDFS_LIB_LINE $bmi_lib"

    ZOIDFS_INCLUDE_LINE=
    if test x"$zoidfs_inc_dir" != x; then
        ZOIDFS_INCLUDE_LINE="-I$zoidfs_inc_dir"
    fi
    if test x"$bmi_inc_dir" != x; then
        ZOIDFS_INCLUDE_LINE="$ZOIDFS_INCLUDE_LINE -I$bmi_inc_dir"
    fi

    AC_SUBST(ZOIDFS_LIB_DIR)
    AC_SUBST(ZOIDFS_LIB_LINE)
    AC_SUBST(ZOIDFS_INCLUDE_DIR)
    AC_SUBST(ZOIDFS_INCLUDE_LINE)
    AC_SUBST(BMI_LIB_DIR)
    AC_SUBST(BMI_INCLUDE_DIR)
])
