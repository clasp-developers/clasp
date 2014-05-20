AC_DEFUN([CHECK_SWIG_PYTHON],
[
    swig_python_error="no"
    check_swig_python="no"
    force_swig_python="no"
    have_swig_python="no"

    AC_ARG_VAR([PYTHON_VERSION], [The installed Python
version to use, for example '2.3'. This string
will be appended to the Python interpreter
canonical name.])

    AC_ARG_VAR([SWIG_PYTHON_OPTS], [SWIG options for generating Python wrappers, default: -python])
    if test x"$SWIG_PYTHON_OPTS" = x; then
        SWIG_PYTHON_OPTS="-python"
    fi

    AC_ARG_VAR([SWIG_PYTHON_CFLAGS], [C compiler flags for compiling SWIG generated Python wrappers])

    AC_ARG_ENABLE([python-bindings],
        AC_HELP_STRING([--enable-python-bindings],
            [enable generation of Python bindings, default: no]),
        [if test "$enableval" = "yes"; then check_swig_python="yes"; force_swig_python="yes"; fi])

    if test "$check_swig_python" = "yes"; then
        AC_CHECK_PROG([PYTHON],[python[$PYTHON_VERSION]], [python[$PYTHON_VERSION]])
        if test x"$PYTHON" = x; then
           AC_MSG_NOTICE([error: no python$PYTHON_VERSION found; check Python installation first...])
           swig_python_error="yes"
        fi

        if test x"$SWIG_PYTHON_CFLAGS" = x -a "$swig_python_error" = "no"; then
            AC_CHECK_PROG([python_config], [python[$PYTHON_VERSION]-config], [python[$PYTHON_VERSION]-config])

            if test x"$python_config" != x; then
                AC_MSG_CHECKING([for python compiler flags])
                # cannot use '--cflags' here; the resulting flags might contain GNU specific flags
                # which are not compatible with other compilers; use '--includes' instead of '--cflags'
                #SWIG_PYTHON_CFLAGS=`$python_config --cflags
                SWIG_PYTHON_CFLAGS=`$python_config --includes`
                AC_MSG_RESULT([$SWIG_PYTHON_CFLAGS])
            fi
        fi

        if test "$swig_python_error" = "no"; then
            sav_PACKAGE=$PACKAGE
            PACKAGE=`echo $PACKAGE | tr '[A-Z]' '[a-z]'`
            AM_PATH_PYTHON([], [], [swig_python_error="yes"])
            PACKAGE=$sav_PACKAGE
        fi

        if test "$swig_python_error" = "no"; then
            sav_CPPFLAGS=$CPPFLAGS
            sav_CFLAGS=$CFLAGS
            CPPFLAGS="$CPPFLAGS $SWIG_PYTHON_CFLAGS"
            CFLAGS="$CFLAGS $SWIG_PYTHON_CFLAGS"
            AC_CHECK_HEADER([Python.h], [],
            [
                AC_MSG_NOTICE([error: no Python.h found; check Python installation and PYTHON_CFLAGS first...])
                swig_python_error="yes"
            ])
            CPPFLAGS=$sav_CPPFLAGS
            CFLAGS=$sav_CFLAGS
        fi

        if test "$swig_python_error" = "no"; then
            AC_MSG_CHECKING([whether python version >= 3.0.0])
            pyver_major=`echo $PYTHON_VERSION | cut -d '.' -f 1`
            if test $pyver_major -ge 3; then
                req_swig_version="1.3.37"
                AC_MSG_RESULT([yes])
                AC_MSG_NOTICE([required SWIG version >= $req_swig_version])
            else
                AC_MSG_RESULT([no])
            fi

            AC_CHECK_PROG([SWIG], [swig], [swig])
            if test x"$SWIG" = x; then
                AC_MSG_NOTICE([error: no swig found; check SWIG installation first...])
                swig_python_error="yes"
            fi
        fi

        if test x"$req_swig_version" != x -a "$swig_python_error" = "no"; then
            AC_MSG_CHECKING([for SWIG version])
            swig_version=`$SWIG -version 2>&1 | grep "SWIG Version" | cut -d ' ' -f 3`
            AC_MSG_RESULT([$swig_version])
            if test x"$swig_version" = x ; then
                AC_MSG_NOTICE([error: cannot determine SWIG version])
                swig_python_error="yes"
            else
                req_major=`echo $req_swig_version | cut -d '.' -f 1`
                avail_major=`echo $swig_version | cut -d '.' -f 1`
                req_minor=`echo $req_swig_version | cut -d '.' -f 2`
                avail_minor=`echo $swig_version | cut -d '.' -f 2`
                req_patch=`echo $req_swig_version | cut -d '.' -f 3`
                avail_patch=`echo $swig_version | cut -d '.' -f 3`

                swig_python_error="yes"
                if test $avail_major -gt $req_major ; then
                    swig_python_error="no"
                elif test $avail_major -eq $req_major ; then
                    if test $avail_minor -gt $req_minor ; then
                        swig_python_error="no"
                    elif test $avail_minor -eq $req_minor ; then
                        if test $avail_patch -ge $req_patch ; then
                            swig_python_error="no"
                        fi
                    fi
                fi

                if test "$swig_python_error" = "yes" ; then
                    AC_MSG_NOTICE([error: SWIG version >= $req_swig_version required])
                fi
            fi
        fi

        if test "$swig_python_error" = "no"; then
            have_swig_python="yes"
        fi
    fi

    AC_SUBST(SWIG)
    AC_SUBST(SWIG_PYTHON_CFLAGS)
    AC_SUBST(SWIG_PYTHON_OPTS)
])
