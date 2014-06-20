dnl 	switch on/off additonal debug output in otf lib
AC_DEFUN([WITH_DEBUG],
[
    AH_TEMPLATE(OTF_DEBUG, [], [switch on/off additonal debug checks in otf lib])

    AC_ARG_WITH([debug],
        AC_HELP_STRING([--with-debug],[additonal debug checks]),
            [debug=$withval],
            [debug=""])

    if test "$debug" = yes; then
     AC_DEFINE(OTF_DEBUG)
    fi
])

