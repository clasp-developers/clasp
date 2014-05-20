dnl 	switch on/off additonal debug output in otf lib
AC_DEFUN([WITH_VERBOSE],
[
    AH_TEMPLATE(OTF_VERBOSE, [], [switch on/off verbose output in otf lib])

    AC_ARG_WITH([verbose],
        AC_HELP_STRING([--with-verbose],[additonal verbose output]),
            [verbose=$withval],
            [verbose=""])

    if test "$verbose" = yes; then
     AC_DEFINE(OTF_VERBOSE)
    fi
])

