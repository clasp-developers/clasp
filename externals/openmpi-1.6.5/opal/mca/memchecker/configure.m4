dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

dnl we only want one :)
m4_define(MCA_memchecker_CONFIGURE_MODE, STOP_AT_FIRST)

AC_DEFUN([MCA_memchecker_CONFIG],[
    ompi_show_subsubtitle "Pre-configuring the framework memchecker"

    AC_MSG_CHECKING([if --enable-memchecker was specified])
    AC_ARG_ENABLE(memchecker,
        AC_HELP_STRING([--enable-memchecker],
                       [Enable memory and buffer checks.  Note that disabling will *also* add "memchecker" to the --enable-mca-no-build list (default: disabled)]))
    if test "$enable_memchecker" = "yes"; then
        AC_MSG_RESULT([yes])
        WANT_MEMCHECKER=1
    else
        AC_MSG_RESULT([no (adding "memchecker" to --enable-mca-no-build list)])
        WANT_MEMCHECKER=0

        # Setting this environment variable causes
        # MCA_COMONENT_BUILD_CHECK (in ompi_mca.m4, invoked by
        # MCA_CONFIGURE_FRAMEWORK, below) to rule that the component
        # should not be built.  It is effectively the same as adding
        # "memchecker" to the --enable-mca-no-build list.
        DISABLE_memchecker=1
    fi
    AC_DEFINE_UNQUOTED([OMPI_WANT_MEMCHECKER],
                       [$WANT_MEMCHECKER],
                       [if the memory and buffer checking should be enabled])
    AM_CONDITIONAL([OMPI_WANT_MEMCHECKER],
                   [test "$WANT_MEMCHECKER" = "1"])

    # first, compile all the components
    MCA_CONFIGURE_FRAMEWORK($1, $2, 1)

    AS_IF([test "$MCA_memchecker_STATIC_COMPONENTS" != "" -o "$MCA_memchecker_DSO_COMPONENTS" != ""],
          [memchecker_base_found=1],
          [memchecker_base_found=0])
    AS_IF([test $WANT_MEMCHECKER -eq 1 -a $memchecker_base_found -eq 0],
          [AC_MSG_WARN([Memchecker support requested, but no memchecker])
           AC_MSG_WARN([components configured successfully.  Did you])
           AC_MSG_WARN([forget --with-valgrind?])
           AC_MSG_ERROR([Cannot continue])])
])
