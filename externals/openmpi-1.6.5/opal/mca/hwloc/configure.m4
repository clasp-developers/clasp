dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# There will only be one component used in this framework, and it will
# be selected at configure time by priority.  Components must set
# their priorities in their configure.m4 files.  They must also set
# the shell variable $opal_hwloc_<component>_include to a header file
# name (relative to the top OMPI source directory) that will be
# included in opal/mca/hwloc/hwloc.h.

# Optionally, components may also set the following shell variables:
#
# opal_hwloc_<component>_ADD_CPPFLAGS
# opal_hwloc_<component>_ADD_LDFLAGS
# opal_hwloc_<component>_ADD_LIBS
# opal_hwloc_<component>_ADD_WRAPPER_EXTRA_CPPFLAGS
# opal_hwloc_<component>_ADD_WRAPPER_EXTRA_LDFLAGS
# opal_hwloc_<component>_ADD_WRAPPER_EXTRA_LIBS
#
# The first 3 will be added to the over all CPPFLAGS/LDFLAGS/LIBS if
# that component is chosen as the winning component.  Similarly, the
# latter 3 will be added to WRAPPER_EXTRA_* if that component wins.

dnl We only want one winning component.
m4_define(MCA_hwloc_CONFIGURE_MODE, STOP_AT_FIRST_PRIORITY)

# Other components may depend on at least 1 hwloc component being
# available.  As such, we may need to artificially force this
# framework to be configured first.  Hence, we move the entirety of
# the hwloc framework's m4 to a separate macro and AC REQUIRE it.
# Other components can do this as well.  This will guarantee that
# OPAL_HAVE_HWLOC is set to 0 or 1 *before* some component needs to
# check it.

AC_DEFUN([MCA_hwloc_CONFIG],[
    # Use a crude shell variable to know whether this component is
    # being required "early".  See below.
    opal_hwloc_its_time_to_configure=1
    AC_REQUIRE([MCA_hwloc_CONFIG_REQUIRE])
])

# See comments above for why this is a separate macro.

AC_DEFUN([MCA_hwloc_CONFIG_REQUIRE],[

   # If this shell variable is not 1, then this m4 is being invoked
   # "early" via AC REQUIRE.  Therefore, since we like having fairly
   # readable configure output, print out a nice banner explaining why
   # this is coming early.
   AS_IF([test "$opal_hwloc_its_time_to_configure" != "1"],
         [echo " "
          echo "==> Pre-emptively configuring the hwloc framework to satisfy dependencies."])

    # See if we want hwloc, and if so, internal vs external
    AC_ARG_WITH(hwloc,
        AC_HELP_STRING([--with-hwloc(=DIR)],
                       [Build hwloc support.  DIR can take one of three values: "internal", "external", or a valid directory name.  "internal" (or no DIR value) forces Open MPI to use its internal copy of hwloc.  "external" forces Open MPI to use an external installation of hwloc.  Supplying a valid directory name also forces Open MPI to use an external installation of hwloc, and adds DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries.]))

    # set defaults of not having any support
    opal_hwloc_base_enable_xml=0
    OPAL_HAVE_HWLOC=0

    # Configure all the components - always have to do this, even if
    # we configure --without-hwloc.  Note that instead of passing in
    # the traditional $1 and $2 as the first arguments, we hard-code
    # "opal" and "hwloc", because this macro is invoked via AC
    # REQUIRE.
    MCA_CONFIGURE_FRAMEWORK([opal], [hwloc], 1)

    # Strip any leading/trailing spaces
    opal_hwloc_winner=`echo $MCA_hwloc_STATIC_COMPONENTS | sed -e 's/^[ ]+//' | sed -e 's/[ ]+$//'`

    # Give a blank line to separate these messages from the last
    # component's configure.m4 output.
    echo " "

    # Unless --with-hwloc[=<foo>] was given, it's ok to have no hwloc
    # component.
    AS_IF([test "$with_hwloc" = "no" -o "$with_hwloc" = ""], [],
       [ # STOP_AT_FIRST_PRIORITY will guarantee that we find at most
        # one.  We need to check here that we found *at least* one.
        AS_IF([test "$MCA_hwloc_STATIC_COMPONENTS" = ""],
              [AC_MSG_WARN([Did not find a suitable static opal hwloc component])
               AC_MSG_ERROR([Cannot continue])])

        # If there's any spaces in the middle of the string, then we
        # found more than 1 eligible static component.  That's no good
        # (and should never happen, but let's be sure)!
        AS_IF([test "`echo $opal_hwloc_winner | sed 's/ //'`" != "$opal_hwloc_winner"],
              [AC_MSG_WARN([Found more than 1 eligible static opal hwloc component])
               AC_MSG_WARN([This should never happen!])
               AC_MSG_ERROR([Cannot continue])])
   ])

   # If we have a winning component, do some more logic
   AS_IF([test "$MCA_hwloc_STATIC_COMPONENTS" != ""],
       [ # We had a winner -- w00t!
        OPAL_HAVE_HWLOC=1

        # The winning component will have told us where their header file
        # is located
        AC_MSG_CHECKING([for winning hwloc component header file])
        eval "opal_hwloc_base_include=\`echo \$opal_hwloc_${opal_hwloc_winner}_include\`"
        AS_IF([test "$opal_hwloc_base_include" = ""],
              [AC_MSG_RESULT([missing])
               AC_MSG_WARN([Missing implementation header])
               AC_MSG_ERROR([Cannot continue])])
        AC_MSG_RESULT([$opal_hwloc_base_include])

        AC_DEFINE_UNQUOTED([MCA_hwloc_IMPLEMENTATION_HEADER],
                           ["$opal_hwloc_base_include"],
                           [Header to include for hwloc implementation])

        # See if they set any flags for us
        _MCA_opal_hwloc_base_flags([CPPFLAGS], [CPPFLAGS])
        _MCA_opal_hwloc_base_flags([LDFLAGS], [LDFLAGS])
        _MCA_opal_hwloc_base_flags([LIBS], [LIBS])
        _MCA_opal_hwloc_base_flags([wrapper CPPFLAGS], [WRAPPER_EXTRA_CPPFLAGS])
        _MCA_opal_hwloc_base_flags([wrapper LDFLAGS], [WRAPPER_EXTRA_LDFLAGS])
        _MCA_opal_hwloc_base_flags([wrapper LIBS], [WRAPPER_EXTRA_LIBS])

        # If we added any -L flags to ADD_LDFLAGS, then we (might)
        # need to add those directories to LD_LIBRARY_PATH.
        # Otherwise, if we try to AC RUN_IFELSE anything here in
        # configure, it might die because it can't find the libraries
        # we just linked against.
        found_l=0
        eval "tmp=\$opal_hwloc_${opal_hwloc_winner}_ADD_LIBS"
        for token in $tmp; do
            case $token in
            -l*) found_l=1 ;;
            esac
        done
        AS_IF([test $found_l -eq 1],
              [eval "tmp=\$opal_hwloc_${opal_hwloc_winner}_ADD_LDFLAGS"
               for token in $tmp; do
                   case $token in
                   -L*)
                       dir=`echo $token | cut -c3-`
                       export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$dir
                       AC_MSG_WARN([Adding to (DY)LD_LIBRARY_PATH: $dir])
                       ;;
                   esac
               done])
    ])

    AM_CONDITIONAL(OPAL_HAVE_HWLOC, test $OPAL_HAVE_HWLOC -eq 1)
    AC_DEFINE_UNQUOTED(OPAL_HAVE_HWLOC, $OPAL_HAVE_HWLOC,
        [Whether we have hwloc support or not])

   # Similar to above, if this m4 is being invoked "early" via AC
   # REQUIRE, print out a nice banner that we have now finished
   # pre-emption and are returning to the Normal Order Of Things.
   AS_IF([test "$opal_hwloc_its_time_to_configure" != "1"],
         [echo " "
          echo "<== Pre-emptive hwloc framework configuration complete."
          echo "<== We now return you to your regularly scheduled programming."
          echo " "]);
])




dnl Helper function
dnl $1 = message to display
dnl $2 = output variable to set / input variable suffix
AC_DEFUN([_MCA_opal_hwloc_base_flags],[
        AC_MSG_CHECKING([for winning hwloc component additional $1])
        eval "opal_hwloc_base_tmp=\`echo \$opal_hwloc_${opal_hwloc_winner}_ADD_$2\`"
        AS_IF([test "$opal_hwloc_base_tmp" != ""],
              [AC_MSG_RESULT([$opal_hwloc_base_tmp])
               $2="[$]$2 $opal_hwloc_base_tmp"],
              [AC_MSG_RESULT([none])])
])dnl
