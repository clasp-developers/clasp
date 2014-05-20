# -*- shell-script -*-
#
# Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved. 
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_hwloc_hwloc132_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# Include hwloc m4 files
m4_include(opal/mca/hwloc/hwloc132/hwloc/config/hwloc.m4)
m4_include(opal/mca/hwloc/hwloc132/hwloc/config/hwloc_pkg.m4)
m4_include(opal/mca/hwloc/hwloc132/hwloc/config/hwloc_check_attributes.m4)
m4_include(opal/mca/hwloc/hwloc132/hwloc/config/hwloc_check_visibility.m4)
m4_include(opal/mca/hwloc/hwloc132/hwloc/config/hwloc_check_vendor.m4)

# MCA_hwloc_hwloc132_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_hwloc_hwloc132_POST_CONFIG],[
    HWLOC_DO_AM_CONDITIONALS
])dnl


# MCA_hwloc_hwloc132_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_hwloc_hwloc132_CONFIG],[
    OMPI_VAR_SCOPE_PUSH([HWLOC_VERSION opal_hwloc_hwloc132_save_CPPFLAGS opal_hwloc_hwloc132_save_LDFLAGS opal_hwloc_hwloc132_save_LIBS opal_hwloc_hwloc132_save_cairo opal_hwloc_hwloc132_save_xml opal_hwloc_hwloc132_basedir opal_hwloc_hwloc132_file opal_hwloc_hwloc132_save_enable_pci opal_hwloc_hwloc132_save_cflags])

    # default to this component not providing support
    opal_hwloc_hwloc132_basedir=opal/mca/hwloc/hwloc132
    opal_hwloc_hwloc132_support=no

    if test "$with_hwloc" = "internal" -o "$with_hwloc" = "" -o "$with_hwloc" = "yes"; then
        opal_hwloc_hwloc132_save_CPPFLAGS=$CPPFLAGS
        opal_hwloc_hwloc132_save_LDFLAGS=$LDFLAGS
        opal_hwloc_hwloc132_save_LIBS=$LIBS

        # Disable PCI support for now, until we can get a proper fix
        # for Oralce's builds on older SuSE machines that only have
        # libpci.a (and support Oracle's weird dual-bitness build
        # mojo).
        opal_hwloc_hwloc132_save_enable_pci=$enable_pci
        enable_pci=no

        # Run the hwloc configuration - set the prefix to minimize
        # the chance that someone will use the internal symbols
        HWLOC_SET_SYMBOL_PREFIX([opal_hwloc132_])

        # save XML or graphical options
        opal_hwloc_hwloc132_save_cairo=$enable_cairo
        opal_hwloc_hwloc132_save_xml=$enable_xml

        # never enable hwloc's graphical option
        enable_cairo=no

        # Override -- disable hwloc's libxml2 support, but enable the
        # native hwloc XML support
        enable_libxml2=no
        enable_xml=yes

        # hwloc checks for compiler visibility, and its needs to do
        # this without "picky" flags.
        opal_hwloc_hwloc132_save_cflags=$CFLAGS
        CFLAGS=$OMPI_CFLAGS_BEFORE_PICKY
        HWLOC_SETUP_CORE([opal/mca/hwloc/hwloc132/hwloc], 
                  [AC_MSG_CHECKING([whether hwloc configure succeeded])
                   AC_MSG_RESULT([yes])
                   HWLOC_VERSION="internal v`$srcdir/$opal_hwloc_hwloc132_basedir/hwloc/config/hwloc_get_version.sh $srcdir/$opal_hwloc_hwloc132_basedir/hwloc/VERSION`"

                   # Build flags for our Makefile.am
                   opal_hwloc_hwloc132_LDFLAGS='$(HWLOC_EMBEDDED_LDFLAGS)'
                   opal_hwloc_hwloc132_LIBS='$(top_ompi_builddir)/'"$opal_hwloc_hwloc132_basedir"'/hwloc/src/libhwloc_embedded.la $(HWLOC_EMBEDDED_LIBS)'
                   opal_hwloc_hwloc132_support=yes], 
                  [AC_MSG_CHECKING([whether hwloc configure succeeded])
                   AC_MSG_RESULT([no])
                   opal_hwloc_hwloc132_support=no])
        CFLAGS=$opal_hwloc_hwloc132_save_cflags

        # Restore some env variables, if necessary
        AS_IF([test -n "$opal_hwloc_hwloc132_save_cairo"],
              [enable_cairo=$opal_hwloc_hwloc132_save_cairo])
        AS_IF([test -n "$opal_hwloc_hwloc132_save_xml"],
              [enable_xml=$opal_hwloc_hwloc132_save_xml])
       
        enable_pci=$opal_hwloc_hwloc132_save_enable_pci
        CPPFLAGS=$opal_hwloc_hwloc132_save_CPPFLAGS
        LDFLAGS=$opal_hwloc_hwloc132_save_LDFLAGS
        LIBS=$opal_hwloc_hwloc132_save_LIBS

        AC_SUBST([opal_hwloc_hwloc132_CFLAGS])
        AC_SUBST([opal_hwloc_hwloc132_CPPFLAGS])
        AC_SUBST([opal_hwloc_hwloc132_LDFLAGS])
        AC_SUBST([opal_hwloc_hwloc132_LIBS])
    fi

    # Done!
    AS_IF([test "$opal_hwloc_hwloc132_support" = "yes"],
          [AC_DEFINE_UNQUOTED([HWLOC_HWLOC132_HWLOC_VERSION], 
                              ["$HWLOC_VERSION"], 
                              [Version of hwloc])

           # Set these variables so that the framework m4 knows
           # what file to include in opal/mca/hwloc/hwloc.h
           opal_hwloc_hwloc132_include="$opal_hwloc_hwloc132_basedir/hwloc132.h"

           # Also pass some *_ADD_* flags upwards to the framework m4
           # for various compile/link flags that are needed a) to
           # build the rest of the source tree, and b) for the wrapper
           # compilers (in the --with-devel-headers case).
           opal_hwloc_hwloc132_file=$opal_hwloc_hwloc132_basedir/hwloc
           opal_hwloc_hwloc132_ADD_CPPFLAGS="-I$OMPI_TOP_SRCDIR/$opal_hwloc_hwloc132_file/include"
           AS_IF([test "$OMPI_TOP_BUILDDIR" != "$OMPI_TOP_SRCDIR"],
                 [opal_hwloc_hwloc132_ADD_CPPFLAGS="$opal_hwloc_hwloc132_ADD_CPPFLAGS -I$OMPI_TOP_BUILDDIR/$opal_hwloc_hwloc132_file/include"])
           if test "$with_devel_headers" = "yes" ; then
               opal_hwloc_hwloc132_ADD_WRAPPER_EXTRA_CPPFLAGS='-I${includedir}/openmpi/'"$opal_hwloc_hwloc132_basedir/hwloc/include"
           fi

           opal_hwloc_hwloc132_ADD_WRAPPER_EXTRA_LIBS=$HWLOC_EMBEDDED_LIBS

           $1],
          [$2])

    OMPI_VAR_SCOPE_POP
])dnl
