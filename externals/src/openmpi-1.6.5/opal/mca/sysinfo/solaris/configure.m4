# -*- shell-script -*-
#
# Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# MCA_sysinfo_solaris_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_sysinfo_solaris_CONFIG],[
    # check to see if we are on a solaris machine
    case $host in
        *solaris*)
            if test "x$ompi_cv_c_compiler_vendor" = "xsun"; then
                sysinfo_solaris_happy=yes
                AC_MSG_NOTICE([*** Open MPI supports solaris sysinfo on this platform])
            else
                sysinfo_solaris_happy=no
            fi
            ;;
        *)
            sysinfo_solaris_happy=no
            AC_MSG_NOTICE([*** Open MPI does not support solaris sysinfo on this platform])
            ;;
    esac

    AS_IF([test "$sysinfo_solaris_happy" = "yes"],
        [OPAL_SETUP_COMPONENT_PACKAGE([sysinfo],
                                     [solaris],
                                     [libpicl],
                                     [include/picl.h],
                                     [lib/libpicl*],
                                     [picl.h],
                                     [picl],
                                     [picl_initialize],
                                     [],
                                     [AC_CHECK_DECLS([MPOL_MF_MOVE])
                                     $1],
                                     [$2])],
        [$2])
])
