# -*- shell-script -*-
#
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This file is m4_included in the top-level configure.ac

AC_DEFUN([OPAL_CONFIG_FILES],[
    AC_CONFIG_FILES([
        opal/Makefile
        opal/etc/Makefile
        opal/include/Makefile
        opal/asm/Makefile
        opal/datatype/Makefile
        opal/event/Makefile
        opal/event/compat/Makefile
        opal/event/compat/sys/Makefile
        opal/util/Makefile
        opal/util/keyval/Makefile
        opal/mca/base/Makefile
        opal/tools/wrappers/Makefile
        opal/tools/wrappers/opalcc-wrapper-data.txt
        opal/tools/wrappers/opalc++-wrapper-data.txt
        opal/tools/wrappers/opal.pc
        opal/tools/opal-checkpoint/Makefile
        opal/tools/opal-restart/Makefile
    ])
])
