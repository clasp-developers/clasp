# -*- shell-script -*-
#
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([ORTE_CONFIG_FILES],[
    AC_CONFIG_FILES([
        orte/Makefile
        orte/include/Makefile
        orte/etc/Makefile
    
        orte/tools/orted/Makefile
        orte/tools/orterun/Makefile
        orte/tools/wrappers/Makefile
        orte/tools/wrappers/ortecc-wrapper-data.txt
        orte/tools/wrappers/ortec++-wrapper-data.txt
        orte/tools/wrappers/orte.pc
        orte/tools/orte-checkpoint/Makefile
        orte/tools/orte-iof/Makefile
        orte/tools/orte-restart/Makefile
        orte/tools/orte-ps/Makefile
        orte/tools/orte-clean/Makefile
        orte/tools/orte-top/Makefile
        orte/tools/orte-bootproxy/Makefile
    ])
])
