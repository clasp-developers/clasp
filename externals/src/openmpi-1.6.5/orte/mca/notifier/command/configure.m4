# -*- command-script -*-
#
# Copyright (c) 2007      Sandia National Laboratories. All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_notifier_command_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_notifier_command_CONFIG], [
    # We need fork() and pipe()
    AC_CHECK_FUNC([fork], 
                  [AC_CHECK_FUNC([pipe], [$1], [$2])], [$2])
])
