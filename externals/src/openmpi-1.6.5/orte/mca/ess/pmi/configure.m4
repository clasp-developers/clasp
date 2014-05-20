# -*- shell-script -*-
#
# Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_ess_pmi_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_ess_pmi_CONFIG],[
    # see if PMI support requested
    ORTE_CHECK_PMI([ess_pmi], [ess_pmi_good=1], [ess_pmi_good=0])
         
    # if check worked, set wrapper flags if so.  
    # Evaluate succeed / fail
    AS_IF([test "$ess_pmi_good" = "1"],
          [ess_pmi_WRAPPER_EXTRA_LDFLAGS="$ess_pmi_LDFLAGS"
           ess_pmi_WRAPPER_EXTRA_LIBS="$ess_pmi_LIBS"
           $1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([ess_pmi_CPPFLAGS])
    AC_SUBST([ess_pmi_LDFLAGS])
    AC_SUBST([ess_pmi_LIBS])

])dnl
