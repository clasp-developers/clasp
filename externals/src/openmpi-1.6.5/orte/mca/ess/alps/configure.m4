# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011      Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# Set the config priority so that, if we can build,
# only ALPS component will build. This is set higher
# than the CNOS component to ensure we don't get both
# since the ALPS component will -only- build if specifically
# ordered to do so - which means we don't want the CNOS one
AC_DEFUN([MCA_ess_alps_PRIORITY], [10])

# MCA_ess_alps_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_ess_alps_CONFIG],[

    AC_CHECK_HEADERS([catamount/cnos_mpi_os.h],
        [orte_mca_ess_alps_have_cnos=1],
        [AC_CHECK_HEADERS([cnos_mpi_os.h],
            [orte_mca_ess_alps_have_cnos=1],
            [orte_mca_ess_alps_have_cnos=0],
            [AC_INCLUDES_DEFAULT])],
        [AC_INCLUDES_DEFAULT])

    dnl one last check to make certain that we have all the right CNOS stuff to
    dnl continue with CNOS support
    AS_IF([test "$orte_mca_ess_alps_have_cnos" = "1"],
        [AC_CHECK_FUNC([cnos_get_rank],
            [orte_mca_ess_alps_have_cnos=1],
            [orte_mca_ess_alps_have_cnos=0])])

    dnl was ess alps requested?
    ORTE_CHECK_ALPS([ess_alps],
        [orte_mca_ess_alps_happy="yes"],
        [orte_mca_ess_alps_happy="no"])

    AC_DEFINE_UNQUOTED([ORTE_MCA_ESS_ALPS_HAVE_CNOS],
                       [$orte_mca_ess_alps_have_cnos],
                       [Whether we have CNOS support in alps ess or not])

    AS_IF([test "$orte_mca_ess_alps_happy" = "yes"],
          [$1],
          [$2])
])dnl
