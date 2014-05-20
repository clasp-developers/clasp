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
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# ORTE_CHECK_SLURM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([ORTE_CHECK_SLURM],[
    AC_ARG_WITH([slurm],
                [AC_HELP_STRING([--with-slurm],
                                [Build SLURM scheduler component (default: yes)])])

    if test "$with_slurm" = "no" ; then
        orte_check_slurm_happy="no"
    elif test "$with_slurm" = "" ; then
        # unless user asked, only build slurm component on linux, AIX,
        # and OS X systems (these are the platforms that SLURM
        # supports)
        case $host in
            *-linux*|*-aix*|*-apple-darwin*)
                orte_check_slurm_happy="yes"
                ;;
            *)
                AC_MSG_CHECKING([for SLURM srun in PATH])
                OMPI_WHICH([srun], [ORTE_CHECK_SLURM_SRUN])
                if test "$ORTE_CHECK_SLURM_SRUN" = ""; then
                    orte_check_slurm_happy="no"
                else
                    orte_check_slurm_happy="yes"
                fi
                AC_MSG_RESULT([$orte_check_slurm_happy])
                ;;
        esac
    else 
        orte_check_slurm_happy="yes"
    fi

    AS_IF([test "$orte_check_slurm_happy" = "yes"],
          [AC_CHECK_FUNC([fork],
                         [orte_check_slurm_happy="yes"],
                         [orte_check_slurm_happy="no"])])

    AS_IF([test "$orte_check_slurm_happy" = "yes"],
          [AC_CHECK_FUNC([execve],
                         [orte_check_slurm_happy="yes"],
                         [orte_check_slurm_happy="no"])])

    AS_IF([test "$orte_check_slurm_happy" = "yes"],
          [AC_CHECK_FUNC([setpgid],
                         [orte_check_slurm_happy="yes"],
                         [orte_check_slurm_happy="no"])])

    AS_IF([test "$orte_check_slurm_happy" = "yes"], 
          [$2], 
          [$3])
])
