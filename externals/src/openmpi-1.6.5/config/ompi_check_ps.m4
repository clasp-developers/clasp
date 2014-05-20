dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.  
dnl Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# See if there is a ps command that will produce the output we are
# interested in.  If so, then save it away.  Otherwise, the string is
# set to unknown.

AC_DEFUN([OMPI_PS_FLAVOR_CHECK],[
AC_MSG_CHECKING([for flavor of ps to use])
PS_FLAVOR="unknown"
ps -A -o fname > /dev/null 2>&1

if test "$?" = "0"; then
     PS_FLAVOR="ps -A -o fname,pid,user"
else
     ps -A -o command > /dev/null 2>&1
     if test "$?" = "0"; then
         PS_FLAVOR="ps -A -o command,pid,user"
     fi
fi
AC_MSG_RESULT([$PS_FLAVOR])
AC_DEFINE_UNQUOTED([ORTE_CLEAN_PS_CMD], ["$PS_FLAVOR"], [Specific ps command to use in orte-clean])
])

