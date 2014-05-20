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
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OPAL_CASE_SENSITIVE_FS_SETUP],[
#
# Arguments: none
#
# Dependencies: None
#
# See if we are on a case sensitive filesystem.  Some filesystems
# (like HFS+ on MacOS X and MS Windows) are not case sensitive - opalcc
# and opalCC are the same file.
#
# Sets prefix_OS_HAVE_CASE_SENSITIVE_FS to 1 if filesystem is case
# sensitive (ie, mpicc and mpiCC will be different files) or 0 if
# filesystem is not case sensitive (ie, mpicc and mpiCC will be
# the same file).
#
#

have_cs_fs=1

AC_MSG_CHECKING([if build filesystem is case sensitive])
cat > conf_fs_test.$$ <<EOF
lowercase
EOF

cat > CONF_FS_TEST.$$ <<EOF
uppercase
EOF

if test "`cat conf_fs_test.$$`" = "lowercase"; then
    have_cs_fs=1
    AC_MSG_RESULT([yes])
else
    have_cs_fs=0
    AC_MSG_RESULT([no])
fi

rm -f conf_fs_test.$$ CONF_FS_TEST.$$

#
# Now see what the user wants to do...
#
AC_MSG_CHECKING([if configuring for case sensitive filesystem])
AC_ARG_WITH(cs_fs, 
            AC_HELP_STRING([--with-cs-fs],
                           [Destination FS is case sensitive (default: set to value of the build FS's case sensitivity)]))

if test "$with_cs_fs" = "yes"; then
    OPAL_WANT_CS_FS=1
elif test -z "$with_cs_fs"; then
    OPAL_WANT_CS_FS=$have_cs_fs
else
    OPAL_WANT_CS_FS=0
fi

if test "$OPAL_WANT_CS_FS" = "1"; then
    AC_MSG_RESULT([yes])
else
    AC_MSG_RESULT([no])
fi

AM_CONDITIONAL(CASE_SENSITIVE_FS, test "$OPAL_WANT_CS_FS" = "1")

if test "$OPAL_WANT_CS_FS" = "0"; then
	cat <<EOF

*******************************************************************************
NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE
*******************************************************************************

Because OPAL is being installed on a non-case sensitive file
system, the C++ wrapper compiler will be named opalc++ instead of the
traditional opalCC.

Please update any makefiles appropriately.

*******************************************************************************
NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE
*******************************************************************************

EOF
fi

# Clean up
unset have_cs_fs])dnl
