# -*- shell-script -*-
#
# Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_installdirs_windows_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_installdirs_windows_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_installdirs_windows_CONFIG],[
    # check for RegOpenKeyEx allowing access to the Windows
    # registry. We should first check that the function is defined,
    # and then check for it's presence in the kernel32 library.
    AC_MSG_CHECKING(for working RegOpenKeyEx)
    AC_RUN_IFELSE([AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT
#include <windows.h>], [
int main( int argc, char** argv ) {
    RegOpenKeyEx( HKEY_CURRENT_USER, "Software\\Open MPI", 0, KEY_READ, NULL);
    return 0; }])],
        [AC_MSG_RESULT([yes])
         $1],
        [AC_MSG_RESULT([no])
         $2],
        [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT
#include <windows.h>], [
int main( int argc, char** argv ) {
    RegOpenKeyEx( HKEY_CURRENT_USER, "Software\\Open MPI", 0, KEY_READ, NULL);
    return 0; }])],
        [AC_MSG_RESULT([yes])
         $1],
        [AC_MSG_RESULT([no])
         $2])])
])dnl

