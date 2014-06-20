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
# Copyright (c) 2012      Cisco Systems,Inc.  All rights reserved.
# Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# _OMPI_CHECK_PACKAGE_HEADER(prefix, header, dir-prefix, 
#                            [action-if-found], [action-if-not-found],
*                            includes)
# --------------------------------------------------------------------
AC_DEFUN([_OMPI_CHECK_PACKAGE_HEADER], [
    # This is stolen from autoconf to peek under the covers to get the
    # cache variable for the library check.  one should not copy this
    # code into other places unless you want much pain and suffering
    AS_VAR_PUSHDEF([ompi_Header], [ac_cv_header_$2])

    # so this sucks, but there's no way to get through the progression
    # of header includes without killing off the cache variable and trying
    # again...  
    unset ompi_Header

    ompi_check_package_header_happy="no"
    AS_IF([test "$3" = "/usr" -o "$3" = "/usr/local"],
           [ # try as is...
            AC_VERBOSE([looking for header without includes])
            AC_CHECK_HEADER([$2], [ompi_check_package_header_happy="yes"],
                            [ompi_check_package_header_happy="no"])
            AS_IF([test "$ompi_check_package_header_happy" = "no"],
                  [# no go on the as is - reset the cache and try again
                   unset ompi_Header])])

    AS_IF([test "$ompi_check_package_header_happy" = "no"],
          [AS_IF([test "$3" != ""],
                 [$1_CPPFLAGS="$$1_CPPFLAGS -I$3/include"
                  CPPFLAGS="$CPPFLAGS -I$3/include"])
          AC_CHECK_HEADER([$2], [$4], [$5], [$6])],
          [$4])
    unset ompi_check_package_header_happy
    AS_VAR_POPDEF([ompi_Header])
])


# _OMPI_CHECK_PACKAGE_LIB(prefix, library, function, extra-libraries,
#                         dir-prefix, libdir,
#                         [action-if-found], [action-if-not-found]])
# --------------------------------------------------------------------
AC_DEFUN([_OMPI_CHECK_PACKAGE_LIB], [
    # This is stolen from autoconf to peek under the covers to get the
    # cache variable for the library check.  one should not copy this
    # code into other places unless you want much pain and suffering
    AS_LITERAL_IF([$2],
                  [AS_VAR_PUSHDEF([ompi_Lib], [ac_cv_lib_$2_$3])],
                  [AS_VAR_PUSHDEF([ompi_Lib], [ac_cv_lib_$2''_$3])])dnl

    # see comment above
    unset ompi_Lib
    ompi_check_package_lib_happy="no"
    AS_IF([test "$6" != ""],
          [ # libdir was specified - search only there
           $1_LDFLAGS="$$1_LDFLAGS -L$6"
           LDFLAGS="$LDFLAGS -L$6"
           AC_CHECK_LIB([$2], [$3], 
                        [ompi_check_package_lib_happy="yes"], 
                        [ompi_check_package_lib_happy="no"], [$4])
           AS_IF([test "$ompi_check_package_lib_happy" = "no"],
                 [LDFLAGS="$ompi_check_package_$1_save_LDFLAGS"
                  $1_LDFLAGS="$ompi_check_package_$1_orig_LDFLAGS"
                  unset ompi_Lib])],
          [ # libdir was not specified - go through search path
           ompi_check_package_libdir="$5"
           AS_IF([test "$ompi_check_package_libdir" = "" -o "$ompi_check_package_libdir" = "/usr" -o "$ompi_check_package_libdir" = "/usr/local"],
               [ # try as is...
                AC_VERBOSE([looking for library without search path])
                AC_CHECK_LIB([$2], [$3], 
                        [ompi_check_package_lib_happy="yes"], 
                        [ompi_check_package_lib_happy="no"], [$4])
                AS_IF([test "$ompi_check_package_lib_happy" = "no"],
                    [ # no go on the as is..  see what happens later...
                     LDFLAGS="$ompi_check_package_$1_save_LDFLAGS"
                     $1_LDFLAGS="$ompi_check_package_$1_orig_LDFLAGS"
                     unset ompi_Lib])])

           AS_IF([test "$ompi_check_package_lib_happy" = "no"],
               [AS_IF([test "$ompi_check_package_libdir" != ""],
                    [$1_LDFLAGS="$$1_LDFLAGS -L$ompi_check_package_libdir/lib"
                     LDFLAGS="$LDFLAGS -L$ompi_check_package_libdir/lib"
                     AC_VERBOSE([looking for library in lib])
                     AC_CHECK_LIB([$2], [$3], 
                               [ompi_check_package_lib_happy="yes"], 
                               [ompi_check_package_lib_happy="no"], [$4])
                     AS_IF([test "$ompi_check_package_lib_happy" = "no"],
                         [ # no go on the as is..  see what happens later...
                          LDFLAGS="$ompi_check_package_$1_save_LDFLAGS"
                          $1_LDFLAGS="$ompi_check_package_$1_orig_LDFLAGS"
                          unset ompi_Lib])])])

           AS_IF([test "$ompi_check_package_lib_happy" = "no"],
               [AS_IF([test "$ompi_check_package_libdir" != ""],
                    [$1_LDFLAGS="$$1_LDFLAGS -L$ompi_check_package_libdir/lib64"
                     LDFLAGS="$LDFLAGS -L$ompi_check_package_libdir/lib64"
                     AC_VERBOSE([looking for library in lib64])
                     AC_CHECK_LIB([$2], [$3], 
                               [ompi_check_package_lib_happy="yes"], 
                               [ompi_check_package_lib_happy="no"], [$4])
                     AS_IF([test "$ompi_check_package_lib_happy" = "no"],
                         [ # no go on the as is..  see what happens later...
                          LDFLAGS="$ompi_check_package_$1_save_LDFLAGS"
                          $1_LDFLAGS="$ompi_check_package_$1_orig_LDFLAGS"
                          unset ompi_Lib])])])])

    AS_IF([test "$ompi_check_package_lib_happy" = "yes"],
          [$1_LIBS="-l$2 $4"
           $7], [$8])

    AS_VAR_POPDEF([ompi_Lib])dnl
])
    

# OMPI_CHECK_PACKAGE(prefix, 
#                    header, 
#                    library, 
#                    function, 
#                    extra-libraries, 
#                    dir-prefix,
#                    libdir-prefix,
#                    [action-if-found], [action-if-not-found],
#                    includes)
# -----------------------------------------------------------
# check for package defined by header and libs, and probably
# located in dir-prefix, possibly with libs in libdir-prefix.
# Both dir-prefix and libdir-prefix can be empty.  Will set
# prefix_{CPPFLAGS, LDFLAGS, LIBS} as needed
AC_DEFUN([OMPI_CHECK_PACKAGE],[
    ompi_check_package_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_package_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_package_$1_save_LIBS="$LIBS"

    ompi_check_package_$1_orig_CPPFLAGS="$$1_CPPFLAGS"
    ompi_check_package_$1_orig_LDFLAGS="$$1_LDFLAGS"
    ompi_check_package_$1_orig_LIBS="$$1_LIBS"

    _OMPI_CHECK_PACKAGE_HEADER([$1], [$2], [$6], 
          [_OMPI_CHECK_PACKAGE_LIB([$1], [$3], [$4], [$5], [$6], [$7],
                [ompi_check_package_happy="yes"],
                [ompi_check_package_happy="no"])],
          [ompi_check_package_happy="no"],
          [$10])

    AS_IF([test "$ompi_check_package_happy" = "yes"],
          [$8],
          [$1_CPPFLAGS="$ompi_check_package_$1_orig_CPPFLAGS"
           $1_LDFLAGS="$ompi_check_package_$1_orig_LDFLAGS"
           $1_LIBS="$ompi_check_package_$1_orig_LIBS"
           $9])

    CPPFLAGS="$ompi_check_package_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_package_$1_save_LDFLAGS"
    LIBS="$ompi_check_package_$1_save_LIBS"
]) 
