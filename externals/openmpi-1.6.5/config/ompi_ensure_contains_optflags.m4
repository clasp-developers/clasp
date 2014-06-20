dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2007-2009 Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_ENSURE_CONTAINS_OPTFLAGS],[

# Modularize this setup so that sub-configure.in scripts can use this
# same setup code.

##################################
# Optimization flags
##################################

# If the user did not specify optimization flags, add some (the value
# from $OPTFLAGS)

co_arg="$1"
co_found=0
for co_word in $co_arg; do
    # See http://www.gnu.org/software/autoconf/manual/html_node/Quadrigraphs.html#Quadrigraphs
    # for an explanation of @<:@ and @:>@ -- they m4 expand to [ and ]
    case $co_word in
    -g)              co_found=1 ;;
    -g@<:@1-3@:>@)   co_found=1 ;;
    +K@<:@0-5@:>@)   co_found=1 ;;
    -O)              co_found=1 ;;
    -O@<:@0-9@:>@)   co_found=1 ;;
    -xO)             co_found=1 ;;
    -xO@<:@0-9@:>@)  co_found=1 ;;
    -fast)           co_found=1 ;;

    # The below Sun Studio flags require or
    # trigger -xO optimization
    -xvector*)     co_found=1 ;;
    -xdepend=yes)  co_found=1 ;;

    esac
done

if test "$co_found" = "0"; then
    co_result="$OPTFLAGS $co_arg"
else
    co_result="$co_arg"
fi

# Clean up

unset co_found co_word co_arg
])
