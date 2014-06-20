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
dnl Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_STRIP_OPTFLAGS],[

# Process a set of flags and remove all debugging and optimization
# flags

s_arg="$1"
s_result=
for s_word in $s_arg; do
    # See http://www.gnu.org/software/autoconf/manual/html_node/Quadrigraphs.html#Quadrigraphs
    # for an explanation of @<:@ and @:>@ -- they m4 expand to [ and ]
    case $s_word in
    -g)                 ;;
    -g@<:@1-3@:>@)      ;;
    +K@<:@0-5@:>@)      ;;
    -O)                 ;;
    -O@<:@0-9@:>@)      ;;
    -xO)                ;;
    -xO@<:@0-9@:>@)     ;;
    -fast)              ;;
    -finline-functions) ;;

    # The below Sun Studio flags require or
    # trigger -xO optimization
    -xvector*)          ;;
    -xdepend=yes)       ;;

    *)     s_result="$s_result $s_word"
    esac
done

# Clean up

unset s_word s_arg])
