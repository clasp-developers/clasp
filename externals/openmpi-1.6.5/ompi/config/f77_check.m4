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


# OMPI_F77_CHECK(Fortran type, c type required, 
#                types to search, expected size)
#----------------------------------------------------------
# Check Fortran type, including:
# - whether compiler supports or not
# - size of type
# - equal to expected size
# - alignment
# - associated C type
#
# types to search is a comma-seperated list of values
AC_DEFUN([OMPI_F77_CHECK], [
    OMPI_VAR_SCOPE_PUSH([ofc_have_type ofc_type_size ofc_type_alignment ofc_c_type ofc_expected_size])

    ofc_expected_size=$4
    ofc_have_type=0
    ofc_type_size=$ac_cv_sizeof_int
    ofc_type_alignment=$ac_cv_sizeof_int
    ofc_c_type=ompi_fortran_bogus_type_t

    # Only check if we actually want the F77 bindings / have a F77
    # compiler.  This allows us to call this macro, even if there is
    # no F77 compiler.  If we have no f77 compiler, then just set a
    # bunch of defaults.
    if test "$OMPI_WANT_F77_BINDINGS" = "1"; then
        OMPI_F77_CHECK_TYPE([$1], [ofc_have_type=1], [ofc_have_type=0])
    else
        AC_MSG_CHECKING([if Fortran 77 compiler supports $1])
        AC_MSG_RESULT([skipped])
    fi

    if test "$ofc_have_type" = "1"; then
        # What is the size of this type?  

        # NOTE: Some Fortran compilers actually will return that a
        # type exists even if it doesn't support it -- the compiler
        # will automatically convert the unsupported type to a type
        # that it *does* support.  For example, if you try to use
        # INTEGER*16 and the compiler doesn't support it, it may well
        # automatically convert it to INTEGER*8 for you (!).  So we
        # have to check the actual size of the type once we determine
        # that the compiler doesn't error if we try to use it
        # (i.e,. the compiler *might* support that type).  If the size
        # doesn't match the expected size, then the compiler doesn't
        # really support it.
        OMPI_F77_GET_SIZEOF([$1], [ofc_type_size])
        if test "$ofc_expected_size" != "-1" -a "$ofc_type_size" != "$ofc_expected_size"; then
            AC_MSG_WARN([*** Fortran 77 $1 does not have expected size!])
            AC_MSG_WARN([*** Expected $ofc_expected_size, got $ofc_type_size])
            AC_MSG_WARN([*** Disabling MPI support for Fortran 77 $1])
            ofc_have_type=0
        else
            # Look for a corresponding C type (will abort by itself if the
            # type isn't found and we need it)
            ofc_c_type=
            m4_ifval([$3], [OMPI_FIND_TYPE([$1], [$3], [$2], [$ofc_type_size], [ofc_c_type])
            if test -z "$ofc_c_type" ; then
                ofc_have_type=0
            fi])

            # Get the alignment of the type
            if test "$ofc_have_type" = "1"; then
                OMPI_F77_GET_ALIGNMENT([$1], [ofc_type_alignment])
            fi
        fi
    fi

    # We always need these defines -- even if we don't have a given type,
    # there are some places in the code where we have to have *something*.
    AC_DEFINE_UNQUOTED([OMPI_HAVE_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]),
                       [$ofc_have_type], 
                       [Whether we have Fortran 77 $1 or not])
    AC_DEFINE_UNQUOTED([OMPI_SIZEOF_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]),
                       [$ofc_type_size], 
                       [Size of Fortran 77 $1])
    AC_DEFINE_UNQUOTED([OMPI_ALIGNMENT_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]),
                       [$ofc_type_alignment], 
                       [Alignment of Fortran 77 $1])
    if test "$3" != ""; then
        AC_DEFINE_UNQUOTED([ompi_fortran_]m4_translit(m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]), [A-Z], [a-z])[_t],
                           [$ofc_c_type], 
                           [C type corresponding to Fortran 77 $1])
    fi

    # Save some in shell variables for later use (e.g., need
    # OMPI_SIZEOF_FORTRAN_INTEGER in OMPI_F77_GET_FORTRAN_HANDLE_MAX)
    [OMPI_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_])[_C_TYPE=$ofc_c_type]
    [OMPI_HAVE_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_])[=$ofc_have_type]
    [OMPI_SIZEOF_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_])[=$ofc_type_size]
    [OMPI_ALIGNMENT_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_])[=$ofc_type_alignment]

    # Clean up
    OMPI_VAR_SCOPE_POP
])dnl
