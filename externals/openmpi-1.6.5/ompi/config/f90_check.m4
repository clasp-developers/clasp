dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006      Cisco Systems, Inc.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_F90_CHECK(Fortran type, expected size)
#----------------------------------------------------------------------------
# Check Fortran type/kind combinations, including:
# - whether compiler supports or not
# - size of type
# - equal to expected size
# - range (optional)
# - precision (optional)

# Note that we do *not* check for the alignment here.  This is a long,
# sordid tale.

# We have been unable to devise a F90 test that will result in a
# consistent answer.  Specifically, our prior tests have been similar
# to the f77 test -- have a small chunk of f90 code compiled with the
# C code to actually compute the offsets.  The f90 code was a
# struct-like entity (a "type") with multiple members -- on a
# character and the other of the target type.  The C code measured the
# distance between them.  But even if you use the keyword to ensure
# that the F90 compiler does not re-order this struct, you may still
# get a different alignment answer than the F77 test (!).  This is
# apparently because F90 allows compilers to align types differently
# according to use (in common blocks, as standalone variables, and as
# a member of a struct).  Hence, the alignment can be different
# depending on how to measure (and use) it.  This was confirmed by
# various members of the Fortran committee and several Fortran
# compiler vendors.

# We check the F77 alignment based on common block usage, but this is
# only one of the available types for F90.  Hence, we may actually get
# a different answer between f77 and f90 in the same compiler series
# (and some compilers do!  E.g., g95 gives different answers even when
# "g95" itself is used as both the f77 and f90 compiler).

# So we gave up.

# Additionally, this was really only a sanity check anyway, because
# the way out F90 MPI layer is organized, there is no translation
# between the data and datatypes performed -- we simply invoke the F77
# layer from the F90 layer.  Hence, we make no distinction between
# them, and therefore the OMPI DDT engine uses only the F77 sizes and
# alignments.  So rather than display a warning to the user for a test
# that was questionable at best, we just eliminated the F90 alignment
# test, corresponding F77 comparison, and ensuring warning -- because
# it really didn't mean anything anyway.

# types to search is a comma-seperated list of values
AC_DEFUN([OMPI_F90_CHECK], [
    ofc_fortran_type="$1"
    ofc_expected_size="$2"

    ofc_have_type=0
    ofc_type_size=$ac_cv_sizeof_int

    # Only check if we actually want the F90 bindings / have a F90
    # compiler.  This allows us to call this macro even if there is
    # no F90 compiler.  If we have no f90 compiler, then just set a
    # bunch of defaults.
    if test "$OMPI_WANT_F90_BINDINGS" = "1"; then
        OMPI_F90_CHECK_TYPE([$1], [ofc_have_type=1], [ofc_have_type=0])
    else
        AC_MSG_CHECKING([if Fortran 90 compiler supports $ofc_fortran_type])
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
        OMPI_F90_GET_SIZEOF([$1], [ofc_type_size])
        if test "$ofc_expected_size" != "" -a "$ofc_type_size" != "$ofc_expected_size"; then
            AC_MSG_WARN([*** Fortran 90 $ofc_fortran_type does not have expected size!])
            AC_MSG_WARN([*** Expected $ofc_expected_size, got $ofc_type_size])
            AC_MSG_WARN([*** Disabling MPI support for Fortran $ofc_fortran_type])
            ofc_have_type=0
        else
            # If this type has an F77 counterpart, see if it's
            # supported.
            [ofc_f77_have_type=$OMPI_HAVE_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_])
            if test "$ofc_f77_have_type" = "0"; then
                AC_MSG_WARN([*** Corresponding Fortran 77 type ($1) not supported])
                AC_MSG_WARN([*** Skipping Fortran 90 type ($1)])
            else

                # Check the size of this type against its F77 counterpart
                [ofc_f77_sizeof=$OMPI_SIZEOF_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_])
                if test "$ofc_f77_sizeof" != ""; then
                    AC_MSG_CHECKING([if Fortran 77 and 90 type sizes match])
                    if test "$ofc_f77_sizeof" != "$ofc_type_size"; then
                        AC_MSG_RESULT([no])
                        AC_MSG_WARN([*** Fortran 77 size for $1 ($ofc_f77_sizeof) does not match])
                        AC_MSG_WARN([*** Fortran 90 size for $1 ($ofc_type_size)])
                        AC_MSG_ERROR([*** Cannot continue])
                    else
                        AC_MSG_RESULT([yes])
                    fi
                fi

                # If we passed in the expected size, then also add the
                # type to the relevant list of types found.
                if test "$ofc_expected_size" != ""; then
                    ofc_letter=m4_translit(m4_bpatsubst($1, [^\(.\).+], [[\1]]), [a-z], [A-Z])
                    ofc_str="OMPI_F90_${ofc_letter}KINDS=\"\$OMPI_F90_${ofc_letter}KINDS $ofc_type_size \""
                    eval $ofc_str
                fi
            fi
        fi
    fi

    # We always need these defines -- even if we don't have a given
    # type, there are some places in the code where we have to have
    # *something*.  Note that the bpatsubst's are the same as used
    # above (see comment above), but we added a translit to make them
    # uppercase.

    # If we got a pretty name, use that as the basis.  If not, use the
    # first part of the provided fortran type (e.g.,
    # "logical(selected_int_kind(2))" -> logical1")

    # Note that there is no need to AC_DEFINE the size of the F90
    # datatype.  We have ensured (above) that they are the same as the
    # corresponding F77 datatypes, and that's good enough (i.e., the
    # DDT engine only looks at the F77 sizes).

    # Finally, note that it is necessary to use the Big Long Ugly m4
    # expressions in the AC_DEFINE_UNQUOTEDs.  If you don't (e.g., put
    # the result of the BLUm4E in a shell variable and use that in
    # AC_DEFINE_UNQUOTED), autoheader won't put them in the
    # AC_CONFIG_HEADER (or AM_CONFIG_HEADER, in our case).

    AC_DEFINE_UNQUOTED([OMPI_HAVE_F90_]m4_translit(m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]), [a-z], [A-Z]),
                       [$ofc_have_type], 
                       [Whether we have Fortran 90 $ofc_fortran_type or not])

    # Save some in shell variables for later use.  Have to use m4
    # functions here (vs. $ompi_upper_var_name, defined above) because
    # these need to be set at autoconf time, not configure time.
    [OMPI_SIZEOF_F90_]m4_translit(m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]), [a-z], [A-Z])[=$ofc_type_size]
    AC_DEFINE_UNQUOTED([OMPI_SIZEOF_F90_]m4_translit(m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]), [a-z], [A-Z]),
                       [$ofc_type_size],
                       [Size of Fortran 77 $ofc_fortran_type])
    AC_SUBST([OMPI_SIZEOF_F90_]m4_translit(m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]), [a-z], [A-Z]))

    # Clean up
    unset ofc_fortran_type ofc_expected_size ofc_want_range ofc_pretty_name
    unset ofc_have_type ofc_type_size ofc_letter ofc_str
    unset ofc_type_range ofc_type_precision
    unset ofc_f77_sizeof 
])dnl
