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
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_F90_FIND_MODULE_INCLUDE_FLAG],[
    AS_VAR_PUSHDEF([f90_inc_var], [ompi_cv_f90_module_include_flag])
    
    if test "$OMPI_WANT_F90_BINDINGS" -eq 1 ; then
        AC_CACHE_CHECK([for Fortran 90 compiler module include flag], 
           f90_inc_var,
           [ofi_possible_flags="-I -p -M"
            mkdir conftest.$$
            cd conftest.$$

            # Try to compile an F90 module
            mkdir subdir
            cd subdir
            cat > conftest-module.f90 <<EOF
module OMPI_MOD_FLAG

  type OMPI_MOD_FLAG_TYPE
    integer :: i
  end type OMPI_MOD_FLAG_TYPE

end module OMPI_MOD_FLAG
EOF

            OMPI_LOG_COMMAND([$FC $FCFLAGS $FCFLAGS_f90 -c conftest-module.f90 $LDFLAGS $LIBS], , 
                [cd  ..
                 rm -rf conftest.$$
                 AC_MSG_RESULT([Whoops!])
                 AC_MSG_WARN([*** Cannot seem to compile an f90 module])
                 AC_MSG_ERROR([Cannot continue])])
            cd ..

            #
            # Now try to compile a simple program usinng that module, iterating
            # through the possible flags that the compiler might use
            #

            cat > conftest.f90 <<EOF
program f90usemodule
  use OMPI_MOD_FLAG
end program f90usemodule
EOF

            ofi_module_flag=
            for flag in $ofi_possible_flags; do
                if test "$ofi_module_flag" = ""; then
                    OMPI_LOG_COMMAND([$FC $FCFLAGS $FCFLAGS_f90 conftest.f90 ${flag}subdir $LDFLAGS $LIBS],
                            [AS_VAR_SET(f90_inc_var, [$flag])
                            ofi_module_flag="$flag"])
                fi
            done
            cd ..
            rm -rf conftest.$$
        ])

        AS_VAR_COPY([OMPI_FC_MODULE_FLAG], [f90_inc_var])
        if test "$OMPI_FC_MODULE_FLAG" = ""; then
        AC_MSG_WARN([*** Could not determine the f90 compiler flag to indicate where modules reside])
        AC_MSG_ERROR([*** Cannot continue])
        fi
    else
        OMPI_FC_MODULE_FLAG=
    fi
    AC_SUBST(OMPI_FC_MODULE_FLAG)
    AS_VAR_POPDEF([f90_inc_var])
])dnl
