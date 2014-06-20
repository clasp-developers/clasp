!
! Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
! 
! Additional copyrights may follow
! 
! $HEADER$
!

! Note about these declarations: these are "external" functions in
! mpif-common.h.  However, if we don't declare them here, compilers will add
! them to the "mpi" module namespace, and result in linker errors if MPI
! F90 applications try to use them.  because the implementations of
! these functions are not in the MPI module namespace -- they're the F77
! functions.

interface

    subroutine mpi_conversion_fn_null(userbuf, datatype, count, filebuf, &
         position, extra_state, ierror)
      include 'mpif-config.h'
      character(len=*), intent(in) :: filebuf
      character(len=*), intent(out) :: userbuf
      integer, intent(in) :: datatype, count, ierror
      integer(kind=MPI_OFFSET_KIND), intent(in) :: position
      integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
    end subroutine mpi_conversion_fn_null

end interface
