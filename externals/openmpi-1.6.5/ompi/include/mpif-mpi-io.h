! -*- fortran -*-
!
! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2005 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Do ***not*** copy this file to the directory where your Fortran
! fortran application is compiled unless it is absolutely necessary!  Most
! modern Fortran compilers now support the -I command line flag, which
! tells the compiler where to find .h files (specifically, this one).  For
! example:
!
!      shell$ mpif77 foo.f -o foo -I$OMPI_HOME/include
!
! will probably do the trick (assuming that you have set OMPI_HOME
! properly).
!
! That being said, OMPI's "mpif77" wrapper compiler should
! automatically include the -I option for you.  The following command
! should be equivalent to the command listed above:
!
!      shell$ mpif77 foo.f -o foo
!
! You should not copy this file to your local directory because it is
! possible that this file will be changed between versions of Open MPI.
! Indeed, this mpif.h is incompatible with the mpif.f of other
! implementations of MPI.  Using this mpif.h with other implementations
! of MPI, or with other versions of Open MPI will result in undefined
! behavior (to include incorrect results, segmentation faults,
! unexplainable "hanging" in your application, etc.).  Always use the
! -I command line option instead (or let mpif77 do it for you).
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
!     This file is included as a back-end file to both mpif.h (i.e., the
!     standardized MPI Fortran header file) and a bunch of the MPI
!     Fortran 90 subroutine implementations found in ompi/mpi/f90.
!
!     This file contains the output from configure that is relevant for
!     Fortran applications (both 77 and 90) and a few values that are
!     necessary to compile the F90 module (e.g., MPI_STATUS_SIZE).
!

      integer MPI_FILE_NULL
      integer MPI_SEEK_SET, MPI_SEEK_CUR, MPI_SEEK_END
      integer MPI_MODE_CREATE
      integer MPI_MODE_RDONLY, MPI_MODE_WRONLY, MPI_MODE_RDWR
      integer MPI_MODE_DELETE_ON_CLOSE, MPI_MODE_UNIQUE_OPEN
      integer MPI_MODE_EXCL, MPI_MODE_APPEND, MPI_MODE_SEQUENTIAL
      integer MPI_DISPLACEMENT_CURRENT

      parameter (MPI_FILE_NULL=0)
      parameter (MPI_SEEK_SET=600)
      parameter (MPI_SEEK_CUR=602)
      parameter (MPI_SEEK_END=604)
      parameter (MPI_MODE_CREATE=1)
      parameter (MPI_MODE_RDONLY=2)
      parameter (MPI_MODE_WRONLY=4)
      parameter (MPI_MODE_RDWR=8)
      parameter (MPI_MODE_DELETE_ON_CLOSE=16)
      parameter (MPI_MODE_UNIQUE_OPEN=32)
      parameter (MPI_MODE_EXCL=64)
      parameter (MPI_MODE_APPEND=128)
      parameter (MPI_MODE_SEQUENTIAL=256)
      parameter (MPI_DISPLACEMENT_CURRENT=-54278278)
