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
! Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
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

!     Include the MPI I/O stuff, if needed
      include "mpif-mpi-io.h"

!
!     OMPI version
!     This file is generated from configure; do not edit it manually.
!
      integer OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION
      integer OMPI_RELEASE_VERSION
      character*32 OMPI_GREEK_VERSION
      character*32 OMPI_SVN_VERSION
      parameter (OMPI_MAJOR_VERSION=1)
      parameter (OMPI_MINOR_VERSION=6)
      parameter (OMPI_RELEASE_VERSION=5)
      parameter (OMPI_GREEK_VERSION="")
      parameter (OMPI_SVN_VERSION="r28673")
!
!     Kind parameters
!
      integer MPI_OFFSET_KIND, MPI_ADDRESS_KIND, MPI_INTEGER_KIND
      parameter (MPI_INTEGER_KIND=0)
      parameter (MPI_ADDRESS_KIND=0)
      parameter (MPI_OFFSET_KIND=0)
!
!     Miscellaneous constants
!
      integer MPI_STATUS_SIZE
      parameter (MPI_STATUS_SIZE=0)
!
!     Configurable length constants
!
      integer MPI_MAX_PROCESSOR_NAME
      integer MPI_MAX_ERROR_STRING
      integer MPI_MAX_OBJECT_NAME
      integer MPI_MAX_INFO_KEY
      integer MPI_MAX_INFO_VAL
      integer MPI_MAX_PORT_NAME
      integer MPI_MAX_DATAREP_STRING
      parameter (MPI_MAX_PROCESSOR_NAME=256-1)
      parameter (MPI_MAX_ERROR_STRING=256-1)
      parameter (MPI_MAX_OBJECT_NAME=64-1)
      parameter (MPI_MAX_INFO_KEY=36-1)
      parameter (MPI_MAX_INFO_VAL=256-1)
      parameter (MPI_MAX_PORT_NAME=1024-1)
      parameter (MPI_MAX_DATAREP_STRING=128-1)
