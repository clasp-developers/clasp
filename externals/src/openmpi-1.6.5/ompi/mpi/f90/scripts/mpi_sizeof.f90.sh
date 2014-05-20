#! /bin/sh
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

. "$1/fortran_kinds.sh"

procedure='MPI_Sizeof'

rank=0

proc="${procedure}${rank}DCH"
echo "subroutine ${proc}(x, size, ierr)"
echo "  implicit none"
echo "  include 'fortran_sizes.h'"
echo "  character, intent(in) :: x"
echo "  integer, intent(out) :: size"
echo "  integer, intent(out) :: ierr"
echo "  size = OMPI_SIZEOF_F90_CHARACTER"
echo "  ierr = 0"
echo "end subroutine ${proc}"
echo

proc="${procedure}${rank}DL"
echo "subroutine ${proc}(x, size, ierr)"
echo "  implicit none"
echo "  include 'fortran_sizes.h'"
echo "  logical, intent(in) :: x"
echo "  integer, intent(out) :: size"
echo "  integer, intent(out) :: ierr"
echo "  size = OMPI_SIZEOF_F90_LOGICAL"
echo "  ierr = 0"
echo "end subroutine ${proc}"
echo

for kind in $ikinds
do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  integer*${kind}, intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_INT${kind}"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo
done

for kind in $rkinds
do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  real*${kind}, intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_REAL${kind}"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo
done

for kind in $ckinds
do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  complex*${kind}, intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_COMPLEX${kind}"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim='*'  ;  esac
  case "$rank" in  2)  dim='1,*'  ;  esac
  case "$rank" in  3)  dim='1,1,*'  ;  esac
  case "$rank" in  4)  dim='1,1,1,*'  ;  esac
  case "$rank" in  5)  dim='1,1,1,1,*'  ;  esac
  case "$rank" in  6)  dim='1,1,1,1,1,*'  ;  esac
  case "$rank" in  7)  dim='1,1,1,1,1,1,*'  ;  esac

    proc="${procedure}${rank}DCH"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  character, dimension(${dim}), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_CHARACTER"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo

    proc="${procedure}${rank}DL"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  logical, dimension(${dim}), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_LOGICAL"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  integer*${kind}, dimension(${dim}), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_INT${kind}"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  real*${kind}, dimension(${dim}), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_REAL${kind}"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  complex*${kind}, dimension(${dim}), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_COMPLEX${kind}"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo
