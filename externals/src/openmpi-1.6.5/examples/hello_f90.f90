!
! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
!
! Sample MPI "hello world" application in Fortran 90
!
program main
    use mpi
    implicit none
    integer :: ierr, rank, size

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
    print *, "Hello, world, I am ", rank, " of ", size
    call MPI_FINALIZE(ierr)
end
