C
C Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
C                         University Research and Technology
C                         Corporation.  All rights reserved.
C Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
C
C Sample MPI "hello world" application in Fortran 77
C
        program main
        implicit none
        include 'mpif.h'
        integer ierr, rank, size

        call MPI_INIT(ierr)
        call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
        call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
        print *, "Hello, world, I am ", rank, " of ", size
        call MPI_FINALIZE(ierr)

        end
