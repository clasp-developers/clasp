!
! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
!
! Simple ring test program
!
program ring
  use mpi
  implicit none
  integer :: rank, size, tag, next, from, message, ierr

! Start up MPI

  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

! Calculate the rank of the next process in the ring.  Use the modulus
! operator so that the last process "wraps around" to rank zero.

  tag = 201
  next = mod((rank + 1), size)
  from = mod((rank + size - 1), size)
      
! If we are the "master" process (i.e., MPI_COMM_WORLD rank 0), put
! the number of times to go around the ring in the message.

  if (rank .eq. 0) then
     message = 10

     print *, 'Process 0 sending ', message, ' to ', next, ' tag ', tag, ' (', size, ' processes in ring)'
     call MPI_SEND(message, 1, MPI_INTEGER, next, tag, MPI_COMM_WORLD, ierr)
     print *, 'Process 0 sent to ', next
  endif

! Pass the message around the ring.  The exit mechanism works as
! follows: the message (a positive integer) is passed around the ring.
! Each time it passes rank 0, it is decremented.  When each processes
! receives a message containing a 0 value, it passes the message on to
! the next process and then quits.  By passing the 0 message first,
! every process gets the 0 message and can quit normally.

10 call MPI_RECV(message, 1, MPI_INTEGER, from, tag, MPI_COMM_WORLD, &
        MPI_STATUS_IGNORE, ierr)

  if (rank .eq. 0) then
     message = message - 1
     print *, 'Process 0 decremented value:', message
  endif

  call MPI_SEND(message, 1, MPI_INTEGER, next, tag, MPI_COMM_WORLD, ierr)
      
  if (message .eq. 0) then
     print *, 'Process ', rank, ' exiting'
     goto 20
  endif
  goto 10

! The last process does one extra send to process 0, which needs to be
! received before the program can exit

 20 if (rank .eq. 0) then
     call MPI_RECV(message, 1, MPI_INTEGER, from, tag, MPI_COMM_WORLD, &
          MPI_STATUS_IGNORE, ierr)
  endif

! All done

  call MPI_FINALIZE(ierr)
end program
      
