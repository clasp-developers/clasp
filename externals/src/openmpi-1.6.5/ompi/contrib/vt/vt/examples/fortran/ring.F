C
C Simple ring test program
C

#ifdef MANUAL
#include "vt_user.inc"
#endif

      program ring
      implicit none
      include 'mpif.h'
      integer rank, size, tag, next, from, message, ierr
      integer status(MPI_STATUS_SIZE)

#ifdef MANUAL
      VT_USER_START('ring')
#endif

C     Start up MPI */

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

C     Calculate the rank of the next process in the ring.  Use the
C     modulus operator so that the last process "wraps around" to rank
C     zero.

      tag = 4711
      next = mod((rank + 1), size)
      from = mod((rank + size - 1), size)
      
C     If we are the "master" process (i.e., MPI_COMM_WORLD rank 0), put
C     the number of times to go around the ring in the message.

      if (rank .eq. 0) then
         message = 100

         print *, 'Process 0 sending ', message, ' to ', next, ' tag ',
     &        tag, ' (', size, ' processes in ring)'
         call MPI_SEND(message, 1, MPI_INTEGER, next, tag, 
     &        MPI_COMM_WORLD, ierr)
         print *, 'Process 0 sent to ', next
      endif

C     Pass the message around the ring.  The exit mechanism works as
C     follows: the message (a positive integer) is passed around the
C     ring.  Each time it passes rank 0, it is decremented.  When each
C     processes receives a message containing a 0 value, it passes the
C     message on to the next process and then quits.  By passing the 0
C     message first, every process gets the 0 message and can quit
C     normally.

 10   continue
#ifdef MANUAL
      VT_USER_START('ring_loop')
#endif
      call MPI_RECV(message, 1, MPI_INTEGER, from, tag, 
     &     MPI_COMM_WORLD, status, ierr)

      if (rank .eq. 0) then
         message = message - 1
         print *, 'Process 0 decremented value:', message
      endif

      call MPI_SEND(message, 1, MPI_INTEGER, next, tag, 
     &     MPI_COMM_WORLD, ierr)
      
      if (message .eq. 0) then
         print *, 'Process ', rank, ' exiting'
#ifdef MANUAL
      VT_USER_END('ring_loop')
#endif
         goto 20
      endif
#ifdef MANUAL
      VT_USER_END('ring_loop')
#endif
      goto 10

C     The last process does one extra send to process 0, which needs to
C     be received before the program can exit

 20   if (rank .eq. 0) then
         call MPI_RECV(message, 1, MPI_INTEGER, from, tag, 
     &        MPI_COMM_WORLD, status, ierr)
      endif

C     All done

      call MPI_FINALIZE(ierr)

#ifdef MANUAL
      VT_USER_END('ring')
#endif

      end
      
