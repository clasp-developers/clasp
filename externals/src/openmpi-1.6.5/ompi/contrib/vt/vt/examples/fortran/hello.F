#ifdef MANUAL
#include "vt_user.inc"
#endif

      program hello

      include 'mpif.h'

      integer rank, size

#ifdef MANUAL
      VT_USER_START('hello')
#endif

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

      print *, "Hello from process ", rank, " of ", size, " !"

      call MPI_FINALIZE(rc)

#ifdef MANUAL
      VT_USER_END('hello')
#endif

      stop
      end
