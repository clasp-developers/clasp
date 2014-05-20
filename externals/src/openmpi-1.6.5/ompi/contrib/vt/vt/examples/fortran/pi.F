#ifdef MANUAL
#include "vt_user.inc"
#endif

      program calcpi

      include 'mpif.h'

      double precision  PI25DT
      parameter        (PI25DT = 3.141592653589793238462643d0)

      double precision  mypi, pi, h, sum, x, f, a
      integer num_iters, rank, size, i, rc

c     Function to integrate

      f(a) = 4.d0 / (1.d0 + a * a)

#ifdef MANUAL
      VT_USER_START('calcpi')
#endif

c     Normal MPI startup

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
      print *, "Process ", rank, " of ", size, " is alive"

c     Loop until finished

      num_iters = 1000
      do 20 iter = 2, num_iters

#ifdef MANUAL
      VT_USER_START('outer_loop');
#endif

c     Calculate the interval size

      h = 1.0d0 / iter
      sum  = 0.0d0

      do 10 i = rank + 1, iter, size
#ifdef MANUAL
         VT_USER_START('inner_loop');
#endif
         x = h * (dble(i) - 0.5d0)
         sum = sum + f(x)
#ifdef MANUAL
         VT_USER_END('inner_loop');
#endif
 10   continue
      mypi = h * sum

c     Collect all the partial sums

      call MPI_REDUCE(mypi, pi, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0,
     $     MPI_COMM_WORLD, ierr)

c     Node 0 prints the answer.

      if (rank .eq. 0) then
         write(6, 97) iter, pi, abs(pi - PI25DT)
 97      format(i3, ' points: pi is approximately: ', F19.16,
     +          ' error is: ', F19.16)
      endif

#ifdef MANUAL
      VT_USER_END('outer_loop');
#endif

 20   continue

c     All finished

      call MPI_FINALIZE(rc)

#ifdef MANUAL
      VT_USER_END('calcpi')
#endif

      stop
      end
