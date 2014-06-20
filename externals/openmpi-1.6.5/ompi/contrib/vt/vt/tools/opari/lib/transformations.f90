      module xxx
      contains
        subroutine writelog(msg, val)
          character(len=*), intent(in)  :: msg
          integer, optional, intent(in) :: val
          integer, external             :: omp_get_thread_num

          if ( present(val) ) then
            write(*,*) "---", omp_get_thread_num(), ": ", msg, val
          else
            write(*,*) "---", omp_get_thread_num(), ": ", msg
          endif
          return
        end subroutine writelog
      end module xxx

      program trans
      use xxx
      integer, parameter :: iterations = 4
      integer            :: i, k = 0
      integer            :: lck
      logical, external  :: omp_test_lock

      !$omp inst init

      call writelog("sequential", 0);

      ! ---- plain parallel region
      !$omp parallel
        call writelog("parallel", 0);
      !$omp end parallel
 
      call writelog("sequential", 1)

      ! ---- parallel region
      !$omp parallel
        call writelog("parallel", 1)

        !$omp inst begin(worksharing)

        ! ---- worksharing for loop without synchronisation
        !$omp do
        do i=1,iterations
          call writelog("for nowait iteration", i)
        enddo
        !$omp end do nowait

        ! ---- user specified barrier
        !$omp barrier

        ! ---- worksharing for loop with implicit synchronisation
        !$omp do
        do i=1,iterations
          call writelog("for iteration", i)
        enddo
        !$omp end do
 
        ! ---- worksharing tasks without synchronisation
        !$omp sections
          !$omp section
            call writelog("section nowait", 1)
          !$omp section
            call writelog("section nowait", 2)
        !$omp end sections nowait

        ! ---- worksharing tasks with implicit synchronisation
        !$omp sections
          !$omp section
            call writelog("section", 1)
          !$omp section
            call writelog("section", 2)
        !$omp end sections

        !$omp inst end(worksharing)

        !$omp inst begin(synchronisation)

        ! ---- critical section
        !$omp critical
          call writelog("critical\n")
          k = k+1
        !$omp end critical

        ! ---- named critical section
        !$omp critical(kincr)
          call writelog("critical\n")
          k = k+1
        !$omp end critical(kincr)

        ! ---- atomic expression
        !$omp atomic
          k = k+1

        ! ---- update k just once without synchronisation
        !$omp single
          call writelog("single nowait\n")
          k = k+1
        !$omp end single nowait

        ! ---- update k just once with implicit synchronisation
        !$omp single
          call writelog("single\n")
          k = k+1
        !$omp end single

        !$omp master
          call writelog("master\n")
          write(*,*) "k = ", k
          k = 0
        !$omp end master

        !$omp inst end(synchronisation)
      !$omp end parallel

      call writelog("sequential", 2)

      !$omp inst begin(parallelworksharing)

      ! ---- combined parallel worksharing for loop
      !$omp parallel do         &
      !$omp   reduction(+:k)    &
      !$omp   private(i)        &
      !$omp   schedule(dynamic)
      do i=1,iterations
        call writelog("pfor", i)
      enddo
      !$omp end parallel do

      call writelog("sequential", 3)

      ! ---- combined parallel worksharing tasks
      !$omp parallel sections
        !$omp section
          call writelog("psection", 1)
        !$omp section
          call writelog("psection", 2)
      !$omp end parallel sections

      call writelog("sequential", 4)

      !$omp inst end(parallelworksharing)

      ! ---- OpenMP locking API
      !$omp inst begin(locking)


      call omp_init_lock(lck)

      !$omp parallel shared(lck)
        call omp_set_lock(lck)
        call writelog("got lock")
        call omp_unset_lock(lck)

        do while (.not. omp_test_lock(lck))
          call writelog("skipping")
        enddo
        call writelog("working")
        call omp_unset_lock(lck)
      !$omp end parallel

      call omp_destroy_lock(lck)

      !$omp flush(k)
      !$omp inst end(locking)

      end program trans
