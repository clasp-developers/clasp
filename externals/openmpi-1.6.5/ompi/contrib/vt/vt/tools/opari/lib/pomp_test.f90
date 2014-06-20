       program test2
       integer i

       !$pomp inst init

       write(*,*) "+++ sequential1"

       !$pomp inst begin(phase1)

       !$omp parallel do
       do i=1,4
         write(*,*) "+++ pdo",i
       enddo
       !$omp end parallel do

       !$pomp inst end(phase1)

       write(*,*) "+++ sequential2"

       !$omp parallelsections
         !$omp section
         write(*,*) "+++ psection 1"
         !$omp section
         write(*,*) "+++ psection 2"
       !$omp end parallelsections

       write(*,*) "+++ sequential3"

       end
