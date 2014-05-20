#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <mpi.h>

#define RANK_DEATH 1

int main(int argc, char **argv)
{
   int rank;
   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&rank);

   sleep(2);
   if (rank==RANK_DEATH) {
       printf("Rank %d exiting without calling finalize...\n", rank);
       exit(1);
   }
   sleep(2);
   printf("Rank %d calling MPI_Finalize\n", rank);
   MPI_Finalize();
   printf("Rank %d exiting\n", rank);
   return 0;
}

