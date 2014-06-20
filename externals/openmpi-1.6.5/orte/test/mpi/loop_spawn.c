/*file .c : spawned  the file Exe*/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "mpi.h"
#include <pthread.h>
#include <signal.h> 
#include <sys/time.h>
#include <errno.h>
#define     EXE_TEST             "./loop_child"


int main(int argc, char **argv)
{
    int iter, err, rank, size;
    MPI_Comm comm, merged;

    /* MPI environnement */    

    printf("parent*******************************\n");
    printf("parent: Launching MPI*\n");

    MPI_Init( &argc, &argv);

    for (iter = 0; iter < 1000; ++iter) {
        MPI_Comm_spawn(EXE_TEST, NULL, 1, MPI_INFO_NULL,
                       0, MPI_COMM_WORLD, &comm, &err);
        printf("parent: MPI_Comm_spawn #%d return : %d\n", iter, err);

        MPI_Intercomm_merge(comm, 0, &merged);
        MPI_Comm_rank(merged, &rank);
        MPI_Comm_size(merged, &size);
        printf("parent: MPI_Comm_spawn #%d rank %d, size %d\n", 
               iter, rank, size);
        MPI_Comm_free(&merged);
    }

    MPI_Finalize();
    printf("parent: End .\n" );
    return 0;
}
