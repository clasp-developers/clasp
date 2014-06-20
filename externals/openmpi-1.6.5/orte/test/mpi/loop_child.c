#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>
#include "mpi.h"


int main( int argc, char **argv ) 
{
    MPI_Comm parent;
    MPI_Comm merged;
    int rank;
    int size;

    MPI_Init(&argc, &argv);   
    printf("Child: launch\n");
    MPI_Comm_get_parent(&parent);   
    MPI_Intercomm_merge(parent, 1, &merged);
    MPI_Comm_rank(merged, &rank);
    MPI_Comm_size(merged, &size);
    printf("Child merged rank = %d, size = %d\n", rank, size);
   
    MPI_Comm_free(&merged);
    MPI_Finalize();
    printf("Child %d: exiting\n", (int)getpid());
    return 0;
}
