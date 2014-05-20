/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include <unistd.h>
#include "mpi.h"

int main(int argc, char* argv[])
{
    int rank, size;
    char hostname[512];
    
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    gethostname(hostname, 512);
    printf("%s: I am %d of %d. pid=%d\n", hostname, rank, size, getpid());
    
    if (rank%3 == 0) {
        printf("%s: rank %d aborts\n", hostname, rank);
        if (rank == 3) {
            printf("%s: rank %d is going to sleep\n", hostname, rank);
            sleep(2);
        }
        MPI_Abort(MPI_COMM_WORLD, 2);
        printf("%s: sleeping. You should not see this\n", hostname);
        sleep(100);
    }

    MPI_Finalize();
    return 0;
}
