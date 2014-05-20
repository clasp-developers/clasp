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
    MPI_Comm parent;
    int msg;

    printf("Slave [pid %ld] starting up!\n", (long)getpid());

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Comm_get_parent(&parent);
    
    MPI_Recv(&msg, 1, MPI_INT, 0, 1, parent, MPI_STATUS_IGNORE);
    printf("Slave %d received msg: %d\n", rank, msg);
    MPI_Comm_disconnect(&parent);

    MPI_Finalize();
    return 0;
}
