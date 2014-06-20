/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include "mpi.h"

int main(int argc, char* argv[])
{
    int rank, size;
    MPI_Info info, srch;
    char port[MPI_MAX_PORT_NAME];

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    printf("Hello, World, I am %d of %d\n", rank, size);
    
    MPI_Info_create(&info);
    MPI_Info_set(info, "ompi_global_scope", "true");
    
    if (0 == rank) {
        MPI_Open_port(MPI_INFO_NULL, port);
        MPI_Publish_name("pubsub-test", info, port);
        printf("Rank %d published port %s\n", rank, port);
    }
    
    MPI_Barrier(MPI_COMM_WORLD);
    
    MPI_Info_create(&srch);
    MPI_Info_set(srch, "ompi_lookup_order", "local,global");
    if (rank != 0) {
        MPI_Lookup_name("pubsub-test", srch, port);
        printf("Rank %d got port %s\n", rank, port);
    }
    
    MPI_Barrier(MPI_COMM_WORLD);
    
    if (0 == rank) {
        MPI_Unpublish_name("pubsub-test", info, port);
    }
    MPI_Info_free(&info);
    MPI_Info_free(&srch);
    MPI_Finalize();
    return 0;
}
