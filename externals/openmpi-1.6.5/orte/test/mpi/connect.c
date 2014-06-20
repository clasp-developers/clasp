/* -*- C -*-
 *
 * $HEADER$
 *
 * Test of connect/accept - the accept (server) side
 */

#include <stdio.h>
#include "mpi.h"

int main(int argc, char* argv[])
{
    int rank, size;
    MPI_Comm server;
    MPI_Info info;
    char port[MPI_MAX_PORT_NAME];
    
    MPI_Init(&argc, &argv);
    
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    
    printf("Hello, World, I am %d of %d\n", rank, size);
    
    MPI_Info_create(&info);
    MPI_Info_set(info, "ompi_global_scope", "true");
    
    MPI_Lookup_name("test-pub", info, port);
    MPI_Comm_connect(port, MPI_INFO_NULL, 0, MPI_COMM_WORLD, &server);
  
    MPI_Barrier(server);

    MPI_Finalize();
    return 0;
}
