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
    MPI_Init(&argc, &argv);

    MPI_Finalize();
    return 0;
}
