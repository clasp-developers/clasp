/* -*- C -*-
 *
 * $HEADER$
 *
 * A program that just spins - provides mechanism for testing user-driven
 * abnormal program termination
 */

#include <stdio.h>
#include "mpi.h"

int main(int argc, char* argv[])
{

    int i;
    double pi;

    MPI_Init(&argc, &argv);

    i = 0;
    while (1) {
        i++;
        pi = i / 3.14159256;
        if (i > 100) i = 0;
    }

    MPI_Finalize();

    return 0;
}
