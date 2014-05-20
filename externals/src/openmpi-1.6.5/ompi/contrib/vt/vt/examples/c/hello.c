#include <stdio.h>
#include <mpi.h>

#ifdef MANUAL
#include "vt_user.h"
#endif

int
main(int argc, char *argv[])
{
  int rank, size;

#if (defined(MANUAL))
  VT_USER_START("main");
#endif

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  printf("Hello from process %i of %i !\n", rank, size);

  MPI_Finalize();

#ifdef MANUAL
   VT_USER_END("main");
#endif

  return 0;
}

