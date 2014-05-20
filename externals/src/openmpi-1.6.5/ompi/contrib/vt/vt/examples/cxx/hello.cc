#include <mpi.h>
#include <iostream>

#ifdef MANUAL
#include "vt_user.h"
#endif

using namespace std;

int
main(int argc, char *argv[])
{
  int rank, size;

#ifdef MANUAL
  VT_TRACER("main");
#endif

  MPI::Init(argc, argv);
  size = MPI::COMM_WORLD.Get_size();
  rank = MPI::COMM_WORLD.Get_rank();

  cout << "Hello from " << rank << " of " << size << " !" << endl;

  MPI::Finalize();

  return 0;
}

