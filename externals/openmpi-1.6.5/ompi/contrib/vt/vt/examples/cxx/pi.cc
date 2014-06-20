#include <mpi.h>
#include <iostream>
#include <math.h>

#ifdef MANUAL
#include "vt_user.h"
#endif

#define NUM_ITERS 100000

using namespace std;

static double f(double);

int
main(int argc, char *argv[])
{
  int n = NUM_ITERS;
  int rank, size, i;
  double PI25DT = 3.141592653589793238462643;
  double mypi, pi, h, sum, x;

#ifdef MANUAL
  VT_TRACER("main");
#endif

  MPI::Init(argc, argv);
  size = MPI::COMM_WORLD.Get_size();
  rank = MPI::COMM_WORLD.Get_rank();

  h = 1.0 / (double) n;
  sum = 0.0;
  for (i = rank + 1; i <= n; i += size) {
    x = h * ((double)i - 0.5);
    sum += f(x);
  }
  mypi = h * sum;
    
  // Combine all the partial results
  MPI::COMM_WORLD.Reduce(&mypi, &pi, 1, MPI::DOUBLE, MPI::SUM, 0);
    
  if (rank == 0) {
    cout << "After " << n << " iterations, pi is approximately " 
       << pi << ", Error is " << fabs(pi - PI25DT) << endl;
  }

  MPI::Finalize();

  return 0;
}

static double
f(double a)
{
  double e;

#ifdef MANUAL
  VT_TRACER("f");
#endif

  e = (4.0 / (1.0 + a * a));

  return e;
}

