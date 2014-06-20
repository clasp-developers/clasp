#include <stdio.h>

int main() {
  int i;

  #pragma pomp inst init

  printf("+++ sequential1\n");

  #pragma pomp inst begin(phase1)

    #pragma omp parallel for
    for (i=0; i<4; ++i) { printf("+++ pdo %d\n", i); }

  #pragma pomp inst end(phase1)

  printf("+++ sequential2\n");

  #pragma omp parallelsections
  {
    #pragma omp section
    printf("+++ psection 1\n");
    #pragma omp section
    printf("+++ psection 2\n");
  }

  printf("+++ sequential3\n");
  return 0;
}
