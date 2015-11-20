#include "stdio.h"

typedef long int Fixnum;

int main(int argc, const char *argv[]) {
  int x1 = 1;
  int xm1 = -1;
  Fixnum fx1 = x1;
  Fixnum fxm1 = xm1;
  long int mostPositiveFixnum = 4611686018427387903 long int mostNegativeFixnum = -4611686018427387904;
  printf("Hello world sizeof(Fixnum): %lu\n", sizeof(Fixnum));
  printf(" fx1: %ld 0x%lX\n", fx1, fx1);
  printf("fxm1: %ld 0x%lX\n", fxm1, fxm1);
  printf("fxm1>>1: %ld 0x%lX\n", fxm1 >> 1, fxm1 >> 1);
  printf("fxm1<<1: %ld 0x%lX\n", fxm1 << 1, fxm1 << 1);
    printf("
}
