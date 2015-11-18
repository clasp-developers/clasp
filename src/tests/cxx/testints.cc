#include <stdio.h>
#include <stdint.h>

int main(int argc, char *argv[]) {
  printf("sizeof(long int) = %lu\n", sizeof(long int));
  printf("sizeof(long long int) = %lu\n", sizeof(long long int));
  printf("sizeof(unsigned long long int) = %lu\n", sizeof(unsigned long long int));
}
