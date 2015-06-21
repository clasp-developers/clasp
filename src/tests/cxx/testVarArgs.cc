#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

long long int dummy;
int bar0(int n_args, ...) {
  ++dummy;
  va_list ap;
  va_start(ap, n_args);
  int x = va_arg(ap, int);
  int y = va_arg(ap, int);
  int z = va_arg(ap, int);
  int u = va_arg(ap, int);
  int w = va_arg(ap, int);
  va_end(ap);
  return x + y + z + u + w;
}

int bar1(int n_args, int x, ...) {
  ++dummy;
  va_list ap;
  va_start(ap, x);
  int y = va_arg(ap, int);
  int z = va_arg(ap, int);
  int u = va_arg(ap, int);
  int w = va_arg(ap, int);
  va_end(ap);
  return x + y + z + u + w;
}

int bar3(int n_args, int x, int y, int z, ...) {
  ++dummy;
  va_list ap;
  va_start(ap, z);
  int u = va_arg(ap, int);
  int w = va_arg(ap, int);
  va_end(ap);
  return x + y + z + u + w;
}


int bar5(int n_args, int x, int y, int z, int u, int w, ...) {
  ++dummy;
  va_list ap;
  va_start(ap, w);
  va_end(ap);
  return x + y + z + u + w;
}

int bar5none(int n_args, int x, int y, int z, int u, int w,...) {
  ++dummy;
  return x + y + z + u + w;
}

int main(int argc, char *argv[]) {
  dummy = 0;
  if (argc != 3) {
    printf("Usage:   %s {num} (bar0|bar1|bar3|bar5)\n", argv[0]);
    exit(1);
  }
  long long int steps = atoi(argv[1]);
  long long int mult = 1000000;
  printf("Starting with %lld\n", steps);
  if (strcmp(argv[2], "bar0") == 0) {
    printf("Running test with bar0 - complete varargs\n");
    for (long long int i = 0; i < mult * steps; i++) {
      bar0(5, 1, 2, 3, 4, 5);
    }
  } else if (strcmp(argv[2], "bar1") == 0 ) {
    printf("Running test with bar1 - prefix 1 fixed arguments\n");
    for (long long int i = 0; i < mult * steps; i++) {
      bar1(5, 1, 2, 3, 4, 5);
    }
  } else if (strcmp(argv[2], "bar3") == 0) {
    printf("Running test with bar3 - prefix 3 fixed arguments\n");
    for (long long int i = 0; i < mult * steps; i++) {
      bar3(5, 1, 2, 3, 4, 5);
    }
  } else if (strcmp(argv[2], "bar5") == 0) {
    printf("Running test with bar5 - prefix 5 fixed arguments\n");
    for (long long int i = 0; i < mult * steps; i++) {
      bar5(5, 1, 2, 3, 4, 5);
    }
  } else if (strcmp(argv[2], "bar5none") == 0) {
    printf("Running test with bar5none - prefix 5 fixed arguments - no varargs\n");
    for (long long int i = 0; i < mult * steps; i++) {
      bar5none(5, 1, 2, 3, 4, 5);
    }
  }
  printf("Done\n");
}
