#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

uintptr_t dummy;

uintptr_t bar(va_list *ap) {
  uintptr_t a1 = va_arg(ap, uintptr_t);
  uintptr_t a2 = va_arg(ap, uintptr_t);
  uintptr_t a3 = va_arg(ap, uintptr_t);
  va_end(ap);
  return a1 + a2 + a3;
}

uintptr_t foo(uintptr_t n_args, uintptr_t a1, uintptr_t a2, uintptr_t a3, ...) {
  ++dummy;
  va_list ap;
  va_start(ap, a3);
  return bar(ap);
}

uintptr_t bar0(uintptr_t n_args, ...) {
  ++dummy;
  va_list ap;
  va_start(ap, n_args);
  uintptr_t a1 = va_arg(ap, uintptr_t);
  uintptr_t a2 = va_arg(ap, uintptr_t);
  uintptr_t a3 = va_arg(ap, uintptr_t);
  uintptr_t a4 = va_arg(ap, uintptr_t);
  uintptr_t a5 = va_arg(ap, uintptr_t);
  va_end(ap);
  return a1 + a2 + a3 + a4 + a5;
}

uintptr_t bar1(uintptr_t n_args, uintptr_t x, ...) {
  ++dummy;
  va_list ap;
  va_start(ap, x);
  uintptr_t y = va_arg(ap, uintptr_t);
  uintptr_t z = va_arg(ap, uintptr_t);
  uintptr_t u = va_arg(ap, uintptr_t);
  uintptr_t w = va_arg(ap, uintptr_t);
  va_end(ap);
  return x + y + z + u + w;
}

uintptr_t bar3(uintptr_t n_args, uintptr_t x, uintptr_t y, uintptr_t z, ...) {
  ++dummy;
  va_list ap;
  va_start(ap, z);
  uintptr_t u = va_arg(ap, uintptr_t);
  uintptr_t w = va_arg(ap, uintptr_t);
  va_end(ap);
  return x + y + z + u + w;
}

uintptr_t bar3Only(uintptr_t n_args, uintptr_t x, uintptr_t y, uintptr_t z, ...) {
  ++dummy;
  //  va_list ap;
  //  va_start(ap, z);
  //  va_end(ap);
  return x + y + z;
}

uintptr_t bar5(uintptr_t n_args, uintptr_t x, uintptr_t y, uintptr_t z, uintptr_t u, uintptr_t w, ...) {
  ++dummy;
  va_list ap;
  va_start(ap, w);
  va_end(ap);
  return x + y + z + u + w;
}

uintptr_t bar5none(uintptr_t n_args, uintptr_t x, uintptr_t y, uintptr_t z, uintptr_t u, uintptr_t w, ...) {
  ++dummy;
  return x + y + z + u + w;
}

int main(int argc, char *argv[]) {
  dummy = 0;
  if (argc != 3) {
    printf("Usage:   %s {num} (bar0|bar1|bar3|bar5)\n", argv[0]);
    exit(1);
  }
  uintptr_t steps = atoi(argv[1]);
  uintptr_t mult = 1000000;
  printf("Starting with %lu\n", steps);
  if (strcmp(argv[2], "bar0") == 0) {
    printf("Running test with bar0 - complete varargs\n");
    for (uintptr_t i = 0; i < mult * steps; i++) {
      bar0(5, (uintptr_t)1, (uintptr_t)2, (uintptr_t)3, (uintptr_t)4, (uintptr_t)5);
    }
  } else if (strcmp(argv[2], "bar1") == 0) {
    printf("Running test with bar1 - prefix 1 fixed arguments\n");
    for (uintptr_t i = 0; i < mult * steps; i++) {
      bar1(5, (uintptr_t)1, (uintptr_t)2, (uintptr_t)3, (uintptr_t)4, (uintptr_t)5);
    }
  } else if (strcmp(argv[2], "bar3") == 0) {
    printf("Running test with bar3 - prefix 3 fixed arguments\n");
    for (uintptr_t i = 0; i < mult * steps; i++) {
      bar3(5, (uintptr_t)1, (uintptr_t)2, (uintptr_t)3, (uintptr_t)4, (uintptr_t)5);
    }
  } else if (strcmp(argv[2], "bar5") == 0) {
    printf("Running test with bar5 - prefix 5 fixed arguments\n");
    for (uintptr_t i = 0; i < mult * steps; i++) {
      bar5(5, (uintptr_t)1, (uintptr_t)2, (uintptr_t)3, (uintptr_t)4, (uintptr_t)5);
    }
  } else if (strcmp(argv[2], "bar5none") == 0) {
    printf("Running test with bar5none - prefix 5 fixed arguments - no varargs\n");
    for (uintptr_t i = 0; i < mult * steps; i++) {
      bar5none(5, (uintptr_t)1, (uintptr_t)2, (uintptr_t)3, (uintptr_t)4, (uintptr_t)5);
    }
  } else if (strcmp(argv[2], "barComplex") == 0) {
    printf("Running test with barComplex - prefix 3 fixed arguments - setup varargs\n");
    for (uintptr_t i = 0; i < mult * steps; i++) {
      barComplex(5, (uintptr_t)1, (uintptr_t)2, (uintptr_t)3);
    }
  } else if (strcmp(argv[2], "bar03") == 0) {
    printf("Running test with bar03 - prefix 0 fixed arguments - use varargs\n");
    for (uintptr_t i = 0; i < mult * steps; i++) {
      bar03(5, (uintptr_t)1, (uintptr_t)2, (uintptr_t)3);
    }
  } else if (strcmp(argv[2], "bar3Only") == 0) {
    printf("Running test with bar3Only - prefix 3 fixed arguments - dont use varargs\n");
    for (uintptr_t i = 0; i < mult * steps; i++) {
      bar3Only(5, (uintptr_t)1, (uintptr_t)2, (uintptr_t)3);
    }
  }
  printf("Done\n");
}
