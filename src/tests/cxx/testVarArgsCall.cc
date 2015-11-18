#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

struct my_va_list {
  va_list args;
};

typedef void *tagged_my_va_list;

uintptr_t bar(uintptr_t n_args, tagged_my_va_list vap) {
  my_va_list *ap = reinterpret_cast<my_va_list *>(((uintptr_t)vap) - 3);
  uintptr_t a = 0;
  for (int i(0); i < n_args; ++i) {
    uintptr_t val = va_arg(ap->args, uintptr_t);
    printf("Argument %d --> %lu\n", i, val);
    a += val;
  }
  va_end(ap->args);
  return a;
}

uintptr_t foo(uintptr_t n_args, ...) {
  my_va_list ap;
  va_start(ap.args, n_args);
  tagged_my_va_list tagged_ptr = reinterpret_cast<tagged_my_va_list>(((uintptr_t)&ap) + 3);
  return bar(n_args, tagged_ptr);
}

int main(int argc, char *argv[]) {
  printf("Running test with bar0 - complete varargs\n");
  uintptr_t res;
  res = foo(5, (uintptr_t)1, (uintptr_t)2, (uintptr_t)3, (uintptr_t)4, (uintptr_t)5);
  printf("Done res: %lu\n", res);
}
